                        #HYSPLIT Program (prototype)

# Load packages
library(splitr)
library(opentraj)
library(lubridate)


# Creating the list with the days of interest ####
dayList <- c(1:31) # put the days of the month here, without caring about short months
monthList <- c(03:04) # months go here
yearList <- c(1980:2020) #years

dateList <- as.vector(5) # just creating a vector
for(i in 1:length(yearList)) {
    for (j in 1:length(monthList)) {
      for (k in 1:length(dayList)) {
        dateList[length(dayList)*length(monthList)*(i-1)+length(dayList)*(j-1)+k] <- as.character(paste(yearList[i], monthList[j], dayList[k], sep = "-"))
    }
  }
} # this loop generates the dates

dateList <- as.Date(dateList)
dateList <- na.omit(dateList) # change from string to Date objects, then remove NA (i.e. remove impossible dates such as February 31)

# Getting the meteorological files ####
# It seems ProcTraj() requires one meteorological file before and after the limits of what you want to plot for some reason,
# so we download the files for the months previous and next to the ones of interest

# List with the dates of the previous month
prevDates <- seq.Date(as.Date("1980-02-01", "%Y-%m-%d", tz = "CET"), # initial date
                     as.Date("2020-02-01", "%Y-%m-%d", tz = "CET"), # final date
                     by = "year",                                   # interval
                     length.out = NULL)                             # period length

# Get meteorological files:
# I think with the loop is better because if you put the whole list directly in the "days" argument it seems to go over all the months in the middle
# and download unnecessary files
for(i in 1:length(prevDates)) {
  get_met_reanalysis(days = prevDates[i], duration = 12, direction = "forward",
                     path_met_files = "C:/hysplit/working")
}

# List with dates of next month
postDates <- seq.Date(as.Date("1980-05-01", "%Y-%m-%d", tz = "CET"), # initial date
                     as.Date("2020-05-01", "%Y-%m-%d", tz = "CET"), # final date
                     by = "year",                                   # interval
                     length.out = NULL)                             # period length

# Get the files; as before the loop should save time
for(i in 1:length(postDates)) {
  get_met_reanalysis(days = postDates[i], duration = 12, direction = "forward",
                     path_met_files = "C:/hysplit/working")
}

# Get the files for the dates of interest; since the files are by month it does not matter here if the list has more days than needed, it will skip files already downloaded
for(i in 1:length(dateList)) {
  get_met_reanalysis(days = dateList[i], duration = 48, direction = "backward",
                     path_met_files = "C:/hysplit/working")
}

# Calculate trajectories ####
# The "dates" argument allows you to subset the list of dates so you can run blocks of 15 days or whatever; we should be able to loop this to make multiple plots for different months and coordinates for example
dayblocks <- list(c(01:15), c(16:31))
library(raster) #needed for the lines that change the raster values, from maxValue until setValues

library(here)

pdf(here("Winds_raster.pdf"))
#png(here("Winds_raster.png"), height=1000, width=700, res=600)
#par(mfcol=c(2,2))
for (i in monthList){
  for (j in dayblocks){
    #Calculate the trajectory
    traj<-ProcTraj(lat = 41.308811, lon = 2.112405, #Llobregat's Delta
                   hour.interval = 1, name = "traj", start.hour = "12:00", end.hour = "12:00", #just one trajectory per day, at 12:00
                   met = "C:/hysplit/working/", out = "C:/hysplit/working/Out_files/", hours = -48, height = 500, 
                   hy.path = "C:/hysplit/", dates = dateList[month(dateList)==i & day(dateList) %in% j], tz = "CET")
    
    # Plot calculated trajectories in a map
    traj_lines<-Df2SpLines(traj, crs = "+proj=longlat +datum=NAD27") #here I just took the crs value from the documentation example as I am not familiar with datums and all that, it may need to be changed
    traj_lines_df<-Df2SpLinesDf(traj_lines, traj, add.distance = T, add.azimuth = T) #this line is not really needed but it may be useful for other things
    
    # pdf(here("Winds_map.pdf"))
    # PlotTraj(traj_lines_df) #plots the trajectories in a map
    # dev.off()
    
    #generates a raster, where each cell has the number of trajectories that pass through it
    traj_freq<- RasterizeTraj(traj_lines, parallel = F) #switch to parallel=T to calculate in parallel, but with very few trajectories (less than 8 I think)
                                                        #it won't work
    
    # these lines change the absolute number of trajectories to relative number (i.e. from 0 to 1)
    max.val <- maxValue(traj_freq)        #gets the max value of the raster
    v <- getValues(traj_freq)             #gets all values of the raster
    v <- v/max.val                        #divides the values
    traj_freq <- setValues(traj_freq, v)  #passes the new values to the raster
    
    max.val<-maxValue(traj_freq)          #get the (new) max value
    breaks<-seq(0, max.val, max.val/10)   #this will set the scale of the plot
    
    #modified version of PlotTrajFreq, so that we can change the scale of the plot (I did not find a way to do it directly, it seemed to be hardcoded)
    plotRaster=function (spGridDf, background = T, overlay = NA, overlay.color = "white", 
                         pdf = F, file.name = "output", ...) 
    {
      if (pdf == T) {
        pdf(file.name, paper = "USr", height = 0, width = 0)
      }
      oldpar <- par(no.readonly = TRUE)
      par(mar = c(0, 0, 0, 0) + 2)
      plot.add <- F
      extra.args <- list(...)
      if (!"main" %in% names(extra.args)) {
        extra.args$main <- NULL
      }
      if (background == T) {
        bb <- bbox(spGridDf)
        PlotBgMap(spGridDf, xlim = bb[1, ], ylim = bb[2, ], 
                  axes = TRUE)
        grid(col = "white")
        plot.add <- T
      }
      grays <- colorRampPalette(c("light green", "green", "greenyellow", 
                                  "yellow", "orange", "orangered", "red"))(10)
      image(spGridDf, col = grays, breaks = (c(0, 0.01, 0.02, 0.03, 
                                               0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)), add = plot.add)
      legend("topleft", legend = c("0.00 - 0.01", "0.01 - 0.02", "0.02 - 0.03", 
                                   "0.03 - 0.04", "0.04 - 0.05", "0.05 - 0.06", "0.06 - 0.07", 
                                   "0.07 - 0.08", "0.08 - 0.09", "0.09 - 0.1"), fill = grays)
      do.call(title, extra.args)
      if (!missing(overlay)) {
        plot(overlay, add = T, col = "black", border = "black")
      }
      par(oldpar)
      if (pdf == T) {
        dev.off()
      }
    }
    
    #plot the rasterized trajectories
    traj_grid<-as(traj_freq, "SpatialGridDataFrame")  #creates object of the necessary type for the package
    plotRaster(traj_grid, main = paste(month.name[i], j[1], "to", month.name[i], j[length(j)], yearList[1], "-", yearList[length(yearList)], sep = " ")) #plots the raster
  }
}
dev.off()

# Attempt to start working with splitR; failed due to formats not parsing? ####
traj_splitR<-hysplit_trajectory(lat = 42.83752,
                   lon = 2.30363,
                   height = 50,
                   duration = 12,
                   days = as.Date("1998-02-21", "%Y-%m-%d", tz = "CET"),
                   daily_hours = c(12),
                   direction = "forward",
                   met_type = "reanalysis",
                   extended_met = F,
                   met_dir = "C:/hysplit4/working/"
                   )
