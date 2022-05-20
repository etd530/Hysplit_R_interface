#"C:\Program Files\R\R-4.1.2\bin\Rscript.exe" Hysplit_wind_analysis_DOS.R --from 06051998_05:00 --to 07082007_09:00

#### HYSPLIT Program (Windows version) ####

#### Load packages ####
library(splitr)       # to work with Hysplit (to download files mostly)
library(opentraj)     # to work with Hysplit (does the calculations and plotting)
library(lubridate)    # for parsing dates
library(ggplot2)      # plotting library
library(raster)       # needed for the lines that change the raster values, from maxValue until setValues, and for pointDistnace()
library(geosphere)    # needed for bearing()
library(viridis)      # colorblind-friendly color palettes
library(plyr)         # diverse useful functions
library(optparse)     # Nice argument parsing


#### Variables for the runs ####
option_list = list(
  make_option(c("-f", "--from"), type="character", default=NULL,
              help="Starting date and time for the Hysplit runs. Provide in ddmmyyyy_hh:mm format, with hours in 24-hour format", metavar="character"),
  
  make_option(c("-t", "--to"), type="character", default=NULL,
              help="Ending date and time for the Hysplit runs. Provide in ddmmyyyy_hh:mm format, with hours in 24-hour format", metavar="character"),
  
  make_option("--byhour", type = "integer", default=NULL,
              help="Hour interval separating different trajectories", metavar = "integer"),
  
  make_option("--byday", type = "integer", default=1,
              help = "Number specifying the intervals of days from which to run trajectories",
              metavar = "integer"),
  
  make_option(c("-d", "--duration"), type = "integer", default = 1,
              help = "Duration of each trajectory calculation in hours. Start with a '-' to do backwards trajectories",
              metavar = "integer"),
  
  make_option(c("-L", "--lat"), type = "double", default = NULL,
              help = "Latitude of the starting point of the trajecotries", metavar = "double"),
  
  make_option(c("l", "--lon"), type = "double", default = NULL,
              help = "Longidute of the starting point of the trajecotries", metavar = "double"),
  
  make_option(c("-a", "--altitude"), type = "integer", default = NULL,
              help = "Altitude (in meters above sea level) at which each trajectory will start",
              metavar = "integer"),
  
  make_option(c("-z", "--timezone"), type = "character", default = "GMT",
              help = "Time zone to use", metavar = "character"),
  
  make_option(c("-m", "--margin"), type = "character", default = NULL,
              help = "Latitude and longitude determining the area of the maps. Provide in the order: minlon, minlat, maxlon, maxlat",
              metavar = "character"),
  
  make_option(c("-v", "--verbose"), type = "logical", default = TRUE, action = "store_true",
              help = "Select this option for verbose execution. Default behavior is TRUE",
              metavar = "boolean"),
  
  make_option(c("-D", "--debug"), type = "logical", default = FALSE, action = "store_true",
              help = "Messages for debugging. Only useulf for development",
              metavar = "boolean")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

`%!in%` = Negate(`%in%`)

if (opt$debug) {
  print(names(opt))
  for (i in opt) {
    print(i)
    }
  }

# Making sure dates are properly provided
if ("from" %!in% names(opt)) {
  print("ERROR: Please specify a start date")
  quit()
}

if ("to" %!in% names(opt)) {
  print("ERROR: Please specify an ending date")
  quit()
}

if (!grepl("([0-9]{8}_[0-9]{2}:[0-9]{2},?)+", opt$from)) {
  print("ERROR: Please specify a valid start date")
  quit()
}

if (!grepl("([0-9]{8}_[0-9]{2}:[0-9]{2},?)+", opt$to)) {
  print("ERROR: Please specify a valid ending date")
  quit()
}


dateList <- seq.Date(from = as.Date(opt$from, "%d%m%Y", tz = opt$timezone), 
                     to = as.Date(opt$to, "%d%m%Y", tz = opt$timezone), 
                     by = opt$byday)


if (opt$debug){print(dateList)}

dayList <- c(22:31)                          # put the days of the month here, without caring about short months
monthList <- c(10)                      # months go here
yearList <- c(2013)                    # years
dayblocks <- list(c(22:23), c(23:24))       # Set the blocks of days you want to run together to plot in the same map
coord <- list(c(opt$lat, opt$lon))        # coordinates
height <- c(opt$altitude)                               # height of the winds at starting point
duration <- opt$duration                             # how long forwards or backwards should the run go
times <- list(c("06:00", "06:00"))          # first and last hour on which the trajectories should start (put the same to run just at one hour)
hourInt <- opt$byhour                                # at which intervals should you start new trajectories (every 2 hours, etc.)

# Get the timezone and print a warning if it is the default one
TZ <- opt$timezone

if (TZ == "GMT") {
  print("#### WARNING: Timezone matches with the default of GMT. Please revise if you forgot to specify the time zone")
} else {
  print(paste("Timezone is", opt$timezone))
}

# Parse the margins to use for the maps
if("margin" %in% names(opt)) {
  opt$margin <- as.numeric(strsplit("-20,30,30,40", split=",")[[1]])
  } else if (opt$verbose) {
    print("No user-defined margins for maps. They will be determined by default")
    }
bb <- as.matrix(cbind(c(opt$margin[1], opt$margin[2]), c(opt$margin[3], opt$margin[4]))) #limits of the map for the plots
colnames(bb) <- c("min", "max")
rownames(bb) <- c("x", "y")

if (opt$debug) {print(bb)}


if(opt$debug){print("All is OK")}
quit()

##### FUNCTIONS #####
#modified version of PlotTrajFreq, so that we can change the scale of the plot and the color scale (I did not find a way to do it directly, it seemed to be hardcoded)
plotRaster=function (spGridDf, background = T, overlay = NA, overlay.color = "white", 
                     pdf = F, file.name = "output", bb,...) 
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
    bb
    PlotBgMap(spGridDf, xlim = bb[1, ], ylim = bb[2, ], 
              axes = TRUE)
    grid(col = "white")
    plot.add <- T
  }
  
  grays <- colorRampPalette(c("yellow", "orange", "orangered", "red"))(12) #names are color range, number is how many colors to generate
  
  grays[length(grays)+1] <- "#FFFFFF00"
  grays[length(grays)+1] <- "#000000"
  
  #if you change the number of colors in the previous line you must change breaks and legend accordingly
  image(spGridDf, col = grays, breaks = (c(0, 0.02, 0.04, 0.06, 
                                           0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.2, 0.22, 0.24, 0.99, 1)), add = plot.add)
  legend("topleft", legend = c("0.00 - 0.02", "0.02 - 0.04", "0.04 - 0.06", 
                               "0.06 - 0.08", "0.08 - 0.10", "0.10 - 0.12", "0.12 - 0.14", 
                               "0.14 - 0.16", "0.16 - 0.18", "0.18 - 0.2" ,"0.2 - 0.22", "0.22 - 0.24"), fill = grays)
  do.call(title, extra.args)
  if (!missing(overlay)) {
    plot(overlay, add = T, col = "black", border = "black")
  }
  par(oldpar)
  if (pdf == T) {
    dev.off()
  }
}

# modified version of ProcTraj from opentraj
ProcTrajMod = function (lat = 51.5, lon = -45.1, hour.interval = 1, name = "london", 
          start.hour = "00:00", end.hour = "23:00", met, out, hours = 12, 
          height = 100, hy.path, ID = 1, dates, script.name = "test", 
          add.new.column = F, new.column.name, new.column.value, tz = "GMT", 
          clean.files = TRUE) 
{
  wd <- getwd()
  script.extension <- ".sh"
  OS <- "unix"
  if (.Platform$OS.type == "windows") {
    script.extension <- ".bat"
    OS <- "windows"
  }
  hy.split.wd <- file.path(hy.path, "working")
  hy.split.wd <- normalizePath(hy.split.wd)
  setwd(hy.split.wd)
  folder.name = paste("process_", ID, sep = "")
  process.working.dir <- file.path(hy.split.wd, folder.name)
  dir.create(process.working.dir, showWarnings = FALSE)
  process.working.dir <- normalizePath(process.working.dir)
  setwd(process.working.dir)
  hy.split.exec.dir <- file.path(hy.path, "exec", "hyts_std")
  bdyfiles.path <- file.path(hy.path, "bdyfiles")
  symb.link.files <- list.files(path = bdyfiles.path)
  for (i in 1:length(symb.link.files)) {
    from <- normalizePath(file.path(bdyfiles.path, symb.link.files[[i]]))
    to <- file.path(process.working.dir, symb.link.files[[i]])
    file.copy(from, to)
  }
  control.file.number <- 1
  script.name <- paste(script.name, "_", ID, script.extension, 
                       sep = "")
  dates.and.times <- laply(.data = dates, .fun = function(d) {
    start.day <- paste(d, start.hour, sep = " ")
    end.day <- paste(d, end.hour, sep = " ")
    posix.date <- seq(as.POSIXct(start.day, tz), as.POSIXct(end.day, 
                                                            tz), by = paste(hour.interval, "hour", sep = " "))
    as.character(posix.date)
  })
  dates.and.times <- unique(dates.and.times)
  hour.interval <- paste(hour.interval, "hour", sep = " ")
  for (i in 1:length(dates.and.times)) {
    control.file <- "CONTROL"
    date <- as.POSIXct(dates.and.times[i], tz = tz)
    control.file.extension <- paste(as.character(ID), "_", 
                                    control.file.number, sep = "")
    control.file <- paste(control.file, control.file.extension, 
                          sep = ".")
    year <- format(date, "%y")
    Year <- format(date, "%Y")
    month <- format(date, "%m")
    day <- format(date, "%d")
    hour <- format(date, "%H")
    script.file <- file(script.name, "w")
    if (OS == "unix") {
      cat("#!/bin/bash", file = script.file, sep = "\n")
    }
    line <- paste("echo", year, month, day, hour, ">", control.file, 
                  sep = " ")
    cat(line, file = script.file, sep = "\n")
    line <- paste("echo 1 >>", control.file, sep = " ")
    cat(line, file = script.file, sep = "\n")
    line <- paste("echo", lat, lon, height, ">>", control.file, 
                  sep = " ")
    cat(line, file = script.file, sep = "\n")
    line <- paste("echo", hours, ">>", control.file, sep = " ")
    cat(line, file = script.file, sep = "\n")
    line <- paste("echo 0 >> ", control.file, "\n", "echo 10000.0 >> ", 
                  control.file, "\n", "echo 3 >> ", control.file, 
                  "\n", sep = "")
    cat(line, file = script.file, sep = "")
    months <- as.numeric(unique(format(date, "%m")))
    months <- c(months, months + 1:2)
    months <- months - 1
    months <- months[months <= 12]
    if (length(months) == 2) {
      months <- c(min(months) - 1, months)
    }
    for (i in 1:3) {
      AddMetFiles(months[i], Year, met, script.file, control.file)
    }
    line <- paste("echo ./ >>", control.file, sep = " ")
    cat(line, file = script.file, sep = "\n")
    line <- paste("echo tdump", "_", ID, "_", year, month, 
                  day, hour, " >> ", control.file, sep = "")
    cat(line, file = script.file, sep = "\n")
    line <- paste(hy.split.exec.dir, control.file.extension, 
                  sep = " ")
    cat(line, file = script.file, sep = "\n")
    close(script.file)
    if (OS == "unix") {
      system(paste0("sh ", script.name))
    }
    else {
      system(paste0(script.name))
    }
    control.file.number <- control.file.number + 1
  }
  traj <- ReadFiles(process.working.dir, ID, dates.and.times, 
                    tz)
  if (add.new.column == T) {
    if (!missing(new.column.name) & !missing(new.column.value)) {
      traj[new.column.name] <- new.column.value
    }
    else {
      stop("Parameters 'new.column.name' and 'new.column.value' are not defined.")
    }
  }
  if (!missing(out)) {
    file.name <- paste(out, name, Year, ".RData", sep = "")
    save(traj, file = file.name)
  }
  setwd(hy.split.wd)
  if (clean.files == T) {
    unlink(folder.name, recursive = TRUE)
  }
  setwd(wd)
  traj
}

##### END FUNCTIONS #####

# Creating the list with the days of interest ####
dateList <- as.vector(5) # just creating a vector
for(i in 1:length(yearList)) {
    for (j in 1:length(monthList)) {
      for (k in 1:length(dayList)) {
        dateList[length(dayList)*length(monthList)*(i-1)+length(dayList)*(j-1)+k] <- as.character(paste(yearList[i], monthList[j], dayList[k], sep = "-"))
    }
  }
} # this loop generates the dates

dateList <- as.Date(dateList) # change from strings to Date objects
dateList <- na.omit(dateList) # remove NAs (i.e. remove impossible dates such as February 31)

# Getting the meteorological files ####
# It seems ProcTraj() requires one meteorological file before and after the limits of what you want to plot for some reason,
# so we download the files for the months previous and next to the ones of interest

# Generate list with the dates of the previous month
prevDates <- seq.Date(if (monthList[1]==01) as.Date(paste(yearList[1]-1, "12", "01", sep="-"), "%Y-%m-%d", tz = TZ)
                      else as.Date(paste(yearList[1], monthList[1]-1, "01", sep="-"), "%Y-%m-%d", tz = TZ), # initial date
                      if (monthList[1]==01) as.Date(paste(yearList[length(yearList)]-1, "12", "01", sep="-"), "%Y-%m-%d", tz = TZ)
                      else as.Date(paste(yearList[length(yearList)], monthList[1]-1, "01", sep="-"), "%Y-%m-%d", tz = TZ), # final date
                     by = "year",                                   # interval
                     length.out = NULL)                             # period length

# Get meteorological files:
# I think with the loop is better because if you put the whole list directly in the "days" argument it seems to go over all the months in the middle
# and download unnecessary files
for(i in 1:length(prevDates)) {
  get_met_reanalysis(days = prevDates[i], duration = 12, direction = "forward",
                     path_met_files = "C:/hysplit/working")
}

# Generate list with dates of next month
postDates <- seq.Date(if (monthList[length(monthList)]==12) as.Date(paste(yearList[1]+1, "01", "01", sep = "-"), "%Y-%m-%d", tz = TZ)
                     else as.Date(paste(yearList[1], monthList[length(monthList)]+1, "01", sep = "-"), "%Y-%m-%d", tz = TZ), # initial date
                     if (monthList[length(monthList)]==12) as.Date(paste(yearList[length(yearList)]+1, "01","01", sep = "-"), "%Y-%m-%d", tz = TZ)
                     else as.Date(paste(yearList[length(yearList)], monthList[length(monthList)]+1,"01", sep = "-"), "%Y-%m-%d", tz = TZ), # final date
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
pdf("./Guiana_winds_25-28.pdf")
#png(here("Winds_raster.png"), height=1000, width=700, res=600)
#par(mfcol=c(2,2))
for (n in coord){
  for (i in monthList){
    for (j in dayblocks){
      if (exists("merged_trajs")){rm(merged_trajs)}
      if (exists("merged_trajlines_df")) {rm(merged_trajlines_df)}
      for(h in height){
        if (exists("traj")){rm(traj)}
        for (dayNum in 1:length(j)) {
          
          # Set starting and ending hours depending on if its the first, last or a middle day
          if (dayNum == 1) {
            startHour = times[[1]][1]
            endHour = "23:00"
          } else if (dayNum == length(j)){
            startHour = "00:00"
            endHour = times[[1]][2]
          } else {
            startHour = "00:00"
            endHour = "23:00"
          }
          ###Calculate the trajectories
          CurrentTraj <- ProcTrajMod(lat = n[1], lon = n[2],
                                  hour.interval = hourInt, name = "traj", start.hour = startHour, end.hour = endHour,
                                  met = "C:/hysplit/working/", out = "C:/hysplit/working/Out_files/", hours = duration, height = h, 
                                  hy.path = "C:/hysplit/", dates = dateList[day(dateList) %in% j][dayNum], tz = TZ)
          
          #Bind trajecotires for previous days to current one
          if (exists("traj")) {
            traj <- rbind(traj, CurrentTraj)
          } else {
            traj <- CurrentTraj
          }
        }
        
        # get the SpatialLinesDf
        traj_lines<-Df2SpLines(traj, crs = "+proj=longlat +datum=NAD27") #here I just took the crs value from the documentation example as I am not familiar with datums and all that, it may need to be changed
        traj_lines_df<-Df2SpLinesDf(traj_lines, traj, add.distance = T, add.azimuth = T) #this line is not really needed but it may be useful for other things
        
        # add starting height to the SpatialLinesDf
        traj_lines_df$start_height <- h
        
        # combine all SpatialLinesDf of the same run and different heights in a single file
        if (exists("merged_trajlines_df")) {
          merged_trajlines_df <- rbind(merged_trajlines_df, traj_lines_df)
        } else {
          merged_trajlines_df <- traj_lines_df
        }
        
        #generates a raster, where each cell has the number of trajectories that pass through it
        traj_freq<- RasterizeTraj(traj_lines, parallel = T) #switch to parallel=T to calculate in parallel, but with very few trajectories (less than 8 I think)
        #it won't work
        
        # these lines change the absolute number of trajectories to relative number (i.e. from 0 to 1)
        max.val <- maxValue(traj_freq)        #gets the max value of the raster
        v <- getValues(traj_freq)             #gets all values of the raster
        v <- v/max.val                        #divides the values
        traj_freq <- setValues(traj_freq, v)  #passes the new values to the raster
        
        max.val<-maxValue(traj_freq)          #get the (new) max value
        breaks<-seq(0, max.val, max.val/10)   #this will set the scale of the plot
        
        #plot the rasterized trajectories
        traj_grid<-as(traj_freq, "SpatialGridDataFrame")  #creates object of the necessary type for the package
        yearString <- if (length(yearList)==1) {yearList[1]} else {paste0(yearList[1],"-",yearList[length(yearList)])}
        plotRaster(traj_grid, main = paste0(month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                                            month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                                            yearString, " (", h, "m AGL)"), bb = bb) #plots the raster
        
        #add the starting height of the trajectories as a variable
        traj$start_height <- h
        
        #join all trajectories of the same days and different starting heights in a single df
        if (exists("merged_trajs")) {
          merged_trajs <- rbind(merged_trajs, traj)
        } else {
          merged_trajs <- traj
        }
      }
      
      rm(traj)
      
      #Plot the trajectories in a map
      setAlpha = ifelse(merged_trajlines_df$day == 28 & merged_trajlines_df$hour == 6, 1, 0.25)
      PlotBgMap(merged_trajlines_df, xlim = bb[1, ], ylim = bb[2, ], axes = TRUE)
      plot(merged_trajlines_df,
           col = ifelse(merged_trajlines_df$start_height == 500, 
                        viridis(n=1, alpha = setAlpha, begin = 1), 
                        ifelse(merged_trajlines_df$start_height == 1000,
                               viridis(n=1, alpha = setAlpha, begin = 0.5),
                               viridis(n=1, alpha = setAlpha, begin=0))),
           add = T,
      )
      #PlotTraj(merged_trajlines_df,
      #         col = ifelse(merged_trajlines_df$start_height == 500, 
      #                                         viridis(n=1, alpha = setAlpha, begin = 1), 
      #                                         ifelse(merged_trajlines_df$start_height == 1000,
      #                                         viridis(n=1, alpha = setAlpha, begin = 0.5),
      #                                         viridis(n=1, alpha = setAlpha, begin=0)))
      #       )
      
      title(main = paste0(month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                          month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                          yearString),
            outer = T, line = -1.6
      )
      
      legend(bb[1,1], bb[2,2], legend = c("500m", "1000m", "2000m"), bg = "transparent",
             fill = c(viridis(n=1, begin = 1), viridis(n=1, begin = 0.5), 
                     viridis(n=1, begin = 0)), title = "Starting altitude (m AGL)")
      
      # Calculate distance from origin and angle relative to origin for each trajectory position
      merged_trajs$dist <- pointDistance(cbind(merged_trajs$lon, merged_trajs$lat), c(coord[[1]][2], coord[[1]][1]), lonlat = T)
      merged_trajs$angle <- bearing(c(coord[[1]][2], coord[[1]][1]), cbind(merged_trajs$lon, merged_trajs$lat))
      
      #we take out values for the starting points, as they are still at the origin so distance is zero and it makes no sense to calculate an angle
      merged_trajs$dist[merged_trajs$hour.inc==0] <- NA
      merged_trajs$angle[merged_trajs$hour.inc==0] <- NA
      
      #add 360 to negative azimuths
      for (ang in 1:length(merged_trajs$angle)) {
        if (!is.na(merged_trajs$angle[ang]) & (merged_trajs$angle[ang] < 0)) {
          merged_trajs$angle[ang] <- merged_trajs$angle[ang] + 360
        }
      }
      
      # turn starting height into factor
      merged_trajs$start_height <- factor(merged_trajs$start_height, levels = c("2000", "1000", "500"))
      
      # Build windrose plots with heights separated
      for (h in height) {
        color = ifelse(h == 500, viridis(n=1, begin = 1), ifelse(h==1000, viridis(n=1, begin = 0.5), viridis(n=1, begin = 0)))
        # Build windrose plot (all time points)
        windrose = ggplot(data=merged_trajs[merged_trajs$start_height == h,], aes(x=angle, y=stat(count/sum(count)))) + 
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360, fill =  gsub("FF", "", color)) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                         month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                         yearString, "\n(all time points, ", h, "m AGL)")) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose)
        
        # Build windrose plot for -200h
        
        windrose = ggplot(data=merged_trajs[merged_trajs$start_height == h & merged_trajs$hour.inc == -200,], aes(x=angle, y=stat(count/sum(count)))) + 
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360, fill =  gsub("FF", "", color)) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                         month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                         yearString, "\n(backwards 200h, ",h, "m AGL)")) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose)
        
        # Build windrose plot for -100h
        windrose = ggplot(data=merged_trajs[merged_trajs$start_height == h & merged_trajs$hour.inc == -100,], aes(x=angle, y=stat(count/sum(count)))) + 
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360, fill =  gsub("FF", "", color)) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                         month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                         yearString, "\n(backwards 100h, ", h, "m AGL)")) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose)
      }
      
      ## Build windrose plots with heights as grouping factor
      # Build windrose plot (all time points)
      windrose = ggplot(data=merged_trajs, aes(x=angle, y=stat(count/sum(count)), group=start_height,
                                                                               fill=start_height)) + 
        geom_histogram(aes(y = stat(count/sum(count))), bins = 360) +
        coord_polar(start = 0, clip = "off") +
        ggtitle(paste0("Wind directions ", month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                       month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                       yearString, " (all time points)")) +
        scale_fill_viridis(discrete = T, alpha = 1) +
        scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
        theme(plot.title = element_text(hjust = 0.5))
      print(windrose)
      
      # Build windrose plot for -200h
      
      windrose = ggplot(data=merged_trajs[merged_trajs$hour.inc == -200,], aes(x=angle, y=stat(count/sum(count)), group=start_height,
                                               fill=start_height)) + 
        geom_histogram(aes(y = stat(count/sum(count))), bins = 360) +
        coord_polar(start = 0, clip = "off") +
        ggtitle(paste0("Wind directions ", month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                       month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                       yearString, " (backwards 200h)")) +
        scale_fill_viridis(discrete = T, alpha = 1) +
        scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
        theme(plot.title = element_text(hjust = 0.5))
      print(windrose)
      
      # Build windrose plot for -100h
      windrose = ggplot(data=merged_trajs[merged_trajs$hour.inc == -100,], aes(x=angle, y=stat(count/sum(count)), group=start_height,
                                                                               fill=start_height)) + 
        geom_histogram(aes(y = stat(count/sum(count))), bins = 360) +
        coord_polar(start = 0, clip = "off") +
        ggtitle(paste0("Wind directions ", month.name[i]," ", j[1], " ", times[[1]][1], " to ", 
                       month.name[i], " ", j[length(j)], " ", times[[1]][2], " ", 
                       yearString, " (backwards 100h)")) +
        scale_fill_viridis(discrete = T, alpha = 1) +
        scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
        theme(plot.title = element_text(hjust = 0.5))
      print(windrose)
    }
  }
}

dev.off()



#### Main trajectories (October 28th at 6:00am) ####

traj500 <- ProcTrajMod(lat = 5.75, lon = -53.93,
                      hour.interval = 1, name = "traj", start.hour = "06:00", end.hour = "06:00",
                      met = "C:/hysplit/working/", out = "C:/hysplit/working/Out_files/", hours = -200, height = 500,
                      hy.path = "C:/hysplit/", dates = c("2013-10-28"), tz = TZ)

traj500$start_height <- 500

traj1000 <- ProcTrajMod(lat = 5.75, lon = -53.93,
                       hour.interval = 1, name = "traj", start.hour = "06:00", end.hour = "06:00",
                       met = "C:/hysplit/working/", out = "C:/hysplit/working/Out_files/", hours = -200, height = 1000,
                       hy.path = "C:/hysplit/", dates = c("2013-10-28"), tz = TZ)

traj1000$start_height <- 1000

traj2000 <- ProcTrajMod(lat = 5.75, lon = -53.93,
                        hour.interval = 1, name = "traj", start.hour = "06:00", end.hour = "06:00",
                        met = "C:/hysplit/working/", out = "C:/hysplit/working/Out_files/", hours = -200, height = 2000,
                        hy.path = "C:/hysplit/", dates = c("2013-10-28"), tz = TZ)

traj2000$start_height <- 2000

# Merge the trajectories
MainTrajs <- rbind(traj500, traj1000, traj2000)


# Convert to Ds2SpLines and Ds2SpLinesDf
MainTrajlines <- Df2SpLines(MainTrajs, crs = "+proj=longlat +datum=NAD27")
MainTrajlines_df <- Df2SpLinesDf(MainTrajlines, MainTrajs, add.distance = T, add.azimuth = T)


# creating SpatialPoitns object for plotting
pts <- cbind(MainTrajs$lon, MainTrajs$lat, MainTrajs$height)
transformedPoints <- SpatialPoints(pts, proj4string = CRS("+proj=longlat +datum=NAD27"))
transformedPoints_df <- SpatialPointsDataFrame(transformedPoints, as.data.frame(pts))

# Plot trajectories and points
pdf("Guiana_main_traj_plots.pdf")
#bb <- bbox(MainTrajlines_df)
PlotBgMap(transformedPoints, xlim = bb[1, ], ylim = bb[2, ], axes = TRUE)
plot(MainTrajlines_df,
         col = ifelse(MainTrajlines_df$height == 500, 
                      viridis(n=1, begin = 1), 
                      ifelse(MainTrajlines_df$height == 1000,
                             viridis(n=1, begin = 0.5),
                             viridis(n=1, begin=0))),
     add = T,
)

thinnedPoints <- transformedPoints_df@coords[seq(10, nrow(transformedPoints_df@coords), 10), 1:2]
points(thinnedPoints, 
       pch=17, col = rep(c(viridis(n=1, begin = 1), viridis(n=1, begin = 0.5), viridis(n=1, begin = 0)), times=c(20,20,20)))

title(main = "Trajectories for October 28th 2013, backwards 200 hours",
      outer = T, line = -1.6)

legend(bb[1,1], bb[2,2], legend = c("500m", "1000m", "2000m"), bg = "transparent",
       fill = c(viridis(n=1, begin = 1), viridis(n=1, begin = 0.5), 
                viridis(n=1, begin = 0)), title = "Starting altitude (m AGL)")

alt_plot <- ggplot(data = MainTrajs[seq(1, nrow(MainTrajs), 10),], aes(x = -1*hour.inc, y = height)) +
  geom_point(aes(color = factor(start_height)), shape = 17, size = 3) +
  ggtitle("Trajectory altitude profile for October 28th 2013, backwards 200 hours") +
  scale_color_viridis(begin = 1, end = 0, discrete = T, alpha = 1) +
  ylab("Altitude (m AGL)") +
  xlab("Hours before observation") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Start height") +
  geom_line(data = MainTrajs, aes(x = -1*hour.inc, y = height, group = start_height, color = factor(start_height)))

ggsave("Guiana_height_profile.pdf", plot = alt_plot, width = 2000, height = 2000*0.5, units = "px")

dev.off()

#####__END__######