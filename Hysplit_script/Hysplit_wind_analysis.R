#!/usr/bin/env Rscript


#### HYSPLIT Program ####

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


#### ARGS ####
option_list = list(
  make_option(c("-f", "--from"), type="character", default=NULL,
              help="Starting date and time for the Hysplit runs. Provide in YYYY-MM-DD-hh-mm format, with hours in 24-hour format", metavar="character"),
  
  make_option(c("-t", "--to"), type="character", default=NULL,
              help="Ending date and time for the Hysplit runs. Provide in YYYY-MM-DD-hh-mm format, with hours in 24-hour format", metavar="character"),
  
  make_option("--byhour", type = "integer", default=0,
              help="Hour interval separating different trajectories", metavar = "integer"),
  
  make_option("--byday", type = "integer", default=0,
              help = "Number specifying the intervals of days from which to run trajectories",
              metavar = "integer"),
  
  make_option("--bymonth", type = "integer", default = 0,
              help = "Number specifying the intervals of months from which to run trajectories",
              metavar = "integer"),
  
  make_option("--byyear", type = "integer", default = 0,
              help = "Number specifying the intervals of years from which to run trajectories",
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
              help = "Latitude and longitude determining the area of the maps. Provide in the order: minlon, minlat, maxlon, maxlat WITHOUT SPACES",
              metavar = "character"),
  
  make_option(c("--windrose_times"), type="character", default="Inf",
              help = "Time points to use for plotting the windrose histograms, separated by commas. use 'Inf' to include all time points",
              metavar = "character"),
  
  make_option(c("-o", "--out"), type = "character", default = "windplots.pdf",
              help = "Name to use for the output file containing the plots",
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


# Print all arguments (for debugging)
if (opt$debug) {
  for (i in c(1:length(names(opt)))) {
    print(opt[i])
  }
  print(names(opt))
  for (i in opt) {
    print(i)
  }
}

#### OPERATORS ####
`%!in%` = Negate(`%in%`)

#### ARG CHECKS ####
# Making sure dates are properly provided
if ("from" %!in% names(opt)) {
  print("ERROR: Please specify a start date")
  quit()
}

if ("to" %!in% names(opt)) {
  print("ERROR: Please specify an ending date")
  quit()
}

if (!grepl("\\(?([0-9]{4},?)+\\)?-(\\(?([0-9]{2},?)+\\)?-?){4}", opt$from, perl=T)) {
  print("ERROR: Please specify a valid start date")
  quit()
}

if (!grepl("\\(?([0-9]{4},?)+\\)?-(\\(?([0-9]{2},?)+\\)?-?){4}", opt$to, perl = T)) {
  print("ERROR: Please specify a valid ending date")
  quit()
}

# Check coordinates are provided
if ("lat" %!in% names(opt)) {
  print("ERROR: Please provide the latitude")
  quit()
}

if ("lon" %!in% names(opt)) {
  print("ERROR: Please provide the longitude")
  quit()
}

# Check altitude is provided
if ("altitude" %!in% names(opt)) {
  print("ERROR: Please specify the altitude at which trajectories should start")
  quit()
}


#### FUNS ####
# Function to get list of dates like the one given as input but +-X months
get_prev_post_dates = function(datesList, increment) {
  for (i in 1:length(datesList)) {
    datesList[[i]][[1]]$mon <- datesList[[i]][[1]]$mon + increment
  }
  return(datesList)
}

# Function to increase a datetime by 1 hours/day/month/year
increase_datetime = function(datetime, 
                             max_values=c(year(Sys.Date()), 12, 31, 23), 
                             min_values=c("0000", "01", "01", "00"),
                             start_level) {
  # check if the value is smaller than the maximum (e.g. if hour is smaller than 23)
  if(as.integer(datetime[start_level]) < max_values[start_level]) {
    # if it is, increase it by one
    datetime[start_level] <- as.character(as.integer(datetime[start_level]) + 1)
    if (nchar(datetime[start_level]) == 1) {
      datetime[start_level] = paste0("0", datetime[start_level])
    }
    # else it is the maximum, so reset it to one and move one level up (e.g. from day to month)
  } else {
    datetime[start_level] <- min_values[start_level]
    if (start_level > 1) {
      datetime = increase_datetime(datetime = datetime, start_level = start_level-1)
    }
  }
  return(datetime)
}

# Function to generate the complete list of datetimes from which to start trajectories
generate_datetimes_lists = function(by_year_month_day_hour, datesList) {
  from_split = datesList[[1]][[1]]
  to_split = datesList[[1]][[2]]
  for (i in 1:4) {
    # Check if they provided an increment, e.g. by 1 hour, by 1 year
    if (by_year_month_day_hour[i]) {
      # if they did, check if start month and end month (or year, etc.) differ or not
      previous_level = ifelse(i==1, 1, i-1)
      if (from_split[i] != to_split[i] & by_year_month_day_hour[previous_level]) {
        # If start and end hour/day/month/year differ, create the corresponding combinations from start to end by N hours/days/months/years
        interval <- seq(from=from_split[i], to=to_split[i], by = by_year_month_day_hour[i])
        datesList_tmp <- list()
        for (j in datesList) {
          start_date <- j[[1]]
          end_date <- j[[2]]
          for (k in 1:length(interval)) {
            start_date[i] <- interval[k]
            end_date[i] <- interval[k]
            datesList_tmp[[length(datesList_tmp)+1]] <- list(start_date, end_date)
          }
        }
        datesList <- datesList_tmp
        
        # if start and end day/year/etc match OR the increment for the previous level is missing (e.g. start and end months are 05 and 04, but by_year is 0)
        # then a continuous interval spanning multiple levels must be created (e.g. from May 2013 to April 2015, month by month)
      } else {
        datesList_tmp <- list()
        for (pair in 1:length(datesList)) {
          
          current_start_date = datesList[[pair]][[1]]
          current_end_date = datesList[[pair]][[2]]
          
          for (level in i:1) {
            current_end_date[level] <- current_start_date[level]
          }
          
          datesList_tmp[[length(datesList_tmp)+1]] <- list(current_start_date, current_end_date)
          
          while (current_end_date[1] != datesList[[pair]][[2]][1] | 
                 current_end_date[2] != datesList[[pair]][[2]][2] | 
                 current_end_date[3] != datesList[[pair]][[2]][3] |
                 current_end_date[4] != datesList[[pair]][[2]][4]) {
            
            current_start_date = increase_datetime(datetime = current_start_date, start_level = i)
            current_end_date = increase_datetime(datetime = current_end_date, start_level = i)
            # print(current_start_date)
            # print(current_end_date)
            datesList_tmp[[length(datesList_tmp)+1]] <- list(current_start_date, current_end_date)
          }
        }
        datesList <- datesList_tmp
        # datesList[(length(datesList)+1):(length(datesList)+length(datesList_tmp))] <- datesList_tmp[1:length(datesList_tmp)]
        #break
      }
    }
  }
  
  rm(datesList_tmp)
  
  # Remove duplicates
  for (i in 1:length(datesList)) {
    datesList[[i]] <- datesList[[i]][[1]]
  }
  return(datesList)
}

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
  
  # grays <- colorRampPalette(c("yellow", "orange", "orangered", "red"))(10) #names are color range, number is how many colors to generate
  # 
  # #if you change the number of colors in the previous line you must change breaks and legend accordingly
  # image(spGridDf, col = grays, breaks = (c(0, 0.1, 0.2, 0.3,
  #                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)), add = plot.add)
  # legend("topleft", legend = c("0.0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5",
  #                              "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"), fill = grays)
  
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

# Function to join year, month and day into a Date object
rejoin_dates = function(dateList, TZ) {
  for (i in 1:length(dateList)) {
    dateList[[i]] <- list(as.POSIXlt(paste(dateList[[i]][1], 
                                        dateList[[i]][2], 
                                        dateList[[i]][3], sep="-"),
                                  "%Y-%m-%d", tz=TZ),
                          dateList[[i]][4], 
                          dateList[[i]][5])
  }
  return(dateList)
}

# Function to eliminate missing date from the datetimes list
remove_missing_dates = function(dateList) {
  remove_indexes = vector()
  for (i in 1:length(dateList)) {
    if (is.na(dateList[[i]][[1]])) {
      remove_indexes[length(remove_indexes)+1] = i
    }
  }
  if (length(remove_indexes) != 0) {
    dateList <- dateList[-remove_indexes]
  }
  return(dateList)
}

# Function to download meteorological files
get_met_files = function(datesList) {
  for (i in 1:length(datesList)) {
    # I think with the loop is better because if you put the whole list directly in the "days" argument it seems to go over all the months in the middle
    # and download unnecessary files
    get_met_reanalysis(days = datesList[[i]][[1]], duration = 12, direction = "forward",
                       path_met_files = paste0(hy_path, "working"))
  }
}

# Function to run the trajectories with the dates from a list
compute_trajectories = function(datesList, latlon, hourInt, hy_path.=hy_path, duration, h) {
  for (coordinate in latlon) {
    for (altitude in h) {
      for (i in 1:length(datesList)) {
        run_hour = paste(datesList[[i]][[2]], datesList[[i]][[3]], sep = ":")
        CurrentTraj <- ProcTrajMod(lat = coordinate[1], lon = coordinate[2],
                                   hour.interval = hourInt, 
                                   name = "traj", start.hour = run_hour, 
                                   end.hour = run_hour, 
                                   met = paste0(hy_path, "working/"), 
                                   out = paste0(hy_path, "working/Out_files/"), 
                                   hours = duration, height = altitude, 
                                   hy.path = hy_path, 
                                   dates = as.Date(datesList[[i]][[1]]), 
                                   tz = attr(datesList[[i]][[1]], "tzone"))
        CurrentTraj$start_height <- altitude
        if(exists("merged_trajs")) {
          merged_trajs <- rbind(merged_trajs, CurrentTraj)
        } else {
          merged_trajs <- CurrentTraj
        }
      }
    } 
  }
  return(merged_trajs)
}

# Function to plot raster maps from Hysplit runs in a list
plot_raster_maps = function(trajs, height, PRJ) {
  for (h in height) {
    # Subset to a single height
    single_height_trajs <- trajs[trajs$start_height == h,]
    # Get the SpatialLines objects
    traj_lines <- Df2SpLines(single_height_trajs, crs = PRJ)
    # Get the SpatialLinesDataFrame object
    traj_lines_df <- Df2SpLinesDf(traj_lines, single_height_trajs, add.distance = T, add.azimuth = T)
    # Get the raster
    traj_freq<- RasterizeTraj(traj_lines, parallel = T)
    
    # Change the absolute number of trajectories to relative number (i.e. from 0 to 1)
    max.val <- maxValue(traj_freq)        #gets the max value of the raster
    v <- getValues(traj_freq)             #gets all values of the raster
    v <- v/max.val                        #divides the values
    traj_freq <- setValues(traj_freq, v)  #passes the new values to the raster
    
    max.val<-maxValue(traj_freq)          #get the (new) max value
    breaks<-seq(0, max.val, max.val/10)   #this will set the scale of the plot
    
    #Get SpatialGridDataFrame object
    traj_grid<-as(traj_freq, "SpatialGridDataFrame")  #creates object of the necessary type for the package
    
    # yearString <- if (length(unique(single_height_trajs$year))==1) {
    #   as.character(single_height_trajs$year[1])
    # } else {
    #     paste0(as.character(min(unique(single_height_trajs$year))),"-",
    #            as.character(max(unique(single_height_trajs$year))))
    #   }
    
    # Get margins for the plots if it has not been provided by the user
    if ("margin" %!in% names(opt)) {
      bb <- sp::bbox(traj_lines_df)
    }
    
    # Plot the raster maps
    plotRaster(traj_grid, main = paste0(min(single_height_trajs$date)," to ", max(single_height_trajs$date)," (", h, "m AGL)"), bb = bb)
  }
}

# Function to plot the trajectories from a list
plot_trajlines = function(trajs, PRJ){
  # get the SpatialLines object
  traj_lines <- Df2SpLines(trajs, crs = PRJ)
  # Get the SpatialLinesDataFrame object
  traj_lines_df <- Df2SpLinesDf(traj_lines, trajs, add.distance = T, add.azimuth = T)
  
  # Get margins for the pltos if it has not been provided by the user
  if ("margin" %!in% names(opt)) {
    bb <- sp::bbox(traj_lines_df) 
  }
  
  # Make color palette
  color_list = viridis(n=length(unique(trajs$start_height)), begin=0, alpha = 0.25)
  names(color_list) = unique(trajs$start_height)
  color_palette <- vector()
  for (traj_height in traj_lines_df$start_height){
    color_palette[length(color_palette)+1] <- color_list[which(names(color_list) == as.character(traj_height))]
  }
  
  # Plot
  PlotBgMap(traj_lines_df, xlim = bb[1, ], ylim = bb[2, ], axes = TRUE)
  plot(traj_lines_df, col = color_palette, add = T)
  
  title(main = paste0(min(trajs$date), " to ", max(trajs$date)),
        outer = T, line = -1.6)
  
  legend(bb[1,1], bb[2,2], legend = unique(trajs$start_height), bg = "transparent",
         fill = color_list, title = "Starting altitude (m AGL)")
}

# Function to plot the windrose histograms (heights separated)
plot_windrose_hist = function(trajs, height, duration=Inf){
  # Get starting coordinates
  coord <- c(unique(trajs$lat[trajs$hour.inc==0]), unique(trajs$lon[trajs$hour.inc==0]))
  
  # Calculate distance from origin and angle relative to origin for each trajectory position
  trajs$dist <- pointDistance(cbind(trajs$lon, trajs$lat), c(coord[2], coord[1]), lonlat = T)
  trajs$angle <- bearing(c(coord[2], coord[1]), cbind(trajs$lon, trajs$lat))
  
  #take out values for the starting points, which are still at the origin so distance is zero and it makes no sense to calculate an angle
  trajs$dist[trajs$hour.inc==0] <- NA
  trajs$angle[trajs$hour.inc==0] <- NA
  
  #add 360 to negative azimuths
  for (ang in 1:length(trajs$angle)) {
    if (!is.na(trajs$angle[ang]) & (trajs$angle[ang] < 0)) {
      trajs$angle[ang] <- trajs$angle[ang] + 360
    }
  }
  
  # Build windrose histograms
  # turn starting height into factor
  trajs$start_height <- factor(trajs$start_height, levels = unique(as.character(sort(trajs$start_height, decreasing = T))))
  
  # Create color palette
  color = viridis(n = length(unique(trajs$start_height)), begin=0)
  
  # Make plot for each user-specified time point (Inf meaning include all time points of the trajectory)
  for (d in duration) {
    # Plots with all time points
    if(d == Inf){
      # Plot with all heights
      if(length(height)>1){
        windrose = ggplot(data=trajs, aes(x=angle, y=stat(count/sum(count)), group=start_height,
                                          fill=start_height)) +
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", min(trajs$date), " to ", max(trajs$date), " (all time points)")) +
          scale_fill_viridis(discrete = T, alpha = 1) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose) 
      }
      
      # Plots separated by height
      for (h in 1:length(height)){
        windrose = ggplot(data=trajs[trajs$start_height == height[h],], aes(x=angle, y=stat(count/sum(count)))) + 
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360, fill =  color[h]) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", min(trajs$date), " to ", max(trajs$date), "\n(all time points, ", as.character(height[h]), "m AGL)")) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose)
      }
    # Plots for a specific time point
    } else if(abs(d) > max(abs(trajs$hour.inc))) {
      print("WARNING: Provided hour for windrose histogram is larger than the duration of the runs and thus will be skipped")
    } else {
      # All heights
      if (length(height)>1){
        windrose = ggplot(data=trajs[trajs$hour.inc == d,], aes(x=angle, y=stat(count/sum(count)), group=start_height,
                                                                fill=start_height)) +
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", min(trajs$date), " to ", max(trajs$date), " (backwards ", abs(d), "h)")) +
          scale_fill_viridis(discrete = T, alpha = 1) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose) 
      }
      
      # Specific height
      for (h in 1:length(height)){
        windrose = ggplot(data=trajs[trajs$hour.inc == d & trajs$start_height == height[h],], aes(x=angle, y=stat(count/sum(count)))) +
          geom_histogram(aes(y = stat(count/sum(count))), bins = 360, fill = color[h]) +
          coord_polar(start = 0, clip = "off") +
          ggtitle(paste0("Wind directions ", min(trajs$date), " to ", max(trajs$date), " \n(backwards ", abs(d), "h, ", height[h], "m AGL)")) +
          scale_fill_viridis(discrete = T, alpha = 1) +
          scale_x_continuous(breaks =c(0, 90, 180, 270) , limits = c(0, 360), labels = c("N", "E", "S", "W")) +
          theme(plot.title = element_text(hjust = 0.5))
        print(windrose)
      }
    }
  }
  return(trajs)
}

# Function to plot the altitudinal profile plot
plot_altitudinal_profile = function(trajs){
  # Create dataframe to store summary statistics of the trajectories
  mean_SE_trajs <- as.data.frame(matrix(nrow=length(unique(trajs$hour.inc))*length(unique(trajs$start_height)), ncol=4))
  colnames(mean_SE_trajs) = c("hour.inc", "start_height","mean_height", "SE_height")
  mean_SE_trajs$start_height <- as.factor(mean_SE_trajs$start_height)
  levels(mean_SE_trajs$start_height) <- unique(trajs$start_height)
  
  # add the hours backwards or forwards that trajectories go over
  mean_SE_trajs$hour.inc <- unique(trajs$hour.inc)
  
  # add the starting heights
  for (alt in 1:length(unique(trajs$start_height))){
    start_index = 1+(alt-1)*length(unique(trajs$hour.inc))
    end_index = start_index + length(unique(trajs$hour.inc))-1 
    mean_SE_trajs$start_height[start_index:end_index] <- unique(trajs$start_height)[alt]
  }
  
  # for each hour and starting height, add the corresponding mean and SE
  for (alt in unique(trajs$start_height)){
    for (h in unique(trajs$hour.inc)){
      output_mask <- mean_SE_trajs$hour.inc == h & mean_SE_trajs$start_height == alt
      input_mask <- trajs$hour.inc == h & trajs$start_height == alt
      mean_SE_trajs$mean_height[output_mask] <- mean(trajs$height[input_mask])
      mean_SE_trajs$SE_height[output_mask] <- sd(trajs$height[input_mask])/sqrt(nrow(trajs[input_mask,]))
    }
  }
  
  # add mean+SE and mean-SE
  mean_SE_trajs$mean_plus_SE <- mean_SE_trajs$mean_height + mean_SE_trajs$SE_height
  mean_SE_trajs$mean_minus_SE <- mean_SE_trajs$mean_height - mean_SE_trajs$SE_height
  
  # Plot altitudinal profiles
  alt_plot <- ggplot(data = mean_SE_trajs, aes(x = abs(hour.inc), y = mean_height)) +
    #geom_point(aes(color = factor(start_height)), shape = 17, size = 3) +
    ggtitle(paste0("Trajectory altitude profile from ", min(trajs$date), "\nto ", max(trajs$date), ", backwards ", abs(duration), " hours")) +
    scale_color_viridis(begin = 1, end = 0, discrete = T, alpha = 1) +
    scale_fill_viridis(begin = 1, end = 0, discrete = T, alpha = 0.25) + 
    ylab("Altitude (m AGL)") +
    xlab("Hours before observation") +
    theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Start height") +
    geom_line(data = mean_SE_trajs, aes(x = abs(hour.inc), y = mean_height, 
                                        group = start_height, 
                                        color = factor(start_height))) +
    geom_line(data = mean_SE_trajs, aes(x = abs(hour.inc), y = mean_plus_SE, 
                                        group = start_height, 
                                        color = factor(start_height))) +
    geom_line(data = mean_SE_trajs, aes(x = abs(hour.inc), y = mean_minus_SE, 
                                        group = start_height, 
                                        color = factor(start_height))) +
    geom_ribbon(aes(ymin = mean_minus_SE, ymax = mean_plus_SE, 
                    group = start_height, fill = factor(start_height),
                    alpha = 0.5)) +
    xlim(0, max(abs(duration)))
}


#### VARIABLES ####
# path to hysplit installation
hy_path <- "C:/hysplit/"

# name for output file
outfile <- paste0("./", opt$out)

# projection, datum, etc.
PRJ <- proj4string(CRS("+init=epsg:4326")) #WGS84
#PRJ <- proj4string(CRS('+proj=eck4')) #ECK4

### Parameters for the program
# Dates to be run
# dateList <- seq.Date(from = as.Date(opt$from, "%d%m%Y", tz = opt$timezone),
#                      to = as.Date(opt$to, "%d%m%Y", tz = opt$timezone),
#                      by = opt$byday)
# if (opt$debug){print(dateList)}
# 
# # Days of the month to be run, without caring about short months
# dayList <- sort(unique(day(dateList)))
# if (opt$debug){print(dayList)}
# 
# # months to be run
# monthList <- unique(month(dateList))
# if(opt$debug){print(monthList)}
# 
# # years to be run
# yearList <- unique(year(dateList))
# if(opt$debug){print(yearList)}
# 
# # blocks of days that go together in a single plot
# dayblocks <- as.list(strsplit(opt$dayblocks, split = ",", fixed = T)[[1]])
# getDays = function(x){
#   c(as.integer(strsplit(x, split = ":", fixed = T)[[1]][1]):as.integer(strsplit(x, split = ":", fixed = T)[[1]][2]))
# }
# dayblocks <- lapply(X = dayblocks, FUN = getDays)
# if(opt$debug){print(dayblocks)}

# coordinates
coord <- list(c(opt$lat, opt$lon))
if(opt$debug){print(coord)}

# height of the winds at starting point
if(is.integer(opt$altitude)) {
  height <- c(opt$altitude) 
} else {
  height <- as.integer(as.vector(strsplit(opt$altitude, split = ",")[[1]]))
}
if(opt$debug){print(height)}

# Duration of the runs; print a warning if it is the default value
duration <- opt$duration
if (duration == 1) {
  print("#### WARNING: Duration matches the default value of 1. Please make sure this is the correct value")
} else {
  print(paste0("Duration is", duration))
}

# Starting and ending hours for the different trajectories
# start_hour <- gsub("[0-9]{8}_", "", opt$from)
# end_hour <- gsub("[0-9]{8}_", "", opt$to)
# 
# times <- list(c(start_hour, end_hour))          # first and last hour on which the trajectories should start (put the same to run just at one hour)
# if (opt$debug) {print(times)}
# rm(start_hour)
# rm(end_hour)

# # Interval of hours between different runs (one each hour, every two hours, etc.)
# hourInt <- opt$byhour
# if(opt$debug){print(hourInt)}

# Get the timezone and print a warning if it is the default one
TZ <- opt$timezone
if(opt$debug){print(TZ)}

if (TZ == "GMT") {
  print("#### WARNING: Timezone matches with the default of GMT. Please revise if you forgot to specify the time zone")
} else {
  if(opt$verbose){print(paste("Timezone is:", opt$timezone))}
}

# Parse the margins to use for the maps
if("margin" %in% names(opt)) {
  opt$margin <- gsub("'|`", "", opt$margin)
  opt$margin <- as.numeric(strsplit(opt$margin, split=",")[[1]])
  bb <- as.matrix(cbind(c(opt$margin[1], opt$margin[2]), c(opt$margin[3], opt$margin[4]))) #limits of the map for the plots
  colnames(bb) <- c("min", "max")
  rownames(bb) <- c("x", "y")
  if (opt$debug) {print(bb)}
} else if (opt$verbose) {
  print("No user-defined margins for maps. They will be determined by default")
  # bb <- as.matrix(cbind(c(round(opt$lon - 40), round(opt$lat - 40)), c(round(opt$lon + 40), round(opt$lat + 40))))
}

# Time points to use for the windrose histograms
windrose_times <- as.vector(as.numeric(strsplit(x = gsub(pattern = "'", replacement = "", x = opt$windrose_times), 
                                                 split = ",", fixed = T)[[1]]))

if(opt$debug){print(windrose_times)}

if(opt$debug){print("All is OK")}
if (opt$debug){quit()}


#### MAIN ####
# Split the input strings into their components
from_split <- (strsplit(opt$from, split = "-")[[1]])
to_split <- (strsplit(opt$to, split = "-")[[1]])

# Names of each component (only used for error printing)
names <- c("year", "month", "day", "hour", "minute")

# Get vector of increments of the different time units
by_year_month_day_hour <- c(opt$byyear, opt$bymonth, opt$byday, opt$byhour)

# Get the number of blocks
block_num = 0
for (i in from_split) {
  # print(i)
  value <- gsub(pattern = "(", replacement = "", i, fixed = T)
  value <- gsub(pattern = ")", replacement = "", value, fixed = T)
  blocks <- strsplit(x = value, split = ",")[[1]]
  if (block_num <length(blocks)) {
    block_num = length(blocks)
  }
}

# Make a list with "from" and "to" dates of all blocks
blocks_list <- list()
for (n in 1:block_num) {
  from_date <- c()
  for (i in from_split) {
    value <- gsub(pattern = "(", replacement = "", i, fixed = T)
    value <- gsub(pattern = ")", replacement = "", value, fixed = T)
    blocks <- strsplit(x = value, split = ",")[[1]]
    if (length(blocks) == 1) {
      from_date[length(from_date)+1] <- blocks
    } else {
      from_date[length(from_date)+1] <- blocks[n]
    }
  }
  to_date <- c()
  for (i in to_split) {
    value <- gsub(pattern = "(", replacement = "", i, fixed = T)
    value <- gsub(pattern = ")", replacement = "", value, fixed = T)
    blocks <- strsplit(x = value, split = ",")[[1]]
    if (length(blocks)==1) {
      to_date[length(to_date)+1] <- blocks
    } else {
      to_date[length(to_date)+1] <- blocks[n]
    }
  }
  blocks_list[[length(blocks_list)+1]] <- list(list(from_date, to_date))
}


#### Create the complete list of dates and times at which trajectories should be started ####
# Parse the provided input to create the whole list of datetimes from which to start Hysplit runs

blocks_list <- lapply(X = blocks_list, 
                      FUN = generate_datetimes_lists, 
                      by_year_month_day_hour = by_year_month_day_hour)

# Turn the dates into actual Date objects
blocks_list <- lapply(X = blocks_list, FUN = rejoin_dates, TZ="UCT")

# Remove NAs (i.e. impossible dates)
blocks_list <- lapply(X = blocks_list, FUN = remove_missing_dates)



#### Download meteorological files ####
# Generate list with the dates of the previous month
prevDates <- lapply(X = blocks_list, FUN = get_prev_post_dates, increment = -1)

# Get meteorological files:
if (opt$verbose){print("Downloading necessary meteorological files...")}
lapply(X = prevDates, FUN = get_met_files)


# Generate list with dates of next month
postDates <- lapply(X = blocks_list, FUN = get_prev_post_dates, increment = 1)

# Get the files; as before the loop should save time
lapply(X = postDates, FUN = get_met_files)


# Get the files for the dates of interest; since the files are by month it does not matter here if the list has more days than needed, it will skip files already downloaded
lapply(X = blocks_list, FUN = get_met_files)

if(opt$verbose){print("All files successfully downloaded.")}

#### Calculate trajectories ####
if(opt$verbose){print("Starting trajectory calculations. Please wait...")}

trajs <- lapply(X=blocks_list, FUN = compute_trajectories, hourInt = 1, 
       latlon = coord, h = height, duration = duration)

#### Plot raster maps ####
if(opt$verbose){print("Plotting raster maps...")}
pdf(outfile)

print(length(trajs[[1]]$hour.inc[trajs[[1]]$hour.inc == 0 &
                                   trajs[[1]]$start_height == unique(trajs[[1]]$start_height)[1]]))

if (length(trajs[[1]]$hour.inc[trajs[[1]]$hour.inc == 0&
                               trajs[[1]]$start_height == unique(trajs[[1]]$start_height)[1]])>1){
  lapply(X=trajs, plot_raster_maps, height = height, PRJ = PRJ)
} else if (opt$verbose){
  print("WARNING: No raster maps will be generated as only one trajectory per height is being run")
}


#### Plot Trajlines####
if(opt$verbose){print("Plotting trajectories...")}
lapply(X=trajs, FUN = plot_trajlines, PRJ = PRJ)


#### Plot windrose histograms ####
if(opt$verbose){print("Plotting windrose histograms...")}
trajs <- lapply(X = trajs, FUN = plot_windrose_hist, height = height, duration = windrose_times)


#### Plot altitudinal profile plots ####
if(opt$verbose){print("Plotting altitudinal profiles...")}
lapply(X = trajs, FUN = plot_altitudinal_profile)


dev.off()
if(opt$verbose){print(paste0("All plots saved to ", outfile))}

#### Save image of data ####
save.image(paste0(outfile,".RData"))

if(opt$verbose) {
  cat(paste0("All R objects saved to ", outfile,".RData"))
}

quit()