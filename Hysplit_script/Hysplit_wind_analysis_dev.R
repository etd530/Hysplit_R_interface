# Testing for the new date parsing
library(lubridate)

# Fake initial arguments
from = "2000-04-01-00-00"
to = "2020-06-10-23-00"

by_year_month_day_hour = c(1, 1, 1, 1)

# Split the input strings into their components
from_split <- (strsplit(from, split = "-")[[1]])
to_split <- (strsplit(to, split = "-")[[1]])
names <- c("year", "month", "day", "hour", "minute")


# Create the complete list of dates and times at which trajectories should be started
datesList <- list(list(from_split, to_split))

for (i in 1:4) {
  if (from_split[i] != to_split[i]) {
    if (by_year_month_day_hour[i]) {
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
      rm("datesList_tmp")
    } else {
        print(paste0("ERROR: starting and ending ", names[i], " differ but increment not provided"))
        quit()
      }
    }
}

# Remove duplicates
for (i in 1:length(datesList)) {
  datesList[[i]] <- datesList[[i]][[1]]
}

# #### Put the input strings into a list ####
# datesList <- list(list(from_split, to_split))
# 
# if (from_split$year != to_split$year) {
#   if (byyear) {
#     yearinterval = seq(from=from_split$year, to=to_split$year, by=byyear)
#     datesList <- list()
#     for (i in 1:length(yearinterval)) {
#       datesList[[i]] <- list(c(from_split$day, from_split$month, yearinterval[i], from_split$hm), c(to_split$day, to_split$month, yearinterval[i], to_split$hm))
#     }
#   } else {
#     print("ERROR: Strarting and ending years differ but argument 'by_year' was not provided")
#     quit()
#   }
# }
# #####
# if (from_split$month != to_split$month) {
#   if (bymonth) {
#     monthinterval <- seq(from=from_split$month, to=to_split$month, by=bymonth)
#     datesList_new <- list()
#     for (j in datesList) {
#       startdate <- j[[1]]
#       enddate <- j[[2]]
#       
#       for (i in 1:length(monthinterval)) {
#         datesList_new[[length(datesList_new)+1]] <- list(c(startdate[1], monthinterval[i], startdate[3], startdate[4]), 
#                                    c(enddate[1], monthinterval[i], enddate[3], enddate[4]))
#       }
#     }
#     datesList <- datesList_new
#     rm("datesList_new")
#   } else {
#     print("ERROR: Starting and ending months differ but argumen 'by_month' was not provided")
#   }
# }
