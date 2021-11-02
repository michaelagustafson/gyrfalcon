library(dplyr)
library(plotKML)

# - List all filenames in folder starting with "Waypoint"
# Works for waypoint files with names like "Waypoints_22-FEB-16.gpx"
files <- list.files(pattern = "*.gpx*")

# Initialise empty data frame
wpfull <- NULL

for (i in 1:length(files)) {
  # - Select first file from the list and import data into R object
  wplist <- readGPX(files[i], way=T)
  # extract latitude, longituDe, elevation, time, name and comments and apppend to R dataframe
  wpdf<- wplist$waypoints
  # append dataframe from last index to a full waypoint object
  wpfull <- bind_rows(wpfull, wpdf)
}


# export object with all waypoints to csv file
write.csv(wpfull, "gyrfalcon/data/finalwp_2019.csv") 
