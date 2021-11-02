### Starting Over for Importing Prey Survey Points ###


library(sf)
library(terra)
library(sp)
library(here)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(taRifx)
library(tidyr)
library(dplyr)
library(stringr)

points.19 <- read.csv(here("data/Point_Count_Env_2019_MG_cleaned.csv"))
gpx.19 <- read.csv(here("data/finalwp_2019.csv"))

# match key id names
gpx.19 <- rename(gpx.19, Waypoint_num = name)
# add leading zero to key id in points.19
points.19$Waypoint_num <- str_pad(points.19$Waypoint_num, 3, pad = "0") #adds leading 0

points.gpx.join <- left_join(points.19, gpx.19)


