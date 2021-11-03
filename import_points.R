### Starting Over for Importing Prey Survey Points ###

### Load library

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


#######################################################################

### Load data
points.19 <- read.csv(here("data/Point_Count_Env_2019_MG_cleaned.csv"))
gpx.19 <- read.csv(here("data/finalwp_2019.csv"))

# match key id names
gpx.19 <- rename(gpx.19, Waypoint_num = name)
# add leading zero to key id in points.19
points.19$Waypoint_num <- str_pad(points.19$Waypoint_num, 3, pad = "0") #adds leading 0

points.gpx.join <- left_join(points.19, gpx.19)

points.gpx.join$id <- paste(points.gpx.join$Road_ID, points.gpx.join$UNIT_ID, points.gpx.join$Transect_ID, points.gpx.join$Point_ID, sep = "_")
head(points.gpx.join)


colnames(points.gpx.join)

points.2019 <- points.gpx.join[,c(35, 28:30)]



#######################################################################

points.2019 <- points.2019[complete.cases(points.2019),]



points19.sf <- st_as_sf(points.2019, 
                         coords = c("lon", "lat"),
                         crs = 4326)

plot(st_geometry(points19.sf))

crs(points19.sf)

st_is_valid(points19.sf)

###############################################################

points.21 <- read.csv(here("data/Point_Count_Env_2021_MG.csv"))
head(points.21); dim(points.21)

# subset id, utms, and elev
colnames(points.21)
points.21 <- points.21[,c(1:4, 19:21)]
points.21$id <- paste(points.21$ROAD_ID, points.21$UNIT_ID, points.21$TRANSECT_ID, points.21$POINT_ID, sep = "_")

head(points.21)
#subset again
points.21 <- points.21[,c(8, 5:7)]
head(points.21); dim(points.21)
# export
# write.csv(latlong21, here("data/utms_21.csv"))

st_crs(points.21)
# need to assign crs
# points were taken in UTM UPS, NAD 83, GRS 80
# crs: 6332

points.21.sf <- st_as_sf(points.21, 
                         coords = c("UTM1", "UTM2"),
                         crs = 26903)

st_crs(points.21.sf)
plot(points.21.sf)

st_is_valid(points.21.sf)


############## STOP HERE - JUST NEED TO REPROJECT INTO SAME CRS AS RASTER

nlcd.rast <- rast("C:/Gyrfalcon/gyrfalcon/NLCD_2016_Land_Cover_AK_20200724.img")
plot(nlcd.rast)

crs(nlcd.rast, describe = TRUE)


# reproject points to same crs as hab raster

#st_transform

# 2019 points
points19.proj <- st_transform(points19.sf, crs(nlcd.rast))
st_crs(points19.proj)
plot(points19.proj)

# 2021 points
points21.proj <- st_transform(points.21.sf, crs(nlcd.rast))
st_crs(points21.proj)


plot(points21.proj)


##### CREATE A BOUNDING BOX 

#bbox <- st_bbox(c(xmin = -590784, ymin = 1652109, xmax = -4604851, ymax = 1783540), crs = st_crs(latlong19.proj))
?terra::crop
?ext
e <- ext(-600000, -450485.1, 1600000, 1800000)
plot(nlcd.rast)
plot(e, add = TRUE)


##### CROP HABITAT RASTER TO BBOX EXTENT 
nlcd.crop <- terra::crop(nlcd.rast, e)

plot(nlcd.crop)
plot(points19.proj, add = TRUE)
plot(points21.proj[1], add = TRUE)

######################################################################

### BUFFER ###

points19.buff <- st_buffer(points19.proj, 400)

points21.buff <- st_buffer(points21.proj, 400)


#######################################################################

# Extract raster information for buffers
































































