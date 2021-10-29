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


# convert 19 lat long data in r

latlong19 <- read.csv(here("data/Point_Count_Env_2019_MG_cleaned.csv"))

# create id column
latlong19$id <- paste(latlong19$Road_ID, latlong19$UNIT_ID, latlong19$Transect_ID, latlong19$Point_ID, sep = "_")
head(latlong19)

colnames(latlong19)

latlong19 <- latlong19[,c(27, 18)]


latlong19 <- latlong19 %>%
  separate(Coordinates, c("Lat", "Lon"), "W")

# keep only numbers
latlong19$Lat <- as.character(destring(latlong19$Lat, keep="0-9-"))
latlong19$Lon <- as.character(destring(latlong19$Lon, keep="0-9-")) 

# separate into d m s
#lat: every 2 then 2
#lon: every 3 then 2

latlong19 <- latlong19 %>% 
  separate(Lat, into = c("d", "m"), sep = 2)
latlong19 <- latlong19 %>% 
  separate(m, into = c("m", "s"), sep = 2)

latlong19$Lat <- paste(latlong19$d, latlong19$m, latlong19$s, sep = " ")


latlong19 <- latlong19 %>% 
  separate(Lon, into = c("d2", "m2"), sep = 3)

latlong19 <- latlong19 %>% 
  separate(m2, into = c("m2", "s2"), sep = 2)

latlong19$Lon <- paste(latlong19$d2, latlong19$m2, latlong19$s2, sep = " ")




latlong19 <- latlong19 %>% 
  separate(Lat, paste("lat",c("d","m","s"), sep="_"))

latlong19 <- latlong19 %>% 
  separate(Lon, paste("lon",c("d2","m2","s2"), sep="_"))

str(latlong19)
latlong19$lat_d <- as.numeric(latlong19$lat_d)
latlong19$lat_m <- as.numeric(latlong19$lat_m)
latlong19$lat_s <- as.numeric(latlong19$lat_s)
latlong19$lon_d2 <- as.numeric(latlong19$lon_d2)
latlong19$lon_m2 <- as.numeric(latlong19$lon_m2)
latlong19$lon_s2 <- as.numeric(latlong19$lon_s2)

?transmute

latlong19_dd <- latlong19 %>%
  transmute(lat_dec=lat_d + lat_m/60 + lat_s/60^2,
            long_dec=lon_d2 + lon_m2/60 + lon_s2/60^2,
            id = latlong19$id)


head(latlong19_dd)
latlong19_dd$long_dec <- latlong19_dd$long_dec*-1

head(latlong19_dd)
# oh boy

#export
# write.csv(latlong19_dd, here("data/latlong19_dd.csv"))
# https://stackoverflow.com/questions/30879429/how-can-i-convert-degree-minute-sec-to-decimal-in-r
# read in 2019 lat long as spatial data


# assign crs that data was collected in: 
# maybe this is where I've gone wrong? Had the wrong crs initially? 
# but I checked that this was what it would have been in the GPS
# also need to go through and get elevations from ARCGIS or basecamp
# need to get 002 GPS from MIKE
# are these weird becuase I converted them from dms to dd?
latlong19.sf <- st_as_sf(latlong19_dd, 
                       coords = c("long_dec", "lat_dec"),
                       crs = 4326)

plot(latlong19.sf)

crs(latlong19.sf)

st_is_valid(latlong19.sf)

############ 2021 DATA ##############################
latlong21 <- read.csv(here("data/Point_Count_Env_2021_MG.csv"))
head(latlong21); dim(latlong21)

# subset id, utms, and elev
colnames(latlong21)
latlong21 <- latlong21[,c(1:4, 19:21)]
latlong21$id <- paste(latlong21$ROAD_ID, latlong21$UNIT_ID, latlong21$TRANSECT_ID, latlong21$POINT_ID, sep = "_")

head(latlong21)
#subset again
latlong21 <- latlong21[,c(8, 5:7)]
head(latlong21); dim(latlong21)
# export
# write.csv(latlong21, here("data/utms_21.csv"))

st_crs(latlong21)
# need to assign crs
# points were taken in UTM UPS, NAD 83, GRS 80
# crs: 6332

latlong21.sf <- st_as_sf(latlong21, 
                         coords = c("UTM1", "UTM2"),
                         crs = 26903)

st_crs(latlong21.sf)
plot(latlong21.sf)

st_is_valid(latlong21.sf)

############## STOP HERE - JUST NEED TO REPROJECT INTO SAME CRS AS RASTER

nlcd.test <- rast("C:/Gyrfalcon/gyrfalcon/NLCD_2016_Land_Cover_AK_20200724/NLCD_2016_Land_Cover_AK_20200724.img")
plot(nlcd.test)

crs(nlcd.test, describe = TRUE)


# reproject points to same crs as hab raster

#st_transform

# 2019 points
latlong19.proj <- st_transform(latlong19.sf, crs(nlcd.test))
st_crs(latlong19.proj)
plot(latlong19.proj)

# 2021 points
latlong21.proj <- st_transform(latlong21.sf, crs(nlcd.test))
st_crs(latlong21.proj)

st_crs(latlong21.proj)$epsg


plot(latlong21.proj)



##### CREATE A BOUNDING BOX 


latlong19.proj
latlong21.proj
nlcd.test
st_crs(latlong19.proj)

#bbox <- st_bbox(c(xmin = -590784, ymin = 1652109, xmax = -4604851, ymax = 1783540), crs = st_crs(latlong19.proj))
?terra::crop
?ext
e <- ext(-600000, -450485.1, 1600000, 1800000)
plot(nlcd.test)
plot(e, add = TRUE)

##### CROP HABITAT RASTER TO BBOX EXTENT 
nlcd.crop <- terra::crop(nlcd.test, e)

plot(nlcd.crop)
plot(latlong19.proj, add = TRUE)

plot(nlcd.crop)
plot(latlong21.proj[1], add = TRUE)


crs(latlong21.proj) 
st_crs(latlong21.proj)

# no part of crs, st_crs, proj4string, wkt match. That's essentially what this means:

crs(nlcd.test) == st_crs(latlong19.proj)
crs(nlcd.test) == st_crs(latlong21.proj)

st_crs(latlong21.proj)$proj4string
st_crs(nlcd.test)$Wkt

st_crs(nlcd.test)$Wkt
st_crs(latlong21.proj)$Wkt
crs(nlcd.test) == st_crs(latlong21.proj)$Wkt
crs(nlcd.test) == st_crs(latlong21.proj)$WktPretty

# figure out which part of wkts don't match

a <- crs(nlcd.test)
b <- crs(latlong21.proj)


a <- CRS(a)

mapply(function(x, y) which(x != y), 
       strsplit(a, ","), strsplit(b, ","))



