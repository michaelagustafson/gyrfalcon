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
ogpoints.19 <- read.csv("C:/Gyrfalcon/gyrfalcon/data/originalpoints_2019.csv", skip = 22)
backup.19 <- read.csv("C:/Gyrfalcon/gyrfalcon/data/GPS002_2019backup.csv", skip = 22)

backup.19$name = str_remove(backup.19$name, "^0")


# match key id names
gpx.19 <- rename(gpx.19, Waypoint_num = name)
# add leading zero to key id in points.19
points.19$Waypoint_num <- str_pad(points.19$Waypoint_num, 3, pad = "0") #adds leading 0

points.gpx.join <- left_join(points.19, gpx.19)

points.gpx.join$id <- paste(points.gpx.join$Road_ID, points.gpx.join$UNIT_ID, points.gpx.join$Transect_ID, points.gpx.join$Point_ID, sep = "_")
head(points.gpx.join)


colnames(points.gpx.join)

points.2019 <- points.gpx.join[,c(35, 28:30)]

missing.points <- points.gpx.join[c(1:17),]

miss.points.koug <- missing.points[c(1:11),]
miss.points.tell <- missing.points[c(12:17),]
# Need id names to match those on the points gpx

miss.points.koug$UNIT_ID <- paste("K", miss.points.koug$UNIT_ID, sep = "")
miss.points.tell$UNIT_ID <- paste("T", miss.points.tell$UNIT_ID, sep = "")
miss.points.koug$Transect_ID <- paste("T", miss.points.koug$Transect_ID, sep = "")
miss.points.tell$Transect_ID <- paste("T", miss.points.tell$Transect_ID, sep = "")

missing.points <- rbind(miss.points.koug, miss.points.tell)

missing.points$name <- paste(missing.points$UNIT_ID, missing.points$Transect_ID, missing.points$Point_ID, sep = "_")

?left_join
miss.og.point.join <- left_join(missing.points, ogpoints.19, by = "name")

colnames(miss.og.point.join)
miss.og.points <- miss.og.point.join[,c(36, 39, 38, 40)]
miss.og.points <- rename(miss.og.points, "lat" = "lat.y")
miss.og.points <- rename(miss.og.points, "lon" = "lon.y")
miss.og.points <- rename(miss.og.points, "ele" = "ele.y")
miss.og.points <- rename(miss.og.points, "id" = "name")


### NEED TO CHANGE ID NAMES BACK TO MATCH POINTS 2019

koug.names <- as.data.frame(miss.points.koug$id)
koug.names <- rename(koug.names, 'id' = 'miss.points.koug$id')
tell.names <- as.data.frame(miss.points.tell$id)
tell.names <- rename(tell.names, 'id' = 'miss.points.tell$id')
miss.names <- rbind(koug.names, tell.names)


miss.og.points$id <- miss.names$id
str(miss.og.points)


#######################################################################

points.2019 <- points.2019[complete.cases(points.2019),]

points.2019.all <- rbind(miss.og.points, points.2019)

points19.sf <- st_as_sf(points.2019.all, 
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

nlcd.rast <- raster::raster("C:/Gyrfalcon/gyrfalcon/NLCD_2016_Land_Cover_AK_20200724.img")
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

?ext
e <- raster::extent(-600000, -450485.1, 1600000, 1800000)
plot(nlcd.rast)
plot(e, add = TRUE)


##### CROP HABITAT RASTER TO BBOX EXTENT 
nlcd.crop <- raster::crop(nlcd.rast, e)

plot(nlcd.crop)
plot(points19.proj, add = TRUE)
plot(points21.proj[1], add = TRUE)


######################################################################
# extract landcover within 400m buffer:

lc.19 <- raster::extract(nlcd.crop, points19.proj, buffer = 400)
lc.21 <- raster::extract(nlcd.crop, points21.proj, buffer = 400)
#######################################################################

landcover_proportions <- lapply(lc.19, function(x) {
  counts_x <- table(x)
  proportions_x <- prop.table(counts_x)
  sort(proportions_x)
})
lc.19.table <- sort(unlist(landcover_proportions))

lc.19.df <- data.frame(Category = names(lc.19.table), Value = lc.19.table)

dumb <- lapply(1:length(lc.19), function(x) {
  counts_x <- table(lc.19[[x]])
  proportions_x <- as.data.frame(prop.table(counts_x))
  proportions_x$ID <- x
  return(proportions_x)
  })

dumb2 <- do.call(rbind, dumb)


# 2021 points


landcover_proportions21 <- lapply(lc.21, function(x) {
  counts_x21 <- table(x)
  proportions_x21 <- prop.table(counts_x21)
  sort(proportions_x21)
})
lc.21.table <- sort(unlist(landcover_proportions21))


lc.21.df <- data.frame(Category = names(lc.21.table), Value = lc.21.table)

dumb21 <- lapply(1:length(lc.21), function(x) {
  counts_x21 <- table(lc.21[[x]])
  proportions_x21 <- as.data.frame(prop.table(counts_x21))
  proportions_x21$ID <- x
  return(proportions_x21)
})


dumb3 <- do.call(rbind, dumb21)

#### Now need to change land class values to names and spread dataframe longways

# import classificaiton legend

lc.class <- read.csv(here("data/lclegend.csv"))
head(lc.class)
# check structure and make sure Var1 is a factor 
str(lc.class)
lc.class$Var1 <- as.factor(lc.class$Var1)

# change name of classification column
lc.class <- rename(lc.class, Land_Class = "X")
head(lc.class)

# join with dumb2 df
dumb2 <- left_join(dumb2, lc.class)

# subset and spread 

lc.19.final <- dumb2 %>%
  dplyr::select(ID, Land_Class, Freq) %>%
  spread(key = Land_Class, value = Freq)

# now do the same for 2021

dumb3 <- left_join(dumb3, lc.class)

unique(dumb3$Land_Class)
# subset and spread 

lc.21.final <- dumb3 %>%
  dplyr::select(ID, Land_Class, Freq) %>%
  spread(key = Land_Class, value = Freq)

# now combine id names back with original

lc.19.final$id <- points19.proj$id
lc.21.final$id <- points21.proj$id


# now summarise columns together to reduce habitat variables:

lc.19.final <- lc.19.final %>%
  replace(is.na(.), 0)

lc.19.final <- lc.19.final %>%
  mutate(Tundra = `Grassland/Herbaceous` + `Sedge/Herbaceous`, #separate tussocks??
         Low_Shrub = `Dwarf Scrub`,
         Bare_Ground = `Barren Land (Rock/Sand/Clay)`,
         Tall_Shrub_Forest = `Shrub/Scrub` + `Mixed Forest` + `Deciduous Forest` + `Evergreen Forest`,
         Wetlands = `Emergent Herbaceous Wetlands` + `Woody Wetlands`,
         Developed = `Developed, Low Intensity`,
         Open_Water = `Open Water`)

lc.19.fin.clip <- lc.19.final %>%
  dplyr::select(id, Tundra, Low_Shrub, Bare_Ground, Tall_Shrub_Forest, Wetlands, Developed, Open_Water)





lc.21.final <- lc.21.final %>%
  replace(is.na(.), 0)

lc.21.final <- lc.21.final %>%
  mutate(Tundra = `Grassland/Herbaceous` + `Sedge/Herbaceous`,
         Low_Shrub = `Dwarf Scrub`,
         Bare_Ground = `Barren Land (Rock/Sand/Clay)`,
         Tall_Shrub_Forest = `Shrub/Scrub` + `Mixed Forest` + `Deciduous Forest` + `Evergreen Forest` + `Woody Wetlands`,
         Wetlands = `Emergent Herbaceous Wetlands`,
         Developed = `Developed, Low Intensity`,
         Open_Water = `Open Water`)


lc.21.fin.clip <- lc.21.final %>%
  dplyr::select(id, Tundra, Low_Shrub, Bare_Ground, Tall_Shrub_Forest, Wetlands, Developed, Open_Water)


# export as .csvs?

write.csv(lc.19.fin.clip, here("data/points19_habitat.csv"))
write.csv(lc.21.fin.clip, here("data/points21_habitat.csv"))

############################################################
############################################################

# LANDFIRE Categories:

lf.tif <- terra::rast(here("LF2016_EVT_200_AK/Tif/LA16_EVT_200.tif"))

plot(lf.tif)

# crop

e <- terra::ext(-600000, -450485.1, 1600000, 1800000)
plot(lf.tif)
plot(e, add = TRUE)


##### CROP HABITAT RASTER TO BBOX EXTENT 

lf.crop <- terra::crop(lf.tif, e)
plot(lf.crop)
crs(lf.crop)
library(foreign)
lf.dbf <- read.dbf(here("LF2016_EVT_200_AK/Tif/LA16_EVT_200.tif.vat.dbf"))

str(lf.crop)

lf.crop <- left_join(lf.crop, lf.dbf)



