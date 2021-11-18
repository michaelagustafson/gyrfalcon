library(terra)
library(sf)
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

test.tif <- rast(here("data/Annual_Landcover_ABoVE_1691/Annual_Landcover_ABoVE_1691/data/ABoVE_LandCover_Bh04v01.tif"))
nlyr(test.tif)


test.tif.14 <- test.tif[[31]]
nlyr(test.tif.14)
plot(test.tif.14)

test.tif2 <- rast(here("data/Annual_Landcover_ABoVE_1691/Annual_Landcover_ABoVE_1691/data/ABoVE_LandCover_Bh04v02.tif"))
test.tif2.14 <- test.tif2[[31]]
plot(test.tif2.14)

test.tif3 <- rast(here("data/Annual_Landcover_ABoVE_1691/Annual_Landcover_ABoVE_1691/data/ABoVE_LandCover_Bh03v01.tif"))
test.tif3.14 <- test.tif3[[31]]
plot(test.tif3.14)

test.tif4 <- rast(here("data/Annual_Landcover_ABoVE_1691/Annual_Landcover_ABoVE_1691/data/ABoVE_LandCover_Bh03v02.tif"))
test.tif4.14 <- test.tif4[[31]]
plot(test.tif4.14)

?src
rast.list <- list(test.tif.14, test.tif2.14, test.tif3.14, test.tif4.14)
rsrc <- terra::src(rast.list)

m <- terra::mosaic(rsrc)

plot(m)
crs(m)

############ LOAD PREY SURVEY POINTS ######################

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

##############################################################
# reproject points to same crs as hab raster

#st_transform

# 2019 points
points19.proj <- st_transform(points19.sf, crs(m))
st_crs(points19.proj)
plot(points19.proj)

# 2021 points
points21.proj <- st_transform(points.21.sf, crs(m))
st_crs(points21.proj)


plot(points21.proj)



plot(m)
plot(points19.proj, add = TRUE)
plot(points21.proj[1], add = TRUE)


######################################################################
# extract landcover within 400m buffer:
lc.19.vect <- terra::vect(points19.proj)
lc.21.vect <- terra::vect(points21.proj)

# buffer
lc.19.buff <- terra::buffer(lc.19.vect, 400)
lc.21.buff <- terra::buffer(lc.21.vect, 400)

# extract
lc.19 <- raster::extract(m, lc.19.buff)
lc.21 <- raster::extract(m, lc.21.buff)
#######################################################################
###### Get extracted land cover types into proportions:

# create frequency table

# 2019
str(lc.19)
lc.19$ID <- as.factor(lc.19$ID)
lc.19$ABo_31 <- as.factor(lc.19$ABo_31)


str(lc.19)
lc.19.table <- table(lc.19)
lc.19.proptbl <- prop.table(lc.19.table, 1) # identifying as margin = 1 creates percentages
lc.19.df <- as.data.frame(lc.19.proptbl)

#2021
str(lc.21)
lc.21$ID <- as.factor(lc.21$ID)
lc.21$ABo_31 <- as.factor(lc.21$ABo_31)


str(lc.21)
lc.21.table <- table(lc.21)
lc.21.proptbl <- prop.table(lc.21.table, 1) # identifying as margin = 1 creates percentages
lc.21.df <- as.data.frame(lc.21.proptbl)


##### Read in ABoVE Land Class labels

aboveLC <- read.csv(here("data/ABoVE_LandClass_Labels.csv"))
str(aboveLC)
aboveLC$Code <- as.factor(aboveLC$Code)

lc.19.df <- rename(lc.19.df, Code = ABo_31)

lc.19.df <- left_join(lc.19.df, aboveLC)

?spread
lc19.final <- lc.19.df %>%
  dplyr::select(ID, Name, Freq) %>%
  spread(lc.19.df, key = Name, value = Freq)


# 2021

lc.21.df <- rename(lc.21.df, Code = ABo_31)

lc.21.df <- left_join(lc.21.df, aboveLC)

lc21.final <- lc.21.df %>%
  dplyr::select(ID, Name, Freq) %>%
  spread(lc.21.df, key = Name, value = Freq)

lc19.final$id <- points19.proj$id
lc21.final$id <- points21.proj$id


# Group some land classifications together:
colnames(lc19.final)
str(lc19.final)

lc19.final[,2:15] <- sapply(lc19.final[,2:15],as.numeric)


lc19.hab <- lc19.final %>%
  mutate(Tundra = `Herbaceous`,
         Tussock = `Tussock Tundra`, #separate tussocks??
         Low_Shrub = `Low Shrub`,
         Bare_Ground = (`Barren` + `Sparsely Vegetated`),
         Tall_Shrub = `Tall Shrub`,
         Open_Shrub = `Open Shrubs`,
         Forest = (`Deciduous Forest` + `Evergreen Forest` + `Mixed Forest` + Woodland),
         Wetland = (`Fen` + `Shallows/littoral`),
         Water = `Water`)
colnames(lc19.hab)
lc19.hab <- lc19.hab[,16:24]



# 2021

lc21.final[,2:15] <- sapply(lc21.final[,2:15],as.numeric)


lc21.hab <- lc21.final %>%
  mutate(Tundra = `Herbaceous`,
         Tussock = `Tussock Tundra`, #separate tussocks??
         Low_Shrub = `Low Shrub`,
         Bare_Ground = (`Barren` + `Sparsely Vegetated`),
         Tall_Shrub = `Tall Shrub`,
         Open_Shrub = `Open Shrubs`,
         Forest = (`Deciduous Forest` + `Evergreen Forest` + `Mixed Forest` + Woodland),
         Wetland = (`Fen` + `Shallows/littoral`),
         Water = `Water`)
colnames(lc21.hab)
lc21.hab <- lc21.hab[,16:24]


write.csv(lc19.hab, here("data/points19_ABoVEhab.csv"))
write.csv(lc21.hab, here("data/points21_ABoVEhab.csv"))











