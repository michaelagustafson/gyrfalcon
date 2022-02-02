library(terra)
library(sf)
library(sp)
library(here)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(tidyr)
library(dplyr)
library(stringr)
library(raster)

###### dem ######
dem <- raster("E:/Dropbox/Back&Forth/GRYF/michaela/DEM_Michaela/dsm_mosaic_AK.tif")

####### load and mosaic land cover rasters ######
rastlist <- list.files(path = "E:/Dropbox/Back&Forth/GRYF/michaela/ABoVE_2014lyrs", pattern='.tif$', all.files=TRUE, full.names = TRUE)
all <- lapply(rastlist, raster)
all.mos <- do.call(merge, all)
all.mos[all.mos <= 0 ] <- NA
writeRaster(all.mos, "E:/Dropbox/Back&Forth/GRYF/michaela/ABoVE_2014lyrs/all.mos.tif", overwrite = TRUE)

############# data points #############
points.19 <- read.csv("E:/Dropbox/Back&Forth/GRYF/michaela/Point_Count_Env_2019_MG_cleaned.csv")
gpx.19 <- read.csv("E:/Dropbox/Back&Forth/GRYF/michaela/finalwp_2019.csv")
ogpoints.19 <- read.csv("E:/Dropbox/Back&Forth/GRYF/michaela/originalpoints_2019.csv", skip = 22)
backup.19 <- read.csv("E:/Dropbox/Back&Forth/GRYF/michaela/GPS002_2019backup.csv", skip = 22)

# There are extra '1' at the end of the waypoint number names, need to remove them so the key id will match when merging dataframes
backup.19$name = str_remove(backup.19$name, "^0")

# match key id names
gpx.19 <- rename(gpx.19, Waypoint_num = name)
# add leading zero to key id in points.19 to match key id (Waypoint Name)
points.19$Waypoint_num <- str_pad(points.19$Waypoint_num, 3, pad = "0")
# join points and locations from GPS together
points.gpx.join <- left_join(points.19, gpx.19)

# create id key column
points.gpx.join$id <- paste(points.gpx.join$Road_ID, points.gpx.join$UNIT_ID, points.gpx.join$Transect_ID, points.gpx.join$Point_ID, sep = "_")
head(points.gpx.join)
colnames(points.gpx.join)

# only keep columns need
points.2019 <- points.gpx.join[,c(35, 28:29)]
head(points.2019)
# lat lon are empty because I will be pulling that data in from the correct gpx file or original points file. 

# separating out the first 17 surveyed points that don't have matching waypoint
# names in the gps files
missing.points <- points.gpx.join[c(1:17),]

# separate into KOUG and TELL points for naming purposes
miss.points.koug <- missing.points[c(1:11),]
miss.points.tell <- missing.points[c(12:17),]
# Need id names to match those on the points gpx

# Add K or T to column characters respectively so that columns have matching id keys
# and I can bind/join table together 
miss.points.koug$UNIT_ID <- paste("K", miss.points.koug$UNIT_ID, sep = "")
miss.points.tell$UNIT_ID <- paste("T", miss.points.tell$UNIT_ID, sep = "")
miss.points.koug$Transect_ID <- paste("T", miss.points.koug$Transect_ID, sep = "")
miss.points.tell$Transect_ID <- paste("T", miss.points.tell$Transect_ID, sep = "")

# bind all points back together into one dataframe
missing.points <- rbind(miss.points.koug, miss.points.tell)

# create the universal id column in the dataframe of missing points
missing.points$name <- paste(missing.points$UNIT_ID, missing.points$Transect_ID, missing.points$Point_ID, sep = "_")

# join the missing points to dataframe to data taken off GPS to get true lat/lon
# for these points
miss.og.point.join <- left_join(missing.points, ogpoints.19, by = "name")

# only keep columns needed and rename them
colnames(miss.og.point.join)
miss.og.points <- miss.og.point.join[,c(36, 38,39)]
miss.og.points <- rename(miss.og.points, "lat" = "lat.y")
miss.og.points <- rename(miss.og.points, "lon" = "lon.y")
miss.og.points <- rename(miss.og.points, "id" = "name")

### NEED TO CHANGE Column ID NAMES BACK TO MATCH POINTS 2019
koug.names <- as.data.frame(miss.points.koug$id)
koug.names <- rename(koug.names, 'id' = 'miss.points.koug$id')
tell.names <- as.data.frame(miss.points.tell$id)
tell.names <- rename(tell.names, 'id' = 'miss.points.tell$id')
miss.names <- rbind(koug.names, tell.names)

miss.og.points$id <- miss.names$id
str(miss.og.points)

#######################################################################
# remove those incorrect 'missing points' from the dataframe so I can correctly add them in later
points.2019 <- points.2019[complete.cases(points.2019),]

# add corrected points with true lat/lon
points.2019.all <- rbind(miss.og.points, points.2019)

# save CSV of these fixed points and full df for safe keeping
write.csv(points.2019.all, "E:/Dropbox/Back&Forth/GRYF/michaela/points_2019_all_FINAL.csv")

## assign crs
xy <- points.2019.all[c(3,2)]
colnames(xy)[1] <- "x"; colnames(xy)[2] <- "y"
xy[,1:2] <- sapply(xy[,1:2], as.numeric)
coordinates(xy) <- ~ x + y
spdf <- SpatialPointsDataFrame(coords = xy, data = points.2019.all)
proj4string(spdf) <- CRS("+init=epsg:4326")
points19.proj <- spTransform(spdf, crs(dem))
plot(dem); points(points19.proj)
writeOGR(points19.proj, dsn="E:/Dropbox/Back&Forth/GRYF/michaela", layer="pts19" , driver="ESRI Shapefile")
points19.proj.lc <- spTransform(spdf, crs(all.mos))
writeOGR(points19.proj.lc, dsn="E:/Dropbox/Back&Forth/GRYF/michaela", layer="pts19.proj.lc" , driver="ESRI Shapefile")

##### 2021 #####
points.21 <- read.csv("E:/Dropbox/Back&Forth/GRYF/michaela/Point_Count_Env_2021_MG.csv")
head(points.21); dim(points.21)

# subset id and utms
colnames(points.21)
points.21 <- points.21[,c(1:4, 19:20)]
points.21$id <- paste(points.21$ROAD_ID, points.21$UNIT_ID, points.21$TRANSECT_ID, points.21$POINT_ID, sep = "_")

head(points.21)
#subset again to keep only final key id and utms
points.21 <- points.21[,c(7, 5:6)]
head(points.21); dim(points.21)
# export for safe keeping
write.csv(points.21, "E:/Dropbox/Back&Forth/GRYF/michaela/utms_21_final.csv")

xy <- points.21[c(2,3)]
colnames(xy)[1] <- "x"; colnames(xy)[2] <- "y"
coordinates(xy) <- ~ x + y
spdf <- SpatialPointsDataFrame(coords = xy, data = points.21)
proj4string(spdf) <- CRS("+init=epsg:26903")
points21.proj <- spTransform(spdf, crs(dem))
plot(dem); points(points21.proj)
writeOGR(points21.proj, dsn="E:/Dropbox/Back&Forth/GRYF/michaela", layer="pts21" , driver="ESRI Shapefile")
points21.proj.lc <- spTransform(spdf, crs(all.mos))
writeOGR(points21.proj.lc, dsn="E:/Dropbox/Back&Forth/GRYF/michaela", layer="pts21.proj.lc" , driver="ESRI Shapefile")

points19.proj.lc@data$year <- 2019
points21.proj.lc@data$year <- 2021
new <- bind(points19.proj.lc[c(1,4)], points21.proj.lc[c(1,4)])
writeOGR(new, dsn="E:/Dropbox/Back&Forth/GRYF/michaela", layer="pts19&21.proj.lc" , driver="ESRI Shapefile")

#### make study extent polygon + some extra ######
pts.all <- readOGR("E:/Dropbox/Back&Forth/GRYF/michaela", "pts19&21.proj.lc")

e <- extent(points19.proj) 
p <- as(e, 'SpatialPolygons') 
proj4string(p) <- "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "
e2 <- rgeos::gBuffer(p, width = 5000)

#### clip dem and land cover by new extent ####
dem.clip <- crop(dem, e2)
dem.clip <- mask(dem.clip, e2)
writeRaster(dem.clip, "E:/Dropbox/Back&Forth/GRYF/michaela/dem.clip.tif", overwrite = TRUE)
dem.clip.lc <- projectRaster(dem.clip, crs = "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" )
writeRaster(dem.clip.lc, "E:/Dropbox/Back&Forth/GRYF/michaela/dem.clip.lc.tif", overwrite = TRUE)

e2.lc <- spTransform(e2, CRS("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" ))
all.mos.clip <- crop(all.mos, e2.lc)
all.mos.clip <- mask(all.mos.clip, e2.lc)
writeRaster(all.mos.clip, "E:/Dropbox/Back&Forth/GRYF/michaela/all.mos.clip.tif", overwrite = TRUE)

#### resample dem to 30 x 30 to match land cover #######
all.mos.clip <- raster("E:/Dropbox/Back&Forth/GRYF/michaela/all.mos.clip.tif")
df <- as.data.frame(all.mos.clip, xy = TRUE)
res(all.mos.clip) <- 30
all.mos.clip.30 <- all.mos.clip

dem.clip.lc <- raster("E:/Dropbox/Back&Forth/GRYF/michaela/dem.clip.lc.tif")
dem.clip.lc.30 <- resample(dem.clip.lc, all.mos.clip.30, method = "bilinear")
writeRaster(dem.clip.lc.30, "E:/Dropbox/Back&Forth/GRYF/michaela/dem.clip.lc.30.tif", overwrite = TRUE)

m <- matrix(1, ncol=13, nrow=13)
d <- focal(dem.clip.lc.30, m, fun="mean", na.rm=TRUE, NAonly=TRUE, pad=TRUE, filename="E:/Dropbox/Back&Forth/GRYF/michaela/dem.lc.30.focal.tif", overwrite=TRUE)
writeRaster(d, "E:/Dropbox/Back&Forth/GRYF/michaela/dem.clip.lc.30.focal.tif", overwrite = TRUE)

 ############### create focal land cover layers ########
all.mos.clip <- raster("E:/Dropbox/Back&Forth/GRYF/michaela/all.mos.clip.tif")

egforest <- (all.mos.clip == 1)
decforest <- (all.mos.clip == 2)
mixforest <- (all.mos.clip == 3)
woodland <- (all.mos.clip == 4)
lowshrub <- (all.mos.clip == 5)
tallshrub <- (all.mos.clip == 6)
openshrub <- (all.mos.clip == 7)
tundra <- (all.mos.clip == 8)
tussock <- (all.mos.clip == 9)
sparseveg <- (all.mos.clip == 10)
fen <- (all.mos.clip == 11)
bog <- (all.mos.clip == 12)
littoral <- (all.mos.clip == 13)
barren <- (all.mos.clip == 14)
water <- (all.mos.clip == 15)

lc_stack <- raster::stack(egforest, decforest, mixforest, woodland, lowshrub, tallshrub,
                          openshrub, tundra, tussock, sparseveg, fen, bog, littoral, barren,
                          water)

lc_stack2 <- list()
lc_stack3 <- list()

# heres the loop
for(i in 1:length(lc_stack)){
  lc_stack2[[i]] <- raster::focal(lc_stack[[i]], w=matrix(1, nrow=13, ncol=13), fun=sum, na.rm=TRUE)
  lc_stack3[[i]] <- (lc_stack2[[i]]/169)
}

lc_stack3 <- raster::stack(lc_stack3)
# now change the names of the columns to match what habitat type they actually are
names(lc_stack3) <- c("egforest", "decforest", "mixforest", "woodland", "lowshrub", "tallshrub", "openshrub", "tundra", "tussock", 'sparseveg', 'fen', 'bog', 'littoral', 'barren', 'water')

# save each layer as it own tif
setwd("E:/Dropbox/Back&Forth/GRYF/michaela/ABoVE_2014lyrs/focal")
writeRaster(lc_stack3, filename=names(lc_stack3), bylayer=TRUE, format="GTiff", overwrite = TRUE)

########### bring in all focal rasters to extract to points #########
##########################################
rastlist <- list.files(path = "E:/Dropbox/Back&Forth/GRYF/michaela/ABoVE_2014lyrs/focal", pattern='.tif$', all.files=TRUE, full.names = TRUE)
all <- lapply(rastlist, raster)
a <- stack(all)

dem <- raster("E:/Dropbox/Back&Forth/GRYF/michaela/dem.clip.lc.30.focal.tif")

a2 <- stack(a, dem)

pts <- readOGR("E:/Dropbox/Back&Forth/GRYF/michaela", "pts19&21.proj.lc")

pts.extract <- raster::extract(a2, pts, df=TRUE)
df <- as.data.frame(pts)
pts.covs <- cbind(df, pts.extract)
pts.covs <- pts.covs[-c(5)]
colnames(pts.covs)[20] <- "elev"

write.csv(pts.covs, "E:/Dropbox/Back&Forth/GRYF/michaela/pts.covs.csv")


