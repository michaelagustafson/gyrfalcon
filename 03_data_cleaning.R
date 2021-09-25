### ---------------------------------------------

## Title: Data Cleaning

## Author: Michaela L. Gustafson

## Description: Full data cleaning and merging of 2019 and 2021 prey
# survey data

## Date created: 08 September 2021

### ---------------------------------------------

### LIBRARY -------------------------------------

library(here) # for file location
library(dplyr) # general cleaning/manipulation
library(lubridate) # for Julian date conversion
library(tidyr) # general cleaning/manipulation
library(hms) # for making column into a time format
library(stringr) 


### END LIBRARY ---------------------------------

### IMPORT DATA ---------------------------------

# Importing the observation and environmental .csv files created
# in the previous module, 01_import_data.R

# I'm using the here package to call a file path instead of setwd(),
# this makes it easier for others to replicate my code

env.19 <- read.csv(here("data/env_main_19_short.csv")) # 2019 point environmental data
obs.19 <- read.csv(here("data/obs_main_19_short.csv")) # 2019 point observations
env.21 <- read.csv(here("data/env_main_21_short.csv")) # 2021 point environmental data
obs.21 <- read.csv(here("data/obs_main_21_short.csv")) # 2021 point observations

### END IMPORT DATA -----------------------------

### DATA CLEANING -------------------------------

# First: Make all data frames similar and use only necessary columns, but keep
# originally imported data frame (for GPS columns)

### SUBSET DATAFRAMES ---------------------------

## 2019 Point Environmental Data:

colnames(env.19)
env.19.sub <- env.19[c(2:6, 8:10, 12, 14)]

## 2021 Point Environmental Data:

colnames(env.21)
env.21.sub <- env.21[c(2:6, 8:10, 12, 15)]

## 2019 Point Observations:

colnames(obs.19)
obs.19.sub <- obs.19[c(2:9, 11:13, 16)]

## 2021 Point Observations:

colnames(obs.21)
obs.21.sub <- obs.21[c(2:9, 11:14, 17)]

### END SUBSET ----------------------------------

### CREATE IDENTITY KEY -------------------------

# I need to create matching Identity keys for all tables
# I will do this by concatenating the Road, Unit, Transect, and Point IDs
# I'll use the 'paste' function from the base r package to do this:


## 2019 Environment:

env.19.sub$Identity <- paste(env.19.sub$Road_ID, 
                             env.19.sub$UNIT_ID, 
                             env.19.sub$Transect_ID, 
                             env.19.sub$Point_ID, 
                             sep = "_")

# check
head(env.19.sub)

# 2019 Observations:

obs.19.sub$Identity <- paste(obs.19.sub$Road_ID, 
                             obs.19.sub$UNIT_ID, 
                             obs.19.sub$Transect_ID, 
                             obs.19.sub$Point_ID, 
                             sep = "_")

# check
head(obs.19.sub)

# 2021 Environment:

env.21.sub$Identity <- paste(env.21.sub$ROAD_ID,
                             env.21.sub$UNIT_ID,
                             env.21.sub$TRANSECT_ID,
                             env.21.sub$POINT_ID,
                             sep = "_")

#check
head(env.21.sub)

# 2021 Observations:

obs.21.sub$Identity <- paste(obs.21.sub$ROAD_ID,
                             obs.21.sub$UNIT_ID,
                             obs.21.sub$TRANSECT_ID,
                             obs.21.sub$POINT_ID,
                             sep = "_")

#check
head(obs.21.sub)

### END CREATE IDENTITY KEY ---------------------

### ADD JULIAN DATE COLUMN ----------------------

## 2019 Environmental
# format 'Date' column as a Date

# found out that need to change the date from full -2019 to -19
# for the year to not convert to 2020 when change to a Date format

env.19.sub$Date <- gsub("-2019", "-19", env.19.sub$Date)

env.19.sub$Date <- base::as.Date(env.19.sub$Date, 
                                 format = "%d-%b-%y")
#check
str(env.19.sub)

# create Julian date column and convert dates:
env.19.sub$julian <- base::as.POSIXlt(env.19.sub$Date, 
                                      format ='%d%b%y')$yday +1
#check
head(env.19.sub)

## 2021 Environmental

# One row has the wrong year 
colnames(env.21.sub)
env.21.sub$SURVEY_DATE <- gsub("-19", "-21", env.21.sub$SURVEY_DATE)

# format 'Date column as Date
env.21.sub$SURVEY_DATE <- base::as.Date(env.21.sub$SURVEY_DATE, 
                                        format = "%d-%b-%y")
#check
str(env.21.sub)

# create Julian date column and convert dates:
env.21.sub$julian <- base::as.POSIXlt(env.21.sub$SURVEY_DATE, 
                                      format = '%d%b%y')$yday +1
#check
head(env.21.sub)

### END ADD JULIAN DATE -------------------------

### FORMAT SURVEY TIME --------------------------

## 2019

#remove ':' that's already in there

env.19.sub$start_time <- str_replace(env.19.sub$start_time, '[:]', '')

# through trial and error need to change str to integer

env.19.sub$start_time <- as.integer(env.19.sub$start_time)

env.19.sub$start_time <- base::paste0(env.19.sub$start_time, "00") #adds 00 to end

env.19.sub$start_time <- sprintf("%06d", as.numeric(env.19.sub$start_time)) #adds leading 0

# separate sunrise times into columns for h:m:s

env.19.sub <- tidyr::extract(env.19.sub, 
                             start_time, 
                             into = c("hr", "min", "sec"), 
                             "(.{2})(.{2})(.{2})", 
                             remove = FALSE)

# paste h:m:s column together into another column and separate with ':'

env.19.sub$start_time <- base::paste(env.19.sub$hr, 
                                     env.19.sub$min, 
                                     env.19.sub$sec, 
                                     sep = ":")

# format column so that R recognizes it as time

env.19.sub$start_time <- hms::as_hms(env.19.sub$start_time)
#check structure
str(env.19.sub$start_time)


## 2021

env.21.sub$START_TIME <- str_replace(env.21.sub$START_TIME, '[:]', '')

# through trial and error need to change str to integer

env.21.sub$START_TIME <- as.integer(env.21.sub$START_TIME)

env.21.sub$START_TIME <- base::paste0(env.21.sub$START_TIME, "00") #adds 00 to end

env.21.sub$START_TIME <- sprintf("%06d", as.numeric(env.21.sub$START_TIME)) #adds leading 0

# separate sunrise times into columns for h:m:s

env.21.sub <- tidyr::extract(env.21.sub, 
                             START_TIME, 
                             into = c("hr", "min", "sec"), 
                             "(.{2})(.{2})(.{2})", 
                             remove = FALSE)

# paste h:m:s column together into another column and separate with ':'

env.21.sub$START_TIME <- base::paste(env.21.sub$hr, 
                                     env.21.sub$min, 
                                     env.21.sub$sec, 
                                     sep = ":")

# format column so that R recognizes it as time

env.21.sub$START_TIME <- hms::as_hms(env.21.sub$START_TIME)
#check structure
str(env.21.sub$START_TIME)


### IMPORT FINAL SUNRISE DATA -------------------

sunrise19 <- read.csv(here("data/nome_sun19_final.csv"))
sunrise19 <- sunrise19[c(2:3)]

# change date to 'Date' format
sunrise19$date <- base::as.Date(sunrise19$date, 
                                format = "%d-%b-%y")

sunrise19$sunrise <- hms::as_hms(sunrise19$sunrise)

sunrise21 <- read.csv(here("data/nome_sun21_final.csv"))
sunrise21 <- sunrise21[c(2:3)]

# change date to 'Date' format
sunrise21$date <- base::as.Date(sunrise21$date, 
                                format = "%d-%b-%y")

sunrise21$sunrise <- hms::as_hms(sunrise21$sunrise)
### END IMPORT FINAL SUNRISE DATA ---------------


### CHANGE COLUMN NAMES -------------------------

# And now, because I'm anal, I'm going to make all the 
# column names the same format for all data frames
# And clean up the tables to include only the rows I need.

# 2019 Obs

colnames(obs.19.sub)

oldnames.obs19 = c("Road_ID", "UNIT_ID", "Transect_ID", "Point_ID", 
                   "Year", "Date", "Species_Alpha_Code", "Time_Interval", 
                   "Exact_Distance", "Distance_category", "Flyover2", 
                   "Group.size", "Identity")

newnames.obs19 = c("road", "unit", "transect", "point", "year", "date", 
                   "species", "time_int", "distance", "dist_cat", 
                   "flyover","count", "id")


for(i in 1:ncol(obs.19.sub)) names(obs.19.sub)[names(obs.19.sub) == oldnames.obs19[i]] = newnames.obs19[i]

colnames(obs.19.sub)


# 2019 Env

colnames(env.19.sub)

oldnames.env19 = c("Road_ID", "UNIT_ID", "Transect_ID", "Point_ID",
                   "Prim_Obs_initials", "Date", "start_time", "Temp_F", 
                   "Sky", "Hearing", "Identity", "julian")

newnames.env19 = c("road", "unit", "transect", "point", "observer", "date",
                   "start_time", "tempf", "sky", "hear", "id", "julian")

for(i in 1:ncol(env.19.sub)) names(env.19.sub)[names(env.19.sub) == oldnames.env19[i]] = newnames.env19[i]

head(env.19.sub)
# 2021 Obs

colnames(obs.21.sub)

oldnames.obs21 = c("ROAD_ID", "UNIT_ID", "TRANSECT_ID", "POINT_ID",
                   "SURVEY_YEAR", "SURVEY_DATE", "SPECIES_ALPHA_CODE",
                   "TIME_INTERVAL", "EXACT_DISTANCE", "DISTANCE_CATEGORY",
                   "FLYOVER", "DISPLAY.USING.AREA", "GROUP_SIZE", "Identity")

newnames.obs21 = c("road", "unit", "transect", "point", "year", "date",
                   "species", "time_int", "distance", "dist_cat", "flyover",
                   "display", "count", "id")

for(i in 1:ncol(obs.21.sub)) names(obs.21.sub)[names(obs.21.sub) == oldnames.obs21[i]] = newnames.obs21[i]

# 2021 Env

colnames(env.21.sub)

oldnames.env21 = c("ROAD_ID", "UNIT_ID", "TRANSECT_ID", "POINT_ID",
                   "PRIM_OBS", "SURVEY_DATE", "START_TIME", "TEMP_F",
                   "SKY", "HEARING", "Identity")

newnames.env21 = c("road", "unit", "transect", "point", "observer",
                   "date", "start_time", "tempf", "sky", "hear",
                   "id")


for(i in 1:ncol(env.21.sub)) names(env.21.sub)[names(env.21.sub) == oldnames.env21[i]] = newnames.env21[i]

head(env.21.sub)

### END CHANGE COLUMN NAMES ---------------------

### MERGE DATAFRAMES ----------------------------

# Merge the sunrise data with the environmental data by date

## 2019 Merge

# Will use inner join with the environmental data on the left

# check that column formats are similar
str(env.19.sub)
str(sunrise19)

# merge environmental and sunrise data frames
env.19.merged <- env.19.sub %>%
  dplyr::inner_join(sunrise19, by = "date")


## 2021 Merge

str(env.21.sub)
str(sunrise21)

# merge environmental and sunrise dataframes

env.21.merged <- env.21.sub %>%
  dplyr::inner_join(sunrise21, by = "date")

### END MERGE DATAFRAMES ------------------------

### CHECK FOR DUPLICATES AND MISSING DATA -------

# One row was lost in the 2021 merge, which was it:
# anti_join(env_21_sub, env_21_merged)

# For some reason TELL_T3_T19_17 on 2019-05-17 is missing
# Oops, put year 2019 for 2021 - will go back up and ammend
# that.

# checking for duplicates:
duplicated(env.19.merged)
duplicated(env.21.merged)

# all looks good

### END DUPLICATES AND ERRORS -------------------


### CALCULATE MIN AFTER SUNRISE -----------------

# And this is a bit out of place but I almost forgot to calculate this column

# 2019 Environmental

# first need to merge date and time to be able to calculate

colnames(env.19.merged)

env.19.merged$min_after_sun <- as.numeric(difftime(strptime(paste(env.19.merged[,6], env.19.merged[,7]), "%Y-%m-%d %H:%M:%S"),
                                                   strptime(paste(env.19.merged[,6], env.19.merged[,16]), "%Y-%m-%d %H:%M:%S"),
                                                   units = "mins")) 



# 2021 Environmental

colnames(env.21.merged)

str(sunrise21)
str(env.21.merged)
# need to format 
env.21.merged$min_after_sun <- as.numeric(difftime(strptime(paste(env.21.merged[,6], env.21.merged[,7]), "%Y-%m-%d %H:%M:%S"),
                                                   strptime(paste(env.21.merged[,6], env.21.merged[,16]), "%Y-%m-%d %H:%M:%S"),
                                                   units = "mins")) 



# One row was lost in the 2021 merge, which was it: FIXED
#anti_join(env_21_sub, env_21_merged)

### END CALCULATE -------------------------------

### ADD YEAR COLUMN  ----------------------------

env.19.merged$year <- 2019
colnames(env.19.merged)
env.19.merged <- env.19.merged[c(14, 6, 15, 18, 7, 17, 5, 11:13)]


env.21.merged$year <- 2021
colnames(env.21.merged)
env.21.merged <- env.21.merged[c(14, 6, 15, 18, 7, 17, 5, 11:13)]

### END ADD YEAR --------------------------------


### FILTER OUT DISTANCE -------------------------

# For observation data
# Going to do this buy subsetting rows based on distance
# Distance must be 0 > < 401 (in 2019 data lots of -999)
# OR dist_cat has a value (>0)

# need to change distance to numeric for some of these arguments

## 2019

str(obs.19.sub)
#check unique values to make sure there are no characters that will be 
#returned as NAs
unique(obs.19.sub$distance)

# change 400 + to -999
# change 100-200 to 150
# change 'U' to ???? -999

obs.19.sub$distance[obs.19.sub$distance == "400+"] <- "999"
obs.19.sub$distance[obs.19.sub$distance == "100-200"] <- "150"
obs.19.sub$distance[obs.19.sub$distance == "U"] <- "999" # looked at original data sheets and birds was far off
# change -999 to 999
obs.19.sub$distance[obs.19.sub$distance == "-999"] <- "999"

obs.19.sub$distance <- as.numeric(obs.19.sub$distance)


obs.19.dist <- dplyr::filter(obs.19.sub, distance < 401 | dist_cat != "NA")

# last one, filter out dist_cats of 201
# also annoyed by column with dist cat and 999 in dist
# if dist_cat !=NA change dist = NA

obs.19.dist <- within(obs.19.dist, distance[distance == '999' & dist_cat != 'NA'] <- 'NA')

# now filter out dist_cats of 201

obs.19.dist <- dplyr::filter(obs.19.dist, dist_cat != "201" | distance != "NA")

# NOW filter out flyovers:

obs.19.dist <- dplyr::filter(obs.19.dist, flyover != "Y")

# okay, think we've got distance filter sorted out. woof.
# this is why entering data is just as important. Ugh. I hate cleaning.

## 2021

str(obs.21.sub) #distance is already in interval... change to numeric?

unique(obs.21.sub$distance)

obs.21.dist <- dplyr::filter(obs.21.sub, distance < 401 | dist_cat != "NA")

# now filter out dist_cats of 201

obs.21.dist <- dplyr::filter(obs.21.dist, dist_cat != "201" | distance != "NA")

# okay this looks good too
# NOW filter out flyovers that aren't displays

obs.21.dist <- dplyr::filter(obs.21.dist, flyover != "Y" | display == "Y")

# Okay now I think finally done.
### JOIN YEAR FRAMES ----------------------------

obs.19.dist$display <- "NA"

#reorder columns

colnames(obs.19.dist)
obs.19.dist <- obs.19.dist[c(13, 5, 7:12, 14)]
head(obs.19.dist)

colnames(obs.21.dist)
obs.21.dist <- obs.21.dist[c(14, 5, 7:11, 13, 12)]
head(obs.21.dist)

# now append tables together

all.obs <- rbind(obs.19.dist, obs.21.dist)
head(all.obs)

## Merge environmental

# now clean environmental data and combine

head(env.19.merged); head(env.21.merged)

# get rid of start time, date

colnames(env.19.merged)
env.19.final <- env.19.merged[c(1, 3:4, 6:10)]

colnames(env.21.merged)
env.21.final <- env.21.merged[c(1, 3:4, 6:10)]


head(env.19.final); head(env.21.final)

str(env.19.final); str(env.21.final)
# change env.21 temp to integer
env.21.final$tempf <- as.integer(env.21.final$tempf)
head(env.21.final)
str(env.21.final)


# rbind dataframes together

all.env <- rbind(env.19.final, env.21.final)
head(all.env)


# check structure:
str(all.env)

### CHECK FOR MISSING IDS BETWEEN DATAFRAMES ----

missing.id1 <- anti_join(all.env, all.obs)
missing.id2 <- anti_join(all.obs, all.env)



## misspelled?
#COUN_3_12_4
#COUN_C3_T3_10 MISSING ENTIRELY FROM ENV.ALL

# fix spelling error:

all.env$id[all.env$id == 'COUN _3_12_4'] <- 'COUN_3_12_4'
all.obs$id[all.obs$id == 'COUN _3_12_4'] <- 'COUN_3_12_4'

missing.check <- anti_join(all.obs, all.env)
missing.check2 <- anti_join(all.env, all.obs)

# will need to add row for missing env data
# other error I fixed going directly into data .csv and rerunning code up to this point

## Add mis-matching ids as 'NO SPP DETECTED'?

# subset out only id column of those missing from observations
# doesn't matter what time interval they are because they will
# be zero counts

miss.ids <- missing.check2[,c(1, 3)]

# now rbind with all.obs using rbind.fill from 'plyr' package

all.obs <- dplyr::bind_rows(all.obs, miss.ids)

# change species NAs to 'NONE' for no species detected
str(all.obs)
all.obs[6173:6195, 3] <- "NOSPP"
# change NA time intervals to zero
all.obs[6173:6195, 4] <- "0"
# change count to 0
all.obs[6173:6195, 8] <- "0"



# In species specific scripts, will spread dataframes



### GET LENGTH OF UNIQUE SPP VALUES -------------

species <- all.obs$species
spp.counts <- as.data.frame(table(species))
write.csv(spp.counts, here("output/spp_counts2.csv"))

### FIX INCORRECT SPECIES NAMES -----------------
#names(data)[names(data) == "oldVariableName"] <- "newVariableName"
all.obs$species[all.obs$species == "AGPL"] <- "AMGP"
all.obs$species[all.obs$species == "ARMO"] <- "AMRO"
all.obs$species[all.obs$species == "PGPL"] <- "PAGP"
all.obs$species[all.obs$species == "UNKNOWN PTARM"] <- "PTARM"
all.obs$species[all.obs$species == "WCP"] <- "WCSP"
all.obs$species[all.obs$species == "SHORE"] <- "SHOREBIRD"
all.obs$species[all.obs$species == "UNK"] <- "UNKNP"


# check

species2 <- all.obs$species
spp.counts2 <- as.data.frame(table(species2))

### SAVE DATAFRAMES -----------------------------

write.csv(all.obs, here("data/all_obs.csv"))
write.csv(all.env, here("data/all_env.csv"))
          
          
### END SAVE DATAFRAMES ------------------------- 



### END SCRIPT ----------------------------------

