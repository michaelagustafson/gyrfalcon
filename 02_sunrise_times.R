### ---------------------------------------------

## Title: Get Sunrise Times Data for Nome, AK

## Author: Michaela L. Gustafson

## Description: Import and clean sunrise times for
## Nome, AK to be merged with environmental
## variables data frame

# Sunrise times are from dateandtime.com

## Date created: 14 September 2021

### ---------------------------------------------

### LIBRARY -------------------------------------

library(here) # for file location
library(hms) # for making column into a time format
library(tidyr) # general cleaning/manipulation
library(dplyr) # general cleaning/manipulation

### IMPORT SUNRISE DATA -------------------------

# 2019

sun19 <- read.csv(here("data/sunrise_2019.csv"))


# 2021

sun21 <- read.csv(here("data/sunrise_2021.csv"))


### END IMPORT SUNRISE DATA ---------------------

### CLEAN SUNRISE DATA --------------------------

## 2019 Sunrise Times ---------------------------


## Format Time:

# first add extra 0 to beginning of those with one less character so
# column can be formatted in h:m:s


# remove colons first
sun19$sunrise <- base::gsub(":", "", sun19$sunrise) 
sun19$sunrise <- base::sprintf("%06d", as.numeric(sun19$sunrise)) #adds leading 0

# separate sunrise times into columns for h:m:s

sun19 <- tidyr::extract(sun19, 
                        sunrise, 
                        into = c("hr", "min", "sec"), 
                        "(.{2})(.{2})(.{2})", 
                        remove=FALSE)

# paste h:m:s column together into another column and separate with ':'

sun19$sunrise <- base::paste(sun19$hr, sun19$min, sun19$sec, sep=":")

# format column so that R recognizes it as time

sun19$sunrise <- hms::as_hms(sun19$sunrise)
#check structure
str(sun19)


# keep only date and sunrise times in the df:
colnames(sun19)
sun19 <- sun19[c(1, 2)]

# write final sunrise times as a .csv
write.csv(sun19, here("data/nome_sun19_final.csv"))


## 2021 Sunrise Times ---------------------------

# remove colons in time to be able to add leading 0
sun21$sunrise <- base::gsub(":", "", sun21$sunrise) 
head(sun21)


## Format Time:

# addextra 0 to 5chr strings of time so column can be formatted in h:m:s

sun21$sunrise <- base::sprintf("%06d", as.numeric(sun21$sunrise)) #adds leading 0
# check by clicking on data frame

# separate sunrise times into columns for h:m:s

sun21 <- tidyr::extract(sun21, 
                        sunrise, 
                        into = c("hr", "min", "sec"), 
                        "(.{2})(.{2})(.{2})", 
                        remove=FALSE)

# paste h:m:s column together into another column and separate with ':'

sun21$sunrise <- base::paste(sun21$hr, sun21$min, sun21$sec, sep=":")

# format column so that R recognizes it as time

sun21$sunrise <- hms::as.hms(sun21$sunrise)
#check structure
str(sun21)


# keep only date and sunrise times in the df:
colnames(sun21)
sun21 <- sun21[c(1, 2)]

# write final sunrise times as a .csv
write.csv(sun21, here("data/nome_sun21_final.csv"))


### END SCRIPT ----------------------------------
