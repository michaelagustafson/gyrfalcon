### ---------------------------------------------

## Title: Arctic Ground Squirrel Time Removal Analysis

## Author: Michaela L. Gustafson

## Description: Analyzing only SQUIR observations in 2019&2021

## Date created: 27 September 2021

## ----------------------------------------------

### LIBRARY -------------------------------------

library(here) # for locating filepath
library(dplyr)
library(tidyverse)
library(ggplot2)
library(unmarked)

### END LIBRARY ---------------------------------


### LOAD DATA -----------------------------------

all.obs <- read.csv(here("data/all_obs.csv"))
all.env <- read.csv(here("data/all_env.csv"))

# get rid of extra 'X' column, this will be taken care of for obs
# when aggregating time intervals and creating a wide data frame
colnames(all.env)
all.env <- all.env[,c(2:9)]

### END LOAD DATA -------------------------------

### CLEAN DATA ----------------------------------

# copy data table for SQUIR

squir <- data.frame(all.obs)

# change all species codes of SQUIR to a 1 and all other species to 0
squir$species[squir$species == "SQUIR"] <- 1
squir$species[squir$species != "1"] <- 0

# change count to 0 for non SQUIR
squir$count[squir$species == 0] <- 0
head(squir)
# now 'squir' df represents counts for SQUIR

# check for na values
sum(is.na(squir$time_int))


# change time intervals to reflect intervals of 2 minutes
squir2 <- mutate(squir, 
                time_int = ifelse(squir$time_int %in% 0:1, "1", 
                                  ifelse(squir$time_int %in% 2:3, "2", 
                                         ifelse(squir$time_int %in% 4:5, "3",
                                                ifelse(squir$time_int %in% 6:7, "4",
                                                       ifelse(squir$time_int %in% 8:9, "5", "NA"))))))


# aggregate rows and sum counts for matching keys and time intervals
# must change formats first:

str(squir2)

squir2$id <- as.factor(squir2$id)
squir2$time_int <- as.factor(squir2$time_int)
squir2$count <- as.integer(squir2$count)

squir.agg <- aggregate(x = squir2$count, 
                      by = list(squir2$id, squir2$time_int), 
                      FUN = sum)




# rename columns in aggregated df

head(squir.agg)

names(squir.agg)[names(squir.agg) == "Group.1"] <- "id"
names(squir.agg)[names(squir.agg) == "Group.2"] <- "time_int"
names(squir.agg)[names(squir.agg) == "x"] <- "count"

head(squir.agg)












