### ---------------------------------------------

## Title: Willow Ptarmigan Time Reomoval Analysis

## Author: Michaela L. Gustafson

## Description: Analyzing only WIPT observations in 2019&2021

## Date created: 15 September 2021

## ----------------------------------------------

### LIBRARY -------------------------------------

library(here) # for locating filepath
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table) # for making copies of data table for each species
library(unmarked)

### END LIBRARY ---------------------------------

### LOAD DATA -----------------------------------

all.obs <- read.csv(here("data/all_obs.csv"))
all.env <- read.csv(here("data/all_env.csv"))
all.hab <- read.csv(here("data/all_hab.csv"))


# get rid of extra 'X' column, this will be taken care of for obs
# when aggregating time intervals and creating a wide data frame
colnames(all.env)
all.env <- all.env[,c(2:9)]

colnames(all.obs)
all.obs <- all.obs[,c(2:10)]

colnames(all.hab)
all.hab <- all.hab[,c(3:8)]

### END LOAD DATA -------------------------------

### CLEAN DATA ----------------------------------


# copy data table for wipt

wipt <- data.frame(all.obs)

# change all species codes of WIPT to a 1 and all other species to 0
wipt$species[wipt$species == "WIPT"] <- 1
wipt$species[wipt$species != "1"] <- 0

# change count to 0 for non WIPT
wipt$count[wipt$species == 0] <- 0
head(wipt)
# now 'wipt' represents counts for WIPT

# check for na values
sum(is.na(wipt$time_int))


# change time intervals to reflect intervals of 2 minutes
wipt2 <- mutate(wipt, 
                time_int = ifelse(wipt$time_int %in% 0:1, "1", 
                                  ifelse(wipt$time_int %in% 2:3, "2", 
                                         ifelse(wipt$time_int %in% 4:5, "3",
                                                ifelse(wipt$time_int %in% 6:7, "4",
                                                       ifelse(wipt$time_int %in% 8:9, "5", "NA"))))))


# aggregate rows and sum counts for matching keys and time intervals
# must change formats first:

str(wipt2)

wipt2$id <- as.factor(wipt2$id)
wipt2$time_int <- as.factor(wipt2$time_int)
wipt2$count <- as.integer(wipt2$count)

wipt.agg <- aggregate(x = wipt2$count, 
                   by = list(wipt2$id, wipt2$time_int), 
                   FUN = sum)




# rename columns in aggregated df

head(wipt.agg)

names(wipt.agg)[names(wipt.agg) == "Group.1"] <- "id"
names(wipt.agg)[names(wipt.agg) == "Group.2"] <- "time_int"
names(wipt.agg)[names(wipt.agg) == "x"] <- "count"

head(wipt.agg)

# okay so wipt_agg is our count dataframe, we don't need any of the other columns,
# those were only used to help filter out for distance and flyover observatiosn


# need to change id to a factor to use anti-join
all.env$id <- as.factor(all.env$id)


# spread dataframes:

unique(wipt.agg$id)


# this is from taking some entries out for distance and flyovers
# so do I need to add empty entry rows for sites to match?

wipt.wide <- wipt.agg %>%
  dplyr::select(id, time_int, count) %>%
  spread(key = time_int, value = count, fill = 0)


##### NEED TO CHANGE SKY VARIABLE TO HIGHEST NUMBER

str(all.env)
all.env$sky <- as.factor(all.env$sky)


# going to have to use dsub and do it for each character string of multiple categories
unique(all.env$sky)

all.env$sky <- gsub("2,3,4", "4", all.env$sky)
all.env$sky <- gsub("3,4", "4", all.env$sky)
all.env$sky <- gsub("2,4", "4", all.env$sky)
all.env$sky <- gsub("2,3", "3", all.env$sky)
all.env$sky <- gsub("0,3", "3", all.env$sky)
all.env$sky <- gsub("1,3", "3", all.env$sky)
all.env$sky <- gsub("3,0", "3", all.env$sky)
all.env$sky <- gsub("3,", "3", all.env$sky)

unique(all.env$sky)

all.env$sky <- as.factor(all.env$sky)


# Combine extracted habitat variables with rest of environmental data

# WILL NEED TO CHANGE THIS INNER JOIN WHEN I HAVE ALL POINTS
missing.join <- anti_join(all.env, all.hab)
missing.join2 <- anti_join(all.hab, all.env)
# for COUN_3_12_4 there is an extra space in the hab data that needs to be taken out to match env
all.hab$id[38]="COUN_3_12_4"

all.env <- inner_join(all.env, all.hab)

str(all.env) 
all.env$year <- as.factor(all.env$year)
all.env$observer <- as.factor(all.env$observer)
all.env$hear <- as.factor(all.env$hear)

all.env <- rename(all.env, "jd" = "julian")

### CHECK DATA FOR CORRELATION ------------------

prednames <- c("jd", "observer", "min_after_sun", "tempf", "sky", "hear")

for( p in 1:length(prednames) ){
  # create an object with the ggplot so that you can display it 
  # in a loop 
  a <- ggplot( all.env ) + #choose your data
    theme_bw( base_size = 15 ) + #choose a preset theme
    labs( x = prednames[p] ) + #label x axis using our predictor names
    geom_histogram( aes( get(prednames[p]) ), bins= 10, stat = "count" ) #plot histogram
  # display your plot object
  print( a )
}

# row removed for non-finite values??
x <- as.data.frame(is.na(all.env))
which(x == "TRUE", arr.ind = TRUE)
# one of my temps is NA
# looked at temps of surveys before (40) and after (44) and can enter a temp

all.env[552,6] <- "42"

for( p in 1:length(prednames) ){
  # create an object with the ggplot so that you can display it 
  # in a loop 
  a <- ggplot( all.env ) + #choose your data
    theme_bw( base_size = 15 ) + #choose a preset theme
    labs( x = prednames[p] ) + #label x axis using our predictor names
    geom_histogram( aes( get(prednames[p]) ), bins= 10, stat = "count" ) #plot histogram
  # display your plot object
  print( a )
}

#scale predictors:
str(all.env)
jul.scaled <- scale(all.env$jd)
min.scaled <- scale(all.env$min_after_sun)
temp.scaled <- scale(as.numeric(all.env$tempf))
t.scaled <- scale(as.numeric(all.env$Tundra))
ls.scaled <- scale(as.numeric(all.env$Low_Shrub))
ts.scaled <- scale(as.numeric(all.env$Tall_Shrub_Forest))
bg.scaled <- scale(as.numeric(all.env$Bare_Ground))
w.scaled <- scale(as.numeric(all.env$Wetlands))


# replace in dataframe
all.env.scaled <- data.frame(all.env)

all.env.scaled$jd <- jul.scaled
all.env.scaled$min_after_sun <- min.scaled
all.env.scaled$tempf <- temp.scaled
all.env.scaled$Tundra <- t.scaled
all.env.scaled$Low_Shrub <- ls.scaled
all.env.scaled$Tall_Shrub_Forest <- ts.scaled
all.env.scaled$Bare_Ground <- bg.scaled
all.env.scaled$Wetlands <- w.scaled

head(all.env.scaled)

# look at correlations between scaled factors:
cor(all.env.scaled$jd, all.env.scaled$tempf) # hmm 0.55 correlation... makes sense I guess? 
# i think in my earlier analysis i used julian date instead of temp...
cor(all.env.scaled$jd, all.env.scaled$min_after_sun) ### -0.184, not bad??
cor(all.env.scaled$tempf, all.env.scaled$min_after_sun)# 0.232
cor(all.env.scaled$Tundra, all.env.scaled$Low_Shrub) #-0.261
cor(all.env.scaled$Tundra, all.env.scaled$Tall_Shrub_Forest) #-0.529
cor(all.env.scaled$Tundra, all.env.scaled$Bare_Ground) # -0.0297
cor(all.env.scaled$Tundra, all.env.scaled$Wetlands) #0.0176
cor(all.env.scaled$Low_Shrub, all.env.scaled$Tall_Shrub_Forest) # -0.649
cor(all.env.scaled$Low_Shrub, all.env.scaled$Bare_Ground) #0.00405
cor(all.env.scaled$Low_Shrub, all.env.scaled$Wetlands) # -0.197
cor(all.env.scaled$Tall_Shrub_Forest, all.env.scaled$Bare_Ground) #-0.147
cor(all.env.scaled$Tall_Shrub_Forest, all.env.scaled$Wetlands) # -0.0203
cor(all.env.scaled$Bare_Ground, all.env.scaled$Wetlands) # 0.165

# high correlatiosn between tundra+tall shrub and tallshrub+lowshrub
# correlations all seem good, probably going to continue to use julian instead of temp?

### RUN ANALYSIS
colnames(all.env.scaled)
str(all.env.scaled)
str(wipt.wide)
############### okay maybe need to combine env data with wipt.wide
# df so that sites are in order or wipt.wide df



wipt.env.join <- left_join(wipt.wide, all.env.scaled)
str(wipt.env.join)

colnames(wipt.env.join)

timeints <- wipt.env.join[,c(2:6)]
siCovs <- wipt.env.join[,c(7, 9:18)]


wiptFrame2 <- unmarkedFrameMPois(
  # import time removal columns(counts):
  y = timeints, 
  siteCovs = siCovs, 
  # define pifun type: 
  type = "removal" )

# fit models: multinomPois order of formulas: detection, abundance
#### DON'T KNOW ABOUT USING FULL MODEL B/C OF HIGH CORRELATION BETWEEN SHRUB+TUNDRA and remember to use temp and not julian date???
fm0 <- multinomPois(~ 1 ~ 1, data = wiptFrame2) #null model
fm.ls <- multinomPois( ~ 1 + jd + min_after_sun + observer + sky + hear ~ 1 + Tundra + Low_Shrub + Bare_Ground, data = wiptFrame2)
fm.ts <- multinomPois( ~ 1 + jd + min_after_sun + observer + sky + hear ~ 1 + Tundra + Tall_Shrub_Forest + Bare_Ground, data = wiptFrame2)

# rank models by AIC:
ms2 <- fitList(
  "lam(.)p(.)" = fm0,
  "lam(Tundra + Low_Shrub + Bare_Ground)p(jd + min_after_sun + observer + sky + hear)" = fm.ls,
  "lam(Tundra + Tall_Shrub_Forest + Bare_Ground)p(jd + min_after_sun + observer + sky + hear)" = fm.ts)

(ms2sel <- modSel(ms2))

# table with everything you could possibly need:
coef(ms2sel)

output.twomin <- as(ms2sel, "data.frame")
output.twomin


### SAVE OUTPUT

# write.csv(output.twomin, here("output/wipt_2min_output.csv"))


### ASSESSING MODEL FIT


set.seed(2021)

fitstats <- function(fm.ts) {
  observed <- getY(fm.ts@data)
  expected <- fitted(fm.ts)
  resids <- residuals(fm.ts)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}


(gof <- parboot(fm.ts, fitstats, nsim = 1000, report = 1))

save.image(here("workspaces/04_wipt_workspace.RData"))

### END SCRIPT