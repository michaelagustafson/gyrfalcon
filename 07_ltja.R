### ---------------------------------------------

## Title: Long-tailed Jaeger Time Removal Analysis

## Author: Michaela L. Gustafson

## Description: Analyzing only LTJA observations in 2019&2021

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

# copy data table for LTJA

ltja <- data.frame(all.obs)

# change all species codes of LTJA to a 1 and all other species to 0
ltja$species[ltja$species == "LTJA"] <- 1
ltja$species[ltja$species != "1"] <- 0

# change count to 0 for non LTJA
ltja$count[ltja$species == 0] <- 0
head(ltja)
# now 'ltja' df represents counts for ltja

# check for na values
sum(is.na(ltja$time_int))


# change time intervals to reflect intervals of 2 minutes
ltja2 <- mutate(ltja, 
                time_int = ifelse(ltja$time_int %in% 0:1, "1", 
                                  ifelse(ltja$time_int %in% 2:3, "2", 
                                         ifelse(ltja$time_int %in% 4:5, "3",
                                                ifelse(ltja$time_int %in% 6:7, "4",
                                                       ifelse(ltja$time_int %in% 8:9, "5", "NA"))))))


# aggregate rows and sum counts for matching keys and time intervals
# must change formats first:

str(ltja2)

ltja2$id <- as.factor(ltja2$id)
ltja2$time_int <- as.factor(ltja2$time_int)
ltja2$count <- as.integer(ltja2$count)

ltja.agg <- aggregate(x = ltja2$count, 
                      by = list(ltja2$id, ltja2$time_int), 
                      FUN = sum)




# rename columns in aggregated df

head(ltja.agg)

names(ltja.agg)[names(ltja.agg) == "Group.1"] <- "id"
names(ltja.agg)[names(ltja.agg) == "Group.2"] <- "time_int"
names(ltja.agg)[names(ltja.agg) == "x"] <- "count"

head(ltja.agg)

# okay so ltja.agg is our count dataframe, we don't need any of the other columns,
# those were only used to help filter out for distance and flyover observatiosn

# check that ids are matching between env and obs
# need to change id to a factor to use anti-join
all.env$id <- as.factor(all.env$id)
miss1 <- anti_join(all.env, ltja.agg) # clear
miss2 <- anti_join(ltja.agg, all.env) # clear



# spread dataframes:

unique(ltja.agg$id) # should end up with 916 rows


ltja.wide <- ltja.agg %>%
  dplyr::select(id, time_int, count) %>%
  spread(key = time_int, value = count, fill = 0)


str(all.env) 
all.env$sky <- as.factor(all.env$sky)
all.env$year <- as.factor(all.env$year)
all.env$observer <- as.factor(all.env$observer)

### CHECK DATA FOR CORRELATION ------------------

prednames <- c("julian", "observer", "min_after_sun", "tempf", "sky", "hear")

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
jul.scaled <- scale(all.env$julian)
min.scaled <- scale(all.env$min_after_sun)
hear.scaled <- scale(all.env$hear)
temp.scaled <- scale(as.numeric(all.env$tempf))


# replace in dataframe
all.env.scaled <- data.frame(all.env)

all.env.scaled$julian <- jul.scaled
all.env.scaled$min_after_sun <- min.scaled
all.env.scaled$hear <- hear.scaled
all.env.scaled$tempf <- temp.scaled
head(all.env.scaled)

# look at correlations between scaled factors:
cor(all.env.scaled$julian, all.env.scaled$tempf) # hmm 0.5 correlation... makes sense I guess? 
# i think in my earlier analysis i used julian date instead of temp...
cor(all.env.scaled$julian, all.env.scaled$min_after_sun) ### -0.18, not bad??
cor(all.env.scaled$julian, all.env.scaled$hear) # -0.125
cor(all.env.scaled$min_after_sun, all.env.scaled$hear) # 0.065
cor(all.env.scaled$tempf, all.env.scaled$hear) #-0.097
cor(all.env.scaled$tempf, all.env.scaled$min_after_sun)# 0.231

# correlations all seem good, probably going to continue to use julian instead of temp?

### RUN ANALYSIS
colnames(all.env.scaled)
str(all.env.scaled)
str(ltja.wide)

ltja.wide$id <- as.factor(ltja.wide$id)
colnames(ltja.wide)


# ?unmarked

timeints <- ltja.wide[,c(2:6)]

siCovs <- all.env.scaled[,c(2, 4:8)]

ltjaFrame2 <- unmarkedFrameMPois(
  # import time removal columns(counts):
  y = timeints, 
  #import site level covariates:
  siteCovs = siCovs, # site covs will also be my spatial habitat data
  # define pifun type: 
  type = "removal" )

# fit models: multinomPois order of formulas: detection, abundance

fm0.two <- multinomPois(~ 1 ~ 1, data = ltjaFrame2) #null model
fm.two.full <- multinomPois( ~ julian + min_after_sun + observer + tempf + sky + hear ~ 1, data = ltjaFrame2)
fm.two.jul <- multinomPois( ~ julian + min_after_sun + observer + sky + hear ~ 1, data = ltjaFrame2)
fm.two.temp <- multinomPois( ~ min_after_sun + observer + tempf + sky + hear ~ 1, data = ltjaFrame2)


# rank models by AIC:
ms2 <- fitList(
  "lam(.)p(.)" = fm0.two,
  "lam(.)p(julian + min_after_sun + observer + tempf + sky + hear)" = fm.two.full,
  "lam(.)p(julian + min_after_sun + observer + sky + hear)" = fm.two.jul,
  "lam(.)p(min_after_sun + observer + tempf + sky + hear)" = fm.two.temp
)

(ms2sel <- modSel(ms2))

# table with everything you could possibly need:
coef(ms2sel)

output.twomin <- as(ms2sel, "data.frame")
output.twomin


### SAVE OUTPUT

write.csv(output.twomin, here("output/ltja_2min_output.csv"))

### END SCRIPT
