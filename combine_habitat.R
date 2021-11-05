# Combining 2019 & 2021 Extracted Habitat Values


# Library

library(dplyr)
library(here)
library(tidyverse)

# Import data

hab19 <- read.csv(here("data/points19_habitat.csv"))
hab21 <- read.csv(here("data/points21_habitat.csv"))

# bind to create one habitat df

all.hab <- rbind(hab19, hab21)
head(all.hab); dim(all.hab)

write.csv(all.hab, here("data/all_hab.csv"))








































