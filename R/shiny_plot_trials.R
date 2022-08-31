knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(scales)
library(lubridate)
library(here)

load(file = here("data/processed_data.RData"))

playerData <- playerData %>% 
  filter(round <= 23)


# Explore options for a single game dashboard -----------------------------

## Which stats do you show?
## How many players?
## How many ways to summarise the data - raw, % of team/game total, percentile rank?

