library(tidyverse)
library(fitzRoy)

load("data/afl.RData")


# Clean column headings ---------------------------------------------------


## AFL data ----------------------------------------------------------------

# Need to create it as a character vector because there are duplicate column names after cleaning
aflDataCols <- aflData %>% 
  colnames() %>% 
  # Remove nesting headers
  str_remove_all("(player|extendedStats|clearances)\\.") %>% 
  janitor::make_clean_names("small_camel") 

aflData %>% 
  set_names(aflDataCols) %>% 
  # Remove duplicate headings
  select(-matches("\\_\\d"))

## AFL tables ----------------------------------------------------------------

aflTables %>% 
  janitor::clean_names("small_camel") %>% 
  # Remove duplicate headings - currently no duplicates
  select(-matches("\\_\\d")) 


# Remove unnecessary columns ----------------------------------------------

## AFL Data

colRemove <- c('status', 'compSeasonShortName', 'roundRoundNumber', 'photoUrl', 'playerJumperNumber', 'gamesPlayed', 'superGoals')

aflData %>% 
  set_names(aflDataCols) %>% 
  # Remove duplicate headings
  select(-matches("\\_\\d"),-any_of(colRemove))

## AFL Tables

colKeep <- c('season', 'round', 'date', 'localStartTime', 'firstName', 'surname', 'id', 'playingFor', 'brownlowVotes', 'umpire1', 'umpire2', 'umpire3', 'umpire4')

aflTables %>% 
  janitor::clean_names("small_camel") %>% 
  # Remove duplicate headings - currently no duplicates
  select(any_of(colKeep))

# Pick some standards
