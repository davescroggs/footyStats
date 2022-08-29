library(tidyverse)
library(fitzRoy)
library(lubridate)

load("data/afl.RData")


# Clean column headings ---------------------------------------------------


## AFL data ----------------------------------------------------------------

# Need to create it as a character vector because there are duplicate column names after cleaning
aflDataCols <- aflData %>% 
  colnames() %>% 
  # Remove nesting headers
  str_remove_all("(player|extendedStats|clearances)\\.") %>% 
  # Remove text after home/away team
  str_remove_all("club.name$") %>% 
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

## AFL Data ---- 

colRemove <- c('status', 'compSeasonShortName','roundRoundNumber', 'photoUrl', 'playerJumperNumber', 'gamesPlayed', 'superGoals')

aflData_clean <- 
  aflData %>% 
  set_names(aflDataCols) %>% 
  # Remove duplicate headings
  select(-matches("\\_\\d"),-any_of(colRemove))

## AFL Tables -----

colKeep <- c('season', 'round', 'date', 'localStartTime', 'firstName', 'surname', 'id', 'playingFor', 'brownlowVotes', 'umpire1', 'umpire2', 'umpire3', 'umpire4')

aflTables_clean <- 
  aflTables %>% 
  janitor::clean_names("small_camel") %>% 
  # Remove duplicate headings - currently no duplicates
  select(any_of(colKeep))

# Standardise common columns ----------------------------------------------

## Clean up data types and create neccessary indentifiers

### Date/times ----

#### aflData ----

aflData_clean %>% 
  mutate(startTime = ymd_hms(utcStartTime),
    season = factor(year(utcStartTime)), .keep = "used") %>% 
  select(-utcStartTime)

#### aflTables ----

aflTables_clean %>% 
  mutate(localStartTime = if_else(localStartTime < 1000, localStartTime + 1200L,localStartTime),
         startTime = ymd_hm(paste(date, localStartTime)),
         season = factor(season)) %>% 
  select(-c(date,localStartTime))


### Team names ----

## Team names are slightly different between tables
## Match the names and fuzzy join to get the corresponding name

afd_teams <- 
  aflData_clean %>%
  summarise(teamAflData = unique(awayTeam),
                                         tbl = 1)

aft_teams <- 
  aflTables_clean %>%
  summarise(teamAflTbls = unique(playingFor),
                                           tbl = 1)

fuzzyjoin::stringdist_full_join(afd_teams,aft_teams,
                                by = c("teamAflData" = "teamAflTbls"),
                                max_dist = 10,distance_col = "n") %>% 
  group_by(teamAflData) %>% 
  slice_min(n) %>% 
  ungroup() %>% 
  select(teamAflData, teamAflTbls)

# This method works for all except Greater Western Sydney so have updated manually below
# Will use AFL data as the standard and change all others to match

teamNameMap <- tibble::tribble(
        ~teamAflTbls,        ~teamAflData,
        "Adelaide",    "Adelaide Crows",
        "Brisbane Lions",    "Brisbane Lions",
        "Carlton",           "Carlton",
        "Collingwood",       "Collingwood",
        "Essendon",          "Essendon",
        "Fremantle",         "Fremantle",
        "Geelong",      "Geelong Cats",
        "Gold Coast",   "Gold Coast Suns",
        "Greater Western Sydney",        "GWS Giants",
        "Hawthorn",          "Hawthorn",
        "Melbourne",         "Melbourne",
        "North Melbourne",   "North Melbourne",
        "Port Adelaide",     "Port Adelaide",
        "Richmond",          "Richmond",
        "St Kilda",          "St Kilda",
        "Sydney",      "Sydney Swans",
        "West Coast", "West Coast Eagles",
        "Western Bulldogs",  "Western Bulldogs"
      ) %>% deframe()


aflTables_clean %>% 
  mutate(teamName = map_chr(playingFor,~teamNameMap[.x]))

### Player name column headers ----

# givenName for first name
# surname for last name

# AFL Data tables meet the standard

aflTables %>%
  rename(givenName = First.name)


## Player IDs --------------------------------------------------------------

### AFL Data ----

# Column name is playerId - this should be the standard name
aflData_clean %>% 
  select(playerId)

### AFL Tables ----

aflTables_clean %>% 
  select(playerId = id)

### Round indicators ----

#### AFL Data ----


aflTables_clean %>% 
  filter(season == "2020") %>% count(round) %>%
  filter(!str_detect(round,"\\d"))
  
## Make the rounds and ordered character variable

  roundsVector <- c("roundName", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "Finals Week 1", "Semi Finals", "Preliminary Finals", "Grand Final")
  
  
aflData_clean %>%
  mutate(round = str_remove(roundName,"Round "),
         round = factor(round,levels = roundsVector,ordered = TRUE))

#### AFL Tables ---- 

aflTables_clean %>% 
  mutate(round = case_when(round == "GF" ~ "Grand Final",
                           round == "PF" ~ "Preliminary Finals",
                           round == "SF" ~ "Semi Finals",
                           # I don't like this, it's losing information
                           round %in% c("EF","QF") ~ "Finals Week 1",
                           TRUE ~ round) %>% 
           factor(levels = roundsVector,ordered = TRUE))

# Everything matches nicely

aflData_clean %>% 
  mutate(round = str_remove(roundName,"Round "),
         round = factor(round,levels = roundsVector,ordered = TRUE)) %>% 
  distinct(round) %>% 
  mutate(typ = "AFL") %>% 
  full_join(aflTables_clean %>% 
              mutate(round = case_when(round == "GF" ~ "Grand Final",
                                       round == "PF" ~ "Preliminary Finals",
                                       round == "SF" ~ "Semi Finals",
                                       # I don't like this, it's losing information
                                       round %in% c("EF","QF") ~ "Finals Week 1",
                                       TRUE ~ round) %>% 
                       factor(levels = roundsVector,ordered = TRUE)) %>% 
              distinct(round) %>% 
              mutate(typ = "AFLT"),
            by = "round") %>% print(n = Inf)


# Combine for data creation pipeline --------------------------------------

## AFL Data ----

aflData_final <- 
  aflData %>% 
  set_names(aflDataCols) %>% 
  # Remove duplicate headings
  select(-matches("\\_\\d"),-any_of(colRemove)) %>% 
  mutate(startTime = ymd_hms(utcStartTime),
         season = factor(year(utcStartTime)),
         round = str_remove(roundName,"Round ") %>% 
           factor(levels = roundsVector,ordered = TRUE)) %>% 
  select(-c(utcStartTime,roundName)) %>% 
  filter(!is.na(playerId))

## AFL Tables ----

aflTables_final <- 
  aflTables %>% 
  janitor::clean_names("small_camel") %>% 
  # Remove duplicate headings - currently no duplicates
  select(any_of(colKeep)) %>% 
  mutate(localStartTime = if_else(localStartTime < 1000, localStartTime + 1200L,localStartTime),
         startTime = ymd_hm(paste(date, localStartTime)),
         season = factor(season),
         teamName = map_chr(playingFor,~teamNameMap[.x]),
         round = case_when(round == "GF" ~ "Grand Final",
                           round == "PF" ~ "Preliminary Finals",
                           round == "SF" ~ "Semi Finals",
                           # I don't like this, it's losing information
                           round %in% c("EF","QF") ~ "Finals Week 1",
                           TRUE ~ round) %>% 
           factor(levels = roundsVector,ordered = TRUE)) %>% 
  rename(givenName = firstName,
         playerId = id) %>% 
  select(-c(date,localStartTime))



# Join tables (#7) -------------------------------------------------------------

## Create join id ----

join_ids <- 
  aflData_final %>% 
  mutate(full_name = clean_player_name(givenName,surname)) %>% 
  distinct(full_name,teamName,season,playerId) %>% 
  nest(names = c(full_name,playerId)) %>% 
  inner_join(
    aflTables_final %>% 
      mutate(full_name = clean_player_name(givenName,surname)) %>% 
      distinct(full_name,teamName,season,playerId) %>% 
      nest(names = c(full_name,playerId)),
    by = c("teamName","season"),
    suffix = c(".afld",".aflt")) %>% 
  transmute(teamName,season,
            matched_names = map2(names.afld, names.aflt,function(x,y){
              
              x <- unnest(x,cols = c(everything()))
              y <- unnest(y,cols = c(everything()))
              
              fuzzyjoin::stringdist_full_join(x,y,by = "full_name",
                                              max_dist = 6,
                                              distance_col = "n") %>% 
                group_by(playerId.x) %>%
                slice_min(n) %>%
                ungroup()
            })) %>% 
  unnest(matched_names) %>% 
  select(teamName,season,
         playerId.alfd = playerId.x,
         playerId.alft = playerId.y,
         full_name = full_name.x)

join_aflTables <- function(df){
  df %>% 
    left_join(join_ids,
              by = c("playerId" = "playerId.alfd","teamName", "season")) %>%
    left_join(aflTables_final %>% 
                select(-c(givenName, surname, startTime)),
              by = c("playerId.alft" = "playerId","teamName", "season","round"))}

playerData <- aflData_final %>% 
  join_aflTables()