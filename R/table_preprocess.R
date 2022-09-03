library(tidyverse)
library(fitzRoy)
library(lubridate)
library(here)

load("data/afl.RData")


# Preprocess game data ----------------------------------------------------

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

colRemove <- c('status', 'compSeasonShortName','roundRoundNumber', 'photoUrl', 'playerJumperNumber', 'gamesPlayed', 'superGoals','lastUpdated','ranking','shotEfficiency','goalEfficiency','interchangeCounts')

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
         startTime = ymd_hm(paste(date, localStartTime))) %>% 
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

  roundsVector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "Finals Week 1", "Semi Finals", "Preliminary Finals", "Grand Final")
  
  
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
         season = year(utcStartTime),
         round = str_remove(roundName,"Round ") %>% 
           factor(levels = roundsVector,ordered = TRUE),
         game = str_sub(providerId,-2) %>% parse_integer()) %>% 
  select(-c(utcStartTime,roundName)) %>% 
  rename(matchId = providerId) %>% 
  filter(!is.na(playerId))

## AFL Tables ----

aflTables_final <- 
  aflTables %>% 
  janitor::clean_names("small_camel") %>% 
  # Remove duplicate headings - currently no duplicates
  select(any_of(colKeep)) %>% 
  mutate(localStartTime = if_else(localStartTime < 1000, localStartTime + 1200L,localStartTime),
         startTime = ymd_hm(paste(date, localStartTime)),
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


# Preprocess match data (#15) ---------------------------------------------

colKeep_match <- c("match.name", "match.date", "match.matchId", "match.homeTeamId", "match.awayTeamId", "match.homeTeam.name", "match.awayTeam.name", "match.homeTeam.nickname", "match.awayTeam.nickname", "venue.name", "venue.state", "round.name", "round.year", "homeTeamScore.matchScore.totalScore", "homeTeamScore.matchScore.goals", "homeTeamScore.matchScore.behinds", "awayTeamScore.matchScore.totalScore", "awayTeamScore.matchScore.goals", "awayTeamScore.matchScore.behinds")

matchData_final <- matchData %>%
  select(any_of(colKeep_match)) %>% 
  set_names(~str_remove_all(.,"match(Score)?\\.")) %>% 
  janitor::clean_names("small_camel") %>% 
  rename(matchup = name,
         startTime = date) %>% 
  mutate(season = as.numeric(roundYear),
         round = str_remove(roundName,"Round ") %>% 
           factor(levels = roundsVector,ordered = TRUE),
         game = str_sub(matchId,-2) %>% parse_integer()) %>% 
  select(matchId,startTime,season,round,game,matchup,venueName,venueState,starts_with("homeTeam"),starts_with("awayTeam"),-c(roundYear,roundName))


# Join tables (#7) -------------------------------------------------------------

clean_player_name <- function(first,last) paste(first,last,sep = "_") %>% str_replace_all("\\s+","\\_") %>% tolower %>% str_remove_all("\\W")

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
                                              # Largest distance in 10 years = 8
                                              max_dist = 6,
                                              distance_col = "n") %>% 
                group_by(playerId.x) %>%
                slice_min(n) %>%
                ungroup()
            })) %>% 
  unnest(matched_names) %>% 
  select(teamName,season,
         playerId.alfd = playerId.x,
         playerId.alft = playerId.y)

join_aflTables <- function(df){
  df %>% 
    left_join(join_ids,
              by = c("playerId" = "playerId.alfd","teamName", "season")) %>%
    left_join(aflTables_final %>% 
                select(-c(givenName, surname, startTime,playingFor)),
              by = c("playerId.alft" = "playerId","teamName", "season","round"))}

playerData <- aflData_final %>% 
  join_aflTables() %>% 
  # Order - player info, game info, metrics
  # Player info
  select(season, round, startTime, playerId, playerId.alft, givenName, surname, 
         # Game info
         teamName,homeTeam,awayTeam,matchId,venueName,
         # metrics
         brownlowVotes,jumperNumber, position,everything(),contains("umpire")) %>% 
  left_join(matchData_final %>% 
              select(-c(season, round, startTime, venueName, game)) %>% 
              mutate(finalMargin = homeTeamScoreTotalScore - awayTeamScoreTotalScore) %>% 
              pivot_longer(cols = -c(matchId:venueState,finalMargin),names_to = c("teamStatus",".value"),names_pattern = "(.{4})Team(.*)") %>% 
              mutate(finalMargin = if_else(teamStatus == "away",-finalMargin,finalMargin)),
            by = c("matchId","teamStatus")) %>% 
  # One record doesn't have a unique fuzzy join.
  filter(!is.na(brownlowVotes))

save(playerData, file = here("data/processed_data.RData"))
