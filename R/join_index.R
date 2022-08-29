library(tidyverse)
library(fuzzyjoin)



# Create a join index between ALF Data and AFL Tables ---------------------

# Join by
# - Team
# - Season
# - Player name

# Create a function to join up a players name, team and season into a single string and standardise
# Standardise to remove punctuation, spaces and upper case letters. This should reduce the difference in names between datasets

clean_player_name <- function(first,last) paste(first,last,sep = "_") %>% str_replace_all("\\s+","\\_") %>% tolower %>% str_remove_all("\\W")

aflData_final %>% 
  mutate(full_name = clean_player_name(givenName,surname)) %>% 
  distinct(full_name,teamName,season,playerId) %>% 
  filter_all(any_vars(is.na(.)))

aflData %>% 
  filter(!is.na(player.playerId))

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
  

# Look at uniqueness of the joined IDs
# All IDs are unique and match one-to-one
join_ids %>% 
  summarise(distinct_AFL = n_distinct(playerId.alfd),
    distinct_AFLT = n_distinct(playerId.alft),
    distinct_both = n_distinct(playerId.alfd,playerId.alft),
    distinct_original = n_distinct(aflData_final$playerId))

join_ids %>% 
  filter(n > 0) %>% 
  arrange(-n) %>% View

# Ian or Bobby Hill?

# Name changed between 2019 - 2020 but still the same player

# Some names are spelt differently in the same season

join_ids %>% 
  group_by(teamName, season,playerId.alfd) %>% 
  summarise(n = n_distinct(full_name.x)) %>% 
  arrange(-n)

# Create a join function --------------------------------------------------

  join_aflTables <- function(df){
    df %>% 
  left_join(join_ids,
            by = c("playerId" = "playerId.alfd","teamName", "season")) %>%
  left_join(aflTables_final %>% 
              select(-c(givenName, surname, startTime)),
            by = c("playerId.alft" = "playerId","teamName", "season","round"))}

aflData_final %>% 
  join_aflTables()
