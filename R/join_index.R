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
        group_by(full_name.x) %>%
        slice_min(n) %>%
        ungroup()
    })) %>% 
    unnest(matched_names)
