library(tidyverse)
library(tidytext)
library(fitzRoy)

# FUll team list
players_2022 <- fitzRoy::fetch_player_details(current = T)

# Appeared in game-day list
lineups_2022 <- fitzRoy::fetch_lineup(2022) 
select(round.name,
       status,
       teamName, 
       firstName = player.playerName.givenName,
       surname = player.playerName.surname,
       teamStatus)


unique_char_vec <- function(x) str_remove_all(tolower(x), "\\W+") %>%
  str_split(.,pattern = "",simplify = F)  %>% unlist() %>% unique()

unique_chars <- function(x,y){
  a = unique_char_vec(x)
  b = unique_char_vec(y)
  return(intersect(a,b) %>% sort() %>% paste(collapse = ", "))
}

# Find where there are the same characters
character_crossover <- 
  # lineups_2022 %>% 
  # distinct(teamName,firstName,surname) %>% 
  players_2022 %>% 
  transmute(playername = paste(firstName,surname),
            name_no_space = str_remove_all(playername,"[^a-zA-Z]"),
    name_len = str_length(name_no_space)) %>% 
    filter(name_len >= 11) %>% 
  unnest_tokens(chars,name_no_space,token = "characters",drop = FALSE) %>% 
  distinct(playername,chars,.keep_all = T) %>%
  arrange(playername) %>% 
  {full_join(.,.,by = "chars")} %>% 
  filter(playername.x > playername.y) %>% 
  count(playername.x, playername.y,sort = T,name = "similar_letters") %>% 
  mutate(name_len.x = str_remove_all(playername.x,"\\W+") %>% str_length(),
         name_len.y = str_remove_all(playername.y,"\\W+") %>% str_length(),
         common_chars = map2_chr(playername.x,playername.y,unique_chars))

# anti-join the full player list with the players that do crossover
no_similar <- 
  # lineups_2022 %>% 
  # distinct(teamName,firstName,surname) %>% 
  players_2022 %>% 
  transmute(playername = paste(firstName,surname),
            name_len = str_remove_all(playername,"[^a-zA-Z]") %>% str_length(),
            type = 1) %>% 
  filter(name_len >= 11) %>% 
  arrange(playername) %>% 
  {full_join(.,.,by = "type")} %>% 
  filter(playername.x > playername.y) %>% 
  anti_join(character_crossover) %>% 
  arrange(-name_len.x) %>% 
  select(-type) %>% 
  mutate(common_chars = map2_chr(playername.x,playername.y,unique_chars))


character_crossover %>% 
  arrange(-similar_letters)
