library(rvest)
library(tidyverse)
library(here)

# The data for this file was created by saving instances of each page of https://www.afl.com.au/brownlow-medal/predictor?page=1&player_type=all&club=0&ShowBettingOdds=0. The instances were saved to the data folder and the HTML read in from there.

aflPredictedVotes <- list.files(here("data/AFL_brownlow_predictor/"),pattern = "*.html",full.names = TRUE) %>% 
  map_dfr(function(fls){
read_html(fls) %>% 
  rvest::html_node(".ladder--brownlow-predictor") %>% 
  html_table(header = T)
  }) %>% 
  select(-c(`Total Votes...2`,...27)) %>% 
  rename(TotalVotes = `Total Votes...26`) %>% 
  mutate(across(`12`:`14`,parse_integer),
         across(where(is.integer),~replace_na(.x,0)),
         Player = str_remove_all(Player,"[^A-Za-z\\s]") %>% str_trim() %>% str_squish()) %>% 
  rename_if(is.integer,~paste("Round",.x)) %>% 
  arrange(-TotalVotes)


save(aflPredictedVotes,file = "aflPredictedVotes.RData")
