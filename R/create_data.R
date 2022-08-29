library(tidyverse)
library(fitzRoy)


# Fetch player data ----------------------------------------------
  
aflData <- map_dfr(2013:2022,~fitzRoy::fetch_player_stats_afl(.x))
aflTables <- map_dfr(2013:2022,~fitzRoy::fetch_player_stats_afltables(.x))


# Fetch match data --------------------------------------------------------

matchData <- map_dfr(2013:2022,~fitzRoy::fetch_results_afl(.x))

save(aflData, aflTables, matchData, file = "data/afl.RData")
