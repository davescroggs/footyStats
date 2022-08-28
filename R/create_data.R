library(tidyverse)
library(fitzRoy)


# Clean names + remove dupes ----------------------------------------------
  
aflData <- map_dfr(2013:2022,~fitzRoy::fetch_player_stats_afl(.x))
aflTables <- map_dfr(2013:2022,~fitzRoy::fetch_player_stats_afltables(.x))

save(aflData, aflTables,file = "data/afl.RData")
