library(tidyverse)
library(fitzRoy)

load("data/afl.RData")


# Clean column headings ---------------------------------------------------


## AFL data ----------------------------------------------------------------

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

# Pick some standards
