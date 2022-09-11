
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Go to Brownlow in 3 weeks

<!-- badges: start -->
<!-- badges: end -->

## Building a predictive model for the 2022 Brownlow medal

The aim of this project is to build a predictive model for the 2022
Brownlow medal, starting from scratch, in 3 weeks. This will be a first
for me, I’ve never attempted anything like this and so I’ll be trying to
record my process along the way, good and bad, and see how I go on the
19th of September. My understanding of football, and what the drivers
for Brownlow votes, is very basic so I’ll be learning as I go. I’ll be
using the [Tidymodels](https://www.tidymodels.org/books/) package to
develop the predictive model.

## Week 1

### Data cleaning and standardisation

This mode will use three sources of information: AFL website for player
data and match data and, AFL tables for brownlow votes. One of the first
tasks was standardising column names. The tables are pulled from nested
websites and often contain information at each level of nesting. To make
the columns more user friendly the names were cleaned. The [janitor
package](https://github.com/sfirke/janitor) has a great function just
for this purpose, and collases column headers down to a given format. It
has also been very handy defining a common standard for tables, both in
terms of the same, and ensuring consistency in common fields, such as
the way rounds are recorded, or team names. This means the data can be
more accurately joined, which is useful for the next step.

#### Joining player data

Unfortunately there is no common link or join between player data from
the AFL and from AFL tables (ie. some sort of unique identifier). Making
matters worse, there is some dissimilarity between players names in the
two tables (Patrick vs Paddy Ryder, Harry vs Harrison Marsh). Both
tables have their own unique player idenfier. This required lining up
each player’s identifier in each table.[fuzzy
joins](https://github.com/dgrtwo/fuzzyjoin) are perfect for this
situation, where the player data is well curated, but slightly
different. Fuzzy join’s `stringdist_join` creates a cross join on each
tables join index(es) that are less than a maximum distance, with
several distance algorithms to choose from. To reduce the search space,
each unique player record in the AFL data in a given season was first
joined to AFL tables by **season** and **team**. See example below of
the standard join before fuzzy joining.

``` r
load(file = "data/readmeExamples.RData")
readmeTeamsJoined
#> # A tibble: 18 × 4
#>    teamName          season names.afld        names.aflt       
#>    <chr>              <dbl> <list>            <list>           
#>  1 Melbourne           2022 <tibble [33 × 2]> <tibble [33 × 2]>
#>  2 Western Bulldogs    2022 <tibble [41 × 2]> <tibble [41 × 2]>
#>  3 Carlton             2022 <tibble [40 × 2]> <tibble [40 × 2]>
#>  4 Richmond            2022 <tibble [37 × 2]> <tibble [37 × 2]>
#>  5 St Kilda            2022 <tibble [35 × 2]> <tibble [35 × 2]>
#>  6 Collingwood         2022 <tibble [38 × 2]> <tibble [38 × 2]>
#>  7 Geelong Cats        2022 <tibble [35 × 2]> <tibble [35 × 2]>
#>  8 Essendon            2022 <tibble [39 × 2]> <tibble [39 × 2]>
#>  9 GWS Giants          2022 <tibble [39 × 2]> <tibble [39 × 2]>
#> 10 Sydney Swans        2022 <tibble [35 × 2]> <tibble [35 × 2]>
#> 11 Brisbane Lions      2022 <tibble [36 × 2]> <tibble [36 × 2]>
#> 12 Port Adelaide       2022 <tibble [37 × 2]> <tibble [37 × 2]>
#> 13 Hawthorn            2022 <tibble [39 × 2]> <tibble [39 × 2]>
#> 14 North Melbourne     2022 <tibble [39 × 2]> <tibble [39 × 2]>
#> 15 Adelaide Crows      2022 <tibble [38 × 2]> <tibble [38 × 2]>
#> 16 Fremantle           2022 <tibble [35 × 2]> <tibble [35 × 2]>
#> 17 West Coast Eagles   2022 <tibble [47 × 2]> <tibble [47 × 2]>
#> 18 Gold Coast Suns     2022 <tibble [40 × 2]> <tibble [40 × 2]>
```

Each of these sub-groups were fuzzy joined using the default
Damerau-Levenshtein distance, with a maximum distance of 6, keeping the
row where each player’s join distance was lowest. Of the 6640 players
that have played in the last year, only one player could not be joined
uniquely in this way, Angus Dewar.

``` r
readmeTeamsJoined %>% 
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
  # The Giants are a good example because 3 players had different spelling
  filter(teamName == "GWS Giants") %>% 
  arrange(-n)
#> # A tibble: 39 × 7
#>    teamName   season full_name.x      playerId.x  full_name.y      playe…¹     n
#>    <chr>       <dbl> <chr>            <chr>       <chr>              <dbl> <dbl>
#>  1 GWS Giants   2022 harry_himmelberg CD_I993107  harrison_himmel…   12462     4
#>  2 GWS Giants   2022 matt_de_boer     CD_I271015  matthew_de_boer    11746     3
#>  3 GWS Giants   2022 callum_m_brown   CD_I1014038 callum_brown       12910     2
#>  4 GWS Giants   2022 isaac_cumming    CD_I1001396 isaac_cumming      12629     0
#>  5 GWS Giants   2022 jarrod_brander   CD_I1002300 jarrod_brander     12646     0
#>  6 GWS Giants   2022 jacob_wehr       CD_I1004530 jacob_wehr         12991     0
#>  7 GWS Giants   2022 sam_taylor       CD_I1005247 sam_taylor         12644     0
#>  8 GWS Giants   2022 james_peatling   CD_I1006013 james_peatling     12934     0
#>  9 GWS Giants   2022 xavier_ohalloran CD_I1006135 xavier_ohalloran   12821     0
#> 10 GWS Giants   2022 bobby_hill       CD_I1006148 bobby_hill         12744     0
#> # … with 29 more rows, and abbreviated variable name ¹​playerId.y
```

### Finding signal

### First model
