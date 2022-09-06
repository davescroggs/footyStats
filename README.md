# Go to Brownlow in 3 weeks

## Building a predictive model for the 2022 Brownlow medal

The aim of this project is to build a predictive model for the 2022 Brownlow medal, starting from scratch, in 3 weeks. This will be a first for me, I've never attempted anything like this and so I'll be trying to record my process along the way, good and bad, and see how I go on the 19th of September. My understanding of football, and what the drivers for Brownlow votes, is very basic so I'll be learning as I go. I'll be using the [Tidymodels](https://www.tidymodels.org/books/) package to develop the predictive model.

## Week 1

### Data cleaning and standardisation

This mode will use three sources of information: AFL website for player data and match data and, AFL tables for brownlow votes. One of the first tasks was standardising column names. The tables are pulled from nested websites and often contain information at each level of nesting. To make the columns more user friendly the names were cleaned. The [janitor package](https://github.com/sfirke/janitor) has a great function just for this purpose, and collases column headers down to a given format. It has also been very handy defining a common standard for tables, both in terms of the same, and ensuring consistency in common fields, such as the way rounds are recorded, or team names. This means the data can be more accurately joined, which is useful for the next step.

#### Joining player data

Unfortunately there is no common link or join between player data from the AFL and from AFL tables (ie. some sort of unique identifier). Making matters worse, there is some dissimilarity between players names in the two tables (Patrick vs Paddy Ryder, Harry vs Harrison Marsh). Both tables have their own unique player idenfier. This required lining up each player's identifier in each table.[fuzzy joins](https://github.com/dgrtwo/fuzzyjoin) are perfect for this situation, where the player data is well curated, but slightly different. Fuzzy join's `stringdist_join` creates a cross join on each tables join index(es) that are less than a maximum distance, with several distance algorithms to choose from. To reduce the search space, each unique player record in the AFL data in a given season was first joined to AFL tables by **season** and **team**. Each of these sub-groups were fuzzy joined using the default Damerau-Levenshtein distance, with a maximum distance of 6, keeping the row where each player's join distance was lowest. Of the 6640 players that have played in the last year, only one player could not be joined uniquely in this way, Angus Dewar.

### Finding signal

### First model
