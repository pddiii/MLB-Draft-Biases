library(baseballr)
library(tidyverse)

# Create an empty list for the draft data year-by-year
draft_list <- list()

# For each year get the mlb_draft data
for (season in c(1965:2023)) {
  draft_list[[as.character(season)]] <- mlb_draft(year = season)
  Sys.sleep(5) # 5 second sleep interval
}

# Create a dataframe with all the draft info
draft_df <- bind_rows(draft_list)

# Create a csv file containing the draft information
# write_csv(draft_df, 'Data/draft_data.csv')

# Collect the Chadwick Bureau player information
chadwick_data <- chadwick_player_lu()

# write_csv(chadwick_data, 'Data/chadwick_data.csv')
