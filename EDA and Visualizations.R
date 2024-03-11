library(data.table)
library(tidyverse)
library(leaflet)
library(maps)
library(mapdata)
library(mapproj)
library(reshape2)

draft_info <- fread('Data/clean_draft.csv')

# Create a dataframe with state naming data
state_data <- data.frame(names = state.name, abbreviation = tolower(state.abb))

draft_info <- 
  draft_info %>% 
  left_join(state_data, by = c("home_state" = "abbreviation")) %>%
  select(-home_state) %>%
  rename(home_state = names) %>% 
  drop_na(home_state)

# Create a frequency of the states of drafted draft_info
state_freq <- table(draft_info$home_state)

# Convert the table to a datafram
state_freq_df <- 
  data.frame(region = tolower(names(state_freq)), # convert states to lowercase
             frequency = as.numeric(state_freq)) %>% # draft_info from each state
  arrange(desc(frequency)) %>% # arrange in descending order of draft_info drafted
  filter(!(region %in% c("hawaii", "alaska"))) # View only contiguous U.S.

# Mapping data for the United States map
states <- map_data("state")

# Create a dataframe with the coordinates, and the frequencies in each state
map.df <- 
  merge(states, state_freq_df, by="region", all.x=T) %>% 
  rename(longitude = long, latitude = lat) %>% 
  select(-subregion) %>% 
  filter(region %in% state_freq_df$region)

map.df <- map.df[order(map.df$order),] # make sure the dataframe is ordered

# Create a heat map of the draft_info drafted by home_state
home_heat_map <- 
  ggplot(map.df, aes(x = longitude, y = latitude, group = group)) +
  geom_polygon(aes(fill=frequency)) +
  geom_path() +
  scale_fill_gradient(low = "white", high = "darkred", 
                      name = "Players Drafted") +
  labs(title = "Heat Map of Drafted Players") +
  theme(plot.title = element_text(hjust = 0.5))

first_10 <- 
  draft_info %>% 
  filter(as.integer(pick_round) %in% c(1:10)) %>% 
  drop_na(high_school) %>% 
  group_by(pick_round, high_school) %>% 
  summarise(num_players = n()) %>% 
  arrange(desc(num_players)) %>% 
  ungroup() %>% 
  mutate(pick_round = factor(pick_round, levels = c(1:10)),
         high_school = as.factor(high_school))

hs_by_round_by_plot <-
  ggplot(first_10, aes(x = pick_round, y = num_players, fill = high_school)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Total Players", x = "Draft Round", 
       title = "Number of Draftees by Pick and Round",
       fill = "High School Player") +
  theme_minimal()

           