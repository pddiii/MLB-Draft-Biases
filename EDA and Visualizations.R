library(data.table)
library(tidyverse)
library(leaflet)
library(maps)
library(mapdata)
library(mapproj)
library(reshape2)

batters <- fread('Data/bat_subset.csv') # load subset batters data
pitchers <- fread('Data/pitch_subset.csv') # load subset pitchers data

# Create a dataframe with state naming data
state_data <- data.frame(names = state.name, abbreviation = state.abb)

# Add the state_data variables to the batters data
batters <- 
  batters %>% 
  left_join(state_data, by = c("home_state" = "abbreviation")) %>%
  select(-home_state) %>%
  rename(home_state = names)

# Add the state_data variables to the pitchers data
pitchers <- 
  pitchers %>% 
  left_join(state_data, by = c("home_state" = "abbreviation")) %>%
  select(-home_state) %>%
  rename(home_state = names)

# Create larger `players` data frame with all drafted players for EDA
players <- bind_rows(batters, pitchers)

# Create a frequency of the states of drafted players
state_freq <- table(players$home_state)

# Convert the table to a datafram
state_freq_df <- 
  data.frame(region = tolower(names(state_freq)), # convert states to lowercase
             frequency = as.numeric(state_freq)) %>% # Players from each state
  arrange(desc(frequency)) %>% # arrange in descending order of Players drafted
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

# Create a heat map of the players drafted by home_state
home_heat_map <- 
  ggplot(map.df, aes(x = longitude, y = latitude, group = group)) +
  geom_polygon(aes(fill=frequency)) +
  geom_path() +
  scale_fill_gradient(low = "white", high = "darkred", 
                      name = "Players Drafted") +
  labs(title = "Heat Map of Drafted Players") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation matrix for `players` object
cor_mat <-
  players %>% 
  select(-fg_playerID, -person_full_name) %>% 
  mutate_if(is.character, as.factor) %>% # convert character to factor
  mutate_if(is.factor, as.numeric) %>% # convert factor for cor() 
  drop_na() %>% # remove the NA values
  cor()

# Reshape the cor_mat for the correlation heat map
melt_cor <- melt(cor_mat) %>% mutate(value = round(value, digits = 2))

# Create the correlation heat map
cor_map <- 
  ggplot(melt_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       name = "Correlation", midpoint = 0, 
                       limit = c(-1, 1)) +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.4) +
  ggtitle("Correlation Heat Map") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank()) 
