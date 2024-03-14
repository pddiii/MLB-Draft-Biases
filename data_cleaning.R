library(data.table)
library(tidyverse)
library(datasets) # for state abbreviations

# Load in the batting and pitching data
batting <- fread('Data/batting_stats.csv')
pitching <- fread('Data/pitching_stats.csv')

# Change the variable name for WAR so that it matches in both data frames
## Our data give us FanGraphs WAR commonly denoted fWAR
batting <- 
  batting %>% 
  rename(fWAR = `L-WAR`,
         fg_playerID = IDfg)

pitching <-
  pitching %>% 
  rename(fWAR = WAR,
         fg_playerID = IDfg)

# View the structure of the batting data
# glimpse(batting)
# View the structure of the pitching data
# glimpse(pitching)

# We want to subset the columns which have less than 50% of values as NA values
bat_columns <- 
  which(batting[, lapply(.SD, function(x) {sum(is.na(x)) / length(x)})] <= 0.5)

# Keep only columns of interest for all players in the data
batting <-
  batting %>% 
  select(all_of(bat_columns), -Events, -Dol, -starts_with("GB"),
         -starts_with("LD"), -starts_with("FB"), -c(Pitches:BUH), 
         -c(`IFFB%`:`BUH%`), -Age, -Team, -IFFB)

# We want to subset the columns which have less than 50% of values as NA values
pitch_columns <- 
  which(pitching[, lapply(.SD, function(x) {sum(is.na(x)) / length(x)})] <= 0.5)

# Keep only columns of interest for all players in the data
pitching <-
  pitching %>% 
  select(all_of(pitch_columns), -Team, -Age, -c(GB:BUH), -c(`GB/FB`:xFIP),
         -c(`-WPA`:`WPA/LI`), -c(`FB% 2`:`SwStr%`), -SD, -MD, -SIERA, -`RS/9`, 
         -c(`O-Swing% (sc)`:`FDP-Wins`), -c(`Pull%`:`Hard%`), 
         -c(`O-Swing% (pi)`:`Pace (pi)`), -c(`LD%+`:`CSW%`), -`xFIP-`) %>% 
  replace_na(list(BS = 0, HLD = 0))

rm(bat_columns, pitch_columns)

# Load the MLB Amateur Draft information
draft_info <- fread('Data/draft_data.csv')

# View the structure of the draft information
## 67,903 rows by 84 columns
# glimpse(draft_info)

## 67,903 rows by 51 columns
draft_info <-
  draft_info %>% 
  # Add year columns to match up with Chadwick
  mutate(mlb_played_first = year(person_mlb_debut_date),
         high_school = ifelse(str_detect(school_name, "\\sHS") == TRUE, "Yes", 
                              "No"),
         person_height = str_replace_all(person_height, "\"$", "")) %>% 
  replace_na(list(home_state = "None", mlb_played_first = 0)) %>% 
  mutate(mlb_played_last = ifelse(mlb_played_first == 0, 0, 
                           year(person_last_played_date))) %>% 
  replace_na(list(mlb_played_last = 2023)) %>% 
  # Remove variables of no interest
  select(-person_link, -c(person_use_name:person_gender),
         -c(person_name_slug:person_init_last_name), 
         -c(person_name_matrilineal:home_city), -c(home_country, school_state),
         -c(person_xref_ids:person_death_country), -person_name_title, -team_id,
         -person_name_suffix, -c(team_link:team_spring_league_abbreviation), 
         -headshot_link, home_state)

# Load the Chadwick baseball player info
## 482,259 rows by 40 columns
chadwick <- fread('Data/chadwick_data.csv')

# Update Chadwick for players who were drafted after
## 109,609 rows by 21 columns
chadwick <- 
  chadwick %>% 
  select(-c(pro_managed_first:mlb_umpired_last), -name_suffix, 
         -name_matrilineal, -key_person, -key_uuid, -key_retro, 
         -key_bbref_minors, -c(key_npb:key_wikidata), -key_bbref) %>% 
  # Add variables to match up with Draft info
  mutate(person_birth_date = 
          str_c(birth_year, birth_month, birth_day, 
                 sep="-"),
         person_full_fml_name = 
           str_c(name_given, name_last, sep = " "),
         person_full_name = 
           str_c(name_first, name_last, sep = " ")) %>% 
  rename(person_id = key_mlbam,
         fg_playerID = key_fangraphs) %>% 
  arrange(mlb_played_first) %>% 
  drop_na(person_id) # Remove players we don't have an identifier for

# missing height is formatted as 0' 0" (0 feet 0 inches tall)
missing_height <- draft_info$person_height[57]

## 47,162 rows by 55 columns
draft_info <- 
  draft_info %>% 
  # add the chadwick player info, particularly the `fg_playerID`
  inner_join(chadwick %>% 
               select(-person_full_name, -person_birth_date, -person_full_fml_name,
                      -mlb_played_first, -mlb_played_last), 
             by = "person_id") %>% 
  # Remove unnecessary variables
  select(-person_first_name, -person_last_name, -person_primary_number, 
         -person_current_age, -c(person_name_first_last:person_strike_zone_bottom),
         person_primary_position_code, person_primary_position_type, 
         person_bat_side_description, person_pitch_hand_description, -name_last,
         -name_first) %>% 
  filter(person_height != missing_height)

# glimpse(draft_info)
# glimpse(chadwick)

rm(chadwick)

# Get the indices for the elements which contain a state abbreviation
# this applies to High School players who have states listed in format of either
# (CA) or (, CA) (e.g. CA = California)
draft_index <- which(str_detect(draft_info$school_name, pattern = "\\((.*?)\\)"))
length(draft_index) # 13,579 elements

# List with all the match results for the pattern "(letters)"
draft_list <- str_match_all(draft_info$school_name, pattern = "\\((.*?)\\)")

# Extracts the text within the "()", and stores it as a vector
draft_states <- unlist(lapply(draft_list, function(x) {x[, 2]}))
rm(draft_list) # Remove the list form of the home states

# removes any results where there was an NA result generated
draft_states <- draft_states[!is.na(draft_states)]
length(draft_states) # 13,579 elements, same as home_index

# Get the index for the draft_stats which contain the pattern ", "
comma_index <- which(str_detect(draft_states, ",\\s"))
length(comma_index) # 6,275 elements

# Extract the states with the pattern ", " from `draft_states`
comma_states <- draft_states[comma_index]
length(comma_states) # 6,275 elements

# Determine if there are any incorrect patterns
wrong <- comma_states[which(!str_detect(comma_states, ", ([:alpha:]+)"))]

# Replace the pattern within the `comma_states` manually
comma_states[which(!str_detect(comma_states, ", ([:alpha:]+)"))] <- ", FL"

# Extract the letters which follow the ", " pattern into a vector
comma_states <- 
  str_match_all(comma_states, ", ([:alpha:]+)") %>% 
  lapply(function(x) {x[, 2]})

# There was an error, so it was detecting Louis and MO
comma_states[[2525]] <- comma_states[[2525]][2]
# Unlist it into a single vector
comma_states <- comma_states %>% unlist()

# Replace the comma states with the correct abbreviations
draft_states[comma_index] <- comma_states

# Create a new variable home_state
draft_info <- draft_info %>% mutate(home_state = person_birth_state_province)

# Store all the draft_states to the new `draft_state` variable within `draft_info`
draft_info$home_state[draft_index] <- draft_states
rm(draft_index, draft_states, wrong)

# wrong abbreviations indices
wrong_index <- which(nchar(draft_info$home_state) > 2)

# Find the wrong abbreviations
wrong_abbrev <- draft_info$home_state[wrong_index]

# Data frame for state_data
state_data <- data.frame(state_name = state.name, state_abb = state.abb)

state_names_wrong <- 
  data.frame(state_name = wrong_abbrev[which(tolower(wrong_abbrev) %in% tolower(state.name))]) %>% 
  inner_join(state_data, by = "state_name")

# Fix the wrong abbreviations
wrong_abbrev[which(tolower(wrong_abbrev) %in% tolower(state.name))] <- state_names_wrong$state_abb

# For those players who didn't go to high school
no_hs <- draft_info %>% 
  slice(wrong_index) %>% 
  filter(high_school == "No")

# Improperly formatted states
wrong_states <- 
  wrong_abbrev[which(!(tolower(wrong_abbrev) %in% tolower(no_hs$home_state)))]

# Find the unique improperly formatted states
wrong <- unique(wrong_states) %>% sort()

correct_states <- c("AL", "ALB", "AZ", "AZ", "AR", "AR", "GA", "MD", "MI", "AL", 
                    "CA", "CA", "CA", "Canada", "Canada", "IL", 
                    "Christian Brothers", "OH", "Coahuila", "CO", "CO", "CT", "CA",
                    "DE", "CO", "FL", "FL", "FL", "FL", "Germany", "HI", "Holland",
                    "HI", "TX", "IA", "IL", "IL", "IL", "IL", "IN", "IN", "IA",
                    "KS", "NV", "CA", "IA", "MAN", "MA", "FL", "FL", "MI", "MI",
                    "MI", "MN", "MS", "CA", "NC", "NE", "NJ", "NJ", "VA", "NY",
                    "NY", "OH", "OK", "OK", "OK", "OR", "PA", "PA", "PA", "PA",
                    "AZ", "CA", "PA", "PA", "TX", "CA", "CA", "CA", "SASK", 
                    "St Thomas", "TN", "TX", "Trabuco Hills", "Trinity International",
                    "TX", "TX", "US Virgin Islands", "Ven", "WA", "WI", "OH", "WV",
                    "Zacatecas")

# Corrected abbreviations
corrections <- data.frame(wrong = wrong, correct = correct_states)

# Correct the wrong_states abbreviations with the correct ones, unless not in
# U.S.A.
wrong_states <- ifelse(wrong_states %in% corrections$wrong, corrections$correct, wrong_states)

# Update wrong_abbrev object with corrected abbreviations
wrong_abbrev[which(!(tolower(wrong_abbrev) %in% tolower(no_hs$home_state)))] <- wrong_states

# Correct leftover wrong_abbrev
correct_abbrev <- ifelse(wrong_abbrev %in% corrections$wrong, corrections$correct, wrong_abbrev)

# Add the correct_abbrev to the draft_info
draft_info$home_state[wrong_index] <- correct_abbrev
draft_info$home_state <- tolower(draft_info$home_state) # For matching up

# Create a list with all drafted player height
height_list <- str_match_all(draft_info$person_height, "\\d+")

# Convert the first element from feet to inches
height_ft <- as.integer(unlist(lapply(height_list, function(x) {x[1]}))) * 12
height_in <- as.integer(unlist(lapply(height_list, function(x) {x[2]}))) + as.integer(height_ft)
rm(height_list, height_ft) # Remove the height_list

# Convert the person_height variable to inches
draft_info$person_height <- height_in

rm(comma_index, comma_states, correct_abbrev, correct_states, height_in,
   missing_height, wrong, wrong_abbrev, wrong_index, wrong_states, state_data,
   state_names_wrong, no_hs, corrections)

# The draft info for the batters
## Separate from the stats because some players are drafted into the MLB
## multiple times. (e.g. Barry Bonds in 2nd round of 1982, and first round 1985)
bat_draft <- 
  batting %>% 
  inner_join(draft_info, by = "fg_playerID") %>% 
  filter(person_primary_position_name != "Pitcher")

rm(batting) # remove the batting data

# Only the distinct instances of batter's draft info, so only one instance for
# each fg_playerID
bat_stats <- 
  bat_draft %>% 
  # For picks whose rounds are none numerical we will designate them with a 1000
  # to ensure it's clear that it was a supplemental draft round
  mutate(pick_round = ifelse(is.na(as.numeric(pick_round)), 1000, 
                             as.numeric(pick_round))) %>% 
  group_by(person_id) %>% 
  slice(which.min(pick_round)) %>% # choose the lower round (the better pick)
  ungroup() %>% # ungroup
  distinct(person_id, .keep_all = T) %>% # distinct occurrences
  arrange(desc(fWAR))


# Removes players who do not have a person_birth_state_province
# e.g. Albert Pujols is from Santo Domingo, Dominican Republic
bat_stats <-
  bat_stats %>% 
  drop_na(home_state)

# Select only those with abbreviations with 2 letters, all those with more
# are international players so they do not have a `home_state`
bat_stats <- 
  bat_stats %>% 
  filter(home_state %in% tolower(state.abb))

# Full Draft information for pitcher's
pitch_draft <- 
  pitching %>% 
  inner_join(draft_info, by = "fg_playerID") %>% 
  filter(person_primary_position_name == "Pitcher")

rm(pitching) # Remove pitching data

# Career Stats for each person from the draft information
pitch_stats <- 
  pitch_draft %>% 
  # For picks whose rounds are none numerical we will designate them with a 1000
  # to ensure it's clear that it was a supplemental draft round
  mutate(pick_round = ifelse(is.na(as.numeric(pick_round)), 1000, 
                             as.numeric(pick_round))) %>% 
  group_by(person_id) %>% 
  slice(which.min(pick_round)) %>% # choose the lower round (the better pick)
  ungroup() %>% # ungroup
  distinct(person_id, .keep_all = T) %>% # distinct occurrences
  arrange(desc(fWAR))

# Removes players who do not have a person_birth_state_province
# e.g. Albert Pujols is from Santo Domingo, Dominican Republic
pitch_stats <-
  pitch_stats %>% 
  drop_na(home_state)

# Select only those with abbreviations with 2 letters, all those with more
# are international players so they do not have a `home_state`
pitch_stats <- 
  pitch_stats %>%  
  # Remove abbreviations for those such as the Canadians with province such as
  # Ontario (ON)
  filter(home_state %in% tolower(state.abb))

# Create csv files for draft information
# write_csv(draft_info, 'Data/clean_draft.csv')
# write_csv(bat_draft, 'Data/batters/batters_full_draft.csv')
# write_csv(bat_stats, 'Data/batters/batters_draft_stats.csv')
# write_csv(pitch_draft, 'Data/pitchers/pitchers_full_draft.csv')
# write_csv(pitch_stats, 'Data/pitchers/pitchers_draft_stats.csv')
# write_csv(bat_stats %>% select(fg_playerID, person_id, person_full_name, fWAR, pick_round,
#                                pick_number, year, person_birth_state_province,
#                                person_height, person_weight, home_state,
#                                person_primary_position_abbreviation,
#                                person_bat_side_code, person_pitch_hand_code,
#                                mlb_played_first, mlb_played_last, high_school
#                                ), 'Data/bat_subset.csv')
# write_csv(pitch_stats %>% select(fg_playerID, person_id, person_full_name, fWAR, pick_round,
#                                pick_number, year, person_birth_state_province,
#                                person_height, person_weight, home_state,
#                                person_primary_position_abbreviation,
#                                person_bat_side_code, person_pitch_hand_code,
#                                mlb_played_first, mlb_played_last, high_school
#                                ), 'Data/pitch_subset.csv')
