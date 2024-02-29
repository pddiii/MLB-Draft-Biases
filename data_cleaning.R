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
glimpse(batting)
# View the structure of the pitching data
glimpse(pitching)

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
glimpse(draft_info)

## 67,903 rows by 47 columns
draft_info <-
  draft_info %>% 
  # Add year columns to match up with Chadwick
  mutate(mlb_played_first = year(person_mlb_debut_date),
         mlb_played_last = year(person_last_played_date)) %>% 
  # Remove variables of no interest
  select(-person_link, -c(person_use_name:person_gender),
         -c(person_name_slug:person_init_last_name), 
         -c(person_name_matrilineal:school_state),
         -c(person_xref_ids:person_death_country), -person_name_title, -team_id,
         -person_name_suffix, -c(team_link:team_spring_league_abbreviation), 
         -headshot_link)

# Load the Chadwick baseball player info
## 482,259 rows by 40 columns
chadwick <- fread('Data/chadwick_data.csv')

# Update Chadwick for players who were drafted after
## 10,790 rows by 21 columns
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
  # Filter mostly players who were drafted in 1965 or later
  filter(mlb_played_first >= 1965) %>% 
  arrange(mlb_played_first)

## 9,792 rows by 49 columns
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
         -name_first)

glimpse(draft_info)
glimpse(chadwick)

rm(chadwick)

# The draft info for the batters
## Separate from the stats because some players are drafted into the MLB
## multiple times. (e.g. Barry Bonds in 2nd round of 1982, and first round 1985)
bat_draft <- 
  batting %>% 
  inner_join(draft_info, by = "fg_playerID") %>% 
  mutate(high_school = ifelse(str_detect(school_name, "\\sHS") == TRUE, "Yes", 
                              "No"),
         person_height = str_replace_all(person_height, "\"$", "")) %>% 
  replace_na(list(mlb_played_last = 2023))

rm(batting) # remove the batting data
# Create a list of the players heights
height_list <- str_match_all(bat_draft$person_height, "\\d+")
# Convert the heights to inches
height_ft <- as.integer(unlist(lapply(height_list, function(x) {x[1]}))) * 12
height_in <- as.integer(unlist(lapply(height_list, function(x) {x[2]}))) + as.integer(height_ft)

bat_draft$person_height <- height_in

# Only the distinct instances of batter's draft info, so only one instance for
# each fg_playerID
bat_stats <- 
  bat_draft %>% 
  distinct(fg_playerID, .keep_all = TRUE) %>% 
  mutate(high_school = ifelse(str_detect(school_name, "\\sHS") == TRUE, "Yes", 
                              "No"))

# Get the indices for the elements which contain a state abbreviation
# this applies to High School players who have states listed in format of either
# (CA) or (, CA) (e.g. CA = California)
bat_home_index <- which(str_detect(bat_stats$school_name, pattern = "\\((.*?)\\)"))
length(bat_home_index) # 3,222 elements

# List with all the match results for the pattern "(letters)"
bat_home_list <- str_match_all(bat_stats$school_name, pattern = "\\((.*?)\\)")

# Extracts the text within the "()", and stores it as a vector
bat_home_states <- unlist(lapply(bat_home_list, function(x) {x[, 2]}))
rm(bat_home_list) # Remove the list form of the home states

# removes any results where there was an NA result generated
bat_home_states <- bat_home_states[!is.na(bat_home_states)]
length(bat_home_states) # 3,222 elements, same as bat_home_index

# Get the index for the bat_home_stats which contain the pattern ", "
comma_index <- which(str_detect(bat_home_states, ",\\s"))
length(comma_index) # 1,726 elements

# Extract the states with the pattern ", " from `bat_home_states`
comma_states <- bat_home_states[comma_index]

# Determine if there are any incorrect patterns
wrong <- comma_states[which(!str_detect(comma_states, ", ([:alpha:]+)"))]

# Replace the pattern within the `comma_states` manually
comma_states[which(!str_detect(comma_states, ", ([:alpha:]+)"))] <- ", FL"

# Extract the letters which follow the ", " pattern into a vector
comma_states <- 
  str_match_all(comma_states, ", ([:alpha:]+)") %>% 
  lapply(function(x) {x[, 2]}) %>% 
  unlist()

length(comma_states) # 1,726 elements

# Replace the comma states with the correct abbreviations
bat_home_states[comma_index] <- comma_states

# Create a new variable home_state
bat_stats <- bat_stats %>% mutate(home_state = person_birth_state_province)

# Store all the home_states to the new `home_state` variable within `bat_stats`
bat_stats$home_state[bat_home_index] <- bat_home_states
rm(bat_home_index, bat_home_states, wrong)
# Removes players who do not have a person_birth_state_province
# e.g. Albert Pujols is from Santo Domingo, Dominican Republic
bat_stats <-
  bat_stats %>% 
  drop_na(home_state)

# Find the wrong abbreviations
wrong_abbrev <- 
  bat_stats %>% 
  select(school_name, person_birth_state_province, home_state) %>% 
  slice(which(nchar(bat_stats$home_state) > 2))

# Corrected abbreviations
correct_abbrev <- c("HI", "CA", "IL", "WA", "MI", "VI", "Sonora", "FL", "TX", 
                    "Sonora", "HI", "ON", "TX", "CA", "NV", "Chihuahua", "BC",
                    "OH", "AZ", "TX", "FL", "QC", "ON", "AZ", "Victoria", "ON",
                    "ON", "Western Australia", "ON", "AZ", "CA", "AB", "CA", 
                    "ON", "CA", "FL", "HI", "NV", "ON", "Sonora", "NY", 
                    "Chihuahua", "TX", "Sonora", "TN", "MN", "FL", "IA", "ON",
                    "KS", "HI", "ON", "IL", "MI", "CT", "Jalisco")

# Replace the wrong abbreviations with the corrected ones
bat_stats$home_state[which(nchar(bat_stats$home_state) > 2)] <- correct_abbrev
rm(correct_abbrev)
# Select only those with abbreviations with 2 letters, all those with more
# are international players so they do not have a `home_state`
bat_stats <- 
  bat_stats %>% 
  slice((which(nchar(bat_stats$home_state) <= 2))) %>% 
  # Remove abbreviations for those such as the Canadians with province such as
  # Ontario (ON)
  filter(home_state %in% state.abb)

# add the home_state variable to the full draft info
bat_draft <-
  bat_draft %>% 
  inner_join(bat_stats %>% select(fg_playerID, home_state), 
             by = "fg_playerID")

# Full Draft information for pitcher's
pitch_draft <- 
  pitching %>% 
  inner_join(draft_info, by = "fg_playerID") %>% 
  mutate(high_school = ifelse(str_detect(school_name, "\\sHS") == TRUE, "Yes", 
                              "No"),
         person_height = str_replace_all(person_height, "\"$", "")) %>% 
  replace_na(list(mlb_played_last = 2023))

rm(pitching) # Remove pitching data

height_list <- str_match_all(pitch_draft$person_height, "\\d+")

height_ft <- as.integer(unlist(lapply(height_list, function(x) {x[1]}))) * 12
height_in <- as.integer(unlist(lapply(height_list, function(x) {x[2]}))) + as.integer(height_ft)
rm(height_list, height_ft) # Remove the height_list

pitch_draft$person_height <- height_in 
rm(height_in)

# Career Stats for each person from the draft information
pitch_stats <- 
  pitch_draft %>% 
  distinct(fg_playerID, .keep_all = TRUE) %>% 
  mutate(high_school = ifelse(str_detect(school_name, "\\sHS") == TRUE, "Yes", 
                              "No"))

# Get the indices for the elements which contain a state abbreviation
# this applies to High School players who have states listed in format of either
# (CA) or (, CA) (e.g. CA = California)
pitch_home_index <- which(str_detect(pitch_stats$school_name, pattern = "\\((.*?)\\)"))
length(pitch_home_index) # 1,899 elements

# List with all the match results for the pattern "(letters)"
pitch_home_list <- str_match_all(pitch_stats$school_name, pattern = "\\((.*?)\\)")

# Extracts the text within the "()", and stores it as a vector
pitch_home_states <- unlist(lapply(pitch_home_list, function(x) {x[, 2]}))
rm(pitch_home_list)

# removes any results where there was an NA result generated
pitch_home_states <- pitch_home_states[!is.na(pitch_home_states)]
length(pitch_home_states) # 1,899 elements, same as pitch_home_index

# Get the index for the pitch_home_stats which contain the pattern ", "
comma_index <- which(str_detect(pitch_home_states, ",\\s"))
length(comma_index) # 977 elements

# Extract the states with the pattern ", " from `pitch_home_states`
comma_states <- pitch_home_states[comma_index]

# Determine if there are any incorrect patterns
wrong <- comma_states[which(!str_detect(comma_states, ", ([:alpha:]+)"))]

# Replace the pattern within the `comma_states` manually
comma_states[which(!str_detect(comma_states, ", ([:alpha:]+)"))] <- ", FL"

# Extract the letters which follow the ", " pattern into a vector
comma_states <- 
  str_match_all(comma_states, ", ([:alpha:]+)") %>% 
  lapply(function(x) {x[, 2]}) %>% 
  unlist()

length(comma_states) # 977 elements, same as `comma_index`

# Replace the comma states with the correct abbreviations
pitch_home_states[comma_index] <- comma_states
rm(comma_index, comma_states, wrong)
# Create a new variable home_state
pitch_stats <- pitch_stats %>% mutate(home_state = person_birth_state_province)

# Store all the home_states to the new `home_state` variable within `pitch_stats`
pitch_stats$home_state[pitch_home_index] <- pitch_home_states
rm(pitch_home_index, pitch_home_states)
# Removes players who do not have a person_birth_state_province
# e.g. Albert Pujols is from Santo Domingo, Dominican Republic
pitch_stats <-
  pitch_stats %>% 
  drop_na(home_state)

# Find the wrong abbreviations
wrong_abbrev <- 
  pitch_stats %>% 
  select(school_name, person_birth_state_province, home_state) %>% 
  slice(which(nchar(pitch_stats$home_state) > 2))

# Corrected abbreviations
correct_abbrev <- c("IL", "CT", "TN", "TX", "KS", "CA", "Sonora", "CA", 
                    "MN", "HI", "ON", "ON", "ON", "AZ", "Chihuahua", "CA", "QC",
                    "OH", "FL", "Victoria", "Holland", "BC", "Sonora", "Jalisco", 
                    "FL", "TX", "Western Australia", "NV", "AZ", "ON", "TX", 
                    "NY", "ON", "AB")

rm(wrong_abbrev) # remove the wrong abbreviations

# Replace the wrong abbreviations with the corrected ones
pitch_stats$home_state[which(nchar(pitch_stats$home_state) > 2)] <- correct_abbrev
rm(correct_abbrev)
# Select only those with abbreviations with 2 letters, all those with more
# are international players so they do not have a `home_state`
pitch_stats <- 
  pitch_stats %>% 
  slice((which(nchar(pitch_stats$home_state) <= 2))) %>% 
  # Remove abbreviations for those such as the Canadians with province such as
  # Ontario (ON)
  filter(home_state %in% state.abb)

# add the home_state variable to the full draft info
pitch_draft <-
  pitch_draft %>% 
  inner_join(pitch_stats %>% select(fg_playerID, home_state), 
             by = "fg_playerID")

# Create csv files for draft information
# write_csv(bat_draft, 'Data/batters/batters_full_draft.csv')
# write_csv(bat_stats, 'Data/batters/batters_draft_stats.csv')
# write_csv(pitch_draft, 'Data/pitchers/pitchers_full_draft.csv')
# write_csv(pitch_stats, 'Data/pitchers/pitchers_draft_stats.csv')
# write_csv(bat_stats %>% select(fg_playerID, person_full_name, fWAR, pick_round,
#                                pick_number, year, person_birth_state_province,
#                                person_height, person_weight, home_state,
#                                person_primary_position_abbreviation,
#                                person_bat_side_code, person_pitch_hand_code,
#                                mlb_played_first, mlb_played_last, high_school
#                                ), 'Data/bat_subset.csv')
# write_csv(pitch_stats %>% select(fg_playerID, person_full_name, fWAR, pick_round,
#                                pick_number, year, person_birth_state_province,
#                                person_height, person_weight, home_state,
#                                person_primary_position_abbreviation,
#                                person_bat_side_code, person_pitch_hand_code,
#                                mlb_played_first, mlb_played_last, high_school
#                                ), 'Data/pitch_subset.csv')
