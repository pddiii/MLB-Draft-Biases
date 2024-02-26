library(data.table)
library(tidyverse)

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

# The draft info for the batters
## Separate from the stats because some players are drafted into the MLB
## multiple times. (e.g. Barry Bonds in 2nd round of 1982, and first round 1985)
bat_draft <- 
  batting %>% 
  inner_join(draft_info, by = "fg_playerID")

# Only the distinct instances of batter's draft info, so only one instance for
# each fg_playerID
bat_stats <- 
  bat_draft %>% 
  distinct(fg_playerID, .keep_all = TRUE)

# Full Draft information for pitcher's
pitch_draft <- 
  pitching %>% 
  inner_join(draft_info, by = "fg_playerID")

# Career Stats for each person from the draft information
pitch_stats <- 
  pitch_draft %>% 
  distinct(fg_playerID, .keep_all = TRUE)

# Create csv files for draft information
# write_csv(bat_draft, 'Data/batters_full_draft.csv')
# write_csv(bat_stats, 'Data/batters_draft_stats.csv')
# write_csv(pitch_draft, 'Data/pitchers_full_draft.csv')
# write_csv(pitch_stats, 'Data/pitchers_draft_stats.csv')