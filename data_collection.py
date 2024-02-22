import pybaseball
from pybaseball import batting_stats, pitching_stats

# Get aggregate batting stats for all players from 1965 to present day (MLB Draft began in 1965)
batting = batting_stats(start_season=1965, end_season=2023, qual=0, split_seasons=False)
# Get aggregate pitching stats for all players from 1965 to present day (MLB Draft began in 1965)
pitching = pitching_stats(start_season=1965, end_season=2023, qual=0, split_seasons=False)
# Create the csv files for the batting and pitching data
# batting.to_csv('batting_stats.csv', index=False)
# pitching.to_csv('pitching_stats.csv', index=False)