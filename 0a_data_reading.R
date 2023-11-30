## load packages and data----
library(tidyverse)
#read in data
nfl_elo_data <- read_csv("data/nfl_elo.csv")
foxboro_temp <- read_csv("data/foxboro_weather_data.csv")
foxboro_snow <- read_csv("data/raw/snowfall_foxboro.csv")
url <- "http://www.habitatring.com/games.csv"
dest_file <- "data/nfl_games.csv"
download.file(url, destfile = dest_file, mode = "wb")
nfl_games <- read_csv(dest_file)


