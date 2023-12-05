## load packages and data----
library(tidyverse)
library(skimr)
library(knitr)
#read in data
nfl_elo_data <- read_csv("data/raw/nfl_elo.csv")
foxboro_temp <- read_csv("data/raw/foxboro_temp_data.csv")
foxboro_snow <- read_csv("data/raw/snowfall_foxboro.csv")
url <- "http://www.habitatring.com/games.csv"
dest_file <- "data/raw/nfl_games.csv"
download.file(url, destfile = dest_file, mode = "wb")
nfl_games <- read_csv(dest_file)


