## loading packages and data----
library(tidyverse)
library(ggcorrplot)
library(skimr)
library(knitr)
#reading in data
nfl_elo_data <- read_csv("data/raw/nfl_elo.csv")
foxboro_temp <- read_csv("data/raw/foxboro_temp_data.csv")
foxboro_snow <- read_csv("data/raw/snowfall_foxboro.csv")
foxboro_rain <- read_csv("data/raw/rain_data.csv")
url <- "http://www.habitatring.com/games.csv"
dest_file <- "data/raw/nfl_games.csv"
download.file(url, destfile = dest_file, mode = "wb")
nfl_games <- read_csv(dest_file)

#Checking to make sure file paths exist
file_path <- "data/game_data.rds"
file.exists(file_path)

if (file.exists(file_path)) {
  my_data <- readr::read_rds(file_path)
} else {
  print("File does not exist.")
}

