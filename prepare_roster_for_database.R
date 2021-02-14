library(tidyverse)
library(uuid)

setwd('../../')

read_roster <- function(file) {
  
  temp_list <- strsplit(file, ' ')
  
  season <- temp_list[[1]][1]
  teamName <- temp_list[[1]][2]
  
  
  return (readr::read_csv(file) %>% 
            mutate(team = teamName,
                   season = season))
}

combine_Rosters <- function() {
  
  setwd('data/rosters')
  
  combined <- list.files(pattern = '.csv') %>% 
    map_dfr(read_roster) %>% 
    rbind()
  
  setwd('../..')  
  
  return (combined)
}

add_UUIDs <- function(players_with_no_UUID) {
  
  return (players_with_no_UUID %>% 
            mutate(ID = UUIDgenerate(use.time = FALSE, 
                                     n = nrow(players_with_no_UUID))) %>% 
            select(c(season, team, ID, name, number, position)) %>% 
            arrange(season, team, number))
}

rosters <- combine_Rosters()
with_UUIDs <-  add_UUIDs(rosters)