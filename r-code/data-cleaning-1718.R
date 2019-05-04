library(tidyverse)
source("r-code/cleaning-helpers.R")

tbraw <- read_csv("data/2017-2018_NBA_PbP_Logs/[10-17-2017]-[06-08-2018]-combined-stats.csv") %>% 
  mutate(game_id = as.numeric(str_extract(game_id, "\\d+")) )

teamsandplayers <- tbraw %>% 
  filter(elapsed > 0,
         type == "jump ball") %>% 
  #jump balls appear to cause a problem with matching teams and players, producing weird results
  select(player, team) %>% 
  filter(!is.na(player)) %>% 
  distinct() 


fl <- list.files("data/2017-2018_NBA_PbP_Logs")[-1] %>% 
  str_split("-", 5) %>% unlist() %>%  matrix(ncol = 5, byrow = T) %>% 
  as_tibble() %>% 
  select(V4, V5) %>% rename(game_id = V4, otherinfo = V5) %>% 
  mutate(game_id = as.double(game_id)) %>% 
  separate(otherinfo, into = c("awayteam", "other"), sep = "@") %>% 
  separate(other, into = "hometeam", sep = ".csv", extra = "drop")

# write_csv(fl, "data/games-by-home-and-away-teams.csv")

tb <- tbraw %>% 
  left_join(fl) %>% 
  mutate(pointchange = map_lgl(points, score_changed)) 


allteams <- tb %>% 
  filter(elapsed > 0) %>% #eliminates weird cases like opening jump ball
  select(team) %>% 
  filter(!is.na(team)) %>% 
  distinct() 
tmp <- left_join(allteams, get_team_events(allteams, tb))


write_rds(tmp, "clean-data/team-events-1718.rds")



tmp %>% 
  mutate(playerlist = map(`team events`, get_team_players)) %>% 
  select(team, playerlist) %>% 
  write_rds("clean-data/players-on-teams-1718.rds")

