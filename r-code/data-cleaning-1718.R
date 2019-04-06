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

tb <- tbraw %>% 
  mutate(hometeam = get_team(h1, teamsandplayers),
         awayteam = get_team(a1, teamsandplayers),
         pointchange = map_lgl(points, score_changed)) 


allteams <- tb %>% 
  filter(elapsed > 0) %>% #eliminates weird cases like opening jump ball
  select(team) %>% 
  filter(!is.na(team)) %>% 
  distinct() 
tmp <- left_join(allteams, get_team_events(allteams, tb))


write_rds(tmp, "clean-data/team-events-1718.rds")

