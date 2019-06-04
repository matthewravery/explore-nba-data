library(tidyverse)
source("r-code/basic-stat-functions.R")

tmp <- read_rds("clean-data/team-events-1718.rds")
pt <-  read_rds("clean-data/players-on-teams-1718.rds")

get_simple_game_log <- function(tb, tn, pt){
  
  tb %>% 
    mutate(team = tn) %>% 
    get_ast_stl_blk(pt) %>% 
    add_simple_stat_indicators() %>% 
    make_simple_stats_game_log() %>% ungroup
  
}


simplestats <- tmp %>% 
  left_join(pt) %>% 
  mutate(`Game Log (Simple stats)` =  
           pmap(list(`team events`, team, playerlist), get_simple_game_log),
         `Regular Season per game (Simple stats)` = 
           map2(`Game Log (Simple stats)`, team, make_simple_stats_pergame))

  
write_rds(simplestats, "clean-data/simple-stats-1718.rds")

