library(tidyverse)
source("r-code/basic-stat-functions.R")

tmp <- read_rds("clean-data/team-events-1718.rds")

get_simple_game_log <- function(tb){
  
  tb %>% get_ast_stl_blk() %>% 
    add_simple_stat_indicators() %>% 
    make_simple_stats_game_log() %>% ungroup
  
}

simplestats <- tmp %>% 
  mutate(`Game Log (Simple stats)` =  
           map(`team events`, get_simple_game_log),
         `Regular Season per game (Simple stats)` = 
           map(`Game Log (Simple stats)`, make_simple_stats_pergame))

  
write_rds(simplestats, "clean-data/simple-stats-1718.rds")
