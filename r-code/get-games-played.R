# library(tidyverse)
# tmp <- read_rds("clean-data/team-events-1718.rds")
# 
# tmp
# 
# bm <- tmp %>% filter(team == "BOS") %>% unnest()
# 
# 
# 
# bm %>% filter(!is.na(player)) %>% 
#   select(player, event_type, description, points)
# 
# 
# thisplayer <- theseplayers$player[1]
# 
# 
# a <- bm %>% 
#   group_by(game_id) %>% 
#   select(game_id, p1, p2, p3, p4, p5) %>% 
#   gather(key = "slot", value = "player", p1, p2, p3, p4, p5) %>% 
#   select(-slot) %>% 
#   distinct() %>% arrange(game_id) %>% 
#   summarise(playersingame = paste0(player, ", ", collapse = "")) 

get_games_played <- function(bm, playernames = "all"){
  
  theseplayers <- bm %>% select(team, p1, p2, p3, p4, p5) %>% 
    distinct() %>% 
    gather(key = "team", value = "player") %>%
    select(player) %>% 
    distinct()
  
  if(playernames == "all"){
    out <- theseplayers %>% 
      mutate(`Games Played` = map_dbl(player, get_player_games_played, bm))
    
  } else{
      
    out <- theseplayers %>% 
      filter(player %in% playernames) %>% 
      mutate(`Games Played` = map_dbl(player, get_player_games_played, bm))
    }
  

  out
}
  
  
get_player_games_played <- function(playername, bm){
  
    out <- bm %>% 
    group_by(game_id) %>% 
    select(game_id, p1, p2, p3, p4, p5) %>% 
    gather(key = "slot", value = "player", p1, p2, p3, p4, p5) %>% 
    select(-slot) %>% 
    distinct() %>% arrange(game_id) %>% 
    summarise(playersingame = paste0(player, ", ", collapse = "")) %>% 
    mutate(didheplay = str_detect(playersingame, playername)) %>% 
    summarise(player = playername,
              `Games Played` = sum(didheplay))
    
    out$`Games Played`
  
  
}

