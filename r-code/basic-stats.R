library(tidyverse)
tmp <- read_rds("clean-data/team-events-1718.rds")

tmp

bm <- tmp %>% filter(team == "BOS") %>% unnest()
  

bm %>% filter(!is.na(player)) %>%
  select(player, event_type, description, points)


# thisplayer <- theseplayers$player[1]


aa <- bm %>%
  group_by(game_id) %>%
  select(game_id, p1, p2, p3, p4, p5) %>%
  gather(key = "slot", value = "player", p1, p2, p3, p4, p5) %>%
  select(-slot) %>%
  distinct() %>% arrange(game_id) %>%
  summarise(playersingame = paste0(player, ", ", collapse = ""))

# FGM FGA FTM FTA 3PM 3PA OREB DREB REB AST STL BLK TO PF PTS

a <- bm %>% 
  mutate(gotblk = map_lgl(description, str_detect, "BLOCK"),
         gotstl = map_lgl(description, str_detect, "STEAL"),
         gotreb = map_lgl(description, str_detect, "REBOUND"),
         tfoulu = map_lgl(description, str_detect, "T.FOUL"),
         tfoull = map_lgl(description, str_detect, "T.Foul"),
         fgmade = event_type == "shot",
         fgmiss = event_type == "miss",
         shotft = event_type == "free throw",
         foul = event_type == "foul",
         turnover = event_type == "turnover",
         shot3 = map_lgl(description, str_detect, "3PT"),
         made3 = map2_lgl(shot3, fgmade, function(a, b) a && b),
         miss3 = map2_lgl(shot3, fgmiss, function(a, b) a && b),
         missathing = map_lgl(description, str_detect, "MISS"),
         madeft = map2_lgl(shotft, !missathing, function(a, b) a && b),
         missft = map2_lgl(shotft, missathing, function(a, b) a && b),
         tfoul = map2_lgl(tfoulu, tfoull, function(a, b) a | b),
         pfoul = map2_lgl(foul, !tfoul , function(a, b) a && b)) 

b <- a %>% 
  filter(!is.na(player), !playoffs) %>% 
  group_by(game_id, player) %>%
  summarise(
    FGM = sum(fgmade),
    FGA = sum(fgmade) + sum(fgmiss),
    FTM = sum(madeft),
    FTA = sum(madeft) + sum(missft),
    `3PM` = sum(made3),
    `3PA` = sum(made3) + sum(miss3),
    REB = sum(gotreb, na.rm = T),
    STL = sum(gotstl, na.rm = T),
    BLK = sum(gotblk, na.rm = T),
    STL = sum(gotstl, na.rm = T),
    TO = sum(turnover, na.rm = T)
  ) 

b %>% 
  group_by(player) %>% 
  summarise(GP = length(game_id),
            `FG%` = sum(FGM, na.rm = T) / sum(FGA, na.rm = T),
            `3PT%` = sum(`3PM`, na.rm = T) / sum(`3PA`, na.rm = T),
            `FT%` = sum(FTM, na.rm = T) / sum(FTA, na.rm = T),
            REB = mean(REB, na.rm = T),
            STL = mean(STL, na.rm = T),
            BLK = mean(BLK, na.rm = T),
            TO = mean(TO, na.rm = T)) %>% 
  arrange(desc(GP))
            
# These above code chunks are arranged into functions for easy replication

# we'll take this step, which makes columns indicating relevant events, and move it to the cleaning helpers
add_simple_stat_indicators <- function(tb){
  
  bm %>% 
    mutate(gotblk = map_lgl(description, str_detect, "BLOCK"),
           gotstl = map_lgl(description, str_detect, "STEAL"),
           gotreb = map_lgl(description, str_detect, "REBOUND"),
           tfoulu = map_lgl(description, str_detect, "T.FOUL"),
           tfoull = map_lgl(description, str_detect, "T.Foul"),
           fgmade = event_type == "shot",
           fgmiss = event_type == "miss",
           shotft = event_type == "free throw",
           foul = event_type == "foul",
           turnover = event_type == "turnover",
           shot3 = map_lgl(description, str_detect, "3PT"),
           made3 = map2_lgl(shot3, fgmade, function(a, b) a && b),
           miss3 = map2_lgl(shot3, fgmiss, function(a, b) a && b),
           missathing = map_lgl(description, str_detect, "MISS"),
           madeft = map2_lgl(shotft, !missathing, function(a, b) a && b),
           missft = map2_lgl(shotft, missathing, function(a, b) a && b),
           tfoul = map2_lgl(tfoulu, tfoull, function(a, b) a | b),
           pfoul = map2_lgl(foul, !tfoul , function(a, b) a && b)) 
  
}

#this generates a game log of simple stats
make_simple_stats_game_log <- function(tb){
  
  tb %>% 
    filter(!is.na(player)) %>% 
    group_by(game_id, player, playoffs, date) %>%
    summarise(
      FGM = sum(fgmade),
      FGA = sum(fgmade) + sum(fgmiss),
      FTM = sum(madeft),
      FTA = sum(madeft) + sum(missft),
      `3PM` = sum(made3),
      `3PA` = sum(made3) + sum(miss3),
      REB = sum(gotreb, na.rm = T),
      STL = sum(gotstl, na.rm = T),
      BLK = sum(gotblk, na.rm = T),
      STL = sum(gotstl, na.rm = T),
      TO = sum(turnover, na.rm = T)
    )
}

# ... and this aggregates them across the whole season. 
# I had to find a good way to filter out for regular-season-only, and I'm 
# stil not sure I ended up with intuitive naming conventions here. 
make_simple_stats_pergame <- function(tb, useregularseason = T){
  
  tb %>% 
    filter(useregularseason != playoffs) %>%  
    # `playoffs` is an indicator taking the value T for playoff games
    group_by(player) %>% 
    summarise(GP = length(game_id),
              `FG%` = sum(FGM, na.rm = T) / sum(FGA, na.rm = T),
              `3PT%` = sum(`3PM`, na.rm = T) / sum(`3PA`, na.rm = T),
              `FT%` = sum(FTM, na.rm = T) / sum(FTA, na.rm = T),
              REB = mean(REB, na.rm = T),
              STL = mean(STL, na.rm = T),
              BLK = mean(BLK, na.rm = T),
              TO = mean(TO, na.rm = T)) %>% 
    arrange(desc(GP))
  
}


bm %>% add_simple_stat_indicators() %>% make_simple_stats_game_log() %>% make_simple_stats_pergame()

bm %>% add_simple_stat_indicators() %>% make_simple_stats_game_log() %>% filter(player == "Al Horford")

bm %>% add_simple_stat_indicators() %>% 
  filter(game_id == "21700007") %>% 
  select(gotblk, event_type, description, player) %>% print(n  = 5000) %>% View()

ab <- bm %>% add_simple_stat_indicators() %>% 
  filter(game_id == "21700007") 

ab %>% 
  mutate(allplayers = paste(p1, p2, p3, p4, p5, sep = ","),
         gotblk2 = map_chr(description, str_extract, "[:alpha:]+ BLOCK"),
         blkerlastname = map_chr(gotblk2, str_sub, end = -7),
         regexname = paste("[:alpha:]+", blkerlastname),
         blockerfull = map2_chr(allplayers, regexname, str_extract)) %>% 
  select(player, event_type, description, gotblk2, gotblk, allplayers, regexname, blockerfull)%>% View(  )
  

get_event_and_player <- function(ab, event){
  
  evplayer <- paste(event, "player")
  ab %>% 
    mutate(allplayers = paste(p1, p2, p3, p4, p5, sep = ","),
           gotit = map_chr(description, str_extract, paste("[:alpha:]+", event)),
           evlastname = map_chr(gotit, str_sub, end = -7),
           regexname = paste("[:alpha:]+", evlastname),
           !!evplayer := map2_chr(allplayers, regexname, str_extract)) %>% 
    select(-allplayers, -gotit, -evlastname, -regexname)
  
}


get_ast_and_player <- function(ab){
  
  ab %>% 
    mutate(allplayers = paste(p1, p2, p3, p4, p5, sep = ","),
           gotast = map_chr(description, str_extract, paste("[:alpha:]+ [:digit:] AST")),
           astlastname = map_chr(gotast, str_sub, end = -7),
           regexname = paste("[:alpha:]+", astlastname),
           `ASSIST player` = map2_chr(allplayers, regexname, str_extract)) %>% 
    select(-allplayers, -gotast, -astlastname, -regexname)
  
}

ab %>% get_event_and_player("BLOCK") %>% 
  get_event_and_player("STEAL") %>% 
  get_ast_and_player() %>% select(`ASSIST player`) %>% View()



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

