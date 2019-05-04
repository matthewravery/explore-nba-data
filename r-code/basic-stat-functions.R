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

# this wrapper applies these functions and lets the results work well
# with our other counting bits. the trick here is that these events involve
# multiple players and therefore we have to create duplicate rows so that
# we can attribute the different parts of each event (e.g. the assist
# and the made basket) to different players
get_ast_stl_blk <- function(tb){
  
  tb %>% 
    get_event_and_player("BLOCK") %>% 
    get_event_and_player("STEAL") %>% 
    get_ast_and_player() %>% 
    mutate(dropit = pmap_lgl(list(`BLOCK player`, `STEAL player`, `ASSIST player`), 
                             function(a, b, d) !(is.na(a) && 
                                                   is.na(b) && 
                                                   is.na(d)
                             )
    )) %>% filter(dropit) %>% 
    select(`BLOCK player`, `STEAL player`, `ASSIST player`, game_id, 
           date, playoffs, mplay_id, possessioncount) %>% 
    gather(-game_id, -mplay_id, -possessioncount, -date, 
           -playoffs,
           key = "description", value = "player") %>% 
    filter(!is.na(player)) %>% 
    mutate(description = map_chr(description, str_sub, end = -8)) %>% 
    bind_rows(tb)
}


add_simple_stat_indicators <- function(tb){
  
  tb %>% 
    mutate(
      gotblk = (description == "BLOCK"),
      gotstl = (description == "STEAL"),
      gotast = (description == "ASSIST"),
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
      FGM = sum(fgmade, na.rm = T),
      FGA = sum(fgmade, na.rm = T) + sum(fgmiss, na.rm = T),
      FTM = sum(madeft, na.rm = T),
      FTA = sum(madeft, na.rm = T) + sum(missft, na.rm = T),
      `3PM` = sum(made3, na.rm = T),
      `3PA` = sum(made3, na.rm = T) + sum(miss3, na.rm = T),
      REB = sum(gotreb, na.rm = T),
      STL = sum(gotstl, na.rm = T),
      AST = sum(gotast, na.rm = T),
      BLK = sum(gotblk, na.rm = T),
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
