## Finally, a file for actually cleaning the raw data and saving it!

get_team <- function(playernames, teamsandplayers){
  
  teamsandplayers$team[match(playernames, teamsandplayers$player)]
  
}

score_changed <- function(points){
  
  !is.na(points) && points > 0
}

## Data re-org
## Goal:  For each team, generate event list from that team's perspective

# To start, do it for a single team



#helper functions used to build up a "team events" table
get_net_points <- function(points, histeam, team){
  if(is.na(team))
    return(NA) else
      if(histeam == team)
        return(points) else
          return(points * -1)
}


add_subsections <- function(teamtbl, thisteamsplayers){
  
  subsec <- rep(0, nrow(teamtbl))
  
  counter <- 1
  for(i in 1:nrow(teamtbl)){
    if(!is.na(teamtbl$entered[i]) && teamtbl$entered[i] %in% thisteamsplayers$player) counter <- counter + 1
    subsec[i] <- counter
  }
  
  teamtbl$subsec <- subsec
  teamtbl
  
}


get_this_teams_players <- function(teamtbl, thatteam){
  
  teamtbl %>% 
    filter(elapsed > 0,             #eliminates weird cases like opening jump ball
           team == thatteam) %>% 
    select(player, team) %>% 
    filter(!is.na(player)) %>% 
    distinct()
}

fiveplayers <- function(tb){
  
  tb1 <- tb %>% filter(currentteam == hometeam) %>% 
    rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5, o1 = a1, o2 = a2, o3 = a3, o4 = a4, o5 = a5)
  
  tb2 <- tb %>% filter(currentteam == awayteam) %>% 
    rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5, o1 = h1, o2 = h2, o3 = h3, o4 = h4, o5 = h5)
  
  bind_rows(tb1, tb2) %>% arrange(game_id)
  
}

##out-of-function version for testing

# thatteam <- "BOS"
# 
# 
# teamtbl <- tb %>%
#   mutate(currentteam = thatteam) %>%
#   filter(hometeam == currentteam | awayteam == currentteam) %>% 
#   fiveplayers()
# 
# thisteamsplayers <- get_this_teams_players(teamtbl, thatteam)
# 
# teamtbl %>%
#   add_subsections(thisteamsplayers) %>%
#   mutate(netpoints = pmap_dbl(list(points, currentteam, team), get_net_points)) %>%
#   select(-team) %>% rename(team = currentteam)

#These next two functions are used to re-arrange the player names to always be alphabetical. So the first position will always be the player who is first alphabetically of the players on the court for his team. This is important when we attempt to group by five-man units, etc.
#note that it would be more efficient to do this at other places where I'm re-naming the player columns, but I don't want to risk screwing that up at this point. 

make_player_list <- function(p1, p2, p3, p4, p5){
  
  fiveplayers <-tibble(players = c(p1, p2, p3, p4, p5)) %>% 
    arrange(players)
  
  fiveplayers
  
}

get_player <- function(fiveplayers, num = 1){
  
  fiveplayers[[1]][num]
}


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

get_team_events <- function(whichteam, tb){
  
  out <- NULL
  for(i in seq_along(whichteam$team)){
    
    thatteam <- whichteam$team[i]
    
    teamtbl <- tb %>% 
      mutate(currentteam = thatteam) %>% 
      filter(hometeam == currentteam | awayteam == currentteam) %>% 
      fiveplayers() %>% 
      mutate(fiveplayers = pmap(list(p1, p2, p3, p4, p5), make_player_list),
             p1 = map_chr(fiveplayers, get_player, 1),
             p2 = map_chr(fiveplayers, get_player, 2),
             p3 = map_chr(fiveplayers, get_player, 3),
             p4 = map_chr(fiveplayers, get_player, 4),
             p5 = map_chr(fiveplayers, get_player, 5)) %>% select(-fiveplayers) %>% 
      arrange(game_id) %>% 
      add_index() %>% 
      id_orebs(thatteam) %>% 
      add_simple_stat_indicators() %>% 
      mutate(playoffs = game_id > 40000000) #game_id starts with 41 to indicate playoffs
      
    
    thisteamsplayers <- get_this_teams_players(teamtbl, thatteam)
    
    out <- teamtbl %>% 
      add_subsections(thisteamsplayers) %>% 
      possession_change() %>% 
      pos_count() %>% 
      mutate(netpoints = pmap_dbl(list(points, currentteam, team), get_net_points)) %>% 
      select(-team) %>% rename(team = currentteam) %>% 
      bind_rows(out)
    
  }
  out %>% group_by(team) %>% nest(.key = `team events`)
  
}


add_index <- function(x){
  n <- nrow(x)
  x$mplay_id <- 1:n
  x
}



#simple map of three-letter city abbreviations to team names
city_to_team <- function(cty3){
  
  city2team <- tibble(city3 = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN",
                                "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA",
                                "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX",
                                "POR", "SAC", "SAS", "TOR", "UTA", "WAS"),
                      team = c("HAWKS", "CELTICS", "NETS", "HORNETS", "BULLS", "CAVELIERS",
                               "MAVERICKS", "NUGGETS", "PISTONS", "WARRIORS", "ROCKETS",
                               "PACERS", "CLIPPERS", "LAKERS", "GRIZZLIES", "HEAT",
                               "BUCKS", "TIMBERWOLVES", "PELICANS", "KNICKS", "THUNDER",
                               "MAGIC", "76ERS", "SUNS", "TRAIL BLAZERS", "KINGS",
                               "SPURS", "RAPTORS", "JAZZ", "WIZARDS"))
  
  filter(city2team, city3 == cty3)$team
  
}




#This monstrosity identifies offensive rebounds in order to make it easier to identify breaks in posession

id_orebs <- function(teamtbl, thatteam){
  # this is currently written to interface with team-specific dataframes. 
  # There's no good reason for this, and it would be more efficient to 
  # re-write and create this column for the full data table.
  
  ftmiss <- teamtbl %>% 
    filter(event_type == "free throw", type %in% c("Free Throw 1 of 1",
                                                   "Free Throw 2 of 2",
                                                   "Free Throw 3 of 3")) %>% 
    mutate(miss = map_chr(description, str_match, "MISS"),
           shooterhome = (player %in% c(o1, o2, o3, o4, o5)), hometeam == thatteam) %>% 
    filter(miss == "MISS") 
  
  
  omiss <- teamtbl %>% filter(event_type == "miss") %>% 
    mutate(miss = "MISS",
           shooterhome = (player %in% c(o1, o2, o3, o4, o5)), hometeam == thatteam)
  
  smiss <- bind_rows(ftmiss, omiss) %>% 
    select(game_id, mplay_id, miss, shooterhome) %>% 
    mutate(old_id = mplay_id,
           mplay_id = mplay_id + 1) %>% 
    arrange(mplay_id) 
  
  #two weird issues remain:  One, sometimes the data frame has a substitute recorded after the 2nd ft instead of before it. we shoudl try to skip these and just get the subsequent play. Two, sometimes the rebound is omitted all together and there's just another play next. This we can basically ignore, assuming that the team that recovered the ball is indicated by the player shooting the next shot. 
  
  smiss$mplay_id[left_join(smiss, teamtbl)$event_type == "sub"] <- 
    smiss$mplay_id[left_join(smiss, teamtbl)$event_type == "sub"] + 1
  
  ab <- smiss %>% left_join(teamtbl) %>% 
    filter(event_type == "rebound") %>% 
    select(event_type, description, shooterhome, o1, o2, o3, o4, o5, player, 
           hometeam, shooterhome, mplay_id, old_id) %>% 
    separate(description, into = "rbname", sep = " ", extra = "drop") %>% 
    mutate(homelong = map_chr(hometeam, city_to_team),
           rbhome = (player %in% c(o1, o2, o3, o4, o5)), hometeam == thatteam,
           teamrbhome = rbname == homelong,
           isoreb = pmap_lgl(list(rbhome, teamrbhome, shooterhome),
                             function(a, b, c) {(a||b) && c }))
  
  
  bb <- smiss %>% left_join(teamtbl) %>% 
    filter(event_type != "rebound", !is.na(player)) %>% 
    select(event_type, description, shooterhome, o1, o2, o3, o4, o5,
           player, hometeam, shooterhome, mplay_id, old_id) %>% 
    separate(description, into = "rbname", sep = " ", extra = "drop") %>% 
    mutate(homelong = map_chr(hometeam, city_to_team),
           rbhome = (player %in% c(o1, o2, o3, o4, o5)), hometeam == thatteam,
           teamrbhome = rbname == homelong,
           isoreb = pmap_lgl(list(rbhome, teamrbhome, shooterhome),
                             function(a, b, c) {(a||b) && c }))
  
  
  
  
  teamtbl %>% left_join(
    (bind_rows(ab, bb) %>% select(isoreb, old_id) %>% 
       rename(mplay_id = old_id))
  )
}


# These two functions break the data table up by possessions
possession_change <- function(tb){
  # This function identifies whether a particular event resulted in a change-of-possession
  
  tb %>% 
    mutate(possessionchange = map2_lgl(event_type, isoreb, 
                                       function(a, b) {a == "turnover" |
                                           a == "shot" |
                                           !b}))
}

pos_count <- function(rb){
  
  possessioncount <- rep(0, nrow(rb))
  
  counter <- 1
  for(i in 1:nrow(rb)){
    if(is.na(rb$possessionchange[i])) counter <- counter else
      if(rb$possessionchange[i]) counter <- counter + 1 
      possessioncount[i] <- counter
  }
  
  rb$possessioncount <- possessioncount
  rb
  
}
get_team_players <- function(tb){
  
  tb %>%
    select(p1, p2, p3, p4, p5) %>%
    gather(key = "slot", value = "player", p1, p2, p3, p4, p5) %>%
    select(-slot) %>%
    distinct()
}

