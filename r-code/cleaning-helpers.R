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
  
  bind_rows(tb1, tb2)
  
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

get_team_events <- function(whichteam, tb){
  
  out <- NULL
  for(i in seq_along(whichteam$team)){
    
    thatteam <- whichteam$team[i]
    
    teamtbl <- tb %>% 
      mutate(currentteam = thatteam) %>% 
      filter(hometeam == currentteam | awayteam == currentteam) %>% 
      fiveplayers()
    
    thisteamsplayers <- get_this_teams_players(teamtbl, thatteam)
    
    out <- teamtbl %>% 
      add_subsections(thisteamsplayers) %>% 
      mutate(netpoints = pmap_dbl(list(points, currentteam, team), get_net_points)) %>% 
      select(-team) %>% rename(team = currentteam) %>% 
      bind_rows(out)
    
  }
  out %>% group_by(team) %>% nest(.key = `team events`)
  
}
