library(tidyverse)

tbraw <- read_csv("data/sample-combined-pbp-stats.csv") 


names(tbraw)
# so just for starters, it's real dumb that this data set doesn't have "home team" and "away team" defined as columns

teamsandplayers <- tbraw %>% 
  filter(elapsed > 0) %>% #eliminates weird cases like opening jump ball
  select(player, team) %>% 
  filter(!is.na(player)) %>% 
  distinct() 

get_team <- function(playernames, teamsandplayers){
  
  teamsandplayers$team[match(playernames, teamsandplayers$player)]
  
}

tb <- tbraw %>% 
  mutate(hometeam = get_team(h1, teamsandplayers),
         awayteam = get_team(a1, teamsandplayers)) 

## Data re-org
## Goal:  For each team, generate event list from that team's perspective

# To start, do it for a single team

score_changed <- function(points){
  
  !is.na(points) && points > 0
}

get_net_points <- function(points, histeam, team){
  if(histeam == team)
    return(points) else
      return(points * -1)
}

get_team_scores <- function(whichteam, tb ){
  
  out <- NULL
  for(i in seq_along(whichteam$team)){
    thatteam <- whichteam$team[i]
    teamtbl <- tb %>% 
      mutate(currentteam = thatteam,
             pointchange = map_lgl(points, score_changed)) %>% 
      filter(pointchange,
             hometeam == currentteam | awayteam == currentteam) %>% 
      mutate(netpoints = pmap_dbl(list(points, currentteam, team), get_net_points)) %>% 
      select(-team) %>% rename(team = currentteam)
    
    out <- bind_rows(out, teamtbl)
    
  }
  out %>% group_by(team) %>% nest(.key = `team events`)
}


allteams <- teamsandplayers %>% select(team) %>% distinct %>% arrange()


tmp <- teamsandplayers %>% 
  group_by(team) %>% 
  nest(.key = "playerlist") %>% 
  left_join(get_team_scores(allteams, tb))

## Five-man units for a given team

# Since we're looking at a single team's perspective, the "home team" and "away team" denotations aren't very useful for us right now, so we'll re-map them

fiveplayers <- function(tb){
  
  tb1 <- tb %>% filter(team == hometeam) %>% 
    rename(p1 = h1, p2 = h2, p3 = h3, p4 = h4, p5 = h5, o1 = a1, o2 = a2, o3 = a3, o4 = a4, o5 = a5)
  
  tb2 <- tb %>% filter(team == awayteam) %>% 
    rename(p1 = a1, p2 = a2, p3 = a3, p4 = a4, p5 = a5, o1 = h1, o2 = h2, o3 = h3, o4 = h4, o5 = h5)
  
  bind_rows(tb1, tb2)
  
}

#Illustrating the concept with a single team
bv <- tmp %>% filter(team == "CLE") %>% 
  select(`team events`, team) 

bv %>% 
  unnest() %>% 
  fiveplayers() %>% 
  group_by(p1, p2, p3, p4, p5) %>% 
  summarise(`Plus/Minus` = sum(netpoints)) 

#It works! Time to write a function and use purrrr

get_fiveman_plusminus <- function(tb, team){
  
  tb %>% 
    mutate(team = team) %>% 
    fiveplayers() %>% 
    group_by(p1, p2, p3, p4, p5) %>% 
    summarise(`Plus/Minus` = sum(netpoints)) 
  
}

#Using map2, we can apply this function across each team using listcolumns and generate a new listcolumn that has the plus-minus for each five-man unit!

fm <- tmp %>% 
  mutate(`Five-man Plus/Minus` = map2(`team events`, team, get_fiveman_plusminus)) 

#To show that this works, we can once again grab Boston and unnest. We'll sort them from best to worst. 
fm %>% 
  filter(team == "BOS") %>% 
  select(`Five-man Plus/Minus`) %>% unnest() %>% arrange(desc(`Plus/Minus`))

#Next thing to do is figure out how to include time on court!
