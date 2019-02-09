library(tidyverse)

tb <- read_csv("data/sample-combined-pbp-stats.csv")

names(tb)

teamsandplayers <- tb %>% 
  filter(elapsed > 0) %>% #eliminates weird cases like opening jump ball
  select(player, team) %>% 
  filter(!is.na(player)) %>% 
  distinct() 



##Individual player plus-minus

playername <- "Kevin Love"
player_in_game <- function(playername, ...){
  playername %in%  c(...)
  
}

score_changed <- function(points){
  
  !is.na(points) && points > 0
}

get_net_points <- function(points, histeam, team){
  if(histeam == team)
    return(points) else
      return(points * -1)
}

get_plus_minus <- function(playername, playbyplay){

  whatteam <- filter(teamsandplayers, player == playername)$team
  playbyplay %>% 
    mutate(ishein = pmap_lgl(list(playername, a1, a2, a3, a4, a5, h1, h2, h3, h4, h5), player_in_game),
           pointchange = map_lgl(points, score_changed),
           histeam = whatteam) %>% 
    filter(ishein, pointchange) %>% 
    mutate(netpoints = pmap_dbl(list(points, histeam, team), get_net_points)) %>% 
    summarise(`plus-minus` = sum(netpoints))
  
}

get_plus_minus("LeBron James", tb)
get_plus_minus("Kyrie Irving", tb)
get_plus_minus("Kevin Love", tb)

# teamsandplayers %>% 
#   mutate(`plus-minus` = map2_dbl(player, list(tb), get_plus_minus))

pm <- rep(0, nrow(teamsandplayers))
for(i in 1:nrow(teamsandplayers)){
  
 pm[i] <- get_plus_minus(teamsandplayers$player[i], tb)[[1]]
 
}

teamsandplayers[["plus-minus"]] <- pm
