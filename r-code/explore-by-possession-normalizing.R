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

fl <- list.files("data/2017-2018_NBA_PbP_Logs")[-1] %>% 
  str_split("-", 5) %>% unlist() %>%  matrix(ncol = 5, byrow = T) %>% 
  as_tibble() %>% 
  select(V4, V5) %>% rename(game_id = V4, otherinfo = V5) %>% 
  mutate(game_id = as.double(game_id)) %>% 
  separate(otherinfo, into = c("awayteam", "other"), sep = "@") %>% 
  separate(other, into = "hometeam", sep = ".csv", extra = "drop")
  
tb <- tbraw %>% 
  left_join(fl) %>% 
  mutate(pointchange = map_lgl(points, score_changed)) 


# (1) attempts a field goal,
# (2) misses a shot and does not get the offensive rebound,
# (3) turns the ball over (some sources add “turnovers that are assigned to teams” for a more precise possession calculation),
# (4) goes to the line for two or three shots and either makes the last shot or does not get the rebound of a missed last shot.


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

subsec <- rep(0, nrow(tb))

counter <- 1
for(i in 1:nrow(teamtbl)){
  if(!is.na(teamtbl$entered[i]) && teamtbl$entered[i] %in% thisteamsplayers$player) counter <- counter + 1
  subsec[i] <- counter
}

teamtbl$subsec <- subsec
teamtbl



thatteam <- "BOS"
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
  id_orebs(thatteam)

thisteamsplayers <- get_this_teams_players(teamtbl, thatteam)

# 
# 
# teamtbl
# 
# teamtbl %>% filter(type == "team rebound") %>% select(currentteam, description)
# 
# # teamabrv <- read_csv("data/games-by-home-and-away-teams.csv") %>% 
# #   select(hometeam) %>% distinct() %>% rename(team3 = hometeam) %>% arrange(team3)
# # 
# # teamabrv


## free throw events

teamtbl %>% filter(event_type == "free throw") %>% select(type) %>% distinct
# Of these, we only care about the ones where posession could change after, meaning flagrants clear path, and technicals are out, as are others which aren't the "last" shot

teamtbl %>% 
  filter(event_type == "free throw", type %in% c("Free Throw 1 of 1",
                                                 "Free Throw 2 of 2",
                                                 "Free Throw 3 of 3")) %>% 
  select(event_type, type, pointchange) 

#of these, we need to know if they made the shot
ftmiss <- teamtbl %>% 
  filter(event_type == "free throw", type %in% c("Free Throw 1 of 1",
                                                 "Free Throw 2 of 2",
                                                 "Free Throw 3 of 3")) %>% 
  mutate(miss = map_chr(description, str_match, "MISS"),
         shooterhome = player %in% c(p1, p2, p3, p4, p5)) %>% 
  filter(miss == "MISS") %>% 
  select(game_id, mplay_id, miss, shooterhome) %>% 
  mutate(mplay_id = mplay_id + 1)

ftmiss %>% left_join(teamtbl) %>% 
  select(event_type, description, shooterhome, p1, p2, p3, p4, p5, player, hometeam, shooterhome, mplay_id) 

#two weird issues remain:  One, sometimes the data frame has a substitute recorded after the 2nd ft instead of before it. we shoudl try to skip these and just get the subsequent play. Two, sometimes the rebound is omitted all together and there's just another play next. This we can basically ignore, assuming that the team that recovered the ball is indicated by the player shooting the next shot. 
ftmiss$mplay_id[left_join(ftmiss, teamtbl)$event_type == "sub"] <- 
  ftmiss$mplay_id[left_join(ftmiss, teamtbl)$event_type == "sub"] + 1

ab <- ftmiss %>% left_join(teamtbl) %>% 
  select(event_type, description, shooterhome, p1, p2, p3, p4, p5, player, hometeam, shooterhome, mplay_id) %>% 
  separate(description, into = "rbname", sep = " ", extra = "drop") %>% 
  mutate(homelong = map_chr(hometeam, city_to_team),
         rbhome = (player %in% c(p1, p2, p3, p4, p5) | rbname == homelong),
         isoreb = rbhome && shooterhome)

#So the plan is to take every potential event like this, look at the next play, and ask if it was an offensive rebound. if it's not, we'll assume it's a possession change. 

teamtbl %>% filter(event_type %in% c("miss", "rebound", "turnover", "free throw", "shot")) %>%  select(event_type, type) %>% distinct()



######################


teamtbl %>% id_orebs("BOS")





teamtbl %>% left_join(
  (bind_rows(ab, bb) %>% select(isoreb, old_id) %>% 
     rename(mplay_id = old_id))
)





















# out <- teamtbl %>% 
#   add_subsections(thisteamsplayers) %>%
#   mutate(netpoints = pmap_dbl(list(points, currentteam, team), get_net_points)) %>% 
#   select(-team) %>% rename(team = currentteam) %>% 
#   bind_rows(out)


# was there a shot without an offensive rebound?


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
             p5 = map_chr(fiveplayers, get_player, 5)) %>% select(-fiveplayers)
    
    
    thisteamsplayers <- get_this_teams_players(teamtbl, thatteam)
    
    out <- teamtbl %>% 
      add_subsections(thisteamsplayers) %>% 
      mutate(netpoints = pmap_dbl(list(points, currentteam, team), get_net_points)) %>% 
      select(-team) %>% rename(team = currentteam) %>% 
      bind_rows(out)
    
  }
  out %>% group_by(team) %>% nest(.key = `team events`)
  
}














teamtbl


