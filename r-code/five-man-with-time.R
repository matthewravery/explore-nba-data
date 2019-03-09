library(lubridate)
library(tidyverse)

get_fiveman_plusminus <- function(tb, team){
  
  tb %>% 
    mutate(team = team) %>% 
    mutate(pl = hms(play_length)) %>% 
    group_by(p1, p2, p3, p4, p5) %>% 
    summarise(`Plus/Minus` = sum(netpoints, na.rm = T),
              unittime = sum(pl)) 
  
}


tmp <- read_rds("clean-data/team-events.rds")

tmp <- tmp %>%
  mutate(`Five-man Plus/Minus` = map2(`team events`, team, get_fiveman_plusminus))


# Here, we verify that we can take this table and get our desired five-man plus-minus results

tmp %>% filter(team == "BOS") %>% select(`Five-man Plus/Minus`) %>% unnest()



#exploratory stuff and getting the right numbers for time elapsed with each unit

# 
# tmp %>% filter(team == "BOS") %>% unnest() %>% select(subsec, elapsed) %>% summarise(sum(elapsed))
# #This is too much time!
# 
# 
# ab <- tmp %>% filter(team == "BOS") %>% unnest() 
# 
# names(ab)
# ab %>% 
#   filter(!(event_type == "start of period")) %>% 
#   select(subsec, elapsed, remaining_time, play_length) %>% 
#   mutate(pl = hms(play_length)) %>% summarise(sum(pl))
# 
# 
# a <- tmp %>% mutate(`Five-man Plus/Minus` = map2(`team events`, team, get_fiveman_plusminus)) 
# 
# a %>% filter()