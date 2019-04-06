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


tmp <- read_rds("clean-data/team-events-1718.rds")

tmp <- tmp %>%
  mutate(`Five-man Plus/Minus` = map2(`team events`, team, get_fiveman_plusminus))


# Here, we verify that we can take this table and get our desired five-man plus-minus results

fpm <- tmp %>% filter(team == "SAS") %>% select(`Five-man Plus/Minus`) %>% unnest() %>%
  filter(unittime > 0) %>% 
  # group_by(p1, p2, p3, p4, p5) %>%
  # summarise(tpm = sum(`Plus/Minus`),
  #           ttime = sum(unittime)) %>%
  mutate(normpm = `Plus/Minus` * 60 * 48 / unittime)

fpm

fpm %>% 
  filter(unittime > 60 * 48) 

fpm %>% 
  filter(unittime > 60 * 48) %>% 
  ggplot(aes(x = normpm)) + geom_histogram(bins = 10)

#My biggest take-away here is that there just aren't many lineups with at least a full 48 minutes worth of data. At least not on the Spurs...

fpm <- tmp %>% select(team, `Five-man Plus/Minus`) %>% 
  unnest() %>% 
  filter(unittime > 0) %>% 
  mutate(normpm = `Plus/Minus` * 48 * 60 / unittime) %>% 
  filter(unittime > 60 * 48)


fpm %>% filter(normpm > -50) %>% 
  ggplot(aes(x = normpm)) + geom_histogram(bins = 10)


#defining a color scale for NBA teams
library(teamcolors)
colortib <- teamcolors %>% filter(league == "nba") %>% 
  bind_cols(tibble(team = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN",
                              "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA",
                              "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX",
                              "POR", "SAC", "SAS", "TOR", "UTA", "WAS"))) %>% 
  arrange(team)
nbacolors <- colortib$primary
names(nbacolors) <- colortib$team
nba_cols <- function(...){
  
  teamname <- c(...)
  
  if (is.null(teamname))
    return (nba_colors)
  
  nbacolors[teamname]
}

nba_pal <- function(whichteams = "all", ...){
  
  theseteams <- whichteams
  if(whichteams == "all") theseteams <- c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN",
                                          "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA",
                                          "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX",
                                          "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
  
  pal <- nba_cols(theseteams)
  
  colorRampPalette(pal, ...)
}


nba_pal()


scale_color_nba <- function(whichteams = "all", ...){
  
  pal <- nba_pal(whichteams = whichteams)
  
  discrete_scale("colour", paste0("nba_discrete_", whichteams), palette = pal, ...)
  
}

scale_fill_nba <- function(whichteams = "all", ...){
  
  pal <- nba_pal(whichteams = whichteams)
  
  discrete_scale("fill", paste0("nba_discrete_", whichteams), palette = pal, ...)
  
}


fpm %>% left_join(colortib) %>% 
  filter(normpm > -50) %>% 
  ggplot(aes(x = normpm, color = team, y = 1, size = (unittime/60))) + geom_jitter(width = 0) +
  scale_color_nba() + theme_bw() + ylab("") + theme(legend.position = "top") + xlab("Plus/Minus") +
  scale_y_continuous(labels = NULL) + guides(size = guide_legend(title = "Minutes Played")) +
  facet_wrap(~team)


tmp
