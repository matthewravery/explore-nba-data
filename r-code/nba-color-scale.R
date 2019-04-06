#nba color scale

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


# nba_pal()


scale_color_nba <- function(whichteams = "all", ...){
  
  pal <- nba_pal(whichteams = whichteams)
  
  discrete_scale("colour", paste0("nba_discrete_", whichteams), palette = pal, ...)
  
}

scale_fill_nba <- function(whichteams = "all", ...){
  
  pal <- nba_pal(whichteams = whichteams)
  
  discrete_scale("fill", paste0("nba_discrete_", whichteams), palette = pal, ...)
  
}