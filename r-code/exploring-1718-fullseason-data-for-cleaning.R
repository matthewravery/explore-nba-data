## Now that we're working with full-season data, I'll need to do some additional data cleaning...
library(tidyverse)
tbraw <- read_csv("data/2017-2018_NBA_PbP_Logs/[10-17-2017]-[06-08-2018]-combined-stats.csv")


# Three things jump out to me:  The first is that the data has remarkably few problems going in.
# The second is that the game_id's are flubbed, and I"m not sure why. We can fix that easily enough:


tbraw %>% 
  mutate(game_id = str_extract(game_id, "\\d{10}")) %>% head

# The third thing is all of the "problems"

problems(tbraw)


## Extra columns 
# Going back to point 1, 676 "problem" rows out of a total of nearly 600k is not very bad at all. But we still need to address them. It looks like there are a couple categories of problems. The easiest to deal with might be the rows where there are two extra columns. It looks like these are all consecutive, so I'm going to go out on a limb and guess that there's a single game that, for whatever reason, we read in as having two extra rows. Let's start by looking at that game:

tbcols <- tbraw %>% slice(
  (problems(tbraw) %>% filter(actual == "46 columns") %>% select(row) %>% unique)$row
) 

tbcols %>% select(game_id) %>% unique


#yeah, definitely just one single game.  Visual inspection doesn't indicate that anything's missing, so we might be able to ignore these "problems" and move on, but let's look at one more thing before we do so:

con <- file("data/2017-2018_NBA_PbP_Logs/[10-17-2017]-[06-08-2018]-combined-stats.csv", "r")
ab <- read.csv(con, header = F,
               skip = min((problems(tbraw) %>% filter(actual == "46 columns") %>% select(row) %>% unique)$row),
               nrow = nrow(tbcols)) %>% 
  as_tibble()
close(con)
ab

# What this does is read in just the rows that we identified earlier as having two extra columns. I had to use `read.csv`` instead of `read_csv` because, as far as I could tell, you can't just read in a slice of the data set with the latter. Please correct me if I'm wrong.

# Either way, I think this gets us to the right place. So let's look at what these two extra columns are!

ab %>% select(V45, V46)

#As we suspected, they're just empty columns that got included in extraneously for some reason. Probably just an error when the data set was being built. No big deal. At this point, I fee safe ignoring the parsing problems associated with this game file, reducing our list greatly!

## Other problems
#We're left with 190 "problems", each of which is related to an "unknown" entry type being located where a "double" was expected:

pr <- problems(tbraw) %>% filter(actual != "46 columns")
pr

#To make things easier, we can pick those rows out of our data set and look at them sepcifically:

ap <- tbraw %>% slice(
  (pr %>% select(row) %>% unique)$row
)
ap

#Conveniently, it looks like these problems occur in a few specific columns:
pr %>% select(col) %>% unique 

#These all relate to where specific shots were taken on the court. Additionally, they come from only three games:

ap %>% select(game_id) %>% unique

# We can look at these games individually, and we see that these values are coded as "unknown" in the raw files. This indicates to me that there was an error with the tracking software or something to that effect. For now, we can safely ignroe these problems. If we ever want to do anything with shot quality or shot locations, it might be worth asking how these missing data points effect our evaluation, but for now, I'm fine ignoring them.

## Data cleaning wrap up

# So after all of that, it looks like `read_csv` did a fine job of parsing our data, and we don't have to take any specific actions to resolve the problems it tagged! Even though we didn't end up doing anything to our data set, it's good to know that we've discovered exactly what abnormalities exist with our data set. We may eventually need to consider the implications of the missing shot location data if we ever get to the point that we want to incoporate shot qualtiy into our DIY statistics, but for now, we're safe to move on!

# Finalizing a cleaned data file

#The last thing we have to do is run the code we'd written to clean the original sample data on our full-season data set:

teamsandplayers <- tbraw %>% 
  filter(elapsed > 0) %>% #eliminates weird cases like opening jump ball
  select(player, team) %>% 
  filter(!is.na(player)) %>% 
  distinct() 

tb <- tbraw %>% 
  mutate(hometeam = get_team(h1, teamsandplayers),
         awayteam = get_team(a1, teamsandplayers),
         pointchange = map_lgl(points, score_changed)) 


allteams <- tb %>% 
  filter(elapsed > 0) %>% #eliminates weird cases like opening jump ball
  select(team) %>% 
  filter(!is.na(team)) %>% 
  distinct() 
tmp <- left_join(allteams, get_team_events(allteams, tb))

#And mirical of miricals, it looks like its worked! We save the cleaned data file and can now run our existing DIY stats code with a full expectation that it'll work properly!