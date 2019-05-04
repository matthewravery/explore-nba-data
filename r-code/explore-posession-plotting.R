tmp <- read_rds("clean-data/team-events-1718.rds")

tmp

bm <- tmp %>% filter(team == "BOS") %>% unnest()

bm %>% 
  group_by(o1, o2, o3, o4, o5, possessioncount) %>% 
  summarise(pts = sum(netpoints, na.rm = T))


bout <- bm %>% 
  group_by(p1, p2, p3, p4, p5) %>% 
  summarise(pts = sum(netpoints, na.rm = T),
            npos = length(unique(possessioncount)),
            netrtg = pts / npos * 100) 

bout %>% 
  filter(npos > 10) %>% 
  ggplot(aes((npos))) + geom_histogram( ) + scale_x_log10() + theme_bw() + xlab("Number of Posessions")


bout %>% 
  filter(npos > 100) %>% 
  ggplot(aes(x = netrtg, y = 1, size = npos)) + 
  geom_jitter(width = 0) + theme_bw() + ylab("") + 
  theme(legend.position = "top") + xlab("Plus/Minus") +
  scale_y_continuous(labels = NULL) + 
  guides(size = guide_legend(title = "Minutes Played")) +
  facet_wrap(~team)
  
         