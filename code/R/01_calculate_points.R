
pacman::p_load(tidyverse,
               Lahman)

batting_with_points <- Batting %>% 
  group_by(playerID, yearID) %>% 
  transmute(
    playerID,
    yearID,
    G,
    AB,
    Hits = H,
    Runs_Scored = R,
    Singles = H - HR - X3B - X2B,
    Doubles = X2B,
    Triples = X3B,
    HR,
    TB = (Singles * 1) + (Doubles * 2) + (Triples * 3) + (HR * 4),
    RBI,
    BB,
    K = SO,
    HBP,
    Sacrifices = SF + SH,
    SB,
    CS,
    Points = Hits + Runs_Scored + Singles + (2 * Doubles) + (3 * Triples) + (4 * HR) +
             (.5 * TB) + RBI + (1.5 * BB) + (-1 * K) + HBP + (.5 * Sacrifices) + (2 * SB) +
             (-1 * CS)
  ) %>% 
  left_join(., Master %>% select(playerID, nameFirst, nameLast), by = "playerID") %>% 
  select(playerID, nameFirst, nameLast, everything()) %>% 
  group_by(playerID, yearID) %>% 
  mutate(
    across(G:Points, ~ sum(., na.rm = T))
  ) %>% 
  distinct() %>% 
  ungroup() %>% 
  glimpse()

write_csv(Batting, "data/raw/Batting.csv")
write_csv(Master, "data/raw/Master.csv")
write_csv(batting_with_points, "data/intermediate/batting_with_points_calculated.csv")  
  
  
  
  
  