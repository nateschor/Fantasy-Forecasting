
pacman::p_load(tidyverse,
               slider,
               Lahman)

batting_with_points <- Batting %>% 
  group_by(playerID, yearID) %>% 
  transmute(
    playerID,
    yearID,
    teamID,
    Era = cut(yearID, 
              breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2100),
              labels = c("19th Century", "Dead Ball", "Lively Ball", "Integration", "Expansion", "Free Agency", "Long Ball")),
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
  left_join(., Master %>% select(playerID, nameFirst, nameLast, birthYear, birthMonth, birthCountry, birthState, birthCity, weight, height, bats, throws), by = "playerID") %>% 
  select(playerID, nameFirst, nameLast, everything()) %>% 
  group_by(playerID, yearID) %>% 
  mutate(
    Age = yearID - birthYear,
    across(G:Points, ~ sum(., na.rm = T)),
  ) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(playerID) %>% 
  arrange(yearID) %>% 
  mutate(
    season_number = row_number(yearID)
  ) %>% 
  glimpse()

write_csv(Batting, "data/raw/Batting.csv")
write_csv(Master, "data/raw/Master.csv")
write_csv(batting_with_points, "data/intermediate/batting_with_points_calculated.csv")  
  
