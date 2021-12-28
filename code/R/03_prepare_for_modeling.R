
pacman::p_load(tidyverse,
               slider)

df <- read_csv("data/intermediate/batting_with_points_calculated.csv") %>% 
  glimpse()

df_filtered <- df %>% 
  filter(Era %in% c("Expansion", "Free Agency", "Long Ball")) %>% 
  mutate(
    birthMonth = case_when(
      birthMonth == 1 ~ "Jan",
      birthMonth == 2 ~ "Feb",
      birthMonth == 3 ~ "Mar",
      birthMonth == 4 ~ "Apr",
      birthMonth == 5 ~ "May",
      birthMonth == 6 ~ "Jun",
      birthMonth == 7 ~ "Jul",
      birthMonth == 8 ~ "Aug",
      birthMonth == 9 ~ "Sep",
      birthMonth == 10 ~ "Oct",
      birthMonth == 11 ~ "Nov",
      birthMonth == 12 ~ "Dec",
    )
  )

df_filtered <- df_filtered %>% 
  group_by(playerID) %>% 
  arrange(playerID, yearID)

Create_Stat_Lag <- function(stat, year) {
  
  df_filtered %>% 
    mutate(lagged_val = slide(.x = .data[[stat]], .f = ~ .x, .before = year)
  ) %>% 
    pull("lagged_val") %>% 
    map_dbl(., ~ pluck(., 1))
}

vars_to_lag <- df_filtered %>% 
  ungroup() %>% 
  select(Hits:Points) %>% 
  names()

num_lags <- 1:5

lags_table <- expand_grid(vars_to_lag, num_lags) %>% 
  mutate(
    var_lags = paste(vars_to_lag, num_lags, sep = "_")
  )

lagged_vars <- map2_dfc(lags_table$vars_to_lag, lags_table$num_lags, function(x, y) {
  print(str_glue("{x} lagged {y}"))
  Create_Stat_Lag(x, y)
})

lagged_vars <- lagged_vars %>% 
  set_names(., lags_table$var_lags)

df_lagged <- bind_cols(df_filtered, lagged_vars) %>% 
  ungroup() %>% 
  group_by(playerID) %>% 
  arrange(playerID, yearID) %>% 
  mutate(
    season_number = row_number(yearID),
    across(contains("_1"), ~ if_else(season_number == 1, 0, .)),
    across(contains("_2"), ~ if_else(season_number %in% 1:2, 0, .)),
    across(contains("_3"), ~ if_else(season_number %in% 1:3, 0, .)),
    across(contains("_4"), ~ if_else(season_number %in% 1:4, 0, .)),
    across(contains("_5"), ~ if_else(season_number %in% 1:5, 0, .))
  ) %>% 
  ungroup()

write_csv(df_lagged, "data/intermediate/all_stats_5_year_lag.csv")

TRAIN_YEARS <- 1961:2010
VALIDATION_YEARS <- 2011:2016
TEST_YEARS <- 2017:2019





