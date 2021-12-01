
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
  arrange(playerID, yearID) %>%
  mutate(
    Points_last_season = slide(.x = Points, .f = ~ .x, .before = 1),
    Points_2_season = slide(.x = Points, .f = ~ .x, .before = 2),
    Points_3_season = slide(.x = Points, .f = ~ .x, .before = 3)
  ) 

Extract_Value <- function(Points_column, obs_number, index) {
  
  lagged_points <- df_filtered %>% 
    pull(Points_column)
    
  row_of_interest <- lagged_points %>% 
    pluck(obs_number)
  
  row_of_interest[index]
  
}


last_season_points <- map_dbl(1:nrow(df_filtered), ~ Extract_Value("Points_last_season", .x, 1))
last_2_season_points <- map_dbl(1:nrow(df_filtered), ~ Extract_Value("Points_2_season", .x, 1))
last_3_season_points <- map_dbl(1:nrow(df_filtered), ~ Extract_Value("Points_3_season", .x, 1))

df_filtered$Points_last_season <- last_season_points
df_filtered$Points_2_season <- last_2_season_points
df_filtered$Points_3_season <- last_3_season_points

df_filtered <- df_filtered %>% 
  mutate(
   season_count = row_number(),
   Points_last_season = if_else(season_count == 1, 0, Points_last_season),
   Points_2_season = if_else(season_count %in% 1:2, 0, Points_2_season),
   Points_3_season = if_else(season_count %in% 1:3, 0, Points_3_season)
  ) %>% 
  select(-season_count) %>% 
  ungroup()

df_filtered %>% 
  filter(Era == "Long Ball") %>% 
  pull(yearID) %>% 
  table()


# Train/Val/Test -----------------------------------------------------------

TRAIN_YEARS <- 1961:2010
VALIDATION_YEARS <- 2011:2016
TEST_YEARS <- 2017:2019

TRAINING_RANGE <- paste(TRAIN_YEARS[1], TRAIN_YEARS[(length(TRAIN_YEARS))], sep = "_to_")
VALIDATION_RANGE <- paste(VALIDATION_YEARS[1], VALIDATION_YEARS[(length(VALIDATION_YEARS))], sep = "_to_")
TEST_RANGE <- paste(TEST_YEARS[1], TEST_YEARS[(length(TEST_YEARS))], sep = "_to_")

train_df <- df_filtered %>% 
  filter(yearID %in% TRAIN_YEARS)

validation_df <- df_filtered %>% 
  filter(yearID %in% VALIDATION_YEARS)

test_df <- df_filtered %>% 
  filter(yearID %in% TEST_YEARS)

Perform_Variable_Min_Max_Normalization <- function(training_dataset, dataset_to_normalize, variable) {
  
  training_var <- training_dataset %>% 
    pull(variable)
  
  training_min_val <- min(training_var, na.rm = T)
  training_max_val <- max(training_var, na.rm = T)
  
  var_of_interest <- dataset_to_normalize %>% 
    pull(variable)
  
  (scaled_var <- (var_of_interest - training_min_val) / (training_max_val - training_min_val))
  
}

vars_to_normalize <- c("birthYear", "weight", "height", "Age")


# Training ----------------------------------------------------------------

training_normalized_vars <- map_dfc(vars_to_normalize, 
                                    ~ Perform_Variable_Min_Max_Normalization(train_df, train_df, .)) %>% 
  set_names(., vars_to_normalize)

training_normalized <- train_df %>% 
  select(-vars_to_normalize) %>% 
  bind_cols(., training_normalized_vars)


# Validation --------------------------------------------------------------

validation_normalized_vars <- map_dfc(vars_to_normalize, 
                                    ~ Perform_Variable_Min_Max_Normalization(train_df, validation_df, .)) %>% 
  set_names(., vars_to_normalize)

validation_normalized <- validation_df %>% 
  select(-vars_to_normalize) %>% 
  bind_cols(., validation_normalized_vars)

# Test --------------------------------------------------------------

test_normalized_vars <- map_dfc(vars_to_normalize, 
                                      ~ Perform_Variable_Min_Max_Normalization(train_df, test_df, .)) %>% 
  set_names(., vars_to_normalize)

test_normalized <- test_df %>% 
  select(-vars_to_normalize) %>% 
  bind_cols(., test_normalized_vars)


# Saving ------------------------------------------------------------------

write_csv(training_normalized, str_glue("data/intermediate/training_normalized_{TRAINING_RANGE}.csv"))
write_csv(validation_normalized, str_glue("data/intermediate/validation_normalized_{VALIDATION_RANGE}.csv"))
write_csv(test_normalized, str_glue("data/intermediate/test_normalized_{TEST_RANGE}.csv"))



