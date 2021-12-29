
pacman::p_load(tidyverse,
               tidymodels)

df <- read_csv("data/intermediate/all_stats_5_year_lag.csv")

df_modeling <- df %>% 
  select(playerID, nameFirst, nameLast, yearID, Points, contains("_")) %>% 
  select(-Runs_Scored)

TRAIN_YEARS <- 1961:2016
TEST_YEARS <- 2017:2019

df_training <- df_modeling %>% 
  filter(yearID %in% TRAIN_YEARS)

df_test <- df_modeling %>% 
  filter(yearID %in% TEST_YEARS)

model_formula <- df_training %>% 
  select(contains("_")) %>% 
  names() %>% 
  reformulate(., response = "Points")

set.seed(1234)

cv_data <- vfold_cv(df_training, v = 5)

xgboost_recipe <- 
  recipe(formula = model_formula, data = df_training) 

xgboost_spec <- 
  boost_tree(trees = 1000, min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune(), mtry = tune(), stop_iter = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

xgboost_grid <- 
  grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), cv_data),
    learn_rate(),
    stop_iter(),
    size = 15
  )

set.seed(5321)
xgboost_tune <-
  tune_grid(xgboost_workflow, 
            resamples = cv_data, 
            grid = xgboost_grid, 
            control = control_grid(save_pred = TRUE))

saveRDS(xgboost_tune, file = "data/xgb.rds")
ex <- readRDS("data/xgb.rds")
show_best(xgboost_tune, metric = "rmse")
