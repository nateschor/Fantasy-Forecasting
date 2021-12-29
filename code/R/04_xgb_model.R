
pacman::p_load(tidyverse,
               tidymodels,
               ggrepel)

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

# make sure players aren't getting their seasons split across folds

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
xgb_model <- readRDS("data/xgb.rds")

best_rmse <- select_best(xgb_model, metric = "rmse")

xgb_final_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune(),
  stop_iter = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost") 

xgb_final_wf <- workflow() %>% 
  add_formula(model_formula) %>% 
  add_model(xgb_final_spec)

final_xgb <- finalize_workflow(
  xgb_final_wf,
  best_rmse
)

fit_xgb <- final_xgb %>% 
  fit(data = df_training)

df_test_preds <- df_test %>% 
  mutate(
    fitted_val = predict(fit_xgb, df_test) %>% pull(.pred),
    residual = Points - fitted_val
  )

rmse_vec(df_test_preds$Points, df_test_preds$fitted_val)
# 115.6095

ggplot(data = df_test_preds, aes(x = Points, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal()

ggplot(data = df_test_preds, aes(x = Points, y = fitted_val)) +
  geom_point() +
  geom_vline(xintercept = 500, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 500, color = "red", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  geom_text_repel(data = df_test_preds %>% filter(Points >= 750, fitted_val < 500), aes(label = paste(nameFirst, nameLast, yearID))) +
  theme_minimal() +
  labs(y = "Fitted Value",
       title = "XGBoost",
       caption = "Trained on 1961-2016\nTested on 2017-2019")

ggplot(data = df_test_preds, aes(x = Points, y = fitted_val)) +
  geom_point() +
  geom_vline(xintercept = 500, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 500, color = "red", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  geom_text_repel(data = df_test_preds %>% filter(Points >= 750, fitted_val >= 750), aes(label = paste(nameFirst, nameLast, yearID))) +
  theme_minimal() +
  labs(y = "Fitted Value",
       title = "XGBoost",
       caption = "Trained on 1961-2016\nTested on 2017-2019")
