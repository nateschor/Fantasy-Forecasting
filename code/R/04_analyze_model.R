
pacman::p_load(tidyverse)

df <- read_csv("data/final/validation_fitted_fixed.csv") %>% 
  mutate(
    resid = Points - pred_points
  ) %>% 
  glimpse()

points <- df %>% 
  select(playerID, yearID, Points, pred_points) %>% 
  pivot_longer(cols = Points:pred_points, names_to = "Predicted or True", values_to = "Points")

ggplot(data = points, aes(x = Points, fill = `Predicted or True`)) +
  geom_density(alpha = .5) +
  labs(title = "Density Plot of Predicted and True Points\nValidation Set: 2011-2016") +
  theme_minimal()

ggplot(data = points, aes(x = Points, fill = `Predicted or True`)) +
  geom_density(alpha = .5) +
  facet_wrap(~ yearID) +
  labs(title = "Density Plot of Predicted and True Points\nValidation Set: 2011-2016") +
  theme_minimal()

correlation <- cor(df$Points, df$pred_points) %>% round(., 3)

ggplot(data = df, aes(x = Points, y = pred_points)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  geom_smooth(se = FALSE, color = "green") +
  labs(x = "True Points",
       y = "Predicted Points",
       title = str_glue("Validation (2011-2016) Predicted Points\nCorrelation: {correlation}"),
       caption = "45 Degree Line in Red\nLOESS Smoother in Green") +
  theme_minimal()


correlation <- cor(df %>% filter(Points >= 500) %>% pull(Points), 
                   df %>% filter(Points >= 500) %>% pull(pred_points)) %>% round(., 3)

ggplot(data = df %>% filter(Points >= 500), aes(x = Points, y = pred_points)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  geom_smooth(se = FALSE, color = "green") +
  labs(x = "True Points",
       y = "Predicted Points",
       title = str_glue("Validation (2011-2016) Predicted Points\nCorrelation: {correlation}"),
       caption = "45 Degree Line in Red\nLOESS Smoother in Green") +
  scale_y_continuous(limits = c(500, 1000)) +
  theme_minimal()

correlation <- cor(df %>% filter(Points <= 500) %>% pull(Points), 
                   df %>% filter(Points <= 500) %>% pull(pred_points)) %>% round(., 3)

ggplot(data = df %>% filter(Points <= 500), aes(x = Points, y = pred_points)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  geom_smooth(se = FALSE, color = "green") +
  labs(x = "True Points",
       y = "Predicted Points",
       title = str_glue("Validation (2011-2016) Predicted Points\nCorrelation: {correlation}"),
       caption = "45 Degree Line in Red\nLOESS Smoother in Green") +
  theme_minimal()

ggplot(df, aes(x = Points, y = resid, color = factor(yearID))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", size = 1.5) +
  labs(y = "Residual",
       title = "Validation Set (2011-2016) Residual Plot") +
  theme_minimal()

ggplot(df, aes(x = Points, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", size = 1.5) +
  labs(y = "Residual",
       title = "Validation Set (2011-2016) Residual Plot") +
  theme_minimal()

ggplot(data = df, aes(x = Points, y = pred_points)) +
  geom_point() +
  geom_vline(xintercept = 500, color = "red", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 500, color = "red", linetype = "dashed", size = 1.5) +
  labs(x = "True Points",
       y = "Predicted Points",
       title = "Validation (2011-2016) Predicted Points",
       caption = "Upper Right and Bottom Left Quadrants Desirable\nUpper Left and Bottom Right Quadrants Undesirable") +
  theme_minimal()


