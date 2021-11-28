
pacman::p_load(tidyverse,
               slider)

df <- read_csv("data/intermediate/batting_with_points_calculated.csv") %>% 
  glimpse()

df_2000_onward <- df %>% 
  filter(yearID >= 2000) %>% 
  mutate(
    year = as.factor(yearID)
  ) %>% 
  glimpse()

df_2000_onward %>% 
  summarize(
    across(everything(), ~ sum(is.na(.)))
  ) %>% 
  glimpse()

# Distributions for Each Statistic ----------------------------------------

p_points <- df_2000_onward %>% 
  ggplot(data = ., aes(x = Points, color = factor(yearID))) +
  geom_density() +
  theme_minimal() +
  labs(title = "Fantasy Points Distributions\n2000-2020")

Plot_Statistic_Distribution <- function(dataset, statistic, save = FALSE) {

  p <- ggplot(data = dataset, aes_string(x = statistic, color = "year")) +
    geom_density() +
    theme_minimal() +
    labs(title = str_glue("{statistic} Distributions\n2000-2020"))
  
  if (save) {
    
    file_name = str_glue("output/plots/EDA/2000-2019_{statistic}_distribution.png")
    ggsave(plot = p, filename = file_name)
    
  } else {
    
    p
  }
  
}

stats_to_plot <- df_2000_onward %>% 
  select(Hits:CS) %>% 
  names()

ggplot(data = df, aes(x = Points)) +
  geom_density() +
  facet_wrap(~ Era, scales = "free_x") +
  theme_bw()

ggplot(data = df, aes(x = Era, y = Points)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = df, aes(x = birthCountry, y = Points)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip()

ggplot(data = df, aes(x = birthState, y = Points)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip()

ggplot(data = df, aes(x = factor(birthMonth), y = Points)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~ Era) +
  coord_flip()

ggplot(data = df, aes(x = weight, y = Points)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ Era)

ggplot(data = df, aes(x = height, y = Points)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~ Era)

ggplot(data = df, aes(x = bats, y = Points)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = df, aes(x = throws, y = Points)) +
  geom_boxplot() +
  theme_minimal()

ggplot(data = df, aes(x = Age, y = Points)) +
  geom_point(alpha = .2) +
  theme_minimal()

ggplot(data = df, aes(x = season_number, y = Points)) +
  geom_point(alpha = .2) +
  theme_minimal()

# Summary Stats by Statistic ----------------------------------------------

sum_table <- df_2000_onward %>% 
  group_by(year) %>% 
  summarize(
    across(Hits:Points, .fns = list(mean = mean,
                                    median = median,
                                    sd = sd,
                                    total = sum))
  ) %>% 
  select(year, ends_with("total")) %>% 
  pivot_longer(., cols = ends_with("total"), names_to = "Statistic", values_to = "Total") %>% 
  mutate(
    Statistic = str_remove(Statistic, "_total")
  ) %>% 
  glimpse()

p_totals <- ggplot(data = sum_table, aes(x = year, y = Total)) +
  geom_col(fill = "blue") +
  facet_wrap(~ Statistic, scales = "free_y") +
  labs(x = "Year",
       y = "Statistic Total",
       title = "Statistic Totals\n2000-2020") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = -.005))


# Player Auto Correlation -------------------------------------------------

yearly_data <- df_2000_onward %>% 
  select(playerID:yearID, Points) %>% 
  pivot_wider(., names_from = yearID, values_from = Points) %>% 
  glimpse()

Plot_Points_2_Years <- function(plotting_data, x_year, y_year, save = FALSE) {
  
  x_column <- paste0("`", x_year, "`")
  y_column <- paste0("`", y_year, "`")
  
  correlation <- cor(plotting_data %>% pull(x_year),
                     plotting_data %>% pull(y_year),
                     use = "complete.obs") %>% 
    round(., 3)
  
  p <- ggplot(data = plotting_data, aes_string(x = x_column, y = y_column)) +
    geom_point() +
    theme_minimal() +
    labs(x = paste("Points in", x_year),
         y = paste("Points in", y_year),
         title = str_glue("{x_year} & {y_year} Points\nEach Point = 1 Player\nCorrelation = {correlation}"))
  
  if (save) {
    
    file_name = str_glue("output/plots/EDA/points_correlation_plot_{x_year}_and_{y_year}.png")
    ggsave(plot = p, filename = file_name)
    
  } else {
    
    p
  }
  
}

x_axis_years <- paste(2000:2019)
y_axis_years <- paste(2001:2020)

Calculate_Correlations <- function(dataset, first_year, second_year) {

  cor(dataset[first_year],
      dataset[second_year],
      use = "complete")[1]
}

lags_5_x <- paste0(2000:2015)
lags_5_y <- paste0(2005:2020)

lags_6_x <- paste0(2000:2014)
lags_6_y <- paste0(2006:2020)

lags_7_x <- paste0(2000:2013)
lags_7_y <- paste0(2007:2020)

lags_8_x <- paste0(2000:2012)
lags_8_y <- paste0(2008:2020)

lags_9_x <- paste0(2000:2011)
lags_9_y <- paste0(2009:2020)

lags_10_x <- paste0(2000:2010)
lags_10_y <- paste0(2010:2020)

lags_5 <- map2_dbl(lags_5_x, lags_5_y, ~ Calculate_Correlations(yearly_data, .x, .y)) %>% mean()
lags_6 <- map2_dbl(lags_6_x, lags_6_y, ~ Calculate_Correlations(yearly_data, .x, .y)) %>% mean()
lags_7 <- map2_dbl(lags_7_x, lags_7_y, ~ Calculate_Correlations(yearly_data, .x, .y)) %>% mean()
lags_8 <- map2_dbl(lags_8_x, lags_8_y, ~ Calculate_Correlations(yearly_data, .x, .y)) %>% mean()
lags_9 <- map2_dbl(lags_9_x, lags_9_y, ~ Calculate_Correlations(yearly_data, .x, .y)) %>% mean()
lags_10 <- map2_dbl(lags_10_x, lags_10_y, ~ Calculate_Correlations(yearly_data, .x, .y)) %>% mean()

tibble("Number of Lag Years" = 5:10,
       "Average Correlation" = c(lags_5, lags_6, lags_7, lags_8, lags_9, lags_10)
)


# Save Plots --------------------------------------------------------------

ggsave(plot = p_points, filename = "output/plots/EDA/2000-2020_points_distribution.png")

walk(stats_to_plot, ~ Plot_Statistic_Distribution(dataset = df_2000_onward, statistic = .x, save = TRUE))

ggsave(plot = p_totals, filename = "output/plots/EDA/2000-2020_statistic_totals_barplots.png")

walk2(x_axis_years, y_axis_years, ~ Plot_Points_2_Years(yearly_data, .x, .y, TRUE))
