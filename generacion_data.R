
library(tidyverse)
library(timetk)

series_raw <- read_csv("data/sales_train_evaluation.csv")
calendar <- read_csv("data/calendar.csv")

set.seed(123)
series_sample_4 <- series_raw$dept_id %>% unique() %>% sample(4)

series_data <- series_raw %>% 
  
  filter(dept_id %in% series_sample_4) %>% 
  
  pivot_longer(
    
    cols = starts_with("d_"),
    names_to = "day",
    values_to = "value",
    
  ) %>% 
  
  left_join(calendar, by =c("day" = "d")) %>% 
  
  select(dept_id, date, value) %>% 
  
  group_by(dept_id, date) %>% 
  
  summarise(value = sum(value))

  
skimr::skim(series_data)


series_data %>% 
  group_by(dept_id) %>%
  plot_anomaly_diagnostics(date, value)

series_data %>% 
  group_by(dept_id) %>% 
  plot_time_series(date,value,
                 .interactive = FALSE)


saveRDS(series_data, "data/series_data.rds")


