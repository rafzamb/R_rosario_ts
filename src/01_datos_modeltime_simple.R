options(scipen=999)

# Librerías ---------------------------------------------------------------
library(sknifedatar)
library(modeltime)
library(workflowsets)
library(tidymodels)
library(tidyverse)
library(timetk)
library(anomalize) 
library(lubridate)

library(kable)

# Datos -------------------------------------------------------------------
df <- readRDS('data/series_data.rds')

# Plot time series --------------------------------------------------------
df %>% 
  group_by(dept_id) %>% 
  plot_time_series(date,value,
                   .interactive = FALSE, 
                   .facet_ncol = 2)


# Descomposición ----------------------------------------------------------
nest_data <- df %>% 
  filter(date>as.Date('2016-01-01')) %>% 
  nest(nested_column = -dept_id)
nest_plots <- nest_data %>%
  mutate(ts_plots = map(nested_column, 
                        ~  plot_stl_diagnostics(.x, 
                                                .date_var=date, 
                                                .value=value, 
                                                .frequency = 'auto',
                                                .trend = 'auto', 
                                                .interactive=FALSE,
                                                .feature_set = c("observed", "season", "trend", "remainder"))
  ))
nest_plots$ts_plots[[1]]

# Anomalías ---------------------------------------------------------------
df %>% 
  group_by(dept_id) %>%
  plot_anomaly_diagnostics(date, value, .facet_ncol = 2, .interactive=FALSE)

# Caso individual ---------------------------------------------------------
df_hobbies <- df %>% 
  filter(dept_id == 'HOBBIES_2')

# 1. Clean anomalies ------------------------------------------------------
df_hobbies_clean <- df_hobbies %>%
  
  # 1. Decompose download counts and anomalize the STL decomposition remainder
  time_decompose(value) %>%
  
  # 2. Fix negative values if any in observed
  mutate(observed = ifelse(observed < 0, 0, observed)) %>%
  
  # 3. Identify anomalies
  anomalize(remainder) %>%
  
  # 4. Clean & repair anomalous data
  clean_anomalies() 

# Show change in observed vs observed_cleaned
df_hobbies_clean %>% 
  filter(anomaly == "Yes") %>%
  select(date, anomaly, observed, observed_cleaned) %>%
  head() %>% 
  kableExtra::kbl()

df_hobbies_clean <- df_hobbies_clean %>% ungroup() %>% 
  select(date, observed_cleaned)


# Partición en train y test -----------------------------------------------
particion <- df_hobbies_clean %>%  
  initial_time_split(prop = 0.8)

particion %>% 
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, observed_cleaned)

# Receta ------------------------------------------------------------------
receta <- recipe(observed_cleaned ~ date, data = training(particion)) %>%
  step_timeseries_signature(date) %>% 
  step_rm(contains("iso"), 
          contains("minute"), 
          contains("hour"),
          contains("am.pm"), 
          contains("xts"), 
          contains("second"),
          date_index.num,
          date_wday.lbl, 
          date_month.lbl)

receta %>% prep() %>% juice() %>% View()


# Modelos -----------------------------------------------------------------
# Modelo: Auto-ARIMA
m_autoarima <- arima_reg() %>% 
  set_engine('auto_arima') %>%  
  fit(observed_cleaned~date, data=training(particion))

# Modelo: exponential smoothing
m_exp_smoothing <- exp_smoothing() %>% 
  set_engine('ets') %>% 
  fit(observed_cleaned~date, data=training(particion))

# Modelo: regresión lineal
m_reg_lineal <- linear_reg() %>%
  set_engine("lm") %>%
  fit(observed_cleaned ~ as.numeric(date) + 
                         factor(month(date, label = TRUE), ordered = FALSE)+
                         factor(wday(date, label=TRUE), ordered=FALSE),
      data = training(particion))


# Workflow: prophet boosted
m_prophet_boost <- workflow() %>% 
  add_recipe(receta) %>% 
  add_model(
    prophet_boost(mode='regression') %>%
      set_engine("prophet_xgboost")
  ) %>% 
  fit(data = training(particion))

# Predicción --------------------------------------------------------------

modelos <- modeltime_table(m_autoarima,
                           m_exp_smoothing,
                           m_reg_lineal,
                           m_prophet_boost
)

calibration_table  <- modelos %>% 
  modeltime_calibrate(new_data = testing(particion))


calibration_table %>% 
  modeltime_accuracy()

forecast_series <- calibration_table %>% 
  modeltime_forecast(
    new_data    = testing(particion),
    actual_data = df_hobbies_clean)

forecast_series %>% 
  plot_modeltime_forecast(
    .legend_max_width     = 30, 
    .interactive          = FALSE,
    .conf_interval_alpha  = 0.2
  )





