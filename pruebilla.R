

# Copy --------------------------------------------------------------------


library(cutpointr)
library(probably)
library(cvms)





thresholds_matrix =
  
  tibble(thresholds = thresholds_cuts) %>%
  
  mutate(tidy_thresholds = map(thresholds,
                               ~ bind_cols(predictions,
                                           .pred =  make_two_class_pred(predictions$.pred_si,
                                                                        c("si","no"),
                                                                        threshold = .x)) %>%
                                 select(fraude,  .pred) %>% table())) %>%
  
  filter(tidy_thresholds %>% lengths == 4) %>%
  
  mutate(tidy_thresholds = map(tidy_thresholds, tidy)) %>%
  
  mutate(plots_matrix = map2(tidy_thresholds,thresholds,
                             ~ plot_confusion_matrix(.x,
                                                     target_col = "fraude",
                                                     prediction_col = ".pred",
                                                     counts_col = "n",
                                                     add_sums = TRUE,
                                                     add_normalized = FALSE,
                                                     sums_settings = sum_tile_settings(tc_tile_border_color = "black")) +
                               labs(title = paste0("Punto de corte: ", round(.y,3))))) 




# play --------------------------------------------------------------------

library(tidymodels)
library(modeldata)
data(credit_data)
credit_data <- credit_data %>% drop_na() %>% mutate(Status = fct_relevel(Status, "bad"))

set.seed(123)
splits <- initial_split(credit_data)
train <- training(splits)
test  <- testing(splits)


modelo <- logistic_reg() 


receta <- recipe(Status ~ ., data = train) %>% 
  
  step_normalize(all_numeric_predictors()) %>% 
  
  step_dummy(all_nominal_predictors())


workflow_credito <- workflow() %>% 
  
  add_model(modelo) %>% 
  
  add_recipe(receta)


predicciones <- workflow_credito %>% 
  
  fit(train) %>% 
  
  augment(test)


predicciones %>% view()

predicciones %>% 
  conf_mat(Status, .pred_class) %>% 
  autoplot(type = "heatmap")


# anidado -----------------------------------------------------------------


predicciones %>% 
  
  mutate(.pred_class = make_two_class_pred(.pred_bad, levels(Status), threshold = 0.7)) %>% 
  
  select(Status, .pred_class) %>% 
  
  conf_mat(Status, .pred_class) %>% 
  
  autoplot(type = "heatmap")
  

  
thresholds_cuts <- predicciones$.pred_bad %>% quantile(seq(0,1,0.1))

thresholds_cuts %>% plot()



thresholds_tibble <- tibble(thresholds = thresholds_cuts) %>%
  
  mutate(matrices_plot = map(thresholds,
                             
                             ~ predicciones %>% 
                               
                               mutate(.pred_class = make_two_class_pred(.pred_bad, levels(Status), threshold = .x)) %>% 
                               
                               select(Status, .pred_class) %>% 
                               
                               conf_mat(Status, .pred_class) %>% 
                               
                               autoplot(type = "heatmap")))





# Broom -------------------------------------------------------------------

library(performance)
library(see)


dataset <- credit_data %>% 
  nest(datos = -Home)

dataset <- dataset %>% filter(Home != "ignore")
  


modelo <- logistic_reg() %>% set_engine("glm")

receta <- recipe(Status ~ ., data = dataset$datos[[1]]) %>% 
  
  step_normalize(all_numeric_predictors()) %>% 
  
  step_dummy(all_nominal_predictors())


workflow_credito <- workflow() %>% 
  
  add_model(modelo) %>% 
  
  add_recipe(receta)



a <- 
  
dataset %>% 
  
  mutate(modelo = map(datos, ~ workflow_credito %>% fit(.x) %>% extract_fit_engine())) %>% 
  
  mutate(graficos = map(modelo, ~ check_model(.x)))

                        
b <- a %>% 
  
  mutate(rendimiento = map(modelo, ~ model_performance(.x)))

b$modelo[[1]] %>%
  tbl_regression(exponentiate = TRUE) %>% 
  bold_p(t = 0.15) %>%
  add_vif() %>% 
  bold_labels() %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("Fuente: R-Rosario*"))
  
                        
d <- b %>% 
  
  mutate(tabla_modelo = map(modelo, ~ tbl_regression(.x, exponentiate = TRUE) %>% 
                             bold_p(t = 0.15) %>%
                             bold_labels() %>% 
                             as_gt() %>%
                             gt::tab_source_note(gt::md("Fuente: R-Rosario*"))))









