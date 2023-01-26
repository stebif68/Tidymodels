### Esempio Tidymodels Classification
### S.B. 25.01.2023

options(scipen = 999)


## libraries to be used 
library(tidymodels)
library(tidyverse)
tidymodels_prefer(quiet=F)

telecom_df <- read_rds("telecom_df.rds")

# dataset con info clienti compagni telefonica
head(telecom_df)

# outcome: cancellamento del servizio (canceled_service)


## train e test

telecom_split <- initial_split(
  data = telecom_df,
  prop = .75,
  strata = canceled_service
)

# for training
telecom_training <- telecom_split %>%
  training()

# for testing
telecom_test <- telecom_split %>%
  testing()

# check
nrow(telecom_training)

## Model Fitting

# Specify a logistic regression model
logistic_model <- logistic_reg() %>%
  # Set the engine
  set_engine("glm") %>%
  # Set the mode
  set_mode("classification")

# Fit to training data
# 3 effetti nel modello
# 
logistic_fit <- logistic_model %>%
  fit(
    canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges,
    data = telecom_training
  )

# Print model fit object
logistic_fit

## Generate Predictions

# Predict outcome categories
class_preds <- predict(
  logistic_fit,
  new_data = telecom_test,
  type = "class"
)
#class_preds
# Obtain estimated probabilities for each outcome value
prob_preds <- predict(
  logistic_fit,
  new_data = telecom_test,
  type = "prob"
)

# Combine test set results
telecom_results <- telecom_test %>%
  select(canceled_service) %>%
  bind_cols(class_preds, prob_preds)

# View results tibble
telecom_results %>%
  head()

### Assessing Model Fitting

# Confusion Matrix: Matrix with counts of all combinations of actual and predicted outcome values.

# Correct Predictions

# True Positive (TP)

# True Negative (TN)

# Classification Errors

# False Positive (FP)

# False Negative (FN)

# Calculate the confusion matrix
yardstick::conf_mat(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
)

# Calculate the accuracy
yardstick::accuracy(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
)

# Calculate the sensitivity
yardstick::sens(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
)

# Create a custom metric function
telecom_metrics <- metric_set(
  yardstick::accuracy,
  yardstick::sens,
  yardstick::spec
)

# Calculate metrics using model results tibble
telecom_metrics(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
)

# Create a confusion matrix
conf_mat(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
) %>%
  # Pass to the summary() function
  summary()

## Visualize Model Performance

conf_mat(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
) %>%
  # Create a heat map
  autoplot(type = "heatmap")

conf_mat(
  telecom_results,
  truth = canceled_service,
  estimate = .pred_class
) %>%
  # Create a mosaic map
  autoplot(type = "mosaic")

## RoC curve

# Plot ROC curve
telecom_results %>%
  # Calculate metrics across thresholds
  roc_curve(
    truth = canceled_service,
    estimate = .pred_yes
  ) %>%
  autoplot()

# Calculate the ROC AUC
telecom_results %>%
  roc_auc(
    truth = canceled_service,
    estimate = .pred_yes
  )

#### more models

rf_model <- rand_forest() %>%
  # Set the engine
  set_engine("randomForest") %>%
  # Set the mode
  set_mode("classification")

nn_model <- mlp() %>% 
  set_engine('nnet') %>% 
  set_mode("classification")

### replicates

folds <- vfold_cv(telecom_df, v = 20,strata = canceled_service)

### workflow

model_vars <- 
  workflow_variables(outcomes = canceled_service, 
                     predictors =c(avg_call_mins,avg_intl_mins,monthly_charges) )

normalized <- 
  workflow_set(preproc = list(simple = model_vars),
               models = list(log_reg=logistic_model,
                             ran_for=rf_model,
                             nue_net=nn_model))

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

grid_results <-
  normalized %>%
  workflow_map(
    seed = 1503,
    resamples = folds,
    grid = 25,
    control = grid_ctrl
  )

grid_results

grid_results %>% 
  rank_results()


autoplot(
  grid_results,
  rank_metric = "accuracy",  # <- how to order models
  metric = "accuracy",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) +
  geom_text(aes(y = mean - 0.03, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(0.55, 0.75)) +
  theme(legend.position = "none")

# 
# rf_fit <- rf_model %>%
#   fit(
#     canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges,
#     data = telecom_training
#   )
# Predict outcome categories
