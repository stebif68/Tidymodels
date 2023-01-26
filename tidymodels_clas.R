### Esempio Tidymodels Classification
### S.B. 25.01.2023

options(scipen = 999)


## libraries to be used 
library(ISLR2)
library(tidymodels)
tidymodels_prefer(quiet=F)

telecom_df <- read_rds("data/telecom_df.rds")

# check
head(telecom_df)

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