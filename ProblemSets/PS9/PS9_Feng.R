library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)

# Load the data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Split the data into training and testing sets
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Create a recipe to preprocess the data
housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  # Convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # Convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # Create interaction term between crime and nox
  step_interact(terms = ~ crim:nox) %>%
  # Create square terms of some continuous variables
  step_poly(dis,nox) %>%
  prep()

# Preprocess the training and testing data
housing_train_prepped <- housing_recipe %>% juice
housing_test_prepped  <- housing_recipe %>% bake(new_data = housing_test)

# Create x and y variables for the training and testing sets
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y  <- housing_test_prepped  %>% select(medv)


###question 8
# Fit an OLS regression model
est.ols <- lm(medv ~ ., data = housing_train_prepped)
ols_predicted <- predict(est.ols, newdata = housing_test_x)
ols_rmse <- sqrt(mean((housing_test_y$medv - ols_predicted)^2))

# Define the tuning specification
tune_spec <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# Define the grid over which to try different values of lambda
lambda_grid <- grid_regular(
  penalty(),
  levels = 50
)

# Use 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Create a workflow to train the model
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

# Tune the model and extract the best result
set.seed(123)
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid,
    metrics = metric_set(rmse),
    control = control_grid(verbose = TRUE)
  )


top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")


# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)
# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print
# show best RMSE
top_rmse %>% print(n = 1)




# Question 9: ridge regression model and tune the penalty parameter λ by 6-fold cross validation
# Define the tuning specification
tune_spec_ridge <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# Define the grid over which to try different values of lambda
lambda_grid <- grid_regular(
  penalty(),
  levels = 50
)

# Use 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Create a workflow to train the model
rec_wf_ridge <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_ridge)

# Tune the model and extract the best result
set.seed(123)
rec_res_ridge <- rec_wf_ridge %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid,
    metrics = metric_set(rmse),
    control = control_grid(verbose = TRUE)
  )

# Get the best model and finalize the workflow
best_model_ridge <- select_best(rec_res_ridge, "rmse")
final_ridge <- finalize_workflow(rec_wf_ridge, best_model_ridge)

# Get the out-of-sample RMSE
housing_split <- initial_split(housing, prop = 0.7, strata = "chas")
housing_test <- testing(housing_split)
housing_test_y <- select(housing_test, medv)
housing_test_x <- select(housing_test, -medv)


out_of_sample_rmse_ridge <- last_fit(final_ridge, split = housing_split) %>% 
  collect_predictions() %>% 
  bind_cols(housing_test_y) %>% 
  mutate(across(all_of(c("medv", ".pred")), exp)) %>%
  yardstick::rmse(truth = medv, estimate = .pred) %>%
  summarize(mean_rmse = mean(.estimate)) %>%
  pull(mean_rmse)



# Print the results
cat("Optimal value of lambda:", best_model_ridge$penalty, "\n")
cat("Out-of-sample RMSE:", out_of_sample_rmse_ridge, "\n")
