library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)

head(housing)

# Assign column names
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Check the structure of the dataset
str(housing)
head(housing)

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)



#question 7

housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:nox) %>%
  # create square terms of some continuous variables
  step_poly(dis,nox) %>%
  prep()


housing_train_prepped <- housing_recipe %>% juice
housing_test_prepped  <- housing_recipe %>% bake(new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y  <- housing_test_prepped  %>% select( medv)
# Fit the regression model
est.ols <- lm(housing_train_y$medv ~ ., data = housing_train_x)
# Predict outcome for the test data
ols_predicted <- predict(est.ols, newdata = housing_test_x)
# Root mean-squared error
sqrt(mean((housing_test_y$medv - ols_predicted)^2))

#question 8
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)


# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) 

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Question 9

library(yardstick)

tune_spec_ridge <- linear_reg(
  penalty = tune(), 
  mixture = 0
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

lambda_grid_ridge <- grid_regular(penalty(), levels = 50)

rec_wf_ridge <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_ridge)

rec_res_ridge <- rec_wf_ridge %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid_ridge
  )

print(rec_res_ridge)

top_rmse_ridge <- select_best(rec_res_ridge, metric = "rmse")

final_ridge_wf <- finalize_workflow(rec_wf_ridge, top_rmse_ridge)

# Fit the finalized workflow using the training data
final_ridge_wf_fit <- fit(final_ridge_wf, data = housing_train_prepped)

# Make predictions using the fitted model
ridge_predicted <- predict(final_ridge_wf_fit, new_data = housing_test_prepped) %>%
  exp()


print(top_rmse_ridge)
print(ridge_rmse)

ridge_predicted_actuals <- bind_cols(ridge_predicted, housing_test_prepped %>% select(medv))
ridge_rmse <- rmse(ridge_predicted_actuals, truth = medv, estimate = .pred)



