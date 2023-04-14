library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)


######################
# tidymodels time!
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)




#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf,
                                       best_acc
)
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "logit") %>% select(-starts_with(".config"))




#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())


# set up the workflow and perform the grid search
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

tree_res <- tree_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df
  )

# extract the best performing model
tree_ans <- show_best(tree_res, metric = "accuracy") %>% 
  slice(1) %>% 
  mutate(alg = "tree") %>% 
  select(-starts_with(".config"))





# YOU FILL IN THE REST




#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

# YOU FILL IN THE REST




#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

# YOU FILL IN THE REST



#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
svm_parm_df1 <- tibble(cost      = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df  <- full_join(svm_parm_df1,svm_parm_df2,by=character())

# YOU FILL IN THE REST



#####################
# combine answers
#####################
all_ans <- bind_rows(logit_ans,tree_ans,nnet_ans,knn_ans,svm_ans)
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown") %>% print



# quesion 5

library(tidymodels)

# Define the task
income_task <- task_classif(income_train, high.earner)

# Define the tuning grid
logit_grid <- grid_regular(penalty(), levels = 50)
tree_grid <- grid_regular(cost_complexity(), min_n(), tree_depth(), levels = 10)
nnet_grid <- grid_max_entropy(hidden_units(), penalty(), levels = 10)
knn_grid <- grid_regular(neighbors(), levels = 30)
svm_grid <- grid_regular(cost(), rbf_sigma(), levels = 6)

# Define the models
logit_model <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

tree_model <- decision_tree(cost_complexity = tune(),
                            min_n = tune(),
                            tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

nnet_model <- mlp(hidden_units = tune(),
                  penalty = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")

knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

svm_model <- svm_rbf(cost = tune(),
                     rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Define the workflows
logit_wf <- workflow() %>%
  add_model(logit_model) %>%
  add_formula(high.earner ~ .)

tree_wf <- workflow() %>%
  add_model(tree_model) %>%
  add_formula(high.earner ~ .)

nnet_wf <- workflow() %>%
  add_model(nnet_model) %>%
  add_formula(high.earner ~ .)

knn_wf <- workflow() %>%
  add_model(knn_model) %>%
  add_formula(high.earner ~ .)

svm_wf <- workflow() %>%
  add_model(svm_model) %>%
  add_formula(high.earner ~ .)

# Tune the models using 3-fold cross-validation
logit_res <- tune_grid(logit_wf, resamples = rec_folds, grid = logit_grid)
tree_res <- tune_grid(tree_wf, resamples = rec_folds, grid = tree_grid)
nnet_res <- tune_grid(nnet_wf, resamples = rec_folds, grid = nnet_grid)
knn_res <- tune_grid(knn_wf, resamples = rec_folds, grid = knn_grid)
svm_res <- tune_grid(svm_wf, resamples = rec_folds, grid = svm_grid)

# Extract the best model for each algorithm
logit_best <- select_best(logit_res, metric = "accuracy")
tree_best <- select_best(tree_res, metric = "accuracy")
nnet_best <- select_best(nnet_res, metric = "accuracy")
knn_best <- select_best(knn_res, metric = "accuracy")
svm_best <- select_best(svm_res, metric = "accuracy")

# Fit the best models on the training data
logit_fit <- finalize_workflow(logit_wf, logit_best) %>%
  fit(data = income_train)
tree_fit <- finalize_workflow(tree_wf, tree_best) %>%
  fit(data = income_train)
nnet_fit <- finalize_workflow(nnet_wf, nnet_best) %>%
  fit(data = income_train)
knn_fit <- finalize_workflow(knn_wf, knn_best) %>%
  fit(data = income_train)
svm_fit <- finalize_workflow(svm
                             
# question 6
library(tidymodels)

# Define hyperparameter grids
# ... (your code for hyperparameter grids) ...

# Set up specifications and engines
# ... (your code for specifications and engines) ...

# Set up workflow
model_wf <- workflow() %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Now, when you want to use a specific model spec, add it to the workflow like this:
logit_wf <- model_wf %>%
  add_model(logit_spec)

# Similarly, for other models
tree_wf <- model_wf %>%
  add_model(tree_spec)

nnet_wf <- model_wf %>%
  add_model(nnet_spec)

knn_wf <- model_wf %>%
  add_model(knn_spec)

svm_wf <- model_wf %>%
  add_model(svm_spec)

# question 7

#####################
# logistic regression
#####################
print('Starting Logistic Regression')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter
lambda_grid <- grid_regular(penalty(), levels = 10)

logit_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ .)

logit_res <- logit_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

logit_best <- select_best(logit_res, metric = "accuracy")
logit_ans <- logit_best %>%
  mutate(alg = "logit") %>%
  select(-starts_with(".config"))

#####################
# decision tree
#####################
print('Starting Decision Tree')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the tuning parameters
tree_parm_df <- tibble(
  cost_complexity = seq(0, 0.1, length.out = 10),
  tree_depth = seq(1, 10)
)

tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ .)

tree_res <- tree_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df
  )

tree_best <- select_best(tree_res, metric = "accuracy")
tree_ans <- tree_best %>%
  mutate(alg = "tree") %>%
  select(-starts_with(".config"))


#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ .)

knn_res <- knn_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = knn_parm_df
  )

knn_best <- select_best(knn_res, metric = "accuracy")
knn_ans <- knn_best %>%
  mutate(alg = "knn") %>%
  select(-starts_with(".config"))

#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ .)

nnet_res <- nnet_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = nnet_parm_df
  )

nnet_best <- select_best(nnet_res, metric = "accuracy")
nnet_ans <- nnet_best %>%
  mutate(alg = "nnet") %>%
  select(-starts_with(".config"))

#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
svm_parm_df1 <- tibble(cost      = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df  <- full_join(svm_parm_df1,svm_parm_df2,by=character())

svm_wf <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(high.earner ~ .)

svm_res <- svm_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = svm_parm_df
  )

svm_best <- select_best(svm_res, metric = "accuracy")
svm_ans <- svm_best %>%
  mutate(alg = "svm") %>%
  select(-starts_with(".config"))

#####################
# combine answers
#####################
all_ans <- bind_rows(logit_ans, tree_ans, knn_ans, nnet_ans, svm_ans)


# question 8


# Combine the training and validation sets to train the final models
final_train_data <- bind_rows(train_data, valid_data)

# Logistic Regression
final_logit <- logistic_reg(penalty = logit_best$penalty) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(high.earner ~ ., data = final_train_data)

# Decision Tree
final_tree <- decision_tree(cost_complexity = tree_best$cost_complexity,
                            tree_depth = tree_best$tree_depth) %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(high.earner ~ ., data = final_train_data)

# KNN
final_knn <- nearest_neighbor(neighbors = knn_best$neighbors,
                              weight_func = knn_best$weight_func,
                              dist_power = knn_best$dist_power) %>%
  set_engine("kknn") %>%
  set_mode("classification") %>%
  fit(high.earner ~ ., data = final_train_data)

# Neural Network
final_nnet <- mlp(hidden_units = nnet_best$hidden_units,
                  activation = nnet_best$activation,
                  penalty = nnet_best$penalty) %>%
  set_engine("nnet") %>%
  set_mode("classification") %>%
  fit(high.earner ~ ., data = final_train_data)

# SVM
final_svm <- svm_poly(degree = svm_best$degree,
                      cost = svm_best$cost,
                      scale = svm_best$scale) %>%
  set_engine("kernlab") %>%
  set_mode("classification") %>%
  fit(high.earner ~ ., data = final_train_data)

# Generate predictions
logit_preds <- predict(final_logit, test_data)
tree_preds <- predict(final_tree, test_data)
knn_preds <- predict(final_knn, test_data)
nnet_preds <- predict(final_nnet, test_data)
svm_preds <- predict(final_svm, test_data)

# Assess performance
logit_metrics <- metric_set(accuracy, roc_auc) %>%
  estimate(truth = test_data$high.earner, estimate = logit_preds)
tree_metrics <- metric_set(accuracy, roc_auc) %>%
  estimate(truth = test_data$high.earner, estimate = tree_preds)
knn_metrics <- metric_set(accuracy, roc_auc) %>%
  estimate(truth = test_data$high.earner, estimate = knn_preds)
nnet_metrics <- metric_set(accuracy, roc_auc) %>%
  estimate(truth = test_data$high.earner, estimate = nnet_preds)
svm_metrics <- metric_set(accuracy, roc_auc) %>%
  estimate(truth = test_data$high.earner, estimate = svm_preds)

# Combine performance metrics
all_metrics <- bind_rows(
  logit_metrics %>% mutate(model = "Logistic Regression"),
  tree_metrics %>% mutate(model = "Decision Tree"),
  knn_metrics %>% mutate(model = "KNN"),
  nnet_metrics %>% mutate(model = "Neural Network"),
  svm_metrics %>% mutate(model = "SVM")
)

print(all_metrics)



# question 9

library(xtable)

# Combine the optimal tuning parameters
optimal_params <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "KNN", "Neural Network", "SVM"),
  Penalty = c(logit_best$penalty, "-", "-", nnet_best$penalty, svm_best$cost),
  TreeDepth = c("-", tree_best$tree_depth, "-", "-", "-"),
  Neighbors = c("-", "-", knn_best$neighbors, "-", "-"),
  Activation = c("-", "-", "-", nnet_best$activation, "-"),
  Degree = c("-", "-", "-", "-", svm_best$degree),
  stringsAsFactors = FALSE
)

# Convert the dataframe to a LaTeX table
optimal_params_latex <- xtable(optimal_params, caption = "Optimal tuning parameters for each algorithm")
print(optimal_params_latex, type = "latex", include.rownames = FALSE, caption.placement = "top")


# Convert the performance metrics to a LaTeX table
all_metrics_latex <- xtable(all_metrics, caption = "Out-of-sample performance metrics for each algorithm")
print(all_metrics_latex, type = "latex", include.rownames = FALSE, caption.placement = "top")

