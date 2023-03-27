setwd("C:/Users/qingh/DScourseS23/Problemsets/PS7")

library(readr)
wages_data <- read_csv("wages.csv")
head(wages_data)

# Load necessary library
library(dplyr)

# Drop rows with missing values in hgc or tenure
wages_data_clean <- wages_data %>%
  filter(!is.na(hgc) & !is.na(tenure))

# Display the first few rows of the cleaned data frame
head(wages_data_clean)

# Load package
library(modelsummary)

# Convert the tibble to a data.frame
wages_data_clean_df <- as.data.frame(wages_data_clean)

# Create the summary table with mean, standard deviation, and histogram
datasummary(All(wages_data_clean_df) ~ Mean + SD + Histogram,
                             data = wages_data_clean_df,
                             output = 'markdown')


###===1. Estimate the regression using only complete cases (listwise deletion)===###

# Filter out rows with missing values in the log_wage variable
wages_data_clean_complete <- wages_data_clean %>% filter(!is.na(logwage))

# Add squared tenure to the data frame
wages_data_clean_complete$tenure2 <- wages_data_clean_complete$tenure^2

# Run the regression using only complete cases
complete_cases_regression <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = wages_data_clean_complete)
summary(complete_cases_regression)

###===2. Perform mean imputation to fill in missing log wages===###

# Calculate the mean of log_wage for complete cases
log_wage_mean <- mean(wages_data_clean_complete$logwage, na.rm = TRUE)

# Fill in missing log_wage values with the mean
wages_data_mean_imputed <- wages_data_clean %>% mutate(logwage = ifelse(is.na(logwage), log_wage_mean, logwage))

# Add squared tenure to the data frame
wages_data_clean$tenure2 <- wages_data_clean$tenure^2


####===3. Impute missing log wages as their predicted values from the complete cases regression===###


# Predict log wages using the complete cases regression
wages_data_clean$predicted_log_wage <- predict(complete_cases_regression, newdata = wages_data_clean)

# Impute missing log wages with their predicted values
wages_data_predicted_imputed <- wages_data_clean %>% mutate(logwage = ifelse(is.na(logwage), predicted_log_wage, logwage))

# Add squared tenure to the data frame
wages_data_predicted_imputed$tenure2 <- wages_data_predicted_imputed$tenure^2


###===4. Use the mice package to perform multiple imputation regression===###

# Install and load the mice package
if (!requireNamespace("mice", quietly = TRUE)) {
  install.packages("mice")
}
library(mice)


# Perform multiple imputation
mice_data <- mice(wages_data_clean[, c("logwage", "hgc", "college", "tenure", "tenure2", "age", "married")], m = 5, maxit = 50, method = "pmm", seed = 500)

# Fit the regression model on each imputed dataset
mice_regression <- with(data = mice_data, exp = lm(logwage ~ hgc + college + tenure + tenure2 + age + married))

# Combine the results from each imputed dataset
mice_summary <- summary(pool(mice_regression), conf.int = TRUE)

# Print the summary of the multiple imputation regression
print(mice_summary)




# 1. Complete cases regression
model1 <- complete_cases_regression

# 2. Mean imputed regression
model2 <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = wages_data_mean_imputed)

# 3. Predicted imputed regression
model3 <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = wages_data_clean)

# 4. Multiple imputation regression (using the mice package)
# Note: This model has already been fitted and pooled in previous steps as 'mice_regression'
model4 <- mice_regression





# Load the modelsummary package
library(modelsummary)


options("modelsummary_format_numeric_latex" = "plain")


# Create the regression table
regression_table <- modelsummary(list(model1, model2, model3, model4),
                                 output = "latex",
                                 stars = TRUE,
                                 gof_map = list(),
                                 coef_map = list("hgc" = "Return to Schooling (\\(\\hat{\\beta}_{1}\\))"))

# Save the table to a .tex file
cat(regression_table, file = "regression_table.tex")

getwd()


