library(rvest)
url1 <- 'https://en.wikipedia.org/wiki/State_income_tax'
html_path <- '#mw-content-text > div.mw-parser-output > table:nth-child(157)'
uf <- read_html(url1) %>% html_nodes(html_path) %>% html_table() %>% '[['(1)

library(httr)
library(jsonlite)
library(dplyr)

# Set Tiingo API key
api_key <- "31203b0ba4d6d628ebeeefdc758a4133234264c8"

# Set endpoint URL
url <- paste0("https://api.tiingo.com/tiingo/daily/aapl/prices?token=", api_key)

# Set parameters for past 30 days
params <- list(
  startDate = "2022-02-05",
  endDate = "2022-03-06"
)

# Make API request
response <- GET(url, query = params)

# Convert response to dataframe
df <- fromJSON(content(response, "text"))

# Filter to only closing price column and sort by date
df <- df %>% select(date, close) %>% arrange(date)

# Print dataframe
print(df)
