
setwd("C:\\Users\\qingh\\DScourseS23\\ProblemSets\\PS3\\")


library(RSQLite)
con <- dbConnect(SQLite(), dbname = "my_database.db")
dbWriteTable(con, "insurance_data", read.csv(file.choose())
results <- dbGetQuery(con, "SELECT * FROM insurance_data LIMIT 10")
print(results)
unique_counties <- dbGetQuery(con, "SELECT DISTINCT county FROM insurance_data")
print(unique_counties)
avg_appreciation <- dbGetQuery(con, "SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM insurance_data")
print(avg_appreciation)
construction_counts <- dbGetQuery(con, "SELECT construction, COUNT(*) AS count FROM insurance_data GROUP BY construction")
print(construction_counts)
dbDisconnect(con)
