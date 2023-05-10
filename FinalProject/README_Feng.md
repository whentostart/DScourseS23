# Final Project using STATA and SAS

## Data Import and Preparation in SAS

1. Import data from a SAS data file named "ent_v1_ent_scr_relationships.sas7bdat" that contains information on relationships between entities in a business network.
2. Manipulate the data to select only the relationships where the relationship type is "SUPPLIER" and remove any duplicates.
3. Sort the remaining unique pairs of entities and their associated start and end dates.
4. Export the sorted data to a CSV file named "sc_pairs.csv" using the `outsheet` command.

## Data Preparation and Regression Analysis Pipeline in SAS

1. Import the CSV file "sc_pairs.csv" and save it as a SAS dataset named "fiproj.sc_pairs".
2. Set the missing values of the "edate" column to '25Apr2023', add a new column called "length" between "sdate" and "edate", and only keep the "length" that is greater than 24.
3. Merge the "fiproj.sc_pairs" dataset with another dataset named "fs_link" to get "permno" for customers.
4. Merge the "fiproj.sc_pairs2" dataset with the "fs_link" dataset again to get "permno" for suppliers.
5. Use the "sdate" column to find the return in the following 12 months of "sdate" for customers and create a new dataset named "sc_pairs4".
6. Use the "sdate" column to find the return in the following 12 months of "sdate" for suppliers and create a new dataset named "sc_pairs5".
7. Calculate the mean of "c_ret", "s_ret", "c_mv", "s_mv" within each pair of "c_permno" and "s_permno", and create a new dataset named "sc_pairs5_means".
8. Join the "sc_pairs5_means" dataset with the "sc_pairs5" dataset and fill in missing values with their respective means, and create a new dataset named "sc_pairs6".
9. Aggregate monthly "mv" (missing value already replaced by mean of each pair) to each pair and create a new dataset named "sc_pairs_mv_mean".
10. Compute the correlations between "c_ret" and "s_ret" for each pair of companies (identified by "c_permno" and "s_permno") during each month period (identified by "sdate" and "edate") and create a new dataset named "fiproj.CorrOutp".
11. Keep only the rows where the **TYPE** variable is "CORR" and the **NAME** variable is "c_ret", and create a new dataset named "fiproj.CorrOutp".
12. Merge in aggregated "mv" to the "fiproj.CorrOutp" dataset, and create a new dataset named "fiproj.CorrOutp_with_mv".
13. Append the "fiproj.CorrOutp_with_mv" dataset and the "fiproj.CorrOutp_with_mv1" dataset.
14. Run a linear regression using the **PROC REG** procedure to investigate the relationship between "ret" and "post", and "ret", "post", "CMV", "SMV".
