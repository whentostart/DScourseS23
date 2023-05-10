
import sas id rel_type source_factset_entity_id target_factset_entity_id start_date end_date revenue_pct using "C:\Users\qingh\Dropbox\Course\2023Spring\ECON5253\Project\data\ent_v1_ent_scr_relationships.sas7bdat", clear

/*Describing the dataset using the describe command.*/
describe


/*Generating a frequency table of the variable rel_type using tab rel_type.*/
tab rel_type

/*Keeping only the observations where rel_type is equal to "SUPPLIER" using keep if rel_type == "SUPPLIER".*/
keep if rel_type == "SUPPLIER"
tab rel_type


/*Converting the start_date and end_date variables from SAS date format to date format using the dofc function, and creating new variables sdate and edate to hold the converted dates.*/
generate sdate = dofc(start_date)
format sdate %td

generate edate = dofc(end_date)
format edate %td


/*Renaming the variables source_factset_entity_id and target_factset_entity_id to source and target respectively.*/
rename source_factset_entity_id source
rename target_factset_entity_id target

/*Identifying and tagging duplicate observations based on the combination of source, target, start_date, and end_date using the duplicates command.*/

duplicates tag source target start_date end_date, generate(duplicates)

/*Sorting the data by source, target, sdate, and edate.*/
sort source target sdate edate

/*Listing the source, target, sdate, and edate variables for observations that are tagged as duplicates (duplicates == 1) and for the first 20 observations (in 1/20) using the list command.*/
list source target sdate edate duplicates if duplicates == 1

/*Dropping the observations that are tagged as duplicates using drop if duplicates == 1.*/
drop if duplicates == 1


/* Sort the data by source and target entities and start date */
sort source target sdate

/* Generate a new variable to identify unique pairs */
by source target: gen pair = _n == 1

/* Keep only the unique pairs */
keep if pair == 1

/*show the first 20 observations of the pair variable after generating it using _n == 1 and then generating a frequency table of the pair variable using tab pair.*/
list source target sdate edate pair in 1/20

tab pair

/* Export table to delimited file in specified folder */
outsheet using "C:\Users\qingh\Dropbox\Course\2023Spring\ECON5253\Project\data\sc_pairs.csv", delim(",")

