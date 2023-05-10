/*============================================================================================*/
/*                First Step,identify firms that share common suppliers                       */
/*============================================================================================*/
/*Key columns in the **ENT_SCR_RELATIONSHIPS** table:
* **id**: Unique identifier representing a relationship.
* **rel_type**: Code denoting the type of relationship as reported by the source_factset_entity_id.
* **source_factset_entity_id**: Unique FactSet-generated identifier assigned to the company that reported the relationship.
* **target_factset_entity_id**: Unique FactSet-generated identifier assigned to the company which was reported as being related to the source company.
* **start_date**: Date the record was first published.
* **end_date**: Date the record was terminated. A null end_date indicate the relationship is considered current.
* **revenue_pct**: Percentage of its revenue that the source company derived from this relationship in the reporting period. 
*This field is only applicable to relationships reported as Customer and only then for those cases where the reporting company provides a percentage.*/;
libname repdata '~/replicate/data';
libname fiproj "/home/ou/qinghuafeng/DSfiproject";

/*(1)imports the CSV file and saves it directly as a SAS dataset named "sc_pairs.sas7bdat"*/
proc import datafile="/home/ou/qinghuafeng/DSfiproject/sc_pairs.csv"
    dbms=csv
    out=fiproj.sc_pairs
    replace;
run;

/* (2)Set the edate missing as '25Apr2023', add a new column called "length" between "sdate" and "edate", and only keep the "length" that is greater than 24 */
data sc_pairs1;
    set fiproj.sc_pairs (drop=id rel_type);
    if missing(edate) then edate = '25Apr2023'd;
    length = intck('month', sdate, edate);
    if length > 24 then output;
run;


/*just check how many observations that the length are equal to 12. (16140 in total)*/
/*
PROC SQL;
    SELECT COUNT(*) AS num_obs
    FROM fiproj.sc_pairs1
    WHERE length = 12;
QUIT;
*/


/* (3)Merge the sc_pairs and fs_link data sets to get permno for customers*/
proc sql;
    create table sc_pairs2 as
	select a.*, b.permno as c_permno
	from sc_pairs1 a left join repdata.fs_link b
	on a.source=b.FACTSET_ENTITY_ID and b.link_BDATE <= a.sdate <= b.LINK_EDATE and b.fsym_id_kind = "S"
    where not missing(c_permno);
quit;


/* (4)Merge the sc_pairs and fs_link data sets to get permno for suppliers*/
proc sql;
    create table fiproj.sc_pairs3 as
    select a.*, b.permno as s_permno
    from sc_pairs2 a left join repdata.fs_link b
    on a.target=b.FACTSET_ENTITY_ID and b.link_BDATE <= a.sdate <= b.LINK_EDATE and b.fsym_id_kind = "S"
    where not missing(s_permno);
quit;

/*(5) use sdate to find ret in the following 12 months of sdate for customers*/
proc sql;
    create table sc_pairs4 as
    select a.*, b.date, b.ret as c_ret, log(abs(b.prc)*b.shrout) as c_mv
    from fiproj.sc_pairs3 as a left join crsp.msf as b
    on a.c_permno=b.permno and 0 < intck('month',a.sdate,b.date) <= 12
    order by a.c_permno, b.date;
quit;

/*(6)use sdate to find ret in the following 12 months of sdate for suppliers*/
proc sql;
    create table sc_pairs5 as
    select a.*, b.ret as s_ret, log(abs(b.prc)*b.shrout) as s_mv
    from sc_pairs4 as a left join crsp.msf as b
    on a.s_permno=b.permno and a.date = b.date and 0 < intck('month',a.sdate,b.date) <= 12
    order by a.s_permno, a.date;
quit;


proc sort data=sc_pairs5;
    by c_permno s_permno sdate edate;
run;


/*(7)calculate the mean of c_ret, s_ret, c_mv, s_mv within each pair of c_permno and s_permno */
proc means data=sc_pairs5 noprint;
    by c_permno s_permno;
    var c_ret s_ret c_mv s_mv;
    output out=sc_pairs5_means mean=mean_c_ret mean=mean_s_ret mean=CMV mean=SMV;
run;

/*(8)join the means table with the original sc_pairs5 table and fill in missing values with their respective means */
data sc_pairs6;
merge sc_pairs5(in=a) sc_pairs5_means(in=b);
by c_permno s_permno;
if a;
if missing(c_ret) or c_ret = .C  then c_ret = mean_c_ret;
if missing(s_ret) or s_ret = .C  then s_ret = mean_s_ret;
if missing(c_mv) then c_mv = CMV;
if missing(s_mv) then s_mv = SMV;
keep c_permno s_permno sdate edate date c_ret s_ret c_mv s_mv;
run;

PROC PRINT DATA=crsp.msf;
    var date ret shrout prc;
    where permno = 10182;
run;

/*(9)aggregate monthly mv (missing value already replaced by mean of each pair) to each pair*/
proc sql;
create table sc_pairs_mv_mean as
select c_permno, s_permno, sdate, mean(c_mv) as CMV, mean(s_mv) as SMV
from sc_pairs6
group by c_permno, s_permno, sdate;
quit;

/*(10)the correlations are computed for each pair of companies (identified by c_permno and s_permno) during each month period (identified by sdate and edate).*/
proc corr data=sc_pairs6 noprint outp=CorrOutp;
var c_ret s_ret;
by c_permno s_permno sdate edate;
run;

/*(11)keep only the rows where the *TYPE* variable is "CORR" and the *NAME* variable is "c_ret". */
data fiproj.CorrOutp;
set CorrOutp;
where _TYPE_ = "CORR" and _NAME_ = "c_ret" and c_ret is not missing;
post =0;
run;

proc sort data=fiproj.CorrOutp;
by c_permno s_permno;
run;

/*(12)merge in aggregated mv*/
data fiproj.CorrOutp_with_mv;
   merge fiproj.CorrOutp(in=a) fiproj.sc_pairs_mv_mean(in=b);
   by c_permno s_permno sdate;
   if a;
   rename mean_c_mv = CMV mean_s_mv = SMV s_ret=ret;
run;


***Append CorrOutp_with_mv and CorrOutp_with_mv1******;

PROC APPEND base=fiproj.CorrOutp_with_mv data=fiproj.CorrOutp_with_mv1 force;
RUN;

PROC REG data=fiproj.CorrOutp_with_mv;
MODEL ret = post;
MODEL ret = post CMV SMV;
RUN;




