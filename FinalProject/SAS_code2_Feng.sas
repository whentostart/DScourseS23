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

/*(5)use edate to find ret in the previous 12 months of edate for customers*/
proc sql;
    create table sc1_pairs4 as
    select a.*, b.date, b.ret as c_ret, log(abs(b.prc)*b.shrout) as c_mv
    from fiproj.sc_pairs3 as a left join crsp.msf as b
    on a.c_permno=b.permno and 0 < intck('month',b.date,a.edate) <= 12
    order by a.c_permno, b.date;
quit;

/*(6)use edate to find ret in the previous 12 months of edate for suppliers*/
proc sql;
    create table sc1_pairs5 as
    select a.*, b.ret as s_ret, log(abs(b.prc)*b.shrout) as s_mv
    from sc1_pairs4 as a left join crsp.msf as b
    on a.s_permno=b.permno and a.date = b.date and 0 < intck('month',b.date,a.edate) <= 12
    order by a.s_permno, a.date;
quit;

proc sort data=sc1_pairs5;
    by c_permno s_permno sdate edate;
run;

/*(7)calculate the mean of c_ret, s_ret, c_mv, s_mv within each pair of c_permno and s_permno */
proc means data=sc1_pairs5 noprint;
    by c_permno s_permno;
    var c_ret s_ret c_mv s_mv;
    output out=sc1_pairs5_means mean=mean_c_ret mean=mean_s_ret mean=CMV mean=SMV
run;

/*(8)join the means table with the original sc_pairs5 table and fill in missing values with their respective means */
data sc1_pairs6;
merge sc1_pairs5(in=a) sc1_pairs5_means(in=b);
by c_permno s_permno;
if a;
if missing(c_ret) or c_ret = .C  then c_ret = mean_c_ret;
if missing(s_ret) or s_ret = .C  then s_ret = mean_s_ret;
if missing(c_mv) then c_mv = CMV;
if missing(s_mv) then s_mv = SMV;
keep c_permno s_permno sdate edate date c_ret s_ret c_mv s_mv;
run;

/*(9)aggregate monthly mv (missing value already replaced by mean of each pair) to each pair*/
proc sql;
create table fiproj.sc1_pairs_mv_mean as
select c_permno, s_permno, sdate, mean(c_mv) as CMV, mean(s_mv) as SMV
from sc1_pairs6
group by c_permno, s_permno, sdate;
quit;

/*(10)the correlations are computed for each pair of companies (identified by c_permno and s_permno) during each month period (identified by sdate and edate).*/
proc corr data=sc1_pairs6 noprint outp=CorrOutp1;
var c_ret s_ret;
by c_permno s_permno sdate edate;
run;

/*(11)keep only the rows where the *TYPE* variable is "CORR" and the *NAME* variable is "c_ret". */
data fiproj.CorrOutp1;
set CorrOutp1;
where _TYPE_ = "CORR" and _NAME_ = "c_ret";
post =1;
run;

proc sort data=fiproj.CorrOutp1;
by c_permno s_permno;
run;

/*(12)merge in aggregated mv*/
data fiproj.CorrOutp_with_mv1;
merge fiproj.CorrOutp1(in=a) fiproj.sc1_pairs_mv_mean(in=b);
by c_permno s_permno sdate;
if a;
rename mean_c_mv = CMV mean_s_mv = SMV s_ret=ret;
run;






