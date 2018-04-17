%let path=/home/christopherjose21/411 - GLM/Week 4-6;
libname orion "&path"; 


data insurance; 
	set orion.logit_insurance;run;

ods graphics on;


proc contents data=insurance; run;
proc means data=insurance nmiss p1 mean median p99; run;
proc means data=insurance mean; class target_flag; var _numeric_; run;
proc freq data = insurance; tables _character_ / missing; run;
proc freq data = insurance; tables _character_*target_flag ; run;






proc hpimpute data=insurance out=insurance2;
	input kidsdriv age homekids yoj income home_val travtime bluebook
	tif oldclaim clm_freq mvr_pts car_age;
	ID index;
	impute kidsdriv / method =median;
	impute age / method =median;
	impute homekids / method =median;
	impute yoj / method =median;
	impute income / method =median;
	impute home_val / method =median;
	impute travtime / method =median;
	impute bluebook / method =median;
	impute tif / method =median;
	impute oldclaim / method =median;
	impute clm_freq / method =median;
	impute mvr_pts / method = median;
	impute car_age / method=median;

run;  

data insurance3;
	set insurance2;
	rename im_kidsdriv=kidsdriv; 
	rename im_age=age;
    rename im_homekids = homekids; 
    rename im_yoj = yoj;
    rename im_income = income;
    rename im_home_val = home_val;
    rename im_travtime = travtime;
    rename im_bluebook = bluebook;
    rename im_tif = tif;
    rename im_oldclaim = oldclaim;
    rename im_clm_freq = clm_freq;
    rename im_mvr_pts = mvr_pts;
    rename im_car_age = car_age;
run;








*Create intermediate datasets;
data insurance_categorical;
set insurance;
keep index target_flag target_amt car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity;
	if missing(job) then job="NA";
	run;


data insurance_missing_flags;
set insurance3;
keep index m_kidsdriv m_age m_homekids m_yoj m_income m_home_val m_travtime 
	m_bluebook m_tif m_oldclaim m_clm_freq m_mvr_pts m_car_age; run;

data insurance_non_trimmed;
	set insurance3;
	keep index kidsdriv age homekids yoj tif clm_freq mvr_pts car_age;
run;

data insurance_trimmed_prep;
	set insurance3;
	keep income home_val travtime bluebook oldclaim;
run;

data insurance_numeric;
	set insurance3;
	keep index income home_val travtime bluebook oldclaim
	kidsdriv age homekids yoj tif clm_freq mvr_pts car_age;
run;










*Trim outliers
*Calculate IQR and first/third quartiles;
proc means data=insurance_trimmed_prep stackods n qrange p1 p99;
ods output summary=ranges;
run;

*create data with outliers to check;
**http://stackoverflow.com/questions/34913022/sas-remove-outliers;
data insurance_trimmed; 
    set insurance_trimmed_prep;
run;

*macro to cap outliers;
%macro cap(dset=,var=, lower=, upper=);
data &dset;
    set &dset;
    if &var>&upper then &var=&upper;
    if &var<&lower then &var=&lower;
run;
%mend;

*create cutoffs and execute macro for each variable;
data cutoffs;
set ranges;
lower=p1;
upper=p99;
string = catt('%cap(dset=insurance_trimmed, var=', variable, ", lower=", lower, ", upper=", upper ,");");
call execute(string);
run;










*Final Dataset with imputed, trimmed data;
data insurance1;
	merge insurance_categorical insurance_non_trimmed insurance_missing_flags
	insurance_trimmed;
	run;
proc means data=insurance4 nmiss mean p1 p50 p99; run;

	
data insuranceF;
set insurance1;
age1=0;
age2=0;
	if age<=20 then age1=1;
	if age>52 then age2=1;
log_income=log(income);
if missing(log_income) then log_income=0;
log_oldclaim=log(oldclaim);
if missing(log_oldclaim) then log_oldclaim=0;
log_home_val=log(home_val);
if missing(log_home_val) then log_home_val=0;
	run;




Title 'Before Trimming';
ods select plots;
proc univariate data=insurance plot; var income home_val travtime bluebook oldclaim; run;

Title 'After Trimming';
ods select plots;
proc univariate data=insurance1 plot; var income home_val travtime bluebook oldclaim; run;

Title 'Should age be made into a categorical variable?';
proc freq data=insuranceF; tables age1*target_flag; run;
Title 'Should age be made into a categorical variable?';
proc freq data=insuranceF; tables age2*target_flag; run;







proc surveyselect data=insuranceF outall method=srs samprate = .7 seed=1
	out=subsets ; run;
data train;
   set subsets;
   if Selected=1; run;
data test; 
   set subsets;
   if Selected=0; run;




	

ods graphics on;
Title "Logistic Regression on all Variables";
proc logistic data=train
	plots(only)=roc(id=prob) outmodel=log1_model;
	class car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity
	
	m_age m_yoj m_income m_home_val m_car_age
	/ param=reference; 
	model target_flag(ref="0") = bluebook
	travtime log_income log_home_val
	kidsdriv age homekids yoj log_oldclaim  
	tif clm_freq mvr_pts car_age

	car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity 
	m_age m_yoj m_income m_home_val m_car_age / 
	outroc = log1_roc;	
	output out = log1_fitted pred=p_target_flag1;
	run;

proc npar1way data=log1_fitted wilcoxon edf;
        class TARGET_FLAG;
        var p_target_flag1;
        run;



Title "Logistic Regression - Variables Removed";
proc logistic data=train
	plots(only)=roc(id=prob) outmodel=log2_model;
	class car_type car_use education job mstatus	
	parent1 revoked sex urbanicity
	/ param=reference; 
	model target_flag(ref="0") = 
	kidsdriv tif bluebook car_type car_use education job mstatus	
	parent1 revoked sex urbanicity bluebook 
	/ 
	outroc = log2_roc;	
	output out = log2_fitted pred=p_target_flag1;
	run;

proc npar1way data=log2_fitted wilcoxon edf;
        class TARGET_FLAG;
        var p_target_flag1;
        run;




Title "Logistic Regression - Stepwise Selection";
proc logistic data=train
	plots(only)=roc(id=prob) outmodel=log3_model;
	class car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity
	m_age m_yoj m_income m_home_val m_car_age; 
	model target_flag(ref="0") = kidsdriv age homekids yoj income home_val 
	travtime bluebook tif oldclaim clm_freq mvr_pts car_age
	
	car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity
	m_age m_yoj m_income m_home_val m_car_age   / 
	selection = stepwise
	outroc = log3_roc;	
	output out = log3_fitted pred=p_target_flag1;
	run;

proc npar1way data=log3_fitted wilcoxon edf;
        class TARGET_FLAG;
        var p_target_flag1;
        run;



Title "Decision Tree";
proc hpsplit data=train seed=15531; 
	class target_flag car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity
	
	m_age m_yoj m_income m_home_val m_car_age;
	 
	model target_flag(event='1') = bluebook
	travtime log_income log_home_val
	kidsdriv age homekids yoj log_oldclaim  
	tif clm_freq mvr_pts car_age

	car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity 
	m_age m_yoj m_income m_home_val m_car_age;
   	prune costcomplexity;
   	code file = "/home/christopherjose21/411 - GLM/Week 4-6/DT_model.sas";
run; 





