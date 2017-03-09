%let path=/home/christopherjose21/411 - GLM/Week 7-9;
libname orion "&path";

data wine;
	set orion.wine;
run;
ods graphics on;


*EDA;
proc contents data=wine;run;
proc means data=wine mean var;var TARGET;run;
proc univariate data=wine noprint;histogram TARGET;run;
proc means data=wine nmiss p1 median mean p99 var;run;
proc means data=wine nmiss median maxdec=1;run;
proc freq data=wine;tables STARS*TARGET /missing;run;




*Median Imputation;
data wine_imp;
set wine;
IMP_ALCOHOL = ALCOHOL;m_ALCOHOL=0;
if missing(IMP_ALCOHOL) then do; IMP_ALCOHOL=10; m_ALCOHOL = 1; end;

IMP_CHLORIDES = CHLORIDES;m_CHLORIDES=0;
if missing(IMP_CHLORIDES) then do; IMP_CHLORIDES=.046; m_CHLORIDES = 1; end;

IMP_FREESULFURDIOXIDE = FREESULFURDIOXIDE; m_FREESULFURDIOXIDE=0;
if missing(IMP_FREESULFURDIOXIDE) then do; IMP_FREESULFURDIOXIDE=30; m_FREESULFURDIOXIDE = 1; end;

IMP_RESIDUALSUGAR = RESIDUALSUGAR; m_RESIDUALSUGAR=0;
if missing(IMP_RESIDUALSUGAR) then do; IMP_RESIDUALSUGAR=3.9; m_RESIDUALSUGAR = 1; end;

IMP_SULPHATES = SULPHATES;m_SULPHATES=0;
if missing(IMP_SULPHATES) then do; IMP_SULPHATES=0.5; m_SULPHATES = 1; end;

IMP_PH = PH;m_PH=0;
if missing(IMP_PH) then do; IMP_PH=3.2; m_PH = 1; end; 

IMP_totalsulfurdioxide = TotalSulfurDioxide;
m_totalsulfurdioxide=0;
if missing(IMP_totalsulfurdioxide) then do; IMP_totalsulfurdioxide=123; 
IMP_totalsulfurdioxide = 1; end;

IMP_SULPHATES = SULPHATES;m_SULPHATES=0;
if missing(IMP_SULPHATES) then do; IMP_SULPHATES=0.5; m_SULPHATES = 1; end;

IMP_PH = PH; m_PH=0; if missing(IMP_PH) then do; IMP_PH=3.2; m_PH = 1; end;

IMP_STARS=STARS; m_STARS=0; if missing(stars) then do; imp_stars=2; m_stars=1; end; run;

*Generate distribution data, histogram, boxplot, and normal qqplot;
proc univariate data = wine plot; run;


*Generate matrix of correlations among all variables;
proc corr data=wine plots=matrix(histogram nvar=all); run; run; 


*Create table of original variable and flag variables for proc corr;
data wine_corr; set wine_imp; keep target acidindex alcohol chlorides citricacid density fixedacidity 
		freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph m_acidindex m_alcohol m_chlorides m_citricacid 
		m_density m_fixedacidity m_freesulfurdioxide m_labelappeal m_residualsugar 
		m_sulphates m_totalsulfurdioxide m_volatileacidity m_ph stars imp_stars m_stars; run;



data wine_nontrim;
	set wine_imp;
	keep index target labelappeal m_acidindex m_alcohol m_chlorides m_citricacid 
		m_density m_fixedacidity m_freesulfurdioxide m_labelappeal m_residualsugar 
		m_sulphates m_totalsulfurdioxide m_volatileacidity m_ph stars imp_stars m_stars;
run;

data wine_trim1;
	set wine_imp;
	drop index target labelappeal m_acidindex m_alcohol m_chlorides m_citricacid 
		m_density m_fixedacidity m_freesulfurdioxide m_labelappeal m_residualsugar 
		m_sulphates m_totalsulfurdioxide m_volatileacidity m_ph stars imp_stars m_stars m_tsd;
run;








*Trim data to remove outliers past P1 and P99 percentiles;
proc means data=wine_trim1 stackods n qrange p1 p99;
	ods output summary=ranges;
run;
data wine_trim;
	set wine_trim1; run;
%macro cap(dset=, var=, lower=, upper=);
	data &dset;
		set &dset;

		if &var>&upper then
			&var=&upper;

		if &var<&lower then
			&var=&lower;
	run;

%mend;
data cutoffs;
	set ranges;
	lower=p1;
	upper=p99;
	string=catt('%cap(dset=wine_trim, var=', variable, ", lower=", lower, 
		", upper=", upper , ");");
	call execute(string);
run;


*Make prefinal table by merging trimmed and nontrimmed data. This table has imputed and non-imputed versions of variables;
data wine_prefinal; merge wine_trim wine_nontrim; run; 
*Make final table by dropping non-imputed versions of variables and renaming imputed versions to their original names;
data wine_final;
set wine_prefinal;
drop alcohol chlorides freesulfurdioxide residualsugar sulphates ph totalsulfurdioxide sulphates
ph stars;
rename IMP_ALCOHOL = ALCOHOL; 
rename IMP_CHLORIDES = CHLORIDES;
rename IMP_FREESULFURDIOXIDE = FREESULFURDIOXIDE; 
rename IMP_RESIDUALSUGAR = RESIDUALSUGAR;
rename IMP_SULPHATES = SULPHATES;
rename IMP_PH = PH;
rename IMP_totalsulfurdioxide = totalsulfurdioxide;
rename IMP_SULPHATES = SULPHATES;
rename IMP_PH = PH;
rename IMP_STARS=STARS; 
run;

*Generate distributions of median imputed, trimmed data;
Title 'Proc Univariate for imputed, trimmed data';
proc univariate data = wine_final plot; run;



proc surveyselect data=wine_final outall method=srs samprate = .7 seed=2
	out=subsets ; run;
data train;
   set subsets;
   if selected=1; run;
data test; 
   set subsets;
   if selected=0; run;




*Considering cubed variables as predictors;
data wine_final_cubed; set wine_final;
	cuberoot_residualsugar=sign(residualsugar)*abs(residualsugar)**(1/3);
	cuberoot_totalsulfurdioxide=sign(totalsulfurdioxide)*abs(totalsulfurdioxide)**(1/3);
	cuberoot_freesulfurdioxide=sign(freesulfurdioxide)*abs(freesulfurdioxide)**(1/3);
	if missing(cuberoot_residualsugar) then cuberoot_residualsugar=0;
	if missing(cuberoot_totalsulfurdioxide) then cuberoot_totalsulfurdioxide=0;
	if missing(cuberoot_freesulfurdioxide) then cuberoot_freesulfurdioxide=0;
	run;

proc surveyselect data=wine_final_cubed outall method=srs samprate = .7 seed=2
	out=subsets_cubed ; run;
data train;
   set subsets_cubed;
   if selected=1; run;
data test; 
   set subsets_cubed;
   if selected=0; run;













*mae and rmse macro that is to be used for pred vs actual tables;
%macro 
		mae_rmse(dataset /* Data set which contains the actual and predicted values */, 
		actual /* Variable which contains the actual or observed valued */, 
		predicted /* Variable which contains the predicted value */);
	%global mae rmse;

	/* Make the scope of the macro variables global */
	data &dataset;
		retain square_error_sum abs_error_sum;
		set &dataset 
        end=last /* Flag for the last observation */;
		error=&actual - &predicted;

		/* Calculate simple error */
		square_error=error * error;

		/* error^2 */
		if _n_ eq 1 then
			do;

				/* Initialize the sums */
				square_error_sum=square_error;
				abs_error_sum=abs(error);
			end;
		else
			do;

				/* Add to the sum */
				square_error_sum=square_error_sum + square_error;
				abs_error_sum=abs_error_sum + abs(error);
			end;

		if last then
			do;

				/* Calculate RMSE and MAE and store in SAS data set. */
				mae=abs_error_sum/_n_;
				rmse=sqrt(square_error_sum/_n_);

				/* Write to SAS log */
				put 'NOTE: ' mae=rmse=;

				/* Store in SAS macro variables */
				call symput('mae', put(mae, 20.10));
				call symput('rmse', put(rmse, 20.10));
			end;
	run;
%mend;










Title 'Linear Regression';
PROC REG data=subsets outest=linreg_summary;weight selected;
	model target=acidindex alcohol chlorides citricacid density fixedacidity 
		freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph 
		/  vif aic bic mse adjrsq stb;
	output out=linreg_scores(where=(selected=0)) predicted=y_linreg residual=resid ucl=ucl lcl=lcl 
		cookd=cook;
	run;
proc print data=linreg_summary(keep=_rmse_ _adjrsq_ _aic_ _bic_);run;
%mae_rmse(linreg_scores, target, y_linreg);

Title 'Linear Regression - Cubed Variables';
PROC REG data=subsets_cubed outest=linreg_cubed_summary; weight selected;
	model target=acidindex alcohol chlorides citricacid density fixedacidity 
		cuberoot_freesulfurdioxide labelappeal cuberoot_residualsugar stars sulphates 
		cuberoot_totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph 
		/  vif aic bic mse adjrsq;
	output out=linreg_cubed_scores(where=(selected=0)) predicted=y_linreg residual=resid ucl=ucl lcl=lcl 
		cookd=cook;
	run;
proc print data=linreg_summary(keep=_rmse_ _adjrsq_ _aic_ _bic_);run;
%mae_rmse(linreg_cubed_scores, target, y_linreg);

Title 'Poisson Regression';
proc genmod data=subsets; weight selected;
	model target=acidindex alcohol chlorides citricacid density fixedacidity 
		freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph/ link=log dist=poi;
	output out=pois_scores(where=(selected=0)) pred=y_poi;
run;
%mae_rmse(pois_scores, target, y_poi);

Title 'Poisson Regression - Cubed Variables';
proc genmod data=subsets_cubed; weight selected;
	model target=acidindex alcohol chlorides citricacid density fixedacidity 
		cuberoot_freesulfurdioxide labelappeal cuberoot_residualsugar stars sulphates 
		cuberoot_totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph / link=log dist=poi;
	output out=pois_cubed_scores(where=(selected=0)) pred=y_poi;
run;
%mae_rmse(pois_cubed_scores, target, y_poi);

Title 'Negative Binomial Regression - Insignificant Predictors Removed';
proc genmod data=subsets; weight selected;
	model target=acidindex alcohol chlorides
		labelappeal stars sulphates 
		totalsulfurdioxide volatileacidity 
		m_stars / link=log dist=nb;
	output out=nb_scores(where=(selected=0)) pred=y_nb;
run;
%mae_rmse(nb_scores, target, y_nb);

Title 'ZIP Regression';
proc genmod data=subsets; weight selected;
	model target=acidindex alcohol chlorides citricacid density 
		fixedacidity freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph/ dist=ZIP link=log;
	zeromodel 	acidindex alcohol chlorides citricacid density 
		fixedacidity freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph /link=logit;
	output out=zip_scores(where=(selected=0)) pred=y_zip pzero=y_zip_zero;
run;

data zip_scores;
	set zip_scores;
	y_zip_score=y_zip*(1-y_zip_zero);
run;
%mae_rmse(zip_scores, target, y_zip_score);

Title 'ZINB Regression';
proc genmod data=subsets; weight selected;
	model target=
		 acidindex alcohol chlorides citricacid density 
		fixedacidity freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph
		 
		 / dist=ZINB link=log;
	zeromodel acidindex alcohol chlorides citricacid density 
		fixedacidity freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph
		
		/ link=logit;
	output out=zinb_scores(where=(selected=0)) pred=y_zinb pzero=y_zinb_zero;
run;

data zinb_scores;
	set zinb_scores;
	y_zinb_score=y_zinb*(1-y_zinb_zero);
run;
%mae_rmse(zinb_scores, target, y_zinb_score);


Title "Decision Tree";
proc hpsplit data=train seed=15531; 
	class m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph;
	 
	model target = acidindex alcohol chlorides citricacid density 
		fixedacidity freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph;
   	prune costcomplexity;
   	output out=DT;
   	code file = "/home/christopherjose21/411 - GLM/Week 7-9/DT_model_wine.sas";
   	
run; 

data DT_scores;
  set test;
  %include "/home/christopherjose21/411 - GLM/Week 7-9/DT_model_wine.sas";
run;
%mae_rmse(DT_scores, target, p_target);










'''
The following models are the same as the above zero inflated models,
except stepwise regression is used for logistic, and true poisson and nb distributions
were used because of having set target_amt = target-1.  I did not use these in the report.    



Title 'Logistic Hurdle Model';
data wine_hurdle;
	set subsets;
	target_flag=(target>0);
	target_amt=target - 1;

	if target_flag=0 then
		target_amt=.;
run;

proc logistic data=wine_hurdle; weight selected;
	model target_flag(ref='0')=acidindex alcohol chlorides citricacid density 
		fixedacidity freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph/ selection=stepwise;
	output out=hurdle_flag_log_scores(where=(selected=0)) pred=y_hurdle_flag;
run;

proc genmod data=wine_hurdle;weight selected;
	model target_amt=acidindex alcohol chlorides citricacid density fixedacidity 
		freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph/ dist=poi link=log;
	output out=hurdle_amt_pois_scores(where=(selected=0)) pred=y_hurdle_amt;
run;

data hurdle_log_pois_scores;
	merge hurdle_flag_log_scores hurdle_amt_pois_scores;
run;

data hurdle_log_pois_scores;
	set hurdle_log_pois_scores;
	y_hurdle_score=y_hurdle_flag*(1+y_hurdle_amt);
run;
%mae_rmse(hurdle_log_pois_scores, target, y_hurdle_score);



Title 'Negative Binomial Hurdle Model';
proc genmod data=wine_hurdle; weight selected;
	model target_amt=acidindex alcohol chlorides citricacid density fixedacidity 
		freesulfurdioxide labelappeal residualsugar stars sulphates 
		totalsulfurdioxide volatileacidity ph 
		
		m_alcohol m_chlorides m_freesulfurdioxide m_residualsugar m_stars 
		m_sulphates m_totalsulfurdioxide m_ph/ dist=nb link=log;
	output out=hurdle_amt_nb_scores(where=(selected=0)) pred=y_hurdle_amt;
run;

data hurdle_log_nb_scores;
	merge hurdle_flag_log_scores hurdle_amt_nb_scores;
run;

data hurdle_log_nb_scores;
	set hurdle_log_nb_scores;
	y_hurdle_score=y_hurdle_flag*(1+y_hurdle_amt);
run;
%mae_rmse(hurdle_log_nb_scores, target, y_hurdle_score);

'''
