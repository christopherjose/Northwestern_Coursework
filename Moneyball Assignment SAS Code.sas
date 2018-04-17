*Chris Jose - Moneyball Code;
%let path=/home/christopherjose21/411 - GLM/Week I;
libname orion "&path"; 

*In scoring the Random Forests, I had to save two files, whose location
will have to be changed:
"/home/christopherjose21/411 - GLM/Week I/model_fit.bin" 
"/home/christopherjose21/411 - GLM/Week I/model_fit2.bin" 
;
data moneyball; 
	set orion.moneyball;
ods graphics on;

proc contents data = moneyball; run; 
proc corr data= moneyball; run;
proc means data=moneyball n nmiss mean median min max;
run;



ods graphics on;
Title 'TARGET_WINS'; 
proc sgplot data=moneyball; hbox TARGET_WINS; run; 
Title 'TARGET_WINS';
proc sgplot data=moneyball; histogram TARGET_WINS; run;
Title 'TEAM_BATTING_H'; 


proc sgplot data=moneyball; hbox TEAM_BATTING_H; run;
Title 'TEAM_BATTING_2B'; 
proc sgplot data=moneyball; hbox TEAM_BATTING_2B; run;
Title 'TEAM_BATTING_3B'; 
proc sgplot data=moneyball; hbox TEAM_BATTING_3B; run;
Title 'TEAM_BATTING_HR'; 
proc sgplot data=moneyball; hbox TEAM_BATTING_HR; run;
Title 'TEAM_BATTING_BB'; 
proc sgplot data=moneyball; hbox TEAM_BATTING_BB; run;
Title 'TEAM_BATTING_SO'; 
proc sgplot data=moneyball; hbox TEAM_BATTING_SO; run;
Title 'TEAM_BASERUN_SB'; 
proc sgplot data=moneyball; hbox TEAM_BASERUN_SB; run;
Title 'TEAM_BASERUN_CS'; 
proc sgplot data=moneyball; hbox TEAM_BASERUN_CS; run;
Title 'TEAM_BATTING_HBP'; 
proc sgplot data=moneyball; hbox TEAM_BATTING_HBP; run;
Title 'TEAM_PITCHING_HR'; 
proc sgplot data=moneyball; hbox TEAM_PITCHING_HR; run;
Title 'TEAM_PITCHING_BB'; 
proc sgplot data=moneyball; hbox TEAM_PITCHING_BB; run;
Title 'TEAM_PITCHING_SO'; 
proc sgplot data=moneyball; hbox TEAM_PITCHING_SO; run;
Title 'TEAM_FIELDING_E'; 
proc sgplot data=moneyball; hbox TEAM_FIELDING_E; run;
Title 'TEAM_FIELDING_DP'; 
proc sgplot data=moneyball; hbox TEAM_FIELDING_DP; run;
ods graphics off;


proc sgplot data=moneyball; histogram TEAM_BATTING_H; run; 
Title 'TEAM_BATTING_2B'; 
proc sgplot data=moneyball; histogram TEAM_BATTING_2B; run;
Title 'TEAM_BATTING_3B'; 
proc sgplot data=moneyball; histogram TEAM_BATTING_3B; run;
Title 'TEAM_BATTING_HR'; 
proc sgplot data=moneyball; histogram TEAM_BATTING_HR; run;
Title 'TEAM_BATTING_BB'; 
proc sgplot data=moneyball; histogram TEAM_BATTING_BB; run;
Title 'TEAM_BATTING_SO'; 
proc sgplot data=moneyball; histogram TEAM_BATTING_SO; run;
Title 'TEAM_BASERUN_SB'; 
proc sgplot data=moneyball; histogram TEAM_BASERUN_SB; run;
Title 'TEAM_BASERUN_CS'; 
proc sgplot data=moneyball; histogram TEAM_BASERUN_CS; run;
Title 'TEAM_BATTING_HBP'; 
proc sgplot data=moneyball; histogram TEAM_BATTING_HBP; run;
Title 'TEAM_PITCHING_HR'; 
proc sgplot data=moneyball; histogram TEAM_PITCHING_HR; run;
Title 'TEAM_PITCHING_BB'; 
proc sgplot data=moneyball; histogram TEAM_PITCHING_BB; run;
Title 'TEAM_PITCHING_SO'; 
proc sgplot data=moneyball; histogram TEAM_PITCHING_SO; run;
Title 'TEAM_FIELDING_E'; 
proc sgplot data=moneyball; histogram TEAM_FIELDING_E; run;
Title 'TEAM_FIELDING_DP'; 
proc sgplot data=moneyball; histogram TEAM_FIELDING_DP; run;
ods graphics off;
*/


*place flags for missing values, so that we can change them to back to missing
after trimming the data;
data moneyball2;
	set moneyball;

M_TEAM_BATTING_SO = '0';
if missing(TEAM_BATTING_SO) then do;
	M_TEAM_BATTING_SO = '1';
	end;

M_TEAM_BASERUN_SB = '0';
if missing(TEAM_BASERUN_SB) then do;
	M_TEAM_BASERUN_SB = '1';
	end;

M_TEAM_BASERUN_CS = '0';
if missing(TEAM_BASERUN_CS) then do;
	M_TEAM_BASERUN_CS = '1';
	end;
	 
M_TEAM_BATTING_HBP = '0';
if missing(TEAM_BATTING_HBP) then do;
	M_TEAM_BATTING_HBP = '1';
	end;
	
M_TEAM_PITCHING_SO = '0';
if missing(TEAM_PITCHING_SO) then do;
	M_TEAM_PITCHING_SO = '1';
	end;

M_TEAM_FIELDING_DP = '0';
if missing(TEAM_FIELDING_DP) then do;
	M_TEAM_FIELDING_DP = '1';
	end;



*Trim outliers
*Calculate IQR and first/third quartiles;
proc means data=moneyball stackods n qrange p25 p75;
ods output summary=ranges;
run;

*create data with outliers to check;
**http://stackoverflow.com/questions/34913022/sas-remove-outliers;
data moneyball3; 
    set moneyball2;
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
lower=p25-1.5*qrange;
upper=p75+1.5*qrange;
string = catt('%cap(dset=moneyball3, var=', variable, ", lower=", lower, ", upper=", upper ,");");
call execute(string);
run;





*change values that started missing back to missing;
data moneyball4;
	set moneyball3;
if M_TEAM_BATTING_SO = '1' then TEAM_BATTING_SO =.;
if M_TEAM_BASERUN_CS = '1' then TEAM_BASERUN_CS =.;
if M_TEAM_BATTING_HBP = '1' then TEAM_BATTING_HBP =.;
if M_TEAM_PITCHING_SO = '1' then TEAM_PITCHING_SO =.;
if M_TEAM_BASERUN_SB = '1' then TEAM_BASERUN_SB =.;
if M_TEAM_FIELDING_DP = '1' then TEAM_FIELDING_DP =.;
run;


*Calculate means for use in imputing missing values;
Title 'trimmed moneyball dataset with missing values';
proc means data=moneyball4 n nmiss mean min max;
run;


*Impute missing values with mean in moneyball4 dataset;
data moneyball5;
set moneyball4;

IMP_TEAM_BATTING_SO = TEAM_BATTING_SO; 
if missing(TEAM_BATTING_SO) then do;
	IMP_TEAM_BATTING_SO = 735.6053358;
	end;
drop TEAM_BATTING_SO;

IMP_TEAM_BASERUN_SB = TEAM_BASERUN_SB;
if missing(TEAM_BASERUN_SB) then do;
	IMP_TEAM_BASERUN_SB = 119.6083916;
	end;
drop TEAM_BASERUN_SB;
	
IMP_TEAM_BASERUN_CS = TEAM_BASERUN_CS; 
if missing(TEAM_BASERUN_CS) then do;
	IMP_TEAM_BASERUN_CS = 51.6542553;
	end;
drop TEAM_BASERUN_CS;
	
IMP_TEAM_BATTING_HBP = TEAM_BATTING_HBP; 
if missing(TEAM_BATTING_HBP) then do;
	IMP_TEAM_BATTING_HBP = 59.3429319;
	end;
drop TEAM_BATTING_HBP;
	
IMP_TEAM_PITCHING_SO = TEAM_PITCHING_SO;
if missing(TEAM_PITCHING_SO) then do;
	IMP_TEAM_PITCHING_SO = 799.1343146; 
	end;
drop TEAM_PITCHING_SO;

IMP_TEAM_FIELDING_DP = TEAM_FIELDING_DP;
if missing(TEAM_FIELDING_DP) then do;
	IMP_TEAM_FIELDING_DP = 146.4537688;  
	end;
drop TEAM_FIELDING_DP;

*Verify that correct variables are in place 
and missing values are gone; 
Title 'Trimmed and mean imputed moneyball data'
proc contents data=moneyball5; run; 

Title 'Transformed Variable Information';
proc means data=moneyball5 n nmiss mean;
run;

Title 'Transformed Variable Correlations';
proc corr data=moneyball5; run;





* Create 70 train split, 30 test split;
proc surveyselect data=moneyball5 outall method=srs samprate = .7 seed=1
	out=subsets ; run;
	
data train;
   set subsets;
   if Selected=1; run;
  
data test; 
   set subsets;
   if Selected=0; run;








*mae and rmse macro that is to be used for pred vs actual tables;
%macro mae_rmse(
        dataset /* Data set which contains the actual and predicted values */, 
        actual /* Variable which contains the actual or observed valued */, 
        predicted /* Variable which contains the predicted value */
        );
%global mae rmse; /* Make the scope of the macro variables global */
data &dataset;
    retain square_error_sum abs_error_sum; 
    set &dataset 
        end=last /* Flag for the last observation */
        ;
    error = &actual - &predicted; /* Calculate simple error */
    square_error = error * error; /* error^2 */
    if _n_ eq 1 then do;
        /* Initialize the sums */
        square_error_sum = square_error; 
        abs_error_sum = abs(error); 
        end;
    else do;
        /* Add to the sum */
        square_error_sum = square_error_sum + square_error; 
        abs_error_sum = abs_error_sum + abs(error);
    end;
    if last then do;
        /* Calculate RMSE and MAE and store in SAS data set. */
        mae = abs_error_sum/_n_;
        rmse = sqrt(square_error_sum/_n_); 
        /* Write to SAS log */
        put 'NOTE: ' mae= rmse=; 
        /* Store in SAS macro variables */
        call symput('mae', put(mae, 20.10)); 
        call symput('rmse', put(rmse, 20.10)); 
        end;
run;
%mend;
 












*Building the models
Running the stepwise method;
*Include the word details after 'BIC MSE' to show each step in the process;
ods graphics on;
Proc Reg data=train outest=Stepwise_Summary;
	Title 'Stepwise Regression - Trimmed, Mean Imputed Data';
	model TARGET_WINS=IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		TEAM_BATTING_HR IMP_TEAM_BATTING_SO IMP_TEAM_FIELDING_DP TEAM_FIELDING_E 
		TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR IMP_TEAM_PITCHING_SO / 
		selection=stepwise slentry=0.01 slstay=0.01 AIC VIF BIC MSE stb details=summary;
	output out=stepwise_out pred=yhat residual=resid ucl=ucl lcl=lcl cookd=cook 
		covratio=cov dffits=dfits press=prss;
	run;
proc print data=Stepwise_Summary; run;


proc score data=test score=Stepwise_Summary out=Stepwise_Scores type=parms;
	var IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_3B TEAM_BATTING_BB 
		TEAM_BATTING_H TEAM_BATTING_HR IMP_TEAM_BATTING_SO IMP_TEAM_FIELDING_DP 
		TEAM_FIELDING_E TEAM_PITCHING_BB IMP_TEAM_PITCHING_SO;
	id TARGET_WINS;
run;

%mae_rmse(Stepwise_Scores, TARGET_WINS, Model1);
%put NOTE: mae=&mae rmse=&rmse; 









*ADJRSQ Methods;
Proc Reg data=train outest=ADJRSQ_Summary;
	Title 'ADJRSQ - Trimmed, Mean Imputed Data ';
	model TARGET_WINS=IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		TEAM_BATTING_HR IMP_TEAM_BATTING_SO IMP_TEAM_FIELDING_DP TEAM_FIELDING_E 
		TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR IMP_TEAM_PITCHING_SO / 
		selection=ADJRSQ AIC VIF BIC MSE stb details=summary;
	output out=ADJRSQ_out pred=yhat residual=resid ucl=ucl lcl=lcl cookd=cook 
		covratio=cov dffits=dfits press=prss;
	run;

proc print data=ADJRSQ_Summary; run;

proc score data=test score=ADJRSQ_Summary (obs=1) out=Adjrsq_Scores type=parms;
	var IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B TEAM_BATTING_3B 
		TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP TEAM_BATTING_HR 
		IMP_TEAM_BATTING_SO IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_BB 
		TEAM_PITCHING_H IMP_TEAM_PITCHING_SO;
	id TARGET_WINS;
run;
%mae_rmse(Adjrsq_Scores, TARGET_WINS, Model1);
%put NOTE: mae=&mae rmse=&rmse;





*ADJRSQ excluding variable with highest VIF;
Proc Reg data=train outest=ADJRSQ2_Summary;
	Title 'ADJRSQ - 1 Variable Removed - Trimmed, Mean Imputed Data ';
	model TARGET_WINS=IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_FIELDING_E IMP_TEAM_BATTING_SO
		TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR IMP_TEAM_PITCHING_SO / 
		selection=ADJRSQ AIC VIF BIC MSE stb details=summary;
	output out=ADJRSQ2_out pred=yhat residual=resid ucl=ucl lcl=lcl cookd=cook 
		covratio=cov dffits=dfits press=prss;
	run;

proc print data=ADJRSQ2_Summary; run;

proc score data=test score=ADJRSQ2_Summary (obs=1) out=Adjrsq2_Scores type=parms;
	var IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B TEAM_BATTING_3B 
		TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP IMP_TEAM_BATTING_SO
		IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_BB 
		TEAM_PITCHING_H IMP_TEAM_PITCHING_SO;
	id TARGET_WINS;
run;
%mae_rmse(Adjrsq2_Scores, TARGET_WINS, Model1);
%put NOTE: mae=&mae rmse=&rmse;








*ADJRSQ excluding variables with 3 highest VIFs;
Proc Reg data=train outest=ADJRSQ3_Summary;
	Title 'ADJRSQ - 3 Variables Removed - Trimmed, Mean Imputed Data ';
	model TARGET_WINS=IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_FIELDING_E 
		TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR IMP_TEAM_PITCHING_SO / 
		selection=ADJRSQ AIC VIF BIC MSE stb details=summary;
	output out=ADJRSQ3_out pred=yhat residual=resid ucl=ucl lcl=lcl cookd=cook 
		covratio=cov dffits=dfits press=prss;
	run;

proc print data=ADJRSQ3_Summary; run;

proc score data=test score=ADJRSQ3_Summary (obs=1) out=Adjrsq3_Scores type=parms;
	var IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B TEAM_BATTING_3B 
		TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_BB 
		TEAM_PITCHING_H IMP_TEAM_PITCHING_SO;
	id TARGET_WINS;
run;
%mae_rmse(Adjrsq3_Scores, TARGET_WINS, Model1);
%put NOTE: mae=&mae rmse=&rmse;






*ADJRSQ excluding variables with 4 highest VIFs;
Proc Reg data=train outest=ADJRSQ4_Summary;
	Title 'ADJRSQ - 4 Variables Removed - Trimmed, Mean Imputed Data ';
	model TARGET_WINS=IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_HR
		TEAM_PITCHING_BB TEAM_PITCHING_H IMP_TEAM_PITCHING_SO / 
		selection=ADJRSQ AIC VIF BIC MSE stb details=summary;
	output out=ADJRSQ4_out pred=yhat residual=resid ucl=ucl lcl=lcl cookd=cook 
		covratio=cov dffits=dfits press=prss;
	run;
proc print data=ADJRSQ4_Summary; run;


proc score data=test score=ADJRSQ4_Summary (obs=1) out=Adjrsq4_Scores type=parms;
	var IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_HR
		TEAM_PITCHING_BB TEAM_PITCHING_H IMP_TEAM_PITCHING_SO;
	id TARGET_WINS;
run;

%mae_rmse(Adjrsq4_Scores, TARGET_WINS, Model1);
%put NOTE: mae=&mae rmse=&rmse;




*ADJRSQ excluding variables with 5 highest VIFs;
Proc Reg data=train outest=ADJRSQ5_Summary;
	Title 'ADJRSQ - 5 Variables Removed - Trimmed, Mean Imputed Data ';
	model TARGET_WINS=IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_PITCHING_HR
		TEAM_PITCHING_BB TEAM_PITCHING_H IMP_TEAM_PITCHING_SO / 
		selection=ADJRSQ AIC VIF BIC MSE stb details=summary;
	output out=ADJRSQ5_out pred=yhat residual=resid ucl=ucl lcl=lcl cookd=cook 
		covratio=cov dffits=dfits press=prss;
	run;

proc print data=ADJRSQ5_Summary; run;

proc score data=test score=ADJRSQ5_Summary (obs=1) out=Adjrsq5_Scores type=parms;
	var IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_H IMP_TEAM_BATTING_HBP 
		IMP_TEAM_FIELDING_DP TEAM_PITCHING_HR
		TEAM_PITCHING_BB TEAM_PITCHING_H IMP_TEAM_PITCHING_SO;
	id TARGET_WINS;
run;

%mae_rmse(Adjrsq5_Scores, TARGET_WINS, Model1);
%put NOTE: mae=&mae rmse=&rmse;











*Random Forests ;
Title 'Random Forests Procedure - Trimmed, Mean Imputed Training Data';
proc hpforest data=train maxtrees=300;
	target TARGET_WINS / level=interval;
	input IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B TEAM_BATTING_3B 
		TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP TEAM_BATTING_HR 
		IMP_TEAM_BATTING_SO IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_BB 
		TEAM_PITCHING_H TEAM_PITCHING_HR IMP_TEAM_PITCHING_SO / level=interval;
	ods output fitstatistics=rf_fitstats;
	save file="/home/christopherjose21/411 - GLM/Week I/model_fit.bin";
run;

proc hp4score data=test;
	id TARGET_WINS;
	score file="/home/christopherjose21/411 - GLM/Week I/model_fit.bin" 
		out=rf_scores;
run;


%mae_rmse(rf_scores, TARGET_WINS, P_TARGET_WINS);
%put NOTE: mae=&mae rmse=&rmse;
















*Random Forests on ALL moneyball data - trimmed and mean imputed;
data mball; 
	set orion.moneyball;
ods graphics on;



*place flags for missing values, so that we can change them to back to missing
after trimming the data;
data mball2;
	set mball;

M_TEAM_BATTING_SO = '0';
if missing(TEAM_BATTING_SO) then do;
	M_TEAM_BATTING_SO = '1';
	end;

M_TEAM_BASERUN_SB = '0';
if missing(TEAM_BASERUN_SB) then do;
	M_TEAM_BASERUN_SB = '1';
	end;

M_TEAM_BASERUN_CS = '0';
if missing(TEAM_BASERUN_CS) then do;
	M_TEAM_BASERUN_CS = '1';
	end;
	 
M_TEAM_BATTING_HBP = '0';
if missing(TEAM_BATTING_HBP) then do;
	M_TEAM_BATTING_HBP = '1';
	end;
	
M_TEAM_PITCHING_SO = '0';
if missing(TEAM_PITCHING_SO) then do;
	M_TEAM_PITCHING_SO = '1';
	end;

M_TEAM_FIELDING_DP = '0';
if missing(TEAM_FIELDING_DP) then do;
	M_TEAM_FIELDING_DP = '1';
	end;
	
M_TEAM_BATTING_H = '0';
if missing(TEAM_BATTING_H) then do;
	M_TEAM_BATTING_H = '1';
	end;	
	
M_TEAM_BATTING_2B = '0';
if missing(TEAM_BATTING_2B) then do;
	M_TEAM_BATTING_2B = '1';
	end;		
	
M_TEAM_BATTING_3B = '0';
if missing(TEAM_BATTING_3B) then do;
	M_TEAM_BATTING_3B = '1';
	end;		

M_TEAM_BATTING_BB = '0';
if missing(TEAM_BATTING_BB) then do;
	M_TEAM_BATTING_BB = '1';
	end;		
	
M_TEAM_BATTING_HR = '0';
if missing(TEAM_BATTING_HR) then do;
	M_TEAM_BATTING_HR = '1';
	end;		

M_TEAM_FIELDING_E = '0';
if missing(TEAM_FIELDING_E) then do;
	M_TEAM_FIELDING_E = '1';
	end;		

M_TEAM_PITCHING_BB = '0';
if missing(TEAM_PITCHING_BB) then do;
	M_TEAM_PITCHING_BB = '1';
	end;		
	
M_TEAM_PITCHING_H = '0';
if missing(TEAM_PITCHING_H) then do;
	M_TEAM_PITCHING_H = '1';
	end;	

M_TEAM_PITCHING_HR = '0';
if missing(TEAM_PITCHING_HR) then do;
	M_TEAM_PITCHING_HR = '1';
	end;	

*Trim outliers
*Calculate IQR and first/third quartiles;
Title 'Moneyball data for Random Forest Procedure';
proc means data=mball stackods n qrange p25 p75;
ods output summary=ranges; 
run;

*create data with outliers to check;
**http://stackoverflow.com/questions/34913022/sas-remove-outliers;
data mball3; 
    set mball2;
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
lower=p25-1.5*qrange;
upper=p75+1.5*qrange;
string = catt('%cap(dset=mball3, var=', variable, ", lower=", lower, ", upper=", upper ,");");
call execute(string);
run;





*change values that started missing back to missing;
data mball4;
	set mball3;
if M_TEAM_BATTING_SO = '1' then TEAM_BATTING_SO =.;
if M_TEAM_BASERUN_CS = '1' then TEAM_BASERUN_CS =.;
if M_TEAM_BATTING_HBP = '1' then TEAM_BATTING_HBP =.;
if M_TEAM_PITCHING_SO = '1' then TEAM_PITCHING_SO =.;
if M_TEAM_BASERUN_SB = '1' then TEAM_BASERUN_SB =.;
if M_TEAM_FIELDING_DP = '1' then TEAM_FIELDING_DP =.;
if M_TEAM_BATTING_H = '1' then TEAM_BATTING_H =.;
if M_TEAM_BATTING_BB = '1' then TEAM_BATTING_BB =.;
if M_TEAM_BATTING_3B = '1' then TEAM_BATTING_3B=.;
if M_TEAM_BATTING_2B = '1' then TEAM_BATTING_2B=.;
if M_TEAM_BATTING_HR = '1' then TEAM_BATTING_HR =.;
if M_TEAM_FIELDING_E = '1' then TEAM_FIELDING_E =.;
if M_TEAM_PITCHING_BB = '1' then TEAM_PITCHING_BB =.;
if M_TEAM_PITCHING_H = '1' then TEAM_PITCHING_H =.;
if M_TEAM_PITCHING_HR = '1' then TEAM_PITCHING_HR =.;
run;





*Only keep variables in the model;
data mball5;
	set mball4;
	keep TARGET_WINS TEAM_BASERUN_CS TEAM_BASERUN_SB TEAM_BATTING_2B 
		TEAM_BATTING_3B TEAM_BATTING_BB TEAM_BATTING_H TEAM_BATTING_HBP 
		TEAM_BATTING_HR TEAM_BATTING_SO TEAM_FIELDING_DP TEAM_FIELDING_E 
		TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_SO INDEX;
	 run; 
	 
	
*Impute means with trimmed means;
proc hpimpute data=mball5 out=mball6;
	input team_baserun_cs team_baserun_sb team_batting_2b team_batting_3b
	team_batting_h team_fielding_dp team_fielding_e team_pitching_hr
	team_pitching_bb team_pitching_h team_batting_hr team_batting_so
	team_batting_hbp team_pitching_so team_batting_bb;
	ID index TARGET_WINS;
	impute team_baserun_cs / method =mean;
	impute team_baserun_sb / method =mean;
	impute team_batting_2b / method =mean;
	impute team_batting_3b/ method =mean;
	impute team_batting_h / method =mean;
	impute team_fielding_dp / method =mean;
	impute team_fielding_e / method =mean;
	impute team_pitching_hr/ method =mean;
	impute team_pitching_bb / method =mean;
	impute team_pitching_h / method =mean;
	impute team_batting_hr / method =mean;
	impute team_batting_so / method = mean;
	impute team_batting_hbp / method=mean;
	impute team_pitching_so / method =mean;
	impute team_batting_bb / method=mean;
run;  
	

*Remove renamed fields and indicator fields;
data mball7;
	set mball6;
	rename  im_team_baserun_cs=team_baserun_cs; 
	rename  im_team_baserun_sb=team_baserun_sb;
    rename im_team_batting_2b= team_batting_2b ;
	rename im_team_batting_3b=team_batting_3b;
	rename im_team_batting_h=team_batting_h ;
	rename im_team_fielding_dp=team_fielding_dp;
	rename im_team_fielding_e=team_fielding_e;
	rename im_team_pitching_hr=team_pitching_hr;
	rename im_team_pitching_bb=team_pitching_bb;
	rename im_team_pitching_h=team_pitching_h;
	rename im_team_batting_hr =team_batting_hr;
	rename im_team_batting_so=team_batting_so ;
	rename im_team_batting_hbp=team_batting_hbp;
	rename im_team_pitching_so=team_pitching_so;
	rename im_team_batting_bb=team_batting_bb;
	drop m_team_baserun_cs m_team_baserun_sb m_team_batting_2b m_team_batting_3b
	 m_team_batting_h m_team_fielding_dp m_team_fielding_e m_team_pitching_hr
	 m_team_pitching_bb m_team_pitching_h m_team_batting_hr m_team_batting_so
	 m_team_batting_hbp m_team_pitching_so m_team_batting_bb;
run;  



data output_rf;
	set mball7;
	rename team_baserun_cs=IMP_TEAM_BASERUN_CS;
	rename team_baserun_sb=IMP_TEAM_BASERUN_sb;
	rename team_batting_hbp=IMP_TEAM_BATTING_HBP;
	rename team_batting_so=IMP_TEAM_BATTING_SO;
	rename team_fielding_dp=IMP_TEAM_fielding_dp;	
	rename team_pitching_so=IMP_TEAM_pitching_so; 
	run;


Title 'Random Forests - All moneyball data, trimmed and mean imputed';
proc hpforest data=output_rf maxtrees=300;
	target TARGET_WINS / level=interval;
	input IMP_TEAM_BASERUN_CS IMP_TEAM_BASERUN_SB TEAM_BATTING_2B TEAM_BATTING_3B 
		TEAM_BATTING_BB TEAM_BATTING_H IMP_TEAM_BATTING_HBP TEAM_BATTING_HR 
		IMP_TEAM_BATTING_SO IMP_TEAM_FIELDING_DP TEAM_FIELDING_E TEAM_PITCHING_BB 
		TEAM_PITCHING_H TEAM_PITCHING_HR IMP_TEAM_PITCHING_SO / level=interval;
	save file="/home/christopherjose21/411 - GLM/Week I/model_fit2.bin";
run;



proc hp4score data=test;
	id index;
	score file="/home/christopherjose21/411 - GLM/Week I/model_fit2.bin" 
		out=rf_scores2;
run;
%mae_rmse(rf_scores2, TARGET_WINS, P_TARGET_WINS);
%put NOTE: mae=&mae rmse=&rmse;



