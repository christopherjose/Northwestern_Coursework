*Chris Jose - Standalone program code;
*This program ony works if the RF model, based on all data,
is created using the other program (Moneyball Code), since the output
of this model is referenced in this program via:
score file="/home/christopherjose21/411 - GLM/Week I/model_fit2.bin" ;


%let path=/home/christopherjose21/411 - GLM/Week I;
libname orion "&path"; 

data mball; 
	set orion.moneyball_test;
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
	ID index;
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





proc hp4score data=output_rf;
	id index;
	score file="/home/christopherjose21/411 - GLM/Week I/model_fit2.bin" 
		out=scores;
run;

proc print data=work.scores;run;






/*
'Code for other models I created;
data output;
    set moneyball7;
  	P_TARGET_WINS =
  	  18.47341 + .04267*team_baserun_sb + .13036*team_batting_3b +.02071*team_batting_bb + .04066*team_batting_h - .10881*team_fielding_dp -.03973*team_fielding_e + .04169*team_pitching_hr;
    keep INDEX P_TARGET_WINS;
    run;
proc print data=output; run; 



3 vars removed
P_TARGET_WINS=
20.49355-.06299*team_baserun_cs+.05205*team_baserun_sb+.13116*team_batting_3b+.03872*team_batting_bb+.03051*team_batting_h-.10904*team_fielding_dp-.05448*team_fielding_e-.01763*team_pitching_bb+.01236*team_pitching_h+.03169*team_pitching_hr;

4 vars removed
  	   24.60472 -0.06782 * team_baserun_cs + .05428*team_baserun_sb - .01141*team_batting_2b + .13545*team_batting_3b + .04173*team_batting_h - .10252*team_fielding_dp - .06023*team_fielding_e + .03435*team_pitching_hr + .01258*team_pitching_bb + .00291*team_pitching_h;

stepwise
18.47341 + .04267*team_baserun_sb + .13036*team_batting_3b +.02071*team_batting_bb + .04066*team_batting_h - .10881*_team_fielding_dp -.03973*team_fielding_e + .04169*team_pitching_hr;








