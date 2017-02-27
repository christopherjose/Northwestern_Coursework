%let path=/home/christopherjose21/411 - GLM/Week 4-6;
libname orion "&path";


data insurance; 
	set orion.logit_insurance_test;
	run;

ods graphics on; 




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
keep index car_type car_use education job mstatus	
	parent1 red_car revoked sex urbanicity;
	if missing(job) then job="NA";
	run;


data insurance_missing_flags;
set insurance3;
keep index m_kidsdriv m_age m_homekids m_yoj m_income m_home_val m_travtime 
	m_bluebook m_tif m_oldclaim m_clm_freq m_mvr_pts m_car_age; run;

data insurance_numeric;
	set insurance3;
	keep index kidsdriv age homekids yoj tif clm_freq mvr_pts car_age
	income home_val travtime bluebook oldclaim;
run;



*Final Dataset with imputed data;
data insurance_final;
	merge insurance_categorical insurance_missing_flags insurance_numeric;
	run;



data output;
set insurance_final;
log_odds=
-1.313	
+0.1786*kidsdriv
-0.0245*tif
-0.00000516*bluebook
-1.0763*(CAR_TYPE in ("Minivan"))
-0.8912*(CAR_TYPE in ("Panel Truck"))
-0.4397*(CAR_TYPE in ("Pickup"))
+0.246*(CAR_TYPE in ("Sports Car"))
-0.6013*(CAR_TYPE in ("Van"))
+0.8852*(CAR_USE in ("Commercial"))
+0.0776*(EDUCATION in ("<High School"))
-0.5106*(EDUCATION in ("Bachelors"))
-0.4599*(EDUCATION in ("Masters"))
-0.5721*(EDUCATION in ("PhD"))
+0.2298*(Job in ("Clerical"))
-0.6625*(Job in ("Doctor"))
+0.2639*(Job in ("Home Maker"))
-0.2768*(Job in ("Lawyer"))
-0.9833*(Job in ("Manager"))
-0.4263*(Job in ("NA"))
-0.1363*(Job in ("Professional"))
+0.2681*(Job in ("Student"))
-0.4969*(MSTATUS in ("Yes"))
-0.5919*(PARENT1 in ("No"))
-0.7624*(REVOKED in ("No"))
+0.3005*(SEX in ("M"))
+2.5384*(URBANICITY in ("Highly Urban/ Urban"));
odds=exp(log_odds);
if missing(odds) then odds = 99999;
p_target_flag = odds / (1 +odds);
keep index log_odds odds p_target_flag;
run;

proc print data=output NOOBS; var index p_target_flag; run; 
