%let path=/home/christopherjose21/411 - GLM/Week 7-9;
libname orion "&path"; 


data wine; 
	set orion.wine_test;run;
ods graphics on;





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


IMP_STARS=STARS; m_STARS=0; if missing(stars) then do; imp_stars = 2; m_stars =1;	end;
	run;



data wine_nontrim;
	set wine_imp;
	keep index labelappeal m_acidindex m_alcohol m_chlorides m_citricacid 
		m_density m_fixedacidity m_freesulfurdioxide m_labelappeal m_residualsugar 
		m_sulphates m_totalsulfurdioxide m_volatileacidity m_ph stars imp_stars m_stars;
run;


data wine_trim1;
	set wine_imp;
	drop index labelappeal m_acidindex m_alcohol m_chlorides m_citricacid 
		m_density m_fixedacidity m_freesulfurdioxide m_labelappeal m_residualsugar 
		m_sulphates m_totalsulfurdioxide m_volatileacidity m_ph stars imp_stars m_stars;
run;










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




data wine_prefinal; merge wine_trim wine_nontrim; 





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





data output;
set wine_final;
logpois=
1.3513	
-0.0184*AcidIndex
+0.0074*ALCOHOL
-0.0206*CHLORIDES
+0.0002*CitricAcid
-0.2098*Density
+0.2378*LabelAppeal
-0.0001*RESIDUALSUGAR
+0.1045*STARS
+0.0001*SULPHATES
-0.0112*VolatileAcidity
+0.0064*PH
+0.0062*m_ALCOHOL
-0.0063*m_CHLORIDES
-0.0006*m_FREESULFURDIOXIDE
+0.0199*m_RESIDUALSUGAR
-0.1814*m_STARS
+0.0032*m_SULPHATES
+0.0001*m_PH;
amt=exp(logpois);


zeroscore=
-1.7327	
+0.4674*AcidIndex
+0.0316*ALCOHOL
+0.1301*CHLORIDES
-0.0505*CitricAcid
-1.1722*Density
+0.0056*FixedAcidity
-0.0005*FREESULFURDIOXIDE
+0.7513*LabelAppeal
-0.0009*RESIDUALSUGAR
-3.665*STARS
+0.1822*SULPHATES
-0.0011*totalsulfurdioxide
+0.2227*VolatileAcidity
+0.2272*PH
-0.2556*m_ALCOHOL
+0.1539*m_CHLORIDES
-0.3095*m_FREESULFURDIOXIDE
+0.0961*m_RESIDUALSUGAR
+5.7779*m_STARS
+0.0873*m_SULPHATES
+0.2136*m_PH;
zeroscore = exp(zeroscore) / (1+exp(zeroscore));
p_target=amt*(1-zeroscore)	;
run;


proc print data=output NOOBS; var index p_target; run; 







