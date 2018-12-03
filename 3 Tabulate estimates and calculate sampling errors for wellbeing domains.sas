**********************************************************************************************************************************;
** Our People - Code to produce wellbeing measures for a multidimensional wellbeing analysis based on the Living Standards Framework
**
** File: 2 Create derived variables and tabulate results.sas
** Description:  Create derived variables and produce initial tabulations of wellbeing domains by population characteristics
**
** Code created: March 2018
** Code last edited: November 2018
**
** Author: Keith McLeod, A&I, The Treasury. 
**
**********************************************************************************************************************************;
** Calculate estimates and sampling errors for domain level comparisons;
proc format;
	value xround1000x
		low-high = [xround1000()]
		;
run;

options mprint;
data gss_wellbeing_1416_derived;
	set wellgss.gss_wellbeing_1416_derived;
	array vars1{*} full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well poor_well good_well;
	array vars2{*} full_lifesatzz full_purposezz full_matsolzz full_healthzz full_housezz full_educationzz full_socialzz full_safetyzz full_civiczz full_culturezz sum_wellzz poor_wellzz good_wellzz;
	do i = 1 to dim(vars1);
		vars2{i}=vars1{i};
	end;
	sum_well2zz=sum_well2;
	sum_well3zz=sum_well3;
	dummy=1;
run;

%macro run_all(year);
%macro samp_err(nrep);
%do rep=0 %to &nrep.;
ods select none;
proc tabulate data=gss_wellbeing_1416_derived out=table_out(drop=_page_ _table_ _type_);
	var dummy lifesat Purpose hhld_inc eq_hhld_inc pers_inc;
	class year sex region european maori pacific asian agegrp nzdep5 hours hours_age family_type
		full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 sum_well3 poor_well good_well
		full_lifesatzz full_purposezz full_matsolzz full_healthzz full_housezz full_educationzz full_socialzz full_safetyzz full_civiczz full_culturezz sum_wellzz sum_well2zz sum_well3zz poor_wellzz good_wellzz ;
	format full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture 
		   full_lifesatzz full_purposezz full_matsolzz full_healthzz full_housezz full_educationzz full_socialzz full_safetyzz full_civiczz full_culturezz inds.;
	tables (ALL year sex region european maori pacific asian agegrp nzdep5 hours hours_age family_type)*
		   (full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 sum_well3 poor_well good_well ALL),
 		   (ALL full_lifesatzz full_purposezz full_matsolzz full_healthzz full_housezz full_educationzz full_socialzz full_safetyzz full_civiczz full_culturezz sum_wellzz sum_well2zz sum_well3zz poor_wellzz good_wellzz )*dummy*SUM;
	%if &rep.=0 %then %do;	weight Person_FinalWgt; %end;
	%else %do;	weight Person_FinalWgt_&rep.; %end;
	%if &year. ne both %then %do;
		where year=&year.;
	%end;
run;
ods select all;

%macro convert_long(dat);
proc contents data=&dat.(drop=dummy_Sum) noprint out=varnames;
run;
proc sort data=varnames nodupkey; by name;run;
proc sort data=varnames; by varnum;run;
data varnames;
	set varnames(keep=name);
	name2=name;
	call symput('numvars',_n_);
run;
** Reformat categories as characters;
%do i=1 %to &numvars.;
data _null_;
	set varnames;
	if _n_=&i. then do;
		call symputx('var',name);
	end;
run;

data &dat(drop=&var. rename=(&var.b=&var.));
	length variable $ 120;
	set &dat;
	&var.b=vvalue(&var.);
	if left(&var.b)='.' then &var.b='';
	if &var.b ne '' then variable=strip(variable)||"#"||"&var."||"*"||strip(&var.b);
run;
%end;

data &dat.b(keep=Var1-Var3 Level1-Level3 dummy_Sum rename=(dummy_sum=value));
	set &dat;
	variable=tranwrd(variable,"zz","");
	v1=scan(variable,1,'#');
	v2=scan(variable,2,'#');
	v3=scan(variable,3,'#');
	Var1=scan(v1,1,'*');
	Var2=scan(v2,1,'*');
	Var3=scan(v3,1,'*');
	Level1=scan(v1,2,'*');
	Level2=scan(v2,2,'*');
	Level3=scan(v3,2,'*');
	if Var1 not in ('agegrp','asian','european','family_type','hours','hours_age','maori','nzdep5','pacific','region','sex','year')
			then do;
		Var3=Var2;
		Var2=Var1;
		Var1="ALL";
		Level3=Level2;
		Level2=Level1;
		Level1="ALL";	
	end;
run;
%mend convert_long;
%convert_long(table_out);

proc sort data=table_outb;
	by Var1 Level1 Var2 Level2 Var3 Level3;
run;

%if &rep.=0 %then %do;
data wellgss.table_out_final_&year.;
	retain Var1 Level1 Var2 Level2 Var3 Level3 value;
	set table_outb;
run;
%end;

%else %do;
data wellgss.table_out_final_&year.;
	merge wellgss.table_out_final_&year. table_outb(rename=(value=value&rep.));
	by Var1 Level1 Var2 Level2 Var3 Level3;
	z=1;
run;
%end;

%end;
%mend samp_err;

%samp_err(100);

proc sort data=wellgss.table_out_final_&year. nodup;
	by Var1 Level1 Var2 Level2 Var3 Level3;
run;

** Population totals;
data table_out_final_pop(drop=value: i);
	set wellgss.table_out_final_&year.;
	where Var1="ALL" and Var2="" and Var3="";
	array val{0:100} value value1-value100;
	array tot{0:100} total total1-total100;
	do i=0 to 100;
		tot{i}=val{i};
	end;	
run;

** Population totals by other vars;
data table_out_all_pop(drop=value: i);
	set wellgss.table_out_final_&year.;
	where Var1="ALL";
	array val{0:100} value value1-value100;
	array all{0:100} alltot alltot1-alltot100;
	do i=0 to 100;
		all{i}=val{i};
	end;	
run;

** Subpopulation totals;
data table_out_final_subpop(drop=value: i Var2 Level2 Var3 Level3 z);
	set wellgss.table_out_final_&year.;
	where Var2="" and Var3="";
	array val{0:100} value value1-value100;
	array subtot{0:100} subtotal subtotal1-subtotal100;
	do i=0 to 100;
		subtot{i}=val{i};
	end;	
run;	

** Domain totals;
data table_out_final_dompop(drop=value: i Var3 Level3 z);
	set wellgss.table_out_final_&year.;
	where Var3="";
	array val{0:100} value value1-value100;
	array domtot{0:100} domtotal domtotal1-domtotal100;
	do i=0 to 100;
		domtot{i}=val{i};
	end;
run;	

** Domain 2 totals;
data table_out_final_dompop2(drop=domtot:);
	set table_out_final_dompop(rename=(Var2=Var3 Level2=Level3));
	array domtot{0:100} domtotal domtotal1-domtotal100;
	array dom2tot{0:100} dom2total dom2total1-dom2total100;
	do i=0 to 100;
		dom2tot{i}=domtot{i};
	end;
run;

** Merge subtotals, domain totals, and totals on;
data table_out_final2(drop=z);
	merge wellgss.table_out_final_&year. table_out_final_pop;
	by z;
run;

data table_out_final2;
	merge table_out_final2 table_out_final_subpop;
	by Var1 Level1;
run;

proc sort data=table_out_final_dompop;
	by Var1 Level1 Var2 Level2;
run;
proc sort data=table_out_final2;
	by Var1 Level1 Var2 Level2;
run;
data table_out_final2;
	merge table_out_final2 table_out_final_dompop;
	by Var1 Level1 Var2 Level2;
run;

proc sort data=table_out_final_dompop2;
	by Var1 Level1 Var3 Level3;
run;
proc sort data=table_out_final2;
	by Var1 Level1 Var3 Level3;
run;
data table_out_final2;
	merge table_out_final2 table_out_final_dompop2;
	by Var1 Level1 Var3 Level3;
run;

proc sort data=table_out_final2;
	by Var2 Level2 Var3 Level3;
run;
data table_out_final2;
	merge table_out_final2 table_out_all_pop;
	by Var2 Level2 Var3 Level3;
run;

proc sort data=table_out_final2;
	by Var1 Level1 Var2 Level2 Var3 Level3;
run;

** And sampling errors of estimates of the percentage of the population;
** And sampling errors of estimates of the percentage of the sub-population;
** And sampling errors of estimates of percentage point differences between the sub-population and the rest of the population;
data table_out_final_calc(drop=i total1-total100 subtotal1-subtotal100 alltot1-alltot100 domtotal1-domtotal100 dom2total1-dom2total100);
	set table_out_final2;
	array val{0:100} value value1-value100;
	array subtot{0:100} subtotal subtotal1-subtotal100;
	array tot{0:100} total total1-total100;
	array all{0:100} alltot alltot1-alltot100;
	array domtot{0:100} domtotal domtotal1-domtotal100;
	array dom2tot{0:100} dom2total dom2total1-dom2total100;
	array pcttot{0:100} pct_tot pct_tot1-pct_tot100;
	array pctsubtot{0:100} pct_subtot pct_subtot1-pct_subtot100;
	array pctrest{0:100} pct_rest pct_rest1-pct_rest100;
	array pptdom{0:100} ppt_dom ppt_dom1-ppt_dom100;
	array pptdiff{0:100} ppt_diff ppt_diff1-ppt_diff100;
	do i=0 to 100;
		pcttot{i}=val{i}/tot{i}*100;
		pctsubtot{i}=val{i}/subtot{i}*100;
		pctrest{i}=(all{i}-val{i})/(tot{i}-subtot{i})*100;
		pptdiff{i}=pctsubtot{i}-pctrest{i};
		pptdom{i}=(val{i}/domtot{i}-((dom2tot{i}-val{i})/(subtot{i}-domtot{i})))*100;
	end;
run;

data wellgss.wellbeing_1416_table_se_&year.(keep=Var1 Level1 Var2 Level2 Var3 Level3 sum se_sum total pct_tot se_pct_tot subtotal pct_subtot domtotal dom2total ppt_dom se_ppt_dom restsum restpop pct_rest se_pct_subtot ppt_diff se_ppt_diff);
	retain Var1 Level1 Var2 Level2 Var3 Level3 sum se_sum total pct_tot se_pct_tot subtotal domtotal dom2total ppt_dom se_ppt_dom pct_subtot se_pct_subtot restsum restpop pct_rest ppt_diff se_ppt_diff;
	set table_out_final_calc;
	array stat1{0:100} value value1-value100;
	array stat2{0:100} pct_tot pct_tot1-pct_tot100;
	array stat3{0:100} pct_subtot pct_subtot1-pct_subtot100;
	array stat4{0:100} ppt_dom ppt_dom1-ppt_dom100;
	array stat5{0:100} ppt_diff ppt_diff1-ppt_diff100;
	sum=value;
	ss_sum=0;
	ss_ptot=0;
	ss_psubtot=0;
	ss_ppt=0;
	ss_ppt2=0;
	do i=1 to 100;
		ss_sum+(stat1{i}-stat1{0})**2;
		ss_ptot+(stat2{i}-stat2{0})**2;
		ss_psubtot+(stat3{i}-stat3{0})**2;
		ss_ppt+(stat4{i}-stat4{0})**2;
		ss_ppt2+(stat5{i}-stat5{0})**2;
	end;
	var_sum=ss_sum*(100-1)/100;
	var_ptot=ss_ptot*(100-1)/100;
	var_psubtot=ss_psubtot*(100-1)/100;
	var_ppt=ss_ppt*(100-1)/100;
	var_ppt2=ss_ppt2*(100-1)/100;
	se_sum=1.96*sqrt(var_sum);
	se_pct_tot=1.96*sqrt(var_ptot);
	se_pct_subtot=1.96*sqrt(var_psubtot);
	se_ppt_dom=1.96*sqrt(var_ppt);
	se_ppt_diff=1.96*sqrt(var_ppt2);

	restsum=alltot-value;
	restpop=total-subtotal;
run;

** And we need to suppress any value of less than a thousand;
** And round all weighted values to a thousand;
** And then re-calculate percents and ppt differences;
data wellgss.wellbeing_1416_table_se_out_&year.(drop=i);
	set wellgss.wellbeing_1416_table_se_&year.;
	by Var1 Level1 Var2 Level2 Var3 Level3;
	array countvars{*} sum subtotal total restpop restsum domtotal dom2total;
	do i=1 to dim(countvars);
		** Suppress weighted unrounded counts of less than a thousand;
		if countvars{i}<1000 then countvars{i}=.;
	end;
	** And now recalculate percentages etc. ;
	pct_tot=sum/total*100;
	pct_subtot=sum/subtotal*100;
	pct_rest=restsum/restpop*100;
	ppt_dom=(sum/domtotal-(dom2total-sum)/(subtotal-domtotal))*100;
	ppt_diff=pct_subtot-pct_rest;
	do i=1 to dim(countvars);
		** Finally round the remaining counts to the nearest 1000;
		countvars{i}=round(countvars{i},1000);
	end;
	if sum=. then se_sum=.;
	if pct_tot=. then se_pct_tot=.;
	if pct_subtot=. then se_pct_subtot=.;
	if ppt_dom=. then se_ppt_dom=.;
	if ppt_diff=. then se_ppt_diff=.;
	format pct_tot pct_subtot pct_rest ppt_diff ppt_dom 6.2 se_pct_tot se_pct_subtot se_ppt_dom se_ppt_diff 6.2 se_sum 7.;
run;
%mend run_all;

*%run_all(2014);
*%run_all(2016);
%run_all(both);

data wellgss.wellbeing_1416_table_se_out;
	set wellgss.wellbeing_1416_table_se_out_2014(in=a) wellgss.wellbeing_1416_table_se_out_2016(in=b) wellgss.wellbeing_1416_table_se_out_both(in=c);
	if a then year='2014';
	else if b then year='2016';
	else if c then year='9999';
run;


/*proc export data=wellgss.wellbeing_1416_table_se_out
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 with std errors.csv" dbms=csv replace;
run;*/

** Break into 5 for checking;
proc export data=wellgss.wellbeing_1416_table_se_out(obs=90000)
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 with std errors 1 of 5.csv" dbms=csv replace;
run;
proc export data=wellgss.wellbeing_1416_table_se_out(firstobs=90001 obs=180000)
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 with std errors 2 of 5.csv" dbms=csv replace;
run;
proc export data=wellgss.wellbeing_1416_table_se_out(firstobs=180001 obs=270000)
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 with std errors 3 of 5.csv" dbms=csv replace;
run;
proc export data=wellgss.wellbeing_1416_table_se_out(firstobs=270001 obs=360000)
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 with std errors 4 of 5.csv" dbms=csv replace;
run;
proc export data=wellgss.wellbeing_1416_table_se_out(firstobs=360001)
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 with std errors 5 of 5.csv" dbms=csv replace;
run;

***********************************************************************************************************************;
** And now we want to calculate estimates and sampling errors for mean subjective wellbeing and income scores;
***********************************************************************************************************************;
%macro samp_err(nrep);
%do rep=0 %to &nrep.;
ods select none;
proc tabulate data=gss_wellbeing_1416_derived out=table_out(drop=_page_ _table_ _type_);
	var dummy lifesat Purpose hhld_inc eq_hhld_inc pers_inc;
	class year sex region european maori pacific asian agegrp nzdep5 hours hours_age family_type
		full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 sum_well3 poor_well good_well poor_good_well;
	format full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture inds.;
	tables (ALL year sex region european maori pacific asian agegrp nzdep5 hours hours_age family_type)*
		   (full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 sum_well3 poor_well good_well poor_good_well ALL),
 		   (lifesat Purpose hhld_inc eq_hhld_inc pers_inc)*MEAN;
	%if &rep.=0 %then %do;	weight Person_FinalWgt; %end;
	%else %do;	weight Person_FinalWgt_&rep.; %end; *weight Person_FinalWgt;
run;

** Produce unrounded counts for confidentiality purposes;
%if &rep.=0 %then %do;	
proc tabulate data=gss_wellbeing_1416_derived out=mean_counts(drop=_page_ _table_ _type_);
	var dummy lifesat Purpose hhld_inc eq_hhld_inc pers_inc;
	class year sex region european maori pacific asian agegrp nzdep5 hours hours_age family_type
		full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 sum_well3 poor_well good_well poor_good_well;
	format full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture inds.;
	tables (ALL year sex region european maori pacific asian agegrp nzdep5 hours hours_age family_type)*
		   (full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 sum_well3 poor_well good_well poor_good_well ALL)*N;
run;

data table_out;
	merge table_out mean_counts(keep=N);
run;
%end;

ods select all;

%macro convert_long(dat);
proc contents data=&dat.(drop=lifesat_mean Purpose_mean hhld_inc_mean eq_hhld_inc_mean pers_inc_mean %if &rep.=0 %then %do;	N %end;) noprint out=varnames;
run;
data varnames;
	set varnames;
run;
proc sort data=varnames nodupkey; by name;run;
proc sort data=varnames; by varnum;run;
data varnames;
	set varnames(keep=name);
	name2=name;
	call symput('numvars',_n_);
run;
** Reformat categories as characters;
%do i=1 %to &numvars.;
data _null_;
	set varnames;
	if _n_=&i. then do;
		call symputx('var',name);
	end;
run;

data &dat(drop=&var. rename=(&var.b=&var.));
	length variable $ 120;
	set &dat;
	&var.b=vvalue(&var.);
	if left(&var.b)='.' then &var.b='';
	if &var.b ne '' then variable=strip(variable)||"#"||"&var."||"*"||strip(&var.b);
run;
%end;

data &dat.b(keep=Var1 Var2 Level1 Level2 outvar value %if &rep.=0 %then %do; N %end;);
	set &dat;
	variable=tranwrd(variable,"zz","");
	v1=scan(variable,1,'#');
	v2=scan(variable,2,'#');
	Var1=scan(v1,1,'*');
	Var2=scan(v2,1,'*');
	Level1=scan(v1,2,'*');
	Level2=scan(v2,2,'*');
	if Var1 not in ('agegrp','asian','european','family_type','hours','hours_age','maori','nzdep5','pacific','region','sex','year')
			then do;
		Var2=Var1;
		Var1="ALL";
		Level2=Level1;
		Level1="ALL";	
	end;
	value=lifesat_mean; 
	outvar='Life satisfaction     ';
	output;	
	value=Purpose_mean;
	outvar='Purpose';
	output; 
	value=hhld_inc_mean;
	outvar='Household income';
	output;
	value=eq_hhld_inc_mean;
	outvar='Equiv household income';
	output;
	value=pers_inc_mean;
	outvar='Personal income';
	output;
run;
%mend convert_long;
%convert_long(table_out);

proc sort data=table_outb;
	by Var1 Level1 Var2 Level2 outvar;
run;

%if &rep.=0 %then %do;
data wellgss.table_out_finalb;
	retain Var1 Level1 Var2 Level2 outvar value;
	set table_outb;
run;
%end;

%else %do;
data wellgss.table_out_finalb;
	merge wellgss.table_out_finalb table_outb(rename=(value=value&rep.));
	by Var1 Level1 Var2 Level2 outvar;
	z=1;
run;
%end;

%end;
%mend samp_err;

%samp_err(100);

** Calculate sampling errors and suppress statistics where fewer than 20 respondents contributed to the numbers;
data wellgss.wellbeing_1416_table_mean_se(keep=Var1 Level1 Var2 Level2 outvar value se_value N);
	retain Var1 Level1 Var2 Level2 outvar value se_value;
	set wellgss.table_out_finalb;
	array stat1{0:100} value value1-value100;
	ss_value=0;
	do i=1 to 100;
		ss_value+(stat1{i}-stat1{0})**2;
	end;
	var_value=ss_value*(100-1)/100;
	se_value=1.96*sqrt(var_value);
	if N < 20 then do;
		value=.;
		se_value=.;
	end;
run;

proc export data=wellgss.wellbeing_1416_table_mean_se(keep=Var1 Level1 Var2 Level2 outvar value se_value N) 
	outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 MEANS with std errors - with counts.csv" dbms=csv replace;
run;

proc export data=wellgss.wellbeing_1416_table_mean_se(drop=N)
	outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Final GSS tables 2014 2016 MEANS with std errors.csv" dbms=csv replace;
run;