** Calculate estimates and sampling errors for domains and subdomains;
data gss_wellbeing_1416_derived;
	set wellgss.gss_wellbeing_1416_derived;
	poor_good_well=compress(poor_well)||"_"||compress(good_well);
	dummy=1;
	** Combine loneliness = -0.5 and 0 - now treated the same;
	if full_lonely=-0.5 then full_lonely=0;
run;

/*proc freq data=wellgss.gss_wellbeing_1416_derived; tables full_lonely;run;
proc freq data=gss_wellbeing_1416_derived; tables full_lonely;run;*/

%macro samp_err(nrep);
%do rep=0 %to &nrep.;
ods select none;
proc tabulate data=gss_wellbeing_1416_derived out=table_out(drop=_page_ _table_ _type_);
	var dummy lifesat Purpose hhld_inc eq_hhld_inc pers_inc;
	class year sex region european maori pacific asian agegrp agegrp2 nzdep5 hours family_type
		full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture sum_well sum_well2 poor_well good_well poor_good_well
		full_mwi full_incsuff low_crowd full_cond full_mould full_cold full_physhlth full_menthlth 
		full_feelsafe low_crime low_safeneigh full_friendfam full_lonely full_trustpple full_trustinst low_discrim;
	format full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture  inds.;
	table (ALL year sex region european maori pacific asian agegrp agegrp2 nzdep5 hours family_type)*
		  (full_lifesat full_purpose full_matsol full_health full_house full_education full_social full_safety full_civic full_culture /*sum_well sum_well2 poor_well good_well poor_good_well*/ ALL)
			,(ALL full_mwi full_incsuff low_crowd full_cond full_mould full_cold full_physhlth full_menthlth 
		full_feelsafe low_crime low_safeneigh full_friendfam full_lonely low_discrim full_trustpple full_trustinst)*dummy*SUM 
			  / nocellmerge;
	%if &rep.=0 %then %do;	weight Person_FinalWgt; %end;
	%else %do;	weight Person_FinalWgt_&rep.; %end;
run;
ods select all;

%macro convert_long(dat);
proc contents data=&dat.(drop=dummy_Sum) noprint out=varnames;
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
	if Var1 not in ('agegrp','agegrp2','asian','european','family_type','hours','maori','nzdep5','pacific','region','sex','year')
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
data wellgss.table_out_final2;
	retain Var1 Level1 Var2 Level2 Var3 Level3 value;
	set table_outb;
run;
%end;

%else %do;
data wellgss.table_out_final2;
	merge wellgss.table_out_final2 table_outb(rename=(value=value&rep.));
	by Var1 Level1 Var2 Level2 Var3 Level3;
	z=1;
run;
%end;

%end;
%mend samp_err;

%samp_err(100);

data table_out_final2 err;
	set wellgss.table_out_final2;
	if var2 in ('full_mwi' 'full_incsuff' 'low_crowd' 'full_cond' 'full_mould' 'full_cold' 'full_physhlth' 'full_menthlth' 
		'full_feelsafe' 'low_crime' 'low_safeneigh' 'full_friendfam' 'full_lonely' 'full_trustpple' 'full_trustinst' 'low_discrim')
		then do;
			if var3 ne '' then output err;
			var3=var2;
			level3=level2;
			var2='';
			level2='';
		end;
		output table_out_final2;
run;

proc sort data=table_out_final2 nodup;
	by Var1 Level1 Var2 Level2 Var3 Level3;
run;

** Population totals;
data table_out_final_pop(drop=value: i);
	set table_out_final2;
	where Var1="ALL" and Var2="" and Var3="";
	array val{0:100} value value1-value100;
	array tot{0:100} total total1-total100;
	do i=0 to 100;
		tot{i}=val{i};
	end;	
run;

** Population totals by other vars;
data table_out_all_pop(drop=value: i);
	set table_out_final2;
	where Var1="ALL";
	array val{0:100} value value1-value100;
	array all{0:100} alltot alltot1-alltot100;
	do i=0 to 100;
		all{i}=val{i};
	end;	
run;

** Subpopulation totals;
data table_out_final_subpop(drop=value: i Var2 Level2 Var3 Level3 z);
	set table_out_final2;
	where Var2="" and Var3="";
	array val{0:100} value value1-value100;
	array subtot{0:100} subtotal subtotal1-subtotal100;
	do i=0 to 100;
		subtot{i}=val{i};
	end;	
run;	

** Domain totals;
data table_out_final_dompop(drop=value: i Var3 Level3 z);
	set table_out_final2;
	where Var3="";
	array val{0:100} value value1-value100;
	array domtot{0:100} domtotal domtotal1-domtotal100;
	do i=0 to 100;
		domtot{i}=val{i};
	end;
run;	

** Domain 2 totals;
data table_out_final_dompop2(drop=value: i Var2 Level2 z);
	set table_out_final2;
	where Var2="";
	array val{0:100} value value1-value100;
	array dom2tot{0:100} dom2total dom2total1-dom2total100;
	do i=0 to 100;
		dom2tot{i}=val{i};
	end;
run;	

** Merge subtotals, domain totals, and totals on;
data table_out_final2(drop=z);
	merge table_out_final2 table_out_final_pop;
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
	array ppt2dom{0:100} ppt_2dom ppt_2dom1-ppt_2dom100;
	array pptdiff{0:100} ppt_diff ppt_diff1-ppt_diff100;
	do i=0 to 100;
		pcttot{i}=val{i}/tot{i}*100;
		pctsubtot{i}=val{i}/subtot{i}*100;
		pctrest{i}=(all{i}-val{i})/(tot{i}-subtot{i})*100;
		pptdiff{i}=pctsubtot{i}-pctrest{i};
		pptdom{i}=(val{i}/domtot{i}-((dom2tot{i}-val{i})/(subtot{i}-domtot{i})))*100;
		ppt2dom{i}=(val{i}/dom2tot{i}-((domtot{i}-val{i})/(subtot{i}-dom2tot{i})))*100;
	end;
run;

data wellgss.wellbeing_1416_table_se2(keep=Var1 Level1 Var2 Level2 Var3 Level3 sum se_sum total pct_tot se_pct_tot subtotal pct_subtot domtotal dom2total ppt_dom se_ppt_dom ppt_dom2 se_ppt_dom2 restsum restpop pct_rest se_pct_subtot ppt_diff se_ppt_diff);
	retain Var1 Level1 Var2 Level2 Var3 Level3 sum se_sum total pct_tot se_pct_tot subtotal domtotal dom2total ppt_dom se_ppt_dom pct_subtot se_pct_subtot restsum restpop pct_rest ppt_diff se_ppt_diff;
	set table_out_final_calc;
	array stat1{0:100} value value1-value100;
	array stat2{0:100} pct_tot pct_tot1-pct_tot100;
	array stat3{0:100} pct_subtot pct_subtot1-pct_subtot100;
	array stat4{0:100} ppt_dom ppt_dom1-ppt_dom100;
	array stat5{0:100} ppt_diff ppt_diff1-ppt_diff100;
	array stat6{0:100} ppt_2dom ppt_2dom1-ppt_2dom100;
	sum=value;
	ss_sum=0;
	ss_ptot=0;
	ss_psubtot=0;
	ss_ppt=0;
	ss_ppt2=0;
	ss_ppt3=0;
	do i=1 to 100;
		ss_sum+(stat1{i}-stat1{0})**2;
		ss_ptot+(stat2{i}-stat2{0})**2;
		ss_psubtot+(stat3{i}-stat3{0})**2;
		ss_ppt+(stat4{i}-stat4{0})**2;
		ss_ppt2+(stat5{i}-stat5{0})**2;
		ss_ppt3+(stat6{i}-stat6{0})**2;
	end;
	var_sum=ss_sum*(100-1)/100;
	var_ptot=ss_ptot*(100-1)/100;
	var_psubtot=ss_psubtot*(100-1)/100;
	var_ppt=ss_ppt*(100-1)/100;
	var_ppt2=ss_ppt2*(100-1)/100;
	var_ppt3=ss_ppt3*(100-1)/100;
	se_sum=1.96*sqrt(var_sum);
	se_pct_tot=1.96*sqrt(var_ptot);
	se_pct_subtot=1.96*sqrt(var_psubtot);
	se_ppt_dom=1.96*sqrt(var_ppt);
	se_ppt_diff=1.96*sqrt(var_ppt2);
	se_ppt_dom2=1.96*sqrt(var_ppt3);

	restsum=alltot-value;
	restpop=total-subtotal;
run;

** And we need to suppress any value of less than a thousand;
** And round all weighted values to a thousand;
** And then re-calculate percents and ppt differences;
data wellbeing_1416_table_se_out(drop=i);
	set wellgss.wellbeing_1416_table_se2;
	by Var1 Level1 Var2 Level2 Var3 Level3;
	array countvars{*} sum subtotal total restpop restsum domtotal dom2total;
	do i=1 to dim(countvars);
		** Suppress weighted unrounded counts of less than a thousand - and the associated se;
		if countvars{i}<1000 then countvars{i}=.;
	end;
	** And now recalculate percentages etc. ;
	pct_tot=sum/total*100;
	pct_subtot=sum/subtotal*100;
	pct_rest=restsum/restpop*100;
	ppt_dom=(sum/domtotal-(dom2total-sum)/(subtotal-domtotal))*100;
	ppt_diff=pct_subtot-pct_rest;
	ppt_dom2=(sum/dom2total-(domtotal-sum)/(subtotal-dom2total))*100;
	do i=1 to dim(countvars);
		** Finally round the remaining counts to the nearest 1000;
		countvars{i}=round(countvars{i},1000);
	end;
	** Suppress the SE if the underlying estimnate is suppressed;
	if sum=. then se_sum=.;
	if pct_tot=. then se_pct_tot=.;
	if pct_subtot=. then se_pct_subtot=.;
	if ppt_dom=. then se_ppt_dom=.;
	if ppt_dom2=. then se_ppt_dom2=.;
	if ppt_diff=. then se_ppt_diff=.;
	format pct_tot pct_subtot pct_rest ppt_diff ppt_dom ppt_dom2 se_pct_tot se_pct_subtot se_ppt_dom se_ppt_dom2 se_ppt_diff 6.2 se_sum 7.;
run;

proc export data=wellbeing_1416_table_se_out 
outfile="\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\Tables\Supplementary subdomain GSS tables 2014 2016 with std errors.csv" dbms=csv replace;
run;