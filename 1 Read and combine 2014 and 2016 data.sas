**********************************************************************************************************************************;
** Our People - Code to produce wellbeing measures for a multidimensional wellbeing analysis based on the Living Standards Framework
**
** File: 1 Read and combine 2014 and 2016 data.sas
** Description: Read in the 2014 and 2016 GSS personal data, transform to idi variable names, and combine the data-sets. 
**		Calculate equivalised income.
**
** Code created: March 2018
** Code last edited: November 2018
**
** Author: Keith McLeod, A&I, The Treasury. Final data step is based on Eric Krassoi Peach's code (MSD). 
**
** Notes: Analysis uses standalone GSS datasets, not IDI-linked datasets as 2016 data was not in the IDI when this project started.
**
**********************************************************************************************************************************;

**********************************************************************************************************************************;
** Read in the 2014 and 2016 personal data, transform to idi variable names, and combine the data-sets
**********************************************************************************************************************************;

** Make all personal and household data long form so we can standardise their variable names;
** 2014 personal data - numeric vars;
proc transpose data=gss14.gss2014pq_usable_cases out=longpq_14_num(drop=_label_ rename=(col1=value _name_=var2014));
	by household_code person_code;
	var _numeric_;
run;

** 2014 personal data - character vars;
proc transpose data=gss14.gss2014pq_usable_cases out=longpq_14_char(drop=_label_ rename=(col1=string _name_=var2014));
	by household_code person_code;
	var _character_;
run;

** 2014 household data - numeric vars;
proc transpose data=gss14.gss2014hq_usable_cases out=longhq_14_num(drop=_label_ rename=(col1=value _name_=var2014));
	by household_code person_code;
	var _numeric_;
run;

** 2014 household data - character vars;
proc transpose data=gss14.gss2014hq_usable_cases out=longhq_14_char(drop=_label_ rename=(col1=string _name_=var2014));
	by household_code person_code;
	var _character_;
run;

** 2016 personal data - numeric vars;
proc transpose data=gss16.gss2016_pqusable out=longpq_16_num(rename=(col1=value _name_=var2016));
	by household_code person_code;
	var _numeric_;
run;

** 2016 personal data - character vars;
proc transpose data=gss16.gss2016_pqusable out=longpq_16_char(drop=_label_ rename=(col1=string _name_=var2016));
	by household_code person_code;
	var _character_;
run;

** 2016 household data - numeric vars;
proc transpose data=gss16.gss2016_hqusable out=longhq_16_num(rename=(col1=value _name_=var2016));
	by household_code person_code;
	var _numeric_;
run;

** 2016 household data - character vars;
proc transpose data=gss16.gss2016_hqusable out=longhq_16_char(drop=_label_ rename=(col1=string _name_=var2016));
	by household_code person_code;
	var _character_;
run;

** Now match variable names - read in excel file of matched variable names;
** Personal;
proc import datafile='\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\GSS 2014 and 2016 var names.xlsx'
			dbms=xlsx out=varnames_pq(keep=var2014 var2016 varname) replace;
	sheet="pq";
run;

** And household;
proc import datafile='\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\GSS 2014 and 2016 var names.xlsx'
			dbms=xlsx out=varnames_hq(keep=var2014 var2016 varname) replace;
	sheet="hq";
run;

** Macro to standardise variable names and convert back to wide form;
%macro varnames(qnnaire,year);
proc sort data=varnames_&qnnaire.;
	by var20&year.;
run;
proc sort data=long&qnnaire._&year._num;
	by var20&year.;
run;
proc sort data=long&qnnaire._&year._char;
	by var20&year.;
run;
data long&qnnaire._&year._num;
	merge long&qnnaire._&year._num(in=a) varnames_&qnnaire.(keep=var20&year. varname);
	by var20&year.;
	if a;
run;
data long&qnnaire._&year._char;
	merge long&qnnaire._&year._char(in=a) varnames_&qnnaire.(keep=var20&year. varname);
	by var20&year.;
	if a;
run;

proc sort data=long&qnnaire._&year._num;
	by household_code person_code varname;
run;
proc sort data=long&qnnaire._&year._char;
	by household_code person_code varname;
run;

proc transpose data=long&qnnaire._&year._num out=wide&qnnaire._&year._num;
	var value;
	id varname;
	by household_code person_code;
run;
proc transpose data=long&qnnaire._&year._char out=wide&qnnaire._&year._char;
	var string;
	id varname;
	by household_code person_code;
run;
%mend varnames;

%varnames(pq,14);
%varnames(pq,16);
%varnames(hq,14);
%varnames(hq,16);

** Combine the 2014 datasets into one;
data wide_14(drop=_name_);
	merge widepq_14_num(in=a) widepq_14_char widehq_14_num(in=b) widehq_14_char;
	by household_code person_code;
	if a and b then pq_respondent=1;
	else pq_respondent=0;
	drop DEM_qDOB GSSPQ_fHQInterviewDate GSSPQ_fPQInterviewDate GSSHQ_fInterviewDate HQS1_qDOB;
run;

** Combine the 2016 datasets into one;
data wide_16(drop=_name_);
	merge widepq_16_num(in=a) widepq_16_char widehq_16_num(in=b) widehq_16_char;
	by household_code person_code;
	if a and b then pq_respondent=1;
	else pq_respondent=0;
	drop DEM_qDOB GSSPQ_fHQInterviewDate GSSPQ_fPQInterviewDate GSSHQ_fInterviewDate HQS1_qDOB;
run;

** And set the 2014 and 2016 data together;
data wellgss.wide_1416;
	set wide_14(in=a) wide_16(in=b);
	if a then year=2014;
	else if b then year=2016;
run;

** Now aggregate up to household level;
proc sort data=wellgss.wide_1416;
	by household_code pq_respondent;
run;

** Calculate the vars we need to take from household to individual level;
data wellgss.gss_wellbeing_1416(drop=v midpoint: topmidpoint) no_person_1416;
	set wellgss.wide_1416;
	by household_code;
	retain income_miss age_miss employ_miss hhold_inc over65 wkng_age employed children;
	if first.household_code then do;
		hhold_inc=.;
		income_miss=0;
		age_miss=0;
		wkng_age=0;
		employed=0;
		over65=0;
		employ_miss=0;
		children=0;
		child=0;
	end;
	** first deal with the individual incomes that were not carried across;
	if HQS3_qIncTotalAmt='' then HQS3_qIncTotalAmt=DEM_qIncTotalAmt;

	v = (log(2463)-log(994))/(log(150001)-log(100001)); /* v = (log(number of people in a+b)-log(number of people in a))/(log(lower limit a)-log(lower limit b))*/
	topmidpoint = 10**(0.301/v)*150001; /*median of a = 10^(0.301/v)*lower limit a*/
	midpoint1	=	0			;
	midpoint2 	=	0			;
	midpoint3	=	2500.50		;
	midpoint4	=	7500.50		;
	midpoint5	=	12500.50	;
	midpoint6	=	17500.50	;
	midpoint7	=	22500.50	;
	midpoint8	=	27500.50	;
	midpoint9	=	32500.50	;
	midpoint10	=	37500.50	;
	midpoint11	=	45000.50	;
	midpoint12	=	55000.50	;
	midpoint13	=	65000.50	;
	midpoint14	=	85000.50	;
	midpoint15	=	125000.50	;
	midpoint16	=	topmidpoint	;

	if HQS3_qIncTotalAmt in ('','11') then income_amt = midpoint1;
	else if HQS3_qIncTotalAmt in ('12') then income_amt = midpoint2;
	else if HQS3_qIncTotalAmt in ('13') then income_amt = midpoint3;
	else if HQS3_qIncTotalAmt in ('14') then income_amt = midpoint4;
	else if HQS3_qIncTotalAmt in ('15') then income_amt = midpoint5;
	else if HQS3_qIncTotalAmt in ('16') then income_amt = midpoint6;
	else if HQS3_qIncTotalAmt in ('17') then income_amt = midpoint7;
	else if HQS3_qIncTotalAmt in ('18') then income_amt = midpoint8;
	else if HQS3_qIncTotalAmt in ('19') then income_amt = midpoint9;
	else if HQS3_qIncTotalAmt in ('20') then income_amt = midpoint10;
	else if HQS3_qIncTotalAmt in ('21') then income_amt = midpoint11;
	else if HQS3_qIncTotalAmt in ('22') then income_amt = midpoint12;
	else if HQS3_qIncTotalAmt in ('23') then income_amt = midpoint13;
	else if HQS3_qIncTotalAmt in ('24') then income_amt = midpoint14;
	else if HQS3_qIncTotalAmt in ('25') then income_amt = midpoint15;
	else if HQS3_qIncTotalAmt in ('26') then income_amt = midpoint16;
	else income_amt = .;
	if income_hq_imputed ne '0' then income_amt = .;
	if income_amt=. then income_miss=1;
	else income_miss=0;

	hhold_inc+income_amt;
	if income_miss then hhold_inc=.;
	
	if DVAge = . then age_miss=1;

	if DVAge > 15 and DVAge < 65 then do;
		if HQS3_qIncSource_Wages or HQS3_qIncSource_SelfEmp then employed + 1; *Are they employed?;
		if HQS3_qIncSource_Dont_Know or HQS3_qIncSource_Refused_ then employ_miss=1;
	end;
	
	if DVAge > 64 then over65+1;

	if DVAge ge 18 and DVAge le 64 then wkng_age+1;

	if 0 <= DVAge <=17 then children+1;
	if 0 <= DVAge <=15 then child+1;

	if age_miss then do; over65=.; wkng_age=.; end;
	if employ_miss then employed=.;

	hh_size = (wkng_age + over65);
	if over65>0 and wkng_age=0 then none_wkng_age = 1;
	else if hh_size>0 then none_wkng_age = 0;

	if none_wkng_age = 0 then wkng_age_employed = (employed/wkng_age);

	adults=wkng_age+over65;
	*identifying first adult and number of subsequent adults;
	if adults gt 1 then do 
		subadults = (adults-1);
		firstadult = 1;
	end;
	else if adults = 1 then do 
		firstadult = 1;
		subadults = 0;
	end;

	*equivalising income according to: first adult =1, subsequent adults = 0.5, and children = 0.3);
	if children gt 0 and subadults gt 0 then eqincome = hhold_inc/(firstadult+(subadults*0.5)+(children*0.3));
		else if children gt 0 and subadults = 0 then eqincome = hhold_inc/(firstadult+(children*0.3));
		else if children = 0 and subadults gt 0 then eqincome = hhold_inc/(firstadult+(subadults*0.5));
		else if children = 0 and subadults = 0 then eqincome = hhold_inc;

	if last.household_code and pq_respondent=1 then output wellgss.gss_wellbeing_1416;
	else if last.household_code then output no_person_1416;
run;
