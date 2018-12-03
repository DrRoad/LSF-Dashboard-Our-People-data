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

** Check median income in each year - use this to set equivalised income cutoff - note this is not used as an Our people measure 
** due to concerns about the reliability of the GSS household income estimate;
proc sort data=wellgss.gss_wellbeing_1416;
	by year;
run;
proc means data=wellgss.gss_wellbeing_1416 median mean; var eqincome;
	by year;
	weight Person_FinalWgt;
run;

** Create a macro list of all variable we want to keep from the input data and drop from the output data;
%let invars=OLS_qFeelAboutLifeScale LWW_qThingsWorthwhileScale DVMWI eqincome wkng_age_employed none_wkng_age SOL_qEnoughIncome
			HOU_qHouseCondition HOU_qHouseMouldProblem HOU_qHouseCold DVHHCrowd DVPhysHealth DVMentHealth HEA_qHealthExcellentPoor
			DVHQual AMS_qGroupMemb_NoBelong SNCqClubMember_No_None SOC_qTimeLonely DSC_qDiscriminated
			SOC_qEnoughContactFriends SNCqFrndSatisHood SNCqFrndSatisTown SNCqFrndSatisReg SNCqFrndSatisOS
			SOC_qEnoughContactFam SNCqFamSatisHood SNCqFamSatisTown SNCqFamSatisReg SNCqFamSatisOS
			SAF_qSafeNightHome SAF_qSafeNightHood SAF_qSafeNightPubTrans SAF_qSafeOnline SAF_qCrimesAgainstYou SAF_qProblemsHood_Vandalsm 
			SAF_qProblemsHood_Burglary SAF_qProblemsHood_Assault SAF_qProblemsHood_Harrass SAF_qProblemsHood_Drugs 
			SAF_qProblemsHood_Dont_Know SAF_qProblemsHood_Refused_ CUL_qCultIdentity
			GTR_qTrustMostPeopleScale ITR_qTrustCour ITR_qTrustEdu ITR_qTrustHlth ITR_qTrustMed ITR_qTrustParl ITR_qTrustPol
			DVRegionGrp6 DVSex DVEthTR_Asian DVEthTR_Euro DVEthTR_Maori DVEthTR_MELAA DVEthTR_NS DVEthTR_Other DVEthTR_Pacific DVNZDep13 DVAge
			DVFamTyp DVHrsWrked dvlfs income_amt hhold_inc
			DEMWOR_qFeelAboutJob DEM_qLookedForWork DEM_qHasJobToStart DEMWOR_qEmployArrangement DEM_qCouldStartLastWk
			DVPersIncSourc_Jobseek DVPersIncSourc_SolePrnt DVPersIncSourc_SuppLive DVPersIncSourc_ACC DVPersIncSourc_OthBen;

** Derive Our people wellbeing measures for all LSF Domains and calculate demographic variables;
data wellgss.gss_wellbeing_1416_derived(drop=&invars.);
	set wellgss.gss_wellbeing_1416(keep=household_code person_code year Person_FinalWgt Hhld_FinalWgt Person_FinalWgt: child &invars. );

	/* GENERAL LIFE SATISFACTION */
	* Convert life satisfation into numeric;
	lifeSat = input(OLS_qFeelAboutLifeScale, 2.) ;
	if (lifeSat = 88 OR lifeSat = 99) then lifeSat = . ;

	* And assign into good and poor life satisfaction categories - approx bottom 20% 0 to 6 / top 20% 10;
	if 0 <= lifeSat <= 6 then full_lifesat=-1;
	else if 7 <= lifeSat <= 8 then full_lifesat=0;
	else if lifeSat in (9,10) then full_lifesat=1; ** July 2018 - Move 9 to excellent life satisfaction;
	if full_lifesat=-1 then low_lifesat=1;
	else if full_lifesat in (0,1) then low_lifesat=0;
	if full_lifesat=1 then high_lifesat=1;
	else if full_lifesat in (-1,0) then high_lifesat=0;

	/* SENSE OF PURPOSE */
	* Convert purpose into numeric;
	Purpose = input(LWW_qThingsWorthwhileScale, 2.) ;
	if (Purpose = 88 OR Purpose = 99) then Purpose = . ;

	* And assign into good and poor life satisfaction categories - approx bottom 15% 0 to 6 / top 25% 10 - stick with this even though lower % at bottom and higher at top;
	if 0 <= Purpose <= 6 then full_purpose=-1;
	else if 7 <= Purpose <= 8 then full_purpose=0;
	else if Purpose in (9,10) then full_purpose=1; ** July 2018 - Move 9 to excellent sense of purpose;
	if full_purpose=-1 then low_purpose=1;
	else if full_purpose in (0,1) then low_purpose=0;
	if full_purpose=1 then high_purpose=1;
	else if full_purpose in (-1,0) then high_purpose=0;

	/* MATERIAL WELLBEING */
	MWI = input(DVMWI, 2.) ;
	if MWI in (77,88,99) then MWI = . ;

	if SOL_qEnoughIncome = '11' then full_incsuff=-1; ** Not enough money;
	else if SOL_qEnoughIncome in ('13','14') then full_incsuff=1;  * Enough or more than enough money;
	else if SOL_qEnoughIncome = '12' then full_incsuff=0; * Just enough;

	** Use standard cutoffs - 0-7 typically considered LOW, 18-20 HIGH;
	IF 0 <= MWI <= 7 THEN full_mwi = -1; 
	ELSE if 18 <= MWI <= 20 THEN full_mwi = 1; 
	ELSE if MWI ne . then full_mwi = 0; 

	if full_mwi=. or full_incsuff=. then full_matsol=.;
	else if full_mwi=-1 or full_incsuff=-1 then full_matsol=-1;
	else if full_mwi=1 and full_incsuff=1 then full_matsol=1;
	else full_matsol=0;
	if full_matsol=-1 then low_matsol=1;
	else if full_matsol in (0,1) then low_matsol=0;
	if full_matsol=1 then high_matsol=1;
	else if full_matsol in (-1,0) then high_matsol=0;

	/* INCOME */
    ** Use equivalised income below 50% of median or more than double;
	if year=2014 then med_income=41667.14;
	else if year=2016 then med_income=43478.70;
	if eqincome ne . and eqincome lt (0.5*med_income) then full_income = -1;  *households with equivalised income less than 60% of median;
	else if eqincome ge (1.5*med_income) then full_income = 1;  *households with equivalised income greater than double the median;
	else if eqincome ne . then full_income=0; *missing or invalid;
	if full_income=-1 then low_income=1;
	else if full_income in (0,1) then low_income=0;
	if full_income=1 then high_income=1;
	else if full_income in (-1,0) then high_income=0;

	eq_hhld_inc=eqincome;
	pers_inc=income_amt;
	hhld_inc=hhold_inc;

	/* HOUSING */
	** Low defined by having at least one major issue with crowding, mould, cold or house condition;
	** High defined by having no issues with crowding, mould, cold or house condition;
	IF DVHHCrowd in ('01','02') then low_crowd = 1; /*Living in a crowded house*/
	Else if DVHHCrowd in ('03','04','05') then low_crowd=0; /*no crowding*/
	else low_crowd = .;

	if HOU_qHouseCondition in (14,15) then full_cond=-1;
	else if HOU_qHouseCondition in (11,12) then full_cond=1;
	else if HOU_qHouseCondition=13 then full_cond=0;

	if HOU_qHouseMouldProblem=13 then full_mould=-1;
	else if HOU_qHouseMouldProblem=11 then full_mould=1;
	else if HOU_qHouseMouldProblem=12 then full_mould=0;
	
	if HOU_qHouseCold=11 then full_cold=-1;
	else if HOU_qHouseCold=14 then full_cold=1;
	else if HOU_qHouseCold in (12,13) then full_cold=0;

	if full_cold=. or full_cond=. or low_crowd=. or full_mould=. then full_house=.;
	else if full_cold=-1 or full_cond=-1 or low_crowd=1 or full_mould=-1 then full_house=-1;
	else if full_cold=1 and full_cond=1 and low_crowd=0 and full_mould=1 then full_house=1;
	else full_house=0;
	if full_house=-1 then low_house=1;
	else if full_house in (0,1) then low_house=0;
	if full_house=1 then high_house=1;
	else if full_house in (-1,0) then high_house=0;

	/* HEALTH */
	* 4_1  physical health;
	IF DVPhysHealth <= 36 then full_physhlth = -1; /*Poor physical health*/ 
	ELSE IF DVPhysHealth = 777 then full_physhlth = .; /*Missing*/
	ELSE IF DVPhysHealth >= 54 then full_physhlth = 1;
	ELSE full_physhlth = 0;

	* 4_2  mental health;
	IF DVMentHealth <= 36 then full_menthlth = -1; /*Poor mental health*/
	ELSE if DVMentHealth = 777 then full_menthlth = .; /*Missing*/
	ELSE IF DVMentHealth >= 54 then full_menthlth = 1;
	ELSE full_menthlth = 0;

	* General health;
	if HEA_qHealthExcellentPoor = '15' then full_genhlth=-1;
	else if HEA_qHealthExcellentPoor in ('11','12') then full_genhlth=1;
	else if HEA_qHealthExcellentPoor in ('88','99') then full_genhlth=.;
	else full_genhlth=0;

	if full_physhlth=. or full_menthlth=. /*or full_genhlth=.*/ then full_health=.;
	else if full_physhlth=-1 or full_menthlth=-1 /*or full_genhlth=-1*/ then full_health=-1;
	else if full_physhlth=1 and full_menthlth=1 /*and full_genhlth=1*/ then full_health=1;
	else full_health=0;
	if full_health=-1 then low_health=1;
	else if full_health in (0,1) then low_health=0;
	if full_health=1 then high_health=1;
	else if full_health in (-1,0) then high_health=0;

	/* EDUCATION */
	* 5_1 No qualifications;
	IF DVHQual = 0 THEN full_education = -1; 
	ELSE If DVHQual in (7,8,9,10) then full_education = 1; 
	*ELSE If DVHQual = 12 then full_education = .; 
	ELSE full_education = 0; 
	if full_education=-1 then low_education=1;
	else if full_education in (0,1) then low_education=0;
	if full_education=1 then high_education=1;
	else if full_education in (-1,0) then high_education=0;

	/* SOCIAL CONNECTIONS */
	* Not a member of a club or organisation;
	if AMS_qGroupMemb_NoBelong or SNCqClubMember_No_None then low_club=1; ** Not a member of a club or organisation;
	else low_club=0;

	* Loneliness;
	if SOC_qTimeLonely='11' then full_lonely=1; ** Never lonely;
	else if SOC_qTimeLonely in ('14','15') then full_lonely=-1;
	else if SOC_qTimeLonely in ('13') then full_lonely=-0.5;
	else if SOC_qTimeLonely in ('12') then full_lonely=0;

	* Not enough or too much contact with friends;
	if sum(SOC_qEnoughContactFriends in ('11','13'),SNCqFrndSatisHood in ('11','13'),SNCqFrndSatisTown in ('11','13'),SNCqFrndSatisReg in ('11','13'),SNCqFrndSatisOS in ('11','13'))>0 then low_friend=1;
	else if sum(SOC_qEnoughContactFriends in ('88','99'),SNCqFrndSatisHood in ('88','99'),SNCqFrndSatisTown in ('88','99'),SNCqFrndSatisReg in ('88','99'),SNCqFrndSatisOS in ('88','99'))>0 then low_friend=.;
	else low_friend=0;

	* Not enough or too much contact with family outside of household;
	if sum(SOC_qEnoughContactFam in ('11','13'),SNCqfamSatisHood in ('11','13'),SNCqfamSatisTown in ('11','13'),SNCqfamSatisReg in ('11','13'),SNCqfamSatisOS in ('11','13'))>0 then low_family=1;
	else if sum(SOC_qEnoughContactFam in ('88','99'),SNCqfamSatisHood in ('88','99'),SNCqfamSatisTown in ('88','99'),SNCqfamSatisReg in ('88','99'),SNCqfamSatisOS in ('88','99'))>0 then low_family=.;
	else low_family=0;

	if low_friend=. or low_family=. then full_friendfam=.;
	else if low_friend=1 and low_family=1 then full_friendfam=-1;
	else if low_friend=1 or low_family=1 then full_friendfam=0;
	else full_friendfam=1;

	* Discrimination - treating dont know different here - can we assume no discrimination if someone doesn't know?;
	if DSC_qDiscriminated = '01' then low_discrim=1; **Discriminated against;
	else if DSC_qDiscriminated in ('02','88') then low_discrim=0; 

	if full_lonely=. or full_friendfam=. or low_discrim=. then full_social=.;
	*else if full_lonely=-1 or sum(full_lonely=-0.5,low_friend=1,low_family=1)>1 or low_discrim=1 then full_social=-1; 
	*else if full_lonely=1 and low_friend=0 and low_family=0 and low_discrim=0 then full_social=1; * Never lonely, satisfied with contact with friends;
	else if sum(full_lonely=-1,full_friendfam=-1,low_discrim=1)>0 then full_social=-1; 
	else if full_lonely=1 and full_friendfam=1 and low_discrim=0 then full_social=1; * Never lonely, satisfied with contact with friends or family, no discrimination;
	else full_social=0;
	if full_social=-1 then low_social=1;
	else if full_social in (0,1) then low_social=0;
	if full_social=1 then high_social=1;
	else if full_social in (-1,0) then high_social=0;

	/* SAFETY */
	** Feeling safe;
	if SAF_qSafeNightHome in ('88','99') or SAF_qSafeNightHood in ('88','99') or SAF_qSafeNightPubTrans in ('88','99') or SAF_qSafeOnline in ('88','99') then full_feelsafe=.;
	else if SAF_qSafeNightHome in ('14','15') or SAF_qSafeNightHood in ('14','15') or SAF_qSafeNightPubTrans in ('14','15') or SAF_qSafeOnline in ('14','15') then full_feelsafe=-1;
	else if SAF_qSafeNightHome in ('11','12','16') and SAF_qSafeNightHood in ('11','12','16') and SAF_qSafeNightPubTrans in ('11','12','16') and SAF_qSafeOnline in ('11','12','16') then full_feelsafe=1;	
	else full_feelsafe=0;

	if SAF_qCrimesAgainstYou='01' then low_crime=1;
	if SAF_qCrimesAgainstYou='02' then low_crime=0;

	if sum(SAF_qProblemsHood_Vandalsm,SAF_qProblemsHood_Burglary,SAF_qProblemsHood_Assault,SAF_qProblemsHood_Harrass,SAF_qProblemsHood_Drugs)>0 then low_safeneigh=1;
	else if sum(SAF_qProblemsHood_Dont_Know,SAF_qProblemsHood_Refused_)>0 then low_safeneigh=.;
	else low_safeneigh=0;

	if full_feelsafe=. or low_crime=. or low_safeneigh=. then full_safety=.;
	else if sum(full_feelsafe=-1,low_crime=1,low_safeneigh=1) >1 then full_safety=-1; 
	else if full_feelsafe=1 and low_crime=0 and low_safeneigh=0 then full_safety=1; * Feel safe or very safe in all situations;
	else full_safety=0;
	if full_safety=-1 then low_safety=1;
	else if full_safety in (0,1) then low_safety=0;
	if full_safety=1 then high_safety=1;
	else if full_safety in (-1,0) then high_safety=0;

	/* CIVIC AND GOVERNANCE */
	* Don't trust people;
	if GTR_qTrustMostPeopleScale in ('00','01','02','03','04') then full_trustpple=-1;
	else if GTR_qTrustMostPeopleScale in ('07','08','09','10') then full_trustpple=1;
	else if GTR_qTrustMostPeopleScale in ('88','99') then full_trustpple=.;
	else full_trustpple=0;

	* Don't trust institutions;
	if sum(ITR_qTrustCour<'05', ITR_qTrustEdu<'05', ITR_qTrustHlth<'05', ITR_qTrustParl<'05', ITR_qTrustPol<'05') > 1 then full_trustinst=-1;
	else if ITR_qTrustCour in ('88','99') and ITR_qTrustEdu in ('88','99') and ITR_qTrustHlth in ('88','99') and 
		ITR_qTrustParl in ('88','99') and ITR_qTrustPol in ('88','99') then full_trustinst=.;
	else if sum(ITR_qTrustCour>='07',ITR_qTrustEdu>='07',ITR_qTrustHlth>='07',ITR_qTrustParl>='07',ITR_qTrustPol>='07')>=4 then full_trustinst=1;
 	else full_trustinst=0;

	* Trust;
	if full_trustpple=-1 or full_trustinst=-1 then full_trust=-1; **Doesn't trust people or institutions;
	else if full_trustpple=1 and full_trustinst=1 then full_trust=1;
	else if full_trustpple=. and full_trustinst=. then full_trust=.;
	else full_trust=0;

	if full_trust=. /*or low_discrim=.*/ then full_civic=.;
	*else if sum(full_trustpple=-1,full_trustinst=-1,low_discrim=1)>0 then full_civic=-1; 
	*else if full_trustpple=1 and full_trustinst=1 and low_discrim=0 then full_civic=1; 
	else if full_trust=-1 /*or low_discrim=1*/ then full_civic=-1; 
	else if full_trust=1 /*and low_discrim=0*/ then full_civic=1; 
	else full_civic=0;
	if full_civic=-1 then low_civic=1;
	else if full_civic in (0,1) then low_civic=0;
	if full_civic=1 then high_civic=1;
	else if full_civic in (-1,0) then high_civic=0;

	/* CULTURE */
	if CUL_qCultIdentity in ('13','14','15') then full_culture=-1;
	else if CUL_qCultIdentity in ('11') then full_culture=1;
	else if CUL_qCultIdentity in ('88','99') then full_culture=.;
	else full_culture=0;
	if full_culture=-1 then low_culture=1;
	else if full_culture in (0,1) then low_culture=0;
	if full_culture=1 then high_culture=1;
	else if full_culture in (-1,0) then high_culture=0;

	/* Try a Jobs measure */
	** Poor jobs wellbeing if unemployed OR worked but dissatisfied or very dissatisfied with job or NILF and on benefit;
	** Excellent jobs wellbeing if worked in last month and satisfied or very satisfied with job;
	if dvlfs='77' or DEMWOR_qFeelAboutJob in ('88','99') then full_job=.;
	else if DEMWOR_qFeelAboutJob in ('11') then full_job=1;
	else if DEMWOR_qFeelAboutJob in ('14','15') or dvlfs='02' 
			or (dvlfs='03' and (DVPersIncSourc_Jobseek or DVPersIncSourc_SolePrnt or DVPersIncSourc_SuppLive or DVPersIncSourc_ACC)) then full_job=-1;
	else full_job=0;
	if full_job=-1 then low_job=1;
	else if full_job in (0,1) then low_job=0;
	if full_job=1 then high_job=1;
	else if full_job in (-1,0) then high_job=0;

	/* NOW DEAL WITH DESCRIPTIVE DEMOGRAPHIC AND POPULATION VARIABLES */
	/* FAMILY TYPE */
	family_type=substr(DVFamTyp,1,1);
	if family_type='9' then family_type=9;

	/* HOURS WORKED */
	if DVHrsWrked='777' then hours=9;
	else if DVHrsWrked>='50' then hours=4;
	else if 30<=DVHrsWrked<'50' then hours=3;
	else if 0 <= DVHrsWrked<'30' then hours=2;
	else if dvlfs in ('02','03') then hours=1;
	else hours=9;

	/* REGION */
	region=DVRegionGrp6;
	if region=. then region=9;
	
	/* SEX */
	sex=DVSex;
	
	/* ETHNIC GROUPS */
	** Note, these are stored as separate variables as people can report multiple ethnicities;
	if DVEthTR_Asian then asian=1;
	else asian=0;
	if DVEthTR_Euro then european=1;
	else european=0;
	if DVEthTR_Maori then maori=1;
	else maori=0;
	if DVEthTR_MELAA or DVEthTR_Other then other_eth=1;
	else other_eth=0;
	if DVEthTR_Pacific then pacific=1;
	else pacific=0;
	if DVEthTR_NS=1 then do;
		asian=9; european=9; maori=9; other_eth=9; pacific=9;
	end;
	
	/* NZDEP area deprivation quintiles */
	if DVNZDep13 = '' then nzdep5=9;
	else if DVNZDep13 <= '02' then nzdep5=1;
	else if DVNZDep13 <= '04' then nzdep5=2;
	else if DVNZDep13 <= '06' then nzdep5=3;
	else if DVNZDep13 <= '08' then nzdep5=4;
	else nzdep5=5;
	
	/* BROAD AGE GROUPS */
 	if '15' <= DVAge <= '34' then agegrp=1;
 	else if '35' <= DVAge <= '64' then agegrp=2;
 	else if DVAge >= '65' then agegrp=3;

	/* DETAILED AGE GROUPS - Youth and older people */
 	if '15' <= DVAge <= '24' then agegrp2=1;
 	else if '25' <= DVAge <= '34' then agegrp2=2;
 	else if '35' <= DVAge <= '64' then agegrp2=3;
 	else if '65' <= DVAge <= '79' then agegrp2=4;
 	else if DVAge >= '80' then agegrp2=5;
	
	/* HOURS WORKED BY AGE */
	if agegrp=3 and hours=1 then hours_age=5;
	else if agegrp=3 and hours in (2,3,4) then hours_age=6;
	else hours_age=hours;
run;

** Function to round output;
proc fcmp outlib=work.func.round;
	function xround1000(value);
		return(round(value,1000));
	endsub;
run;

options cmplib=work.func;

** Create a format for rounding to the nearest 1000 as per SNZ output rules;
proc format;
	value xround1000x
		low-high = [xround1000()]
	;
run;

** Create cross-domain wellbeing measures and label variables;
data wellgss.gss_wellbeing_1416_derived;
	length poor_well2 good_well2 $ 5;
	set wellgss.gss_wellbeing_1416_derived;
	** Note: job wellbeing commented out as we're not using it in final Our people analysis;
	array full_doms{*} full_matsol full_health full_house full_education full_social full_safety full_civic full_culture /*full_job*/;
	array low_doms{*} low_matsol low_health low_house low_education low_social low_safety low_civic low_culture /*low_job*/;
	array high_doms{*} high_matsol high_health high_house high_education high_social high_safety high_civic high_culture /*high_job*/;
	sum_well=sum(of full_doms{*});
	poor_well=sum(of low_doms{*});
	good_well=sum(of high_doms{*});
	dummy=1;
	if sum_well<=-4 then sum_well2='1 Very poor';
	else if -3 <= sum_well <=-2 then sum_well2='2 Poor';
	else if -1 <= sum_well <= 2 then sum_well2='3 Good';
	else if 3 <= sum_well <= 4 then sum_well2='4 Very good';
	else if sum_well>=5 then sum_well2='5 Excellent';
	if sum_well<=-2 then sum_well3='1 Very poor to poor';
	else if -1 <= sum_well <= 2 then sum_well3='2 Good';
	else if sum_well>=3 then sum_well3='3 Very good to excellent';
	if poor_well=0 then poor_well2='0';
	else if poor_well=1 then poor_well2='1';
	else if poor_well in (2,3) then poor_well2='2-3';
	else if poor_well>3 then poor_well2='4-9';
	if good_well=0 then good_well2='0';
	else if good_well=1 then good_well2='1';
	else if good_well in (2,3) then good_well2='2-3';
	else if good_well>3 then good_well2='4-9';

	** Set a weight to estimate the number of children in households with people with different multi-domain wellbeing; 
	child_weight=Hhld_FinalWgt*child;

	label full_lifesat = 'General life satisfaction'
		  full_purpose = 'Sense of purpose'
		  full_matsol = 'Material standard of living'
		  full_income = 'Equivalised HH income'
		  full_health = 'Health'
		  full_house = 'Housing'
		  full_education = 'Education'
		  full_social = 'Social connections'
		  full_civic = 'Civic and governance'
		  full_safety='Safety'
		  full_culture = 'Culture'
		  full_job='Jobs'

		  lifesat = 'General life satisfaction (0 to 10)'
		  purpose = 'Sense of purpose (0 to 10)'
		  agegrp = 'Age group'
		  nzdep5 = 'NZ deprivation index quintile'
		  pacific = 'Pacific ethnicity'
		  maori = 'Maori ethnicity'
		  european = 'European ethnicity'
		  asian = 'Asian ethnicity'
		  region = 'Region'
		  sex = 'Sex'
		  poor_well = 'Number of domains with poor wellbeing'
		  good_well = 'Number of domains with good wellbeing'
		  poor_well2 = 'Number of domains with poor wellbeing'
		  good_well2 = 'Number of domains with good wellbeing'
		  sum_well='Multi-domain wellbeing'
		  sum_well2='Multi-domain wellbeing groups'
		  sum_well3='Multi-domain wellbeing groups'
		  hours = 'Employment status'
		  hours_age = 'Employment by age'
		  family_type='Family type'
			;
	format sex $sex. region $region. european maori pacific asian eth. family_type $familytype. hours hours. agegrp age_group.;
run;