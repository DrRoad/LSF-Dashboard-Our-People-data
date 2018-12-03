**********************************************************************************************************************************;
** Our People - Code to produce wellbeing measures for a multidimensional wellbeing analysis based on the Living Standards Framework
**
** File: 0 Assign libnames and formats.sas
** Description: This code should be run first - allocates libnames and creates formats.
** 
** Code created: March 2018
** Code last edited: November 2018
**
** Author: Keith McLeod, A&I, The Treasury
**
** Notes: Analysis uses standalone GSS datasets, not IDI-linked datasets as 2016 data was not in the IDI when this project started.
**
**********************************************************************************************************************************;

** Libraries where GSS raw data is stored;
libname gss14 '\\wprdfs09\RODatalab\GSS2014';
libname gss16 '\\wprdfs09\RODatalab\GSS2016';

** Library to store data for this project;
libname wellgss '\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Wellbeing - GSS\data';

** Create variable formats;
proc format;
	value xround1000x
		low-high = [xround1000()]
		;
	value inds
		-1 = 'Poor'
		0 = 'Good'
		1 = 'Very good';

	value age_group
		1 = '15 to 34 years'
		2 = '35 to 64 years'
		3 = '65+ years';

	value age_groupalt
		1 = '15 to 24 years'
		2 = '25 to 34 years'
		3 = '35 to 64 years'
		4 = '65 to 79 years'
		5 = '80+ years';

	value $ sex
		'11' = 'Male'
		'12' = 'Female';

	value $ region
		'11' = 'Auckland'
		'12' = 'Wellington'
		'13' = 'Northland, Bay of Plenty, Gisborne'
		'14' = 'Rest of North Island'
		'15' = 'Canterbury'
		'16' = 'Rest of South Island'
		'19' = 'Missing region'
		;

	value hours
		1 = 'Not employed'
		2 = 'Working part-time hours (<30 per week)'
		3 = 'Working full-time hours (30-49 per week)'
		4 = 'Working long hours (50+ per week)'
		9 = 'Missing employment'
		;

	value $ familytype
		'1' = 'Couple without children'
		'2' = 'Couple with children'
		'3' = 'Sole parent'
		'4' = 'Not in a family nucleus'
		'9' = 'Missing family type'
		;
			
	value eth 
		1='Yes' 
		0='No'
		9='Missing ethnicity';
run;
