

set more off


//The local snap is used for defining the name in the output files and the log file
local snap = "CAS2504"


******************************************************************/
// High level working directory
cd "R:\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production"


log using "./2_Stata/Logs/RTDS_90DM_`snap'_lung_log", replace


/************************************************************************************/
/*				1. CHOOSE SITE OPTION												*/
/************************************************************************************/
//Loading the data from Part 1 for head_and_neck
**# Bookmark #1
use "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15_clean_lung.dta", clear

**# Bookmark #1
format %12.0g nhsnumber

count // 14,875   lung
describe

/************************************************************************************/
/*				2. CHOOSE OPTION FOR DROPPING MULTIPLE PATIENTIDS					*/
/************************************************************************************/


/************************************************************************************/
/*			3. CHOOSE OPTION FOR DATA QUALITY TRUSTS INCLUSION OR EXCLUSION	*/							
/************************************************************************************/

// SL comment - did not remove any trusts 
// OPTION 1: include all Trusts
//do nothing

// OPTION 2: exclude Trusts with known Data Quality issues
//Royal Wolverhampton (RL4), intent DQ, in fyear: 18/19, 19/20, 20/21 , 21/22
//tab orgcodeprovider fyear if orgcodeprovider == "RL4" //914
//drop if orgcodeprovider == "RL4"



//University Hospital Cocentry and Warwickshire , modality (brachy) DQ, FY 2018, 19, 20, 21
//tab orgcodeprovider fyear if orgcodeprovider == "RKB"
//drop if orgcodeprovider == "RKB"
 
//count // 23 794



/************************************************************************************/
/* 				4. CHOOSE OPTION FOR EXCLUSION	of episodes longer than 90 days 	*/
/************************************************************************************/
count if epi_duration_flag>90 // 4 lung



/************************************************************************************/
/* 							DESCRIPTIVE STATISTICS									*/
/************************************************************************************/
//tab fractions, m
* // any data needed for the waterfall graphs???

bysort provider_name: egen long_episodes = total ( epi_duration_flag)
bysort provider_name: gen long_episodes_percent = long_episodes/_N
tabstat long_episodes_percent,  stat(min median max) col(stat) long //min, max and median acrodd Providers' average episodes lenghth


/************************************************************************************/
/* 						CRUDE AND ADJUSTED RATES									*/
/************************************************************************************/
// Set up variables to hold the England 90 day proportion and the confidence intervals
gen all_england_ndm = .
gen all_england_lowerbound = .
gen all_england_upperbound = .


// Set up variables to hold the Provider 90 day proportion and the confidence intervals
gen provider_ndm = .
gen provider_lowerbound = .
gen provider_upperbound = .


levelsof fyear
local years = r(levels)

/*!!!! Funnel plots control limits using a normal approximation rather than excat formula   */

foreach yoi in `years' {
	
	//for each year, preserve, keep just one year of data, calculate crude England rate, and crude Trust rates, and respective adjusted rates
	preserve
	keep if fyear == `yoi'
	//Crude England rate for a financial 
	proportion m_flag, citype(wilson)
	matrix res_all = r(table)
	matrix list res_all

	replace all_england_ndm = res_all[1,2]*100.00
	replace all_england_lowerbound = res_all[5,2]*100.00
	replace all_england_upperbound = res_all[6,2]*100.00
	
	//Crude rate foreach Trust for a financial 
	encode(provider_name), gen(providername)
	proportion m_flag, over(providername) citype(wilson)
	matrix res = r(table)
	matrix list res
	levelsof providername
	local providers = r(levels)
	su providername 
	local p_offset = r(max)
	display `p_offset'
	
	foreach provider in `providers' { //number of providers changes between years, this may effect the proportions output......
			replace provider_ndm = res[1,`provider'+`p_offset']* 100.00 if fyear == `yoi' & providername == `provider'
			replace provider_lowerbound = res[5,`provider'+`p_offset']* 100.00 if fyear == `yoi' & providername == `provider'
			replace provider_upperbound = res[6,`provider'+`p_offset']* 100.00 if fyear == `yoi' & providername == `provider'
		}
	

	save "./2_Stata/Output/temp/RTDS_90DM_CAS2504_analysis_lung_`yoi'.dta", replace
		
	bysort orgcodeprovider provider_name providername fyear:gen denom = _N
	bysort orgcodeprovider provider_name providername fyear:egen numer = total(m_flag)
	
	
	contract orgcodeprovider provider_name providername fyear all_england_ndm all_england_lowerbound all_england_upperbound provider_ndm 	    provider_upperbound provider_lowerbound numer denom
	drop _freq
	
	save "./2_Stata/Output/temp/RTDS_90DM_CAS2504_provider_analysis_lung_`yoi'.dta", replace
		

	local baseline = all_england_ndm  //English crude rate
	
	funnelcompar provider_ndm denom providername, binomial ext_stand(`baseline') constant(100) exact unitlabel(`yoi') markcontour(.2) display 	 saving(./2_Stata/Output/temp/RTDS_90DM_CAS2504_crude_funnel_lung_`yoi')
	graph export "./2_Stata/Output/temp/RTDS_90DM_CAS2504_crude_funnel_lung_`yoi'.jpg", replace
	use "./2_Stata/Output/temp/RTDS_90DM_CAS2504_crude_funnel_lung_`yoi'", clear 
	replace fyear=`yoi' if fyear==.
	save "./2_Stata/Output/temp/RTDS_90DM_CAS2504_crude_funnel_lung_`yoi'", replace
	restore
}

//SL might need to take out the replace fyear=yoi as odd didnt run for lung 

use		  	"./2_Stata/Output/temp/RTDS_90DM_CAS2504_provider_analysis_lung_2022.dta", clear
append using "./2_Stata/Output/temp/RTDS_90DM_CAS2504_provider_analysis_lung_2023.dta"
drop providername

 //save "./Stata/Output/Base/RTDS_90DM_CAS2210_provider_analysis_head_and_neck.dta", replace // All, head_and_neck
save "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_analysis_lung.dta", replace // No DQ trusts, head_and_neck


use 		 "./2_Stata/Output/temp/RTDS_90DM_CAS2504_crude_funnel_lung_2022.dta", clear
append using "./2_Stata/Output/temp/RTDS_90DM_CAS2504_crude_funnel_lung_2023.dta"
gen outlier="Within 3SD limits"
replace outlier="Low outlier" if provider_ndm <_lb_2  & provider_ndm !=.
replace outlier="High outlier" if  provider_ndm >_ub_2 & provider_ndm !=.
gen metric_type="crude rate"
drop providername

//save "./Stata/Output/Base/RTDS_90DM_CAS2210_provider_crude_funnel_head_and_neck.dta", replace // 
save "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_crude_funnel_lung.dta", replace // 




	
/****** 						Outliers								**************/
/***********************************************************************************/
use "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_crude_funnel_lung.dta", clear 


keep if outlier!="Within 3SD limits"
keep provider_name fyear denom  provider_ndm metric_type outlier
duplicates drop
sort outlier provider_name  fyear  metric_type


save "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_outliers_lung.dta", replace 


/************************************************************************************************************/
/*			FIGURES FOR THE REPORT 	for analysis with episodes longer than 30 days and 6 fractions groups	*/
/************************************************************************************************************/

//NATIONAL CRUDE RATES

use "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_analysis_lung.dta", clear
keep fyear all_england_ndm
duplicates drop
sort fyear 
format all_england_ndm %9.1f
list fyear all_england_ndm  // listing  ENGLAND CRUDE RATES (2022/23 4.8 / 2023/24 4.0)

//CRUDE RATES BY TRUST RANGE
use  "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_analysis_lung.dta", clear
keep provider_name provider_ndm fyear
duplicates drop
format provider_ndm %9.1f
summ provider_ndm, format
summ provider_ndm, detail format

//TRUST VOLUME RANGE
use  "./2_Stata/Output/Excl_DQ_Trust/RTDS_90DM_CAS2504_provider_analysis_lung.dta", clear
keep provider_name denom fyear
duplicates drop
format denom %9.1f
summ  denom, format
summ  denom, detail format

