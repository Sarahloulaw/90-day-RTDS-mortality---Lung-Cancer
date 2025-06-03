/*
QA analyst: Daniela Tataru 
Date: 20 June 2023
*/

/* The analysis here should pull in the CAS data extract 
and then create the flags for those records where the patient has died within 90 days of start of first radical episode
The records will then be grouped by provider and finanical year so the crude 90 day mortality metric can be calculated
This do file only looks at the crude mortality, there is nothing done with the fractionation or cancer site
*/

// Importing the data set - should change directory to where we want to save the data
cd "R:\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production"

//The local snap is used for defining the name in the output files and the log file
local snap = "CAS2504"

set more off

log using "./2_Stata/Logs/RTDS_90DM_`snap'_DataCleaning_log", replace

// Importing the CAS data extract 
import delimited "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15.csv", clear
//19 vars, 28 313 obs
**# Bookmark #6
save "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15.dta", replace

use "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15.dta", clear
describe // 28 313


// Generating a Stata useable financial year - this done so that we can use the over option later
gen double date_treatmentstart =  date(treatmentstartdate, "YMD#hms#")
format date_treatmentstart %tdDD/NN/CCYY
gen fyear = yofd(date_treatmentstart)
replace fyear = fyear-1 if month(date_treatmentstart) <=3

tab cancer_group


keep if cancer_group=="lung" // 14,888 


**# Bookmark #16
save "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15_clean_Lung.dta", replace

use "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15_clean_Lung.dta", replace


/******************************************************************************/
/* This section is iteration specific, dealing with Trusts with  very low 
activity less then 10 cases which is insuficient  for calculating robust
 rates; We exclude these Trusts from the analysis */

bysort provider_name fyear cancer_group: gen Trust_year_site_total = _N
tab Trust_year_site_total, m
drop if Trust_year_site_total <10 // 0 dropped

/******************************************************************************/

count if epi_duration_flag  >153 // 1 episode with duration greater than 5 months that we exclude as they are most likely data quality 
drop if epi_duration_flag  >153 

count // 14,887
codebook cancer_group
		
codebook  birthdatebest  //
codebook  radiotherapydiagnosisicd  
codebook  epi_duration_flag  // 
codebook  nhsnumber   //
codebook  nhsnumber_check
codebook  orgcodeprovider //
tab provider_name // 
codebook  patientid  //
codebook  radiotherapyepisodeid 
codebook  gender       
codebook  treatmentstartdate // 
codebook  tumourcount  
count if  tumourcount  ==0  // 40


// Converting data field dates to Stata format dates
// NOTE: date format depends on export method. AK switched to using R to run the SQL extract which yields a different date format to the Oracle NLS date format.
//gen dob = date(birthdatebest, "DMY") 
//gen date_vitalstatus = date(vitalstatusdate, "DMY") //
//gen date_treatmentstart = date(treatmentstartdate, "DMY")
gen double dob = date(birthdatebest, "YMD#hms#") //0 missing lung// o missing for h&n
//drop if dob==.  // 1 obs deleted
gen double date_vitalstatus = date(vitalstatusdate, "YMD#hms#") //17 missing lung // 12 missing head and neck
gen double date_deathdatebest = date(deathdatebest, "YMD#hms#") //13,472 missing lung // 18,699 missing head and neck


format dob %tdDD/NN/CCYY
format date_vitalstatus %tdDD/NN/CCYY
format date_deathdatebest %tdDD/NN/CCYY


// Generating the age at treatment start and the time between vital status date and treatment start date
gen age_treatment = age(dob, date_treatmentstart) //



/********************************************************************************************************/
// Data cleaning based on age at treatment start, NHS number, vital status data quality and any duplicates
//Dropping records in order of presentation diagram
// Age filter
count if age_treatment < 18 // 0
drop if age_treatment < 18 // 0
count // 14,887

// Filtering NHS numbers to only those that can be traced (English and Welsh NHS numbers)
count if nhsnumber==. //9 lung
codebook nhsnumber_check
count if nhsnumber_check != "Pass" // 12 lung
keep if nhsnumber_check == "Pass" // 12 deleted
count // 14,875 

tab vitalstatus, m //A and D so all good

**# Bookmark #8
drop if inlist(vitalstatus, "D3", "D4", "D5", "I") //0

count if vitalstatus=="D" & date_vitalstatus!=date_deathdatebest // 0
count if vitalstatus=="D" & date_vitalstatus!=date_deathdatebest & date_deathdatebest!=. //0

tab date_vitalstatus if !inlist(vitalstatus, "D", "A"), m  
tab vitalstatus if !inlist(vitalstatus, "D", "A"), m
count if date_vitalstatus==. //0

//calculating the time between vitalstatusdate and treatmentstartdate
gen time_to_vital = date_vitalstatus - date_treatmentstart // SL if patients have died this should be the same date as the dod - check below
tab time_to_vital, m
tab time_to_vital if vitalstatus=="A" // SL this is time from treatment start to follow-up 5th January 2025
**# Bookmark #11
br date_vitalstatus deathdatebest time_to_vital if vitalstatus=="D" // SL this is time from treatment start to follow-up 5th January 2025


count if !inlist(vitalstatus, "D", "A") & time_to_vital <=90  // H&N 0
//For each X* we drop the rows that embarked before 30 days, if vitalstatusdate is 90 days after treamentment start we know they survived 90 days... can be included in the dataset
//we also need to drop the X5, as lost tofollow-up will not allow to determine who genuinelly died within 90 days


count //  14,875 lung
count if vitalstatus=="" //0
count if date_vitalstatus==. //0

duplicates report radiotherapyepisodeid orgcodeprovider // 1 lung
//bysort radiotherapyepisodeid orgcodeprovider :  gen dup = cond(_N==1,0,_n)
//drop if dup > 0

codebook time_to_vital //none missing
count if time_to_vital < 0 // 0
drop if time_to_vital < 0  // 0
count //  14,875  lung
tab vitalstatus, m // we have A, D

drop if birthdatebest=="" // none, as this was dropped as part of nhsnumber  null
summ epi_duration_flag
count if  epi_duration_flag > 90 // 4 lung
br if  epi_duration_flag > 90 // 

 
 /********************************************************************************************************/
 /* !!!!!!!     STOP HERE IF RUNNING ANALYSIS 2 TOPICK UP NUMBERS FOR FLOWCHART				 */
 /*********************************************************************************************************/
* we decided to keep this in - see notes from 30DM project meeting on 7th March 2023
// epi_duration_flag = 1 if episodes duration >= 30, 0 otherwise
//drop if epi_duration_flag == 1 //3,158 dropped

 
// Generating the flag for records where the patient has died within 90 days
// m_flag = 1 - the patient has died within 90 days of the start of the episodes
// m_flag = 0 - the patient's vitalstatus is alive at 90 days after starting RT episode 
gen m_flag = 0
replace m_flag = 1 if (inlist(vitalstatus, "D") & time_to_vital <= 90) // lung 650 died within 90 days
tab m_flag, m

// Saving the episode level records 
// for lung
save "./2_Stata/Data/RTDS_90DM_CAS2504_2025-05-15_clean_Lung.dta", replace
// for head and neck
//save "./Stata/Data/RTDS_90DM_CAS2210_2023-06-19_clean_head_and_neck_patient.dta", replace



log close 