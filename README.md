# RTDS-90-day-mortality (Lung cancer)
Radiotherapy 90-day mortality (Unadjusted)

Crude and Adjusted Rates

Files and folders 
for data and analysis:
\\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production

for reports:
\\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production\4_Report

The following steps are required to reproduce the reports circulated to RT providers

1. Extract data from CAS 
This release used the snapshot CAS2504l
Altered script to remove unbound code as not needed for this type of analysis and linked in Trustics

1.1 
	The data for analysis is obtained by running the following SQL query which was updated and you run the r
	a Rmarkdown file:
	SQL query:
	\\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production\1_SQL
 export and clean data files are saved in the folder below:
 \\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production\2_Stata\Data

 1.2 
   There is also three data quality scripts that need running (Intent, Modality and cancer site) these scripts were udpated you run them using the Trust_DQ r script. SQL scripts are in the SQL and export folder, r file is in the 3_DataQuality folder.
   \\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production\3_DataQuality

2. Data analysis (Stata)

Expected file structure is set up in the folder below: Includes two scripts per site, 1) data cleaning and 2) running the analysis
\\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production\2_Stata\Do Files

files used to produce the r-markdown reports are saved in the folder below:
\\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\90 day mortality\Development 2025\CAS2504\Production\2_Stata\Output\Excl_DQ_Trust

2.1
	Do files needed for analysis are:
		\\corp.internal\ndrs\RTDS\Analytical work\Sarah Lawton\30 day mortality\Development 2025\CAS2504\Production\Stata\Do files:

	1. RTDS_90DM_Part1_DataCleaning_patient_Lung.do
	2. RTDS_90DM_Part2_Lung.do

for the re-run of 2022/2023 and 2023/2024 no trusts were excluded. 
		
3. Reports
The reports are generated using Rmarkdown and are output as *.html files

3.1 
	There are two Rmarkdown files to generate the reports:
	1. 90 day mortality after radical radiotherapy - Lung Report.Rmd
	2. 90 day mortality after radical radiotherapy - Lung Technical Document.Rmd
