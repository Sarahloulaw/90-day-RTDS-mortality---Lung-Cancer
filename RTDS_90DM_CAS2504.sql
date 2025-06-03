/*
	90 Day Mortality in radiotherapy patients receiving radical radiotherapy
	------------------------------------------------------------------------
    initial_patients:
    - Select fields from the patient table, keeping only records with a matching patientid in the episodes table. Join on prescription table where there's a matching patientid to get modality
    - Exclude providers ('7A1', '7A3', 'RQF') and patients with an alias flag of 0
    - Calculate first intent, icd 10, duration, number of patients, number of treatment start dates, number of modalities per episode/provider

    filtered_patients
	- From the initial_patients, partition to get the first treatment start date.
    - Include only external beam radiotherapy modality ('05'), with curative first intent and one of the relevant ICD-10 sites.
    
    dated_patients
    - From the filtered_patients include only cases of the date range FY2018-FY2021. For these records, calculate a financial year based on treatment start date. 
	
        lung_patients:
        - From dated_patients, include only episodes with a first diagnosis of C33-C34.
        - Rank the records for each patient partitioned by patientid and financial year
        
        headandneck_patients:
        - From dated_patients, include only episodes with a first diagnosis of C00-C14 or C30-C32.
        - Rank the records for each patient partitioned by patientid and financial year 
        
    all_patients
    - Take all the records in lung_patients and append them to all the records in headandneck_patients. Include only the first episode of the financial year for each patient from each table
    
    extract_patients
    - Select specific fields and all the records in the all_patients table. Apply NHS number check and join on additional provider information
    
    traced_patients
    - Join on most recent vital status information from patient table in cas2306 to extract_patients 
*/
WITH 
	initial_patients AS(
		SELECT DISTINCT
			episode.patientid,
			episode.tumourid,
			episode.radiotherapyepisodeid,
			patient.nhsnumber,
			patient.birthdatebest,
			patient.gender,
			patient.vitalstatus,
			patient.vitalstatusdate,
			patient.deathdatebest,
			patient.embarkation,
			patient.embarkationdate,
			patient.tumourcount,
			episode.treatmentstartdate,
			prescription.rttreatmentmodality,
			DECODE(episode.orgcodeprovider,'RH1','R1H','RNL','RNN','RGQ','RDE','RQ8','RAJ','RDD','RAJ','RBA','RH5','RA3','RA7','RDZ','R0D','RD3','R0D','RNJ','R1H','RXH','RYR','E0A','RYR', 'RM3', 'RBV', episode.orgcodeprovider) AS orgcodeprovider,
            episode.radiotherapyintent,
			episode.radiotherapydiagnosisicd,

			(
				MAX(episode.apptdate)
					OVER(
						PARTITION BY
							episode.patientid,
							episode.radiotherapyepisodeid, 
							episode.orgcodeprovider
					) 
				- 
				MIN(episode.apptdate)
					OVER(
						PARTITION BY 
							episode.patientid,
							episode.radiotherapyepisodeid,
							episode.orgcodeprovider
					) 
			) epi_duration_flag,
			COUNT(DISTINCT episode.patientid)
				OVER(
					PARTITION BY 
						episode.radiotherapyepisodeid, 
						episode.orgcodeprovider
				) patients_per_episode,
			COUNT(DISTINCT episode.treatmentstartdate) 
				OVER(
					PARTITION BY 
						episode.radiotherapyepisodeid,
						episode.orgcodeprovider
				) treatstartdate_per_episode,
			COUNT(DISTINCT prescription.rttreatmentmodality)
				OVER(
					PARTITION BY 
						episode.radiotherapyepisodeid,
						episode.orgcodeprovider
				) modalities_per_epi
		FROM rtds.at_patient_england patient
		INNER JOIN rtds.at_episodes_england episode ON patient.patientid = episode.patientid
		LEFT JOIN rtds.at_prescriptions_england prescription ON episode.patientid = prescription.patientid 
				AND episode.radiotherapyepisodeid = prescription.radiotherapyepisodeid 
				AND episode.attendid = prescription.attendid 
				AND episode.orgcodeprovider = prescription.orgcodeprovider 
				AND episode.apptdate = prescription.apptdate
		WHERE
			episode.orgcodeprovider NOT IN ('7A1', '7A3', 'RQF', 'RP6') /*SL comment: RP6 moorfield eye clinic*/
			AND patient.aliasflag = 0                   
	),

	filtered_patients AS(
		SELECT DISTINCT
			initial_patients.*,
			MIN(initial_patients.treatmentstartdate) 
				OVER(
					PARTITION BY 
						initial_patients.radiotherapyepisodeid,
						initial_patients.orgcodeprovider
				) first_treatmentstartdate
		FROM initial_patients
		WHERE
			initial_patients.radiotherapyintent = '02'
			AND initial_patients.rttreatmentmodality = '05'
			AND	initial_patients.patients_per_episode = 1
			AND (
					(SUBSTR(radiotherapydiagnosisicd,1,3) BETWEEN 'C33' AND 'C34' OR SUBSTR(radiotherapydiagnosisicd,1,3) BETWEEN 'C00' AND 'C14') 
					OR (SUBSTR(radiotherapydiagnosisicd,1,3) BETWEEN 'C30' AND 'C32')
				)
	),

	dated_patients AS(
		SELECT DISTINCT
			filtered_patients.*,
			CASE
				WHEN EXTRACT(MONTH FROM filtered_patients.first_treatmentstartdate) IN (1,2,3) 
					THEN EXTRACT(YEAR FROM filtered_patients.first_treatmentstartdate) - 1
				ELSE EXTRACT(YEAR FROM filtered_patients.first_treatmentstartdate)
			END AS financialyear
		FROM filtered_patients
		WHERE TO_DATE(filtered_patients.first_treatmentstartdate) BETWEEN TO_DATE('01/04/2022','dd/mm/yyyy') AND TO_DATE('31/03/2024 23:59:00', 'DD/MM/YY HH24:MI:SS')
	),

    lung_patients AS(
		SELECT DISTINCT
			dated_patients.*,
			'lung' AS cancer_group,
            ROW_NUMBER() 
                OVER(
                    PARTITION BY 
                        dated_patients.patientid,
                        dated_patients.financialyear 
                    ORDER BY
                        dated_patients.first_treatmentstartdate ASC,
                        dated_patients.radiotherapyepisodeid ASC
                ) episode_rank
		FROM dated_patients
		WHERE
			SUBSTR(dated_patients.radiotherapydiagnosisicd,1,3) BETWEEN 'C33' AND 'C34'
	),

    headandneck_patients AS(
		SELECT DISTINCT
			dated_patients.*,
			'head_and_neck' AS cancer_group,
            ROW_NUMBER() 
                OVER(
                    PARTITION BY 
                        dated_patients.patientid,
                        dated_patients.financialyear 
                    ORDER BY
                        dated_patients.first_treatmentstartdate ASC,
                        dated_patients.radiotherapyepisodeid ASC
                ) episode_rank
		FROM dated_patients
		WHERE 
			(SUBSTR(dated_patients.radiotherapydiagnosisicd,1,3) BETWEEN 'C00' AND 'C14' OR SUBSTR(dated_patients.radiotherapydiagnosisicd,1,3) BETWEEN 'C30' AND 'C32')
	),
	
	all_patients AS(
		SELECT DISTINCT * FROM headandneck_patients
		WHERE headandneck_patients.episode_rank = 1
		
		UNION
		
		SELECT DISTINCT * FROM lung_patients
		WHERE lung_patients.episode_rank = 1
	),
    
    extract_patients AS(
        SELECT DISTINCT
            birthdatebest,
            deathdatebest,
            embarkation,
            embarkationdate,
            cancer_group,
            radiotherapydiagnosisicd,
            epi_duration_flag,
            nhsnumber,
            analysiscongchen.nhs_number_classification_2020@casref02(nhsnumber) nhsnumber_check,
            orgcodeprovider,
            patientid,
			tumourid,
	        a.Providername_new AS Provider_name,
            radiotherapyepisodeid,
            gender, 
            treatmentstartdate,
            tumourcount,
            vitalstatus,
            vitalstatusdate
        FROM all_patients
        LEFT JOIN analysisncr.trustsics a on all_patients.orgcodeprovider = a.CODE
    )
SELECT *
from extract_patients;



