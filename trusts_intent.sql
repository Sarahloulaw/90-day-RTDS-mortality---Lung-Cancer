WITH
	pre_provider AS(
		SELECT DISTINCT
			episode.radiotherapyepisodeid,
			DECODE(episode.orgcodeprovider,'RH1','R1H','RNL','RNN','RGQ','RDE','RQ8','RAJ','RDD','RAJ','RBA','RH5','RA3','RA7','RDZ','R0D','RD3','R0D','RNJ','R1H','RXH','RYR','E0A','RYR', 'RM3', 'RBV', episode.orgcodeprovider) AS orgcodeprovider,
			--episode.radiotherapyintent,
			 episode.radiotherapyintent,
			episode.radiotherapydiagnosisicd,
			CASE
				WHEN EXTRACT(MONTH FROM treatmentstartdate) IN (1,2,3) 
					THEN EXTRACT(YEAR FROM treatmentstartdate) - 1
				ELSE EXTRACT(YEAR FROM treatmentstartdate)
			END financialyear
		FROM rtds.at_episodes_england@cas2504l episode
		LEFT JOIN rtds.at_prescriptions_england@cas2504l prescription
			ON 
				episode.patientid = prescription.patientid 
				AND episode.radiotherapyepisodeid = prescription.radiotherapyepisodeid 
				AND episode.attendid = prescription.attendid 
				AND episode.orgcodeprovider = prescription.orgcodeprovider 
				AND episode.apptdate = prescription.apptdate
		WHERE episode.orgcodeprovider NOT IN ('7A1', '7A3', 'RQF', 'RP6')/*SL comment: RP6 moorfield eye clinic*/
	),

	post_provider AS(
		SELECT 
            pre_provider.*,
			CASE
				WHEN SUBSTR(pre_provider.radiotherapydiagnosisicd,1,3) BETWEEN 'C00' AND 'C14' OR SUBSTR(pre_provider.radiotherapydiagnosisicd,1,3) BETWEEN 'C30' AND 'C32'
					THEN 'head_and_neck'
				WHEN SUBSTR(pre_provider.radiotherapydiagnosisicd,1,3) BETWEEN 'C33' AND 'C34'
				THEN 'lung'
			ELSE 'other'
			END cancer_group,
             a.Providername_new AS Provider_name
        FROM pre_provider
		LEFT JOIN analysisncr.trustsics a on pre_provider.orgcodeprovider = a.CODE
		WHERE
		financialyear BETWEEN 2022 AND 2024
		--AND cancer_group IN('head_and_neck','lung')
	)

SELECT DISTINCT
    provider_name,
    cancer_group,
    financialyear,
    radiotherapyintent,
    COUNT(*)
FROM post_provider
WHERE cancer_group IN ('head_and_neck', 'lung')
GROUP BY
    provider_name,
    cancer_group,
    financialyear,
    radiotherapyintent
ORDER BY
    provider_name DESC,
    cancer_group DESC,
    financialyear ASC,
    radiotherapyintent ASC;