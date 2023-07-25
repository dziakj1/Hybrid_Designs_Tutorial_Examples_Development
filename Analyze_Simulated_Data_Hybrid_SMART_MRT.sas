PROC IMPORT OUT=person_level
            DATAFILE= "C:\Users\dziak\OneDrive - University of Illinois Chicago\Documents\SimExamples\Simulated_Data_Hybrid_SMART_MRT_person_level.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN; 

PROC MEANS DATA=person_level; RUN;

DATA person_level; 
	SET person_level;
	replicate_weight = 4*(R=0) + 2*(R=1);
RUN;

DATA person_level_positive_pseudo;
	SET person_level;
	WHERE R=1;
	meal = 1;
	replicate = 1;
	/* We keep the same subject ID to show that we don't really have all those
	 new participants.  So we have to distinguish the new observations somehow,
	 and so we treat them as new waves of data on the same person.   */
RUN;
DATA person_level_negative_pseudo;
	SET person_level;
	WHERE R=1;
	meal = -1;
	replicate = 2;
RUN;
DATA person_level_not_replicated;
	SET person_level;
	WHERE R=0;
	replicate = 1;
RUN;

DATA person_level_for_analysis;
	SET person_level_not_replicated 
		person_level_positive_pseudo 
		person_level_negative_pseudo;
RUN;
 
PROC GENMOD DATA=person_level_for_analysis;
	CLASS ID;
	MODEL final_kg_lost = is_female baseline_bmi_centered coaching | meal;
	REPEATED SUBJECT=ID;
	WEIGHT replicate_weight;
RUN;

PROC IMPORT OUT=occasion_level
            DATAFILE= "C:\Users\dziak\OneDrive - University of Illinois Chicago\Documents\SimExamples\Simulated_Data_Hybrid_SMART_MRT_occasion_level.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN; 

DATA occasion_level; 
	SET occasion_level;
	replicate_weight = 4*(R=0) + 2*(R=1);
RUN;

PROC MEANS DATA=occasion_level;
RUN;

DATA occasion_level_positive_pseudo;
	SET occasion_level;
	WHERE R=1;
	meal = 1;
	replicate = 1; 
RUN;
DATA occasion_level_negative_pseudo;
	SET occasion_level;
	WHERE R=1;
	meal = -1;
	replicate = 2;
RUN;
DATA occasion_level_not_replicated;
	SET occasion_level;
	WHERE R=0;
	replicate = 1;
RUN;

DATA occasion_level_for_analysis;
	SET occasion_level_not_replicated 
		occasion_level_positive_pseudo 
		occasion_level_negative_pseudo;
RUN;

PROC GENMOD DATA=occasion_level_for_analysis DESCENDING;
	CLASS ID;
	MODEL proximal_outcome = is_female baseline_bmi_centered coaching | meal | A/
                             DIST=BIN
                             LINK=LOG;
	WEIGHT replicate_weight;
	REPEATED SUBJECT=ID;
RUN;
