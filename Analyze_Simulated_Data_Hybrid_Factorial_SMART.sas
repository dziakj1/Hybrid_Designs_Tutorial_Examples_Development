PROC IMPORT OUT=sim_data 
            DATAFILE= "C:\Users\dziak\OneDrive - University of Illinois Chicago\Documents\SimExamples\Simulated_Data_Hybrid_Factorial_SMART.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC MEANS DATA=sim_data; RUN;

DATA sim_data; 
	SET sim_data;
	replicate_weight = 4*(R=0) + 2*(R=1);
RUN;

DATA positive_pseudodata;
	SET sim_data;
	WHERE R=1;
	meal = 1;
	replicate = 1;
	/* We keep the same subject ID to show that we don't really have all those
	 new participants.  So we have to distinguish the new observations somehow,
	 and so we treat them as new waves of data on the same person.   */
RUN;
DATA negative_pseudodata;
	SET sim_data;
	WHERE R=1;
	meal = -1;
	replicate = 2;
RUN;
DATA not_replicated;
	SET sim_data;
	WHERE R=0;
	replicate = 1;
RUN;

DATA for_analysis;
	SET not_replicated positive_pseudodata negative_pseudodata;
RUN;

PROC FREQ DATA=for_analysis; 
	TABLES R replicate meal;
RUN;


PROC GENMOD DATA=for_analysis;
	CLASS ID;
	MODEL final_kg_lost = is_female baseline_bmi_centered app | coaching | meal;
	REPEATED SUBJECT=ID;
	WEIGHT replicate_weight;
RUN;
