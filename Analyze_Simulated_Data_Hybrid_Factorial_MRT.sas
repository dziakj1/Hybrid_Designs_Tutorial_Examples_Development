PROC IMPORT OUT=person_level
            DATAFILE= "C:\Users\dziak\OneDrive - University of Illinois Chicago\Documents\SimExamples\Simulated_Data_Hybrid_Factorial_MRT_person_level.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC MEANS DATA=person_level;
RUN;

PROC GENMOD DATA=person_level;
	MODEL final_kg_lost = is_female baseline_bmi_centered coaching | meal;
RUN; QUIT;

PROC IMPORT OUT=occasion_level 
            DATAFILE= "C:\Users\dziak\OneDrive - University of Illinois Chicago\Documents\SimExamples\Simulated_Data_Hybrid_Factorial_MRT_occasion_level.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC MEANS DATA=occasion_level;
RUN;

PROC GENMOD DATA=occasion_level DESCENDING;
	CLASS ID;
	MODEL proximal_outcome = is_female baseline_bmi_centered 
                             A | coaching | meal /
                             DIST=BIN
                             LINK=LOG;
	REPEATED SUBJECT=ID;
RUN; QUIT; 
