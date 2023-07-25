library(geepack)   

# Read the simulated person-level data for distal outcome analysis
person_level <- read.csv("Simulated_Data_Hybrid_SMART_MRT_person_level.csv")
print(head(person_level))
print(summary(person_level))



person_level$replicate_weight <- 4*(person_level$R==0) + 2*(person_level$R==1)
person_level$replicate <- 1
person_level_rows_to_replicate <- person_level[which(person_level$R==1),]
person_level_rows_not_to_replicate <- person_level[which(person_level$R==0),]
person_level_positive_pseudo <- person_level_rows_to_replicate
person_level_positive_pseudo$meal <- 1
person_level_positive_pseudo$replicate <- 1
person_level_negative_pseudo <- person_level_rows_to_replicate
person_level_negative_pseudo$meal <- -1
person_level_negative_pseudo$replicate <- 2
# We keep the same subject ID to show that we don't really have all those
# new participants.  So we have to distinguish the new observations somehow,
# and so we treat them as new waves of data on the same person.   
person_level_data_for_analysis <- rbind(person_level_positive_pseudo,
                                        person_level_negative_pseudo,
                                        person_level_rows_not_to_replicate)
person_level_data_for_analysis <- person_level_data_for_analysis[order(person_level_data_for_analysis$ID,
                                                                       person_level_data_for_analysis$replicate),]
rownames(person_level_data_for_analysis) <- NULL
print(head(person_level_data_for_analysis))



distal_outcome_model <- geeglm(formula = final_kg_lost ~ is_female + 
                                 baseline_bmi_centered + 
                                 coaching * meal,
                               weights=replicate_weight,
                               family=gaussian,
                               id=ID,
                               data=person_level_data_for_analysis)
print(summary(distal_outcome_model))



# Read the simulated occasion-level data for proximal outcome analysis
occasion_level <- read.csv("Simulated_Data_Hybrid_SMART_MRT_occasion_level.csv")
print(head(occasion_level))
print(summary(occasion_level))



occasion_level$replicate_weight <- 4*(occasion_level$R==0) + 2*(occasion_level$R==1)
occasion_level$replicate <- 1
occasion_level_rows_to_replicate <- occasion_level[which(occasion_level$R==1),]
occasion_level_rows_not_to_replicate <- occasion_level[which(occasion_level$R==0),]
occasion_level_positive_pseudo <- occasion_level_rows_to_replicate
occasion_level_positive_pseudo$meal <- 1
occasion_level_positive_pseudo$replicate <- 1
occasion_level_negative_pseudo <- occasion_level_rows_to_replicate
occasion_level_negative_pseudo$meal <- -1
occasion_level_negative_pseudo$replicate <- 2 
occasion_level_data_for_analysis <- rbind(occasion_level_positive_pseudo,
                                          occasion_level_negative_pseudo,
                                          occasion_level_rows_not_to_replicate)
occasion_level_data_for_analysis <- occasion_level_data_for_analysis[order(occasion_level_data_for_analysis$ID,
                                                                           occasion_level_data_for_analysis$replicate),]
rownames(occasion_level_data_for_analysis) <- NULL
print(head(occasion_level_data_for_analysis))

proximal_outcome_model <- geeglm(formula = proximal_outcome ~ is_female + 
                                      baseline_bmi_centered + 
                                      A * coaching * meal,
                                    family=binomial(link=log),
                                    id=ID,
                                    weight=replicate_weight,
                                    data=occasion_level_data_for_analysis)
print(summary(proximal_outcome_model))
