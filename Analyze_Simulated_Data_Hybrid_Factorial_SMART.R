
library(geepack)   

# Read the simulated data
sim_data <- read.csv("Simulated_Data_Hybrid_Factorial_SMART.csv")

print(head(sim_data))

print(summary(sim_data))

# # Apply pseudo replication :
sim_data$replicate_weight <- 4*(sim_data$R==0) + 2*(sim_data$R==1)
sim_data$replicate <- 1
rows_to_replicate <- sim_data[which(sim_data$R==1),]
rows_not_to_replicate <- sim_data[which(sim_data$R==0),]
positive_pseudodata <- rows_to_replicate
positive_pseudodata$meal <- 1
positive_pseudodata$replicate <- 1
negative_pseudodata <- rows_to_replicate
negative_pseudodata$meal <- -1
negative_pseudodata$replicate <- 2
# We keep the same subject ID to show that we don't really have all those
# new participants.  So we have to distinguish the new observations somehow,
# and so we treat them as new waves of data on the same person.   
data_for_analysis <- rbind(positive_pseudodata,
                           negative_pseudodata,
                           rows_not_to_replicate)
data_for_analysis <- data_for_analysis[order(data_for_analysis$ID,
                                             data_for_analysis$replicate),]
rownames(data_for_analysis) <- NULL
print(head(data_for_analysis))

# Fit analysis model 
weighted_and_replicated_model <- geeglm(formula = final_kg_lost ~ is_female + 
                                          baseline_bmi_centered + 
                                          app * coaching * meal,
                     id=ID, 
                     weights = replicate_weight, 
                     data=data_for_analysis,
                     corstr = "independence") 
print(summary(weighted_and_replicated_model))