library(geepack)   

# Read the simulated person-level data for distal outcome analysis
person_level <- read.csv("Simulated_Data_Hybrid_SMART_MRT_person_level.csv")
print(head(person_level))
print(summary(person_level))





# Read the simulated occasion-level data for proximal outcome analysis
occasion_level <- read.csv("Simulated_Data_Hybrid_SMART_MRT_occasion_level.csv")
print(head(occasion_level))
print(summary(occasion_level))


