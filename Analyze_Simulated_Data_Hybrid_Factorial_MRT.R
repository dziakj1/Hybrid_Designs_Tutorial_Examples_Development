library(geepack)   

# Read the simulated person-level data for distal outcome analysis
person_level <- read.csv("Simulated_Data_Hybrid_Factorial_MRT_person_level.csv")
print(dim(person_level))
print(head(person_level))
print(summary(person_level))
# Fit analysis model for distal outcome
distal_outcome_model <- lm(formula = final_kg_lost ~ is_female + 
                             baseline_bmi_centered + 
                             coaching * meal,
                           data=person_level)
print(summary(distal_outcome_model))

# Read the simulated occasion-level data for proximal outcome analysis
occasion_level <- read.csv("Simulated_Data_Hybrid_Factorial_MRT_occasion_level.csv")
print(dim(occasion_level))
print(head(occasion_level))
print(summary(occasion_level))
# Fit analysis model for proximal outcome
gee_proximal <- geeglm(formula = proximal_outcome ~ is_female + 
                       baseline_bmi_centered + 
                       A * coaching * meal,
                     family=binomial(link=log),
                     data=occasion_level,
                     id=ID,
                     corstr = "independence"); 
print(summary(gee_proximal))