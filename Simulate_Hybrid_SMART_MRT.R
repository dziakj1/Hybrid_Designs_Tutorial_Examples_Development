rm(list = ls())
set.seed(654321)
library(dplyr)

# Code to simulate hybrid SMART-MRT following example 
# in Figure 3 of Nahum‑Shani et al., (2023, Behavior Research Methods)

# Page 26 in Arxiv version is start of description, p. 58 in Arxiv version is Figure 2

# Distal outcome: weight loss, kg, month 6
# Proximal outcome: daily intake monitoring
n = 400

# Generate random data
is_female <- sample(c(-1,1),n,replace=TRUE,prob=c(.3,.7))
baseline_bmi <- round(runif(n, min=31, max=42),1)
baseline_bmi_centered <- round(baseline_bmi - mean(baseline_bmi), 2)
Z1 <- sample(c(-1,+1),size=n,replace=TRUE)
# represents coaching, +1 for yes, -1 for no
p_respond <- .5 + .1*Z1

R <- rbinom(n,1,p_respond)
# 0 for nonresponders, 1 for responders,

Z2 <- rep(0,n)
Z2[which(R==0)] <- sample(c(-1,+1),size=sum(R==0),replace=TRUE)
# represents meal replacement, +1 for yes, -1 for no, NA for not assigned
 

true_distal_params <- data.frame(
  sigma	= 2.5,	# error SD; variable is kg lost
  theta_0_0 =  1.5 ,	# intercept
  theta_0_female =  0,	# female
  theta_0_bmi = -.1,	# baseline BMI
  theta_coaching	= .3,	# coaching main effect
  theta_meal	= .3,	# meal replacement main effect
  theta_coaching_meal	= 0,	# coaching X meal replacement
  eta	= 1.2)	    # noncausal effect of being a responder 
# The above are the true parameters of conditional data-generating model; 
# theta_coaching through theta_coaching_meal are also the same for the 
# marginal model assuming identity link function;

# True parameters

EY_distal <- true_distal_params$theta_0_0 +                 # intercept
  true_distal_params$theta_0_female * is_female + 
  true_distal_params$theta_0_bmi * baseline_bmi_centered + 
  true_distal_params$theta_coaching * Z1 +               # main effect of coaching
  true_distal_params$theta_meal * Z2 +                   # main effect of meal replacement
  true_distal_params$theta_coaching_meal * Z1 * Z2 +              # interaction of coaching and meal replacement
  true_distal_params$eta  * R
# expected kg lost;

residuals_distal_Y <- rnorm(n,0,true_distal_params$sigma)
distal_Y <- round(EY_distal + residuals_distal_Y,1)

sim_person_level_data <- data.frame(
  ID = 1:n,
  is_female = is_female,
  baseline_bmi_centered = baseline_bmi_centered,
  coaching = Z1,
  meal = Z2,
  R = R,
  final_kg_lost = distal_Y)

distal_outcome_model <- lm(formula = final_kg_lost ~ is_female + 
                             baseline_bmi_centered + 
                             coaching * meal,
                           data=sim_person_level_data)
print(summary(distal_outcome_model))

# Generate occasion-level outcomes

# Simulate the long dataset used in the proximal analysis,
# i.e., generate occasion-level outcomes 
nobs <- 84;
true_proximal_params <- data.frame(beta_0_0=log(.6),
                                   beta_0_female=.01,
                                   beta_0_bmi=0,
                                   beta_coaching=.02,
                                   beta_meal=.01,
                                   beta_coaching_meal=.01,
                                   gamma_A=.01,
                                   gamma_A_coaching=.01,
                                   gamma_A_meal=0,
                                   gamma_A_coaching_meal=0,
                                   eta_R=.02,
                                   eta_backwards=.04);
random_effect_sigma <- .1;
A <- matrix(sample(c(+1,-1),
                   prob=c(.5,.5),
                   replace=TRUE,
                   size=n*nobs),
            n,nobs);
# In this version of the code, everybody gets randomized daily from the very beginning, as in the 
# conceptual example in Nahum‑Shani et al., p. 58.  If participants are not randomized daily unless and until they are nonresponders, then A should be zero before responder status assessment, and also zero for responders after that. 
sim_data_for_generating_proximal <- reshape2::melt(data=cbind(sim_person_level_data,A),  
                                                   id=colnames(sim_person_level_data),
                                                   variable.name="day",
                                                   value.name="A");
sim_data_for_generating_proximal$day <- as.integer(sim_data_for_generating_proximal$day);
linear_predictor_Y_prox <- as.numeric( true_proximal_params$beta_0_0 +
                                         true_proximal_params$beta_0_female * sim_data_for_generating_proximal$is_female +
                                         true_proximal_params$beta_0_bmi * sim_data_for_generating_proximal$baseline_bmi_centered +
                                         true_proximal_params$beta_coaching * sim_data_for_generating_proximal$coaching +
                                         true_proximal_params$beta_meal * sim_data_for_generating_proximal$meal +
                                         true_proximal_params$beta_coaching_meal * sim_data_for_generating_proximal$coaching * sim_data_for_generating_proximal$meal +
                                         true_proximal_params$gamma_A * sim_data_for_generating_proximal$A +
                                         true_proximal_params$gamma_A_coaching * sim_data_for_generating_proximal$A* sim_data_for_generating_proximal$coaching +
                                         true_proximal_params$gamma_A_meal * sim_data_for_generating_proximal$A* sim_data_for_generating_proximal$meal +
                                         true_proximal_params$gamma_A_coaching_meal * sim_data_for_generating_proximal$A * sim_data_for_generating_proximal$coaching * sim_data_for_generating_proximal$meal +
                                         true_proximal_params$eta_R * sim_data_for_generating_proximal$R +
                                         true_proximal_params$eta_backwards * scale(sim_data_for_generating_proximal$final_kg_lost));


prob_Y_prox <- exp(linear_predictor_Y_prox);
stopifnot(min(prob_Y_prox)>= 0);
stopifnot(max(prob_Y_prox)<= 1);

sim_data_for_generating_proximal$proximal_outcome <- rbinom(n=length(prob_Y_prox),
                                                            size=1,
                                                            prob=prob_Y_prox); 

print(round(cor(sim_data_for_generating_proximal),2));

sim_occasion_level_data <- sim_data_for_generating_proximal %>% arrange(ID, day) %>%
  select(ID, day, is_female, baseline_bmi_centered, 
         coaching, R, meal, A, proximal_outcome)

print(head(sim_occasion_level_data))


naive_proximal_outcome_model <- glm(formula = proximal_outcome ~ is_female + 
                                      baseline_bmi_centered + 
                                      A * coaching * meal,
                                    family=binomial(link=log),
                                    data=sim_occasion_level_data)
print(summary(naive_proximal_outcome_model))


sim_person_level_data$meal[which(sim_person_level_data$meal==0)] <- NA
write.csv(sim_person_level_data,
          "Simulated_Data_Hybrid_SMART_MRT_person_level.csv", 
          na="",
          row.names = FALSE)

sim_occasion_level_data$meal[which(sim_occasion_level_data$meal==0)] <- NA
write.csv(sim_occasion_level_data,
          "Simulated_Data_Hybrid_SMART_MRT_occasion_level.csv", 
          na="",
          row.names = FALSE)