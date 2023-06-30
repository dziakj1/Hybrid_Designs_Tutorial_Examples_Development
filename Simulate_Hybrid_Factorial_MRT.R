rm(list = ls())
set.seed(60611)
library(dplyr)

# Code to simulate hybrid factorial-MRT following example 
# in Figure 2 of Nahum‑Shani et al., (2023, Behavior Research Methods)

# Page 17 in Arxiv version is start of description, p. 57 in Arxiv version is Figure 2

# Distal outcome: weight loss , kg, month 6
# Proximal outcome: daily intake monitoring
n <- 400

# Generate random data
is_female <- sample(c(-1,1),n,replace=TRUE,prob=c(.3,.7))
baseline_bmi <- round(runif(n, min=31, max=42),1)
baseline_bmi_centered <- round(baseline_bmi - mean(baseline_bmi), 2)
Z11 <- sample(c(-1,+1),size=n,replace=TRUE)
# represents coaching, +1 for yes, -1 for no
Z12 <- sample(c(-1,+1),size=n,replace=TRUE)
# represents meal replacement, +1 for yes, -1 for no
p_respond <- .5 + .1*Z11 + .1*Z12

true_distal_params <- data.frame(
  sigma	= 2.5,	# error SD; variable is kg lost
  theta_0_0 =  2.5 ,	# intercept
  theta_0_female =  0,	# female
  theta_0_bmi = -.1,	# baseline BMI
  theta_coaching	= .3,	# coaching main effect
  theta_meal	= .2,	# meal replacement main effect
  theta_coaching_meal	= 0)	# coaching X meal replacement
# The above are the true parameters of conditional data-generating model; 
# theta_coaching through theta_coaching_meal are also the same for the 
# marginal model assuming identity link function;

# True parameters

EY_distal <- true_distal_params$theta_0_0 +                 # intercept
  true_distal_params$theta_0_female * is_female + 
  true_distal_params$theta_0_bmi * baseline_bmi_centered + 
  true_distal_params$theta_coaching * Z11 +               # main effect of coaching
  true_distal_params$theta_meal * Z12 +                   # main effect of meal replacement
  true_distal_params$theta_coaching_meal * Z11 * Z12               # interaction of coaching and meal replacement
# expected kg lost;

residuals_distal_Y <- rnorm(n,0,true_distal_params$sigma)
distal_Y <- round(EY_distal + residuals_distal_Y,1)

sim_person_level_data <- data.frame(
  ID = 1:n,
  is_female = is_female,
  baseline_bmi_centered = baseline_bmi_centered,
  coaching = Z11,
  meal = Z12,
  final_kg_lost = distal_Y)

print(head(sim_person_level_data))
print(summary(sim_person_level_data))

distal_outcome_model <- lm(formula = final_kg_lost ~ is_female + 
                             baseline_bmi_centered + 
                             coaching * meal,
                           data=sim_person_level_data)
print(summary(distal_outcome_model))

write.csv(sim_person_level_data,
          "Simulated_Data_Hybrid_Factorial_MRT_person_level.csv", 
          row.names = FALSE)

# Generate occasion-level outcomes

# Simulate the long dataset used in the proximal analysis,
# i.e., generate occasion-level outcomes 
nobs <- 84;
true_proximal_params <- data.frame(beta_0_0=log(.6),
                                   beta_0_female=.01,
                                   beta_0_bmi=0,
                                   beta_coaching=.02,
                                   beta_meal=.02,
                                   beta_coaching_meal=.01,
                                   gamma_A=.01,
                                   gamma_A_coaching=.01,
                                   gamma_A_meal=0,
                                   gamma_A_coaching_meal=0,
                                   eta_backwards=.04);
random_effect_sigma <- .1;
A <- matrix(sample(c(+1,-1),
                   prob=c(.5,.5),
                   replace=TRUE,
                   size=n*nobs),
            n,nobs);
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
                                         true_proximal_params$eta_backwards * scale(sim_data_for_generating_proximal$final_kg_lost));
               # Note: eta_backwards is a correlational "effect" of the distal
               # on the proximal outcomes.  Of course the real causation would be
               # in the other direction, and would probably involve a tiny 
               # contribution from each proximal time point, but the important
               # thing is that, for realism, the proximal outcome should have
               # something to do with the distal.

prob_Y_prox <- exp(linear_predictor_Y_prox);
stopifnot(min(prob_Y_prox)>= 0);
stopifnot(max(prob_Y_prox)<= 1);

sim_data_for_generating_proximal$proximal_outcome <- rbinom(n=length(prob_Y_prox),
                                                            size=1,
                                                            prob=prob_Y_prox); 

print(round(cor(sim_data_for_generating_proximal),2));

sim_occasion_level_data <- sim_data_for_generating_proximal %>% arrange(ID, day) %>%
  select(ID, day, is_female, baseline_bmi_centered, 
         coaching, meal, A, proximal_outcome);

print(head(sim_occasion_level_data))
print(summary(sim_occasion_level_data))

proximal_outcome_model <- glm(formula = proximal_outcome ~ is_female + 
                                baseline_bmi_centered + 
                                A * coaching * meal,
                              family=binomial(link=log),
                              data=sim_occasion_level_data) 
print(summary(proximal_outcome_model))


write.csv(sim_occasion_level_data,
          "Simulated_Data_Hybrid_Factorial_MRT_occasion_level.csv", 
          row.names = FALSE) 