set.seed(60611)

# Code to simulate hybrid factorial-MRT following example 
# in Figure 2 of Nahum‑Shani et al., (2023, Behavior Research Methods)

# Page 17 in Arxiv version is start of description, p. 57 in Arxiv version is Figure 2

# Distal outcome: weight loss , kg, month 6
# Proximal outcome: daily intake monitoring
n = 400

# Generate random data
is_female <- sample(c(-1,1),n,replace=TRUE,prob=c(.3,.7))
baseline_bmi <- round(runif(n, min=31, max=42),1)
baseline_bmi_centered <- round(baseline_bmi - mean(baseline_bmi), 2)
Z11 <- sample(c(-1,+1),size=n,replace=TRUE)
# represents coaching, +1 for yes, -1 for no
Z12 <- sample(c(-1,+1),size=n,replace=TRUE)
# represents meal replacement, +1 for yes, -1 for no
p_respond <- .5 + .1*Z11 + .1*Z12

R <- rbinom(n,1,p_respond)
# 0 for nonresponders, 1 for responders,

true_distal_params <- data.frame(
  sigma	= 2.5,	# error SD; variable is kg lost
  theta_0_0 =  1.5 ,	# intercept
  theta_0_1 =  0,	# female
  theta_0_2 = -.1,	# baseline BMI
  theta_1	= .3,	# coaching main effect
  theta_2	= .2,	# meal replacement main effect
  theta_3	= 0,	# coaching X meal replacement
  eta	= 1.5)	    # noncausal effect of being a responder 
# The above are the true parameters of conditional data-generating model; 
# theta_1 through theta_3 are also the same for the 
# marginal model assuming identity link function;

# True parameters

EY_distal <- true_distal_params$theta_0_0 +                 # intercept
  true_distal_params$theta_0_1 * is_female + 
  true_distal_params$theta_0_2 * baseline_bmi_centered + 
  true_distal_params$theta_1 * Z11 +               # main effect of coaching
  true_distal_params$theta_2 * Z12 +                   # main effect of meal replacement
  true_distal_params$theta_3 * Z11 * Z12 +              # interaction of coaching and meal replacement
  true_distal_params$eta  * R
# expected kg lost;

residuals_distal_Y <- rnorm(n,0,true_distal_params$sigma)
distal_Y <- round(EY_distal + residuals_distal_Y,1)

sim_person_level_data <- data.frame(
  ID = 1:n,
  is_female = is_female,
  baseline_bmi_centered = baseline_bmi_centered,
  coaching = Z11,
  meal = Z12,
  R = R,
  final_kg_lost = distal_Y)

person_level_fit <- lm(formula = final_kg_lost ~ is_female + 
                         baseline_bmi_centered + 
                         coaching * meal,
                       data=sim_person_level_data)
print(summary(person_level_fit))



write.csv(sim_person_level_data,
          "Simulated_Data_Hybrid_Factorial_MRT_person_level.csv", 
          row.names = FALSE)

# Generate occasion-level outcomes

