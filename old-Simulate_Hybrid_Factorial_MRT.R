set.seed(60607)

# Code to simulate hybrid factorial-SMART following example 
# in Figure 1 of Nahum‑Shani et al., (2023, Behavior Research Methods)

# Outcome: weight loss (percent? overall?)
# Scenario:  App has a very small positive effect, that might get dwarfed by sampling error. Coaching has a moderate positive effect
# What baseline covariates are being controlled for?
# Any missing data?
n = 400
p_respond_given_Z12_pos <- .6
p_respond_given_Z12_neg <- .4

# Generate random data
is_female <- sample(c(-1,1),n,replace=TRUE,prob=c(.3,.7))
baseline_bmi <- round(runif(n, min=31, max=42),1)
baseline_bmi_centered <- round(baseline_bmi - mean(baseline_bmi), 2)
Z11 <- sample(c(-1,+1),size=n,replace=TRUE)
  # represents app, +1 for yes, -1 for no
Z12 <- sample(c(-1,+1),size=n,replace=TRUE)
  # represents coaching, +1 for yes, -1 for no
R <- 1*(Z12<0)*(rbinom(n,1,p_respond_given_Z12_pos)) + 
  1*(Z12>0)*(rbinom(n,1,p_respond_given_Z12_neg))
  # 0 for nonresponders, 1 for responders,
Z21 <- rep(0,n)
Z21[which(R==0)] <- sample(c(-1,+1),size=sum(R==0),replace=TRUE)
  # represents rescue treatment (meal replacement) for nonresponders 
  # +1 = yes, -1 = no, 0 = not relevant / responder
true_params <- data.frame(
  sigma	= 2,	# error SD; variable is kg lost
  theta_0_0 =  1.5 ,	# intercept
  theta_0_1 =  0,	# female
  theta_0_2 = -.1,	# baseline BMI
  theta_1	= .1,	# app main effect
  theta_2	= 1,	# coaching main effect
  theta_3	= .2,	# app X coaching
  theta_4	= 2,	# meal main effect
  theta_5	= 0,	# app X meal
  theta_6	= -.2,	# coaching X meal
  theta_7	= 0,	# three way ixn
  eta	= 1.5)	    # noncausal effect of being a responder 
# The above are the true parameters of conditional data-generating model; 
# theta_1 through theta_7 are also the same for the 
# marginal model assuming identity link function;

# True parameters

EY <- true_params$theta_0_0 +                 # intercept
  true_params$theta_0_1 * is_female + 
  true_params$theta_0_2 * baseline_bmi_centered + 
  true_params$theta_1 * Z11 +               # main effect of app
  true_params$theta_2 * Z12 +                   # main effect of coaching
  true_params$theta_3 * Z11 * Z12 +             # interaction of app and coaching
  true_params$theta_4 * Z21 +                   # main effect of rescue treatment on nonresponders
  true_params$theta_5 * Z11 * Z21 +             # inter
  true_params$theta_6 * Z12 * Z21 + 
  true_params$theta_7 * Z11 * Z12 * Z21 + 
  true_params$eta  * R
# expected kg lost;

residuals_Y <- rnorm(n,0,true_params$sigma)
Y <- round(EY + residuals_Y,1)

sim_data <- data.frame(
  ID = 1:n,
  is_female = is_female,
  baseline_bmi_centered = baseline_bmi_centered,
  app = Z11,
  coaching = Z12,
  R = R,
  meal = Z21,
  final_kg_lost = Y)

unweighted_model_fit <- lm(formula = final_kg_lost ~ is_female + 
                             baseline_bmi_centered + 
                             app * coaching * meal,
                        data=sim_data)
print(summary(unweighted_model_fit))

sim_data$meal[which(sim_data$meal==0)] <- NA

print(head(sim_data))
print(summary(sim_data))

write.csv(sim_data,
            "Simulated_Data_Hybrid_Factorial_SMART.csv", 
            row.names = FALSE)