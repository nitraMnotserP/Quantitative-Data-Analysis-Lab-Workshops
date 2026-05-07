########################################################################################
####Quantitative and Qualitative Data Analysis Lab: Fall 2026 Workshop Series###########
########################################################################################
####Advanced Analyses with Complex-Survey Data in R with the Survey Package#############
#####################QQDAL GA: Preston Martin###########################################
########################################################################################


##install the packages we will use (***if you haven't already***)
install.packages("survey")
install.packages("svyVGAM")
install.packages("tidyverse")
install.packages("marginaleffects")
install.packages("quantreg")

##load these packages to start
library(tidyverse)
library(survey)
library(svyVGAM)

##set YOUR working directory (whenever you store the data I sent in the email you got this script)
setwd("~/Documents/QQDAL/Workshops/Advanced Survey Methods in R")

##read in the data files starting with the student file
els.2002 <- read_csv("ELS_2002.csv")
##next the BRR weights
BRR <- read_csv("ELS_2002_BRR.csv")

##We must ensure our analytic data set reflects the target population defined by the survey design.
##in this case, the nationally representative 10th-grade cohort.
##we are also going to merge our BRR weights into our data set.

els.g10 <- els.2002 %>% 
  select(-BYSTUWT) %>% ##remove student weight so that it does not duplicate
  left_join(BRR, by = "STU_ID") %>%  ##merge BRR weights
  filter(G10COHRT == 1) ##filter data to only those where the G10 flag is 1 (i.e., exclude those not in 10th grade sample).
  
##let's quickly recode our raw variables to the form we need them for the subsequent analysis

##set our missing values
els.g10 <- els.g10 %>% 
  mutate(
    BYSTEXP = if_else(BYSTEXP %in% c(-8, -4, -1), NA_real_, BYSTEXP),
    BYSES1 = if_else(BYSES1 %in% c(-8, -4), NA_real_, BYSES1),
    BYMATHSE = if_else(BYMATHSE %in% c(-9, -8, -4), NA_real_, BYMATHSE),
    BYTXCSTD = if_else(BYTXCSTD %in% c(-8), NA_real_, BYTXCSTD),
    BYRACE = if_else(BYRACE %in% c(-8,-4), NA_real_, BYRACE),
    BYSEX = if_else(BYSEX %in% c(-8,-4), NA_real_, BYSEX),
    BYGRDRPT = if_else(BYGRDRPT %in% c(97,98,99), NA_real_, BYGRDRPT),
    BYRISKFC = if_else(BYRISKFC %in% c(-9, -8, -4), NA_real_, BYRISKFC))

###now we will transform and/or compute some variables
els.g10.r <- els.g10 %>% 
  mutate(
    ##create dummy indicator for expecting at least a Masters degree
    high_expect = case_when(
      BYSTEXP %in% c(6,7) ~ 1,
      BYSTEXP %in% c(-1,1,2,3,4,5) ~ 0,
      TRUE ~ NA_real_
    ),
    high_expect = as.factor(high_expect), ##set is a factor
    
    ##create and ordinal educational expectations variables
    Expectations_Ord = as.ordered(BYSTEXP),
    
    ##create binary high self-efficacy variable 
    high_math_self_eff = if_else(BYMATHSE > 0.781, 1, 0, missing = NA_real_),
    high_math_self_eff = as.factor(high_math_self_eff),
    
    ##create math groups based on tertiles (high, middle, low)
    Math_Group = case_when(
      BYTXCSTD < 46.21 ~ 1,
      BYTXCSTD >= 46.21 & BYTXCSTD <= 55.22 ~ 2,
      BYTXCSTD > 55.22 ~ 3,
      TRUE ~ NA_real_
    ),
    Math_Group = as.ordered(Math_Group), ##set as ordered
    
    ##create gender dummy indicator
    Female = case_when(
      BYSEX == 1 ~ 0,
      BYSEX == 2 ~ 1, ##female = 1
      TRUE ~ NA_real_
    )) %>%  ##finally, to keep things clean, select only the variables we want in our resultant data frame
  select(STU_ID, STRAT_ID, PSU, BYTXCSTD, BYSES1, BYRISKFC, high_expect:Female, BYSTUWT, BYSTU1:BYSTU200)
  
  
##################################################################################################################
###################Defining Surveys with Different Variance Estimation Strategies#################################
##################################################################################################################

##We we can begin by creating survey designs using different variance estimation strategies

####Taylor Series Linearization with 'survey' package and svydesign()

els.TSL.survey <- svydesign(
  ids = ~PSU, ##Defines clusters (schools) to account for similarity among students in the same building.
  strata = ~STRAT_ID, ##Groups clusters to reflect the specific categories used in the ELS sampling plan.
  weights = ~BYSTUWT, ##Scales each student to represent their portion of the national 10th-grade population.
  data = els.g10.r, ##Specifies the data frame containing your ELS:2002 variables and weights.
  nest = TRUE) ##Ensures R treats school IDs as unique only within their specific stratum to prevent calculation errors.

##examine survey design
summary(els.TSL.survey)

####Balanced Repeated Replication with 'survey' package and svrepdesign()

els.BRR.survey <-  
  svrepdesign( ##function tells R to use a replicate based variance estimation strategy
    weights = ~BYSTUWT, ##This is the sampling weight (MUST have tilde '~' in front of weight name)
    repweights = "BYSTU[0-9]+", ##This is the range of replicates; change name or replicate counts to fit your needs
    type = "BRR", ##Tell what type of replication these weights are used for
    data = els.g10.r, ##Data set you are using
    combined.weights = TRUE) ##TRUE if the repweights already include the sampling weights (they often do). 

##examine survey design
summary(els.BRR.survey)

##For subgroup analyses (within our population), we must subset from within our survey design to ensure our 
##standard errors (variance) are estimated correctly; this works for both TSL and BRR designs

##say we want to do subgroup analyses for male and female students, we filter our data like this:
els.TSL.survey.female <- subset(els.TSL.survey, Female == 1) ##reduces survey design to only female students
els.TSL.survey.male <- subset(els.TSL.survey, Female == 0) ##reduces survey design to only male students

els.TSL.survey.female
els.TSL.survey.male

##################################################################################################################
##############################Logistic Regression Models##########################################################
##################################################################################################################

##We calculate standard design-based logistic regression using the svyglm()
##to specify logit link: family=quasibinomial(link = "logit"); there are alternative specification such as probit, cloglog, ect.

logit <- svyglm(high_expect ~ BYSES1 + BYTXCSTD + Female, ##regression equation
                family=quasibinomial(link = "logit"), ##to calculate a logistic regression, we change out family name to quasi-binomial and include logit-link function
                design = els.TSL.survey, ##same survey design
                na.action = na.omit) ##listwise deletion

summary(logit) ##print results
exp(logit$coefficients) ##get odds ratios

##################################################################################################################
#######################################Predicted Probabilities####################################################
##################################################################################################################

##Create a "Hypothetical Data set" (The "New Data"):
##We create a sequence of SES scores from -2 to 2 (Low to High).
##We hold 'Female' at 0.5 (the average) and 'BYTXCSTD' at its mean.
##This allows us to isolate the effect of SES on our "average" student.

hypothetical_students <- data.frame(
  BYSES1 = seq(-2, 2, length.out = 100),
  Female = mean(els.g10.r$Female, na.rm = TRUE),
  BYTXCSTD = mean(els.g10.r$BYTXCSTD, na.rm = TRUE)
)

hypothetical_students

##Generate Predictions using predict():
##'logit' = our model we just calculated
##'type' = "response" gives us probabilities (0 to 1) instead of log-odds.
##'se = TRUE' allows us to calculate confidence intervals in the following steps.
predictions <- predict(logit, newdata = hypothetical_students, type = "response", se = TRUE)

predictions

##Bind the hypothetical students data frame and the predictions and then add 95% CIs:
pred_data <- cbind(hypothetical_students, as.data.frame(predictions)) %>%
  mutate(
    lower = response - 1.96 * SE,
    upper = response + 1.96 * SE
  )

##Visualize the effect of SES on the probability of expecting a graduate degree
ggplot(pred_data, aes(x = BYSES1, y = response)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "darkgreen") +
  labs(
    title = "Probability of Expecting a Graduate Degree",
    subtitle = "Calculated using 'survey' Predict Function",
    x = "Socioeconomic Status (SES)",
    y = "Predicted Probability (0-100%)"
  ) +
  theme_bw()


##################################################################################################################
#######################################Average Marginal Effects###################################################
##################################################################################################################

##Average Marginal Effects are helpful because, according to Mood (2010), they are comparable across models and groups.

##To calculate the average marginal effects, we will use the avg_slopes() for the "marginaleffects" package:

ame.1 <- marginaleffects::avg_slopes(logit, ##model name from above
                                     variables = c("BYSES1", "BYTXCSTD", "Female"), ##variable for which probabilities will be calculated
                                     type = "response")   ##puts result on probability scale

summary(ame.1)
###Interpretations of the AMEs
## A one-SD increase in a students socioeconomic status is associated 
##with a 7.3 percentage point increase in the probability of expecting a graduate degree.
##For every 1-point increase on the standardized test, the probability increases 
##by 1.3 percentage points. 


##Let's calculate the following models for the male and female subgroups and calculate comparable AMEs
logit.m <- svyglm(high_expect ~ BYSES1 + BYTXCSTD + high_math_self_eff, 
                  family=quasibinomial(link = "logit"),
                  design = els.TSL.survey.male, ##survey design with just male students
                  na.action = na.omit)

logit.f <- svyglm(high_expect ~ BYSES1 + BYTXCSTD + high_math_self_eff, 
                  family=quasibinomial(link = "logit"), 
                  design = els.TSL.survey.female, ##survey design with just female students
                  na.action = na.omit) 


ame.m <- marginaleffects::avg_slopes(logit.m, ##model name from above
                                     variables = c("BYSES1", "BYTXCSTD", "high_math_self_eff"), ##variable for which probabilities will be calculated
                                     type = "response")   ##puts result on probability scale

ame.f <- marginaleffects::avg_slopes(logit.f, 
                                     variables = c("BYSES1", "BYTXCSTD", "high_math_self_eff"), 
                                     type = "response")  

##compare the magnitudes of the effect
summary(ame.m)
summary(ame.f)

##For Male students, having high math self-efficacy increases the probability of high expectations by 17.2 percentage points.
##For Female students, it only increases it by 9.9 percentage points.

##################################################################################################################
############################################Linear Probability Model##############################################
##################################################################################################################

##The linear probability models uses the standard multiple linear regression framework 
##but includes a binary, rather than continuous, outcome variable. The coefficients then 
##represent changes in probability (percentage-point changes) of the outcome.

els.lpm <- svyglm( ##this is the generic regression function for linear and logistic regression in the survey package
  formula = as.numeric(high_math_self_eff) ~ BYSES1 + BYTXCSTD + Female, ##the actual variables in the regression [DV ~ IV1 + IV2 +...IVx]
  design = els.TSL.survey, ##survey design from above
  family = gaussian(link = "identity"), ##defines the type of regression; in the case of linear regression, this argument is optional as it the default if not specified.
  na.action = na.omit ##since this has missing values, we will use listwise deletion; take Dr. Han's CEP527 course to learn how to fill these values in!!!
)

# View the results
summary(els.lpm)

##interpretation:
##A one-unit increase in standardized math test score increases probability of high self-efficacy by 1.21 percentage points.
##Being female is associated with a 7.2 percentage point lower probability of high math self-efficacy


##################################################################################################################
###############Ordinal Logistic Regression Models (proportional odds model)#######################################
##################################################################################################################

##We first need to test the (very strict) main assumption of the Ordinal Logistic Regression.
##To do this, we will calculate two binary logistic regressions, if the Betas coefficients
#for our predictors are very different, we likely fail the assumption. 
##This isn't a perfect test, but it will roughly approximate it.

m2 <- svyglm(I(Math_Group >= 2) ~ BYSES1 + Female + high_math_self_eff, 
             ##Creates a binary variable equal to 1 if Math_Group >= 2, and 0 if Math_Group == 1;
             ##I() ensures the logical condition is treated "as is"
             design = els.TSL.survey, family = quasibinomial(link = "logit"))

m3 <- svyglm(I(Math_Group >= 3) ~ BYSES1 + Female + high_math_self_eff,
             design = els.TSL.survey, family = quasibinomial(link = "logit"))

##Similar coefficients → proportional odds assumption seems reasonable 
summary(m2)
summary(m3) ##female seems to vary

##For demonstration purposes, we will proceed with the calculation 
##*Currently, the svyolr() function only works with Taylor Series Linerazation.*

olr.1 <- svyolr(
  Math_Group ~ BYSES1 + Female + high_math_self_eff,
  method = "logistic", ##this is default setting
  design = els.TSL.survey
)

summary(olr.1)
exp(olr.1$coefficients) ##odds ratios for those coefficients 

#######interpretation:
##The intercept terms represent the "cut points" or thresholds between the math groups.

##The proportional odds model suggests a significant positive association between
##socioeconomic status (BYSES1) and high math group placement. For every one-unit increase 
##in SES, students are more than 3 times as likely (OR = 3.24) to be classified into a
##higher math proficiency group.


###We can also change the method argument to "probit" to calculate a Ordinal Probit Regression
##This model still uses our 3-level Math_Group (Low, Mid, High).
##It assumes these 3 groups are "slices" of an underlying normal distribution.
##The 'Thresholds' (Intercepts) in the output still represent the points where a student "crosses over" from Low to Mid, or Mid to High.

opr.1 <- svyolr(
  Math_Group ~ BYSES1 + Female + high_math_self_eff,
  method = "probit", ##this changes the link function
  design = els.TSL.survey
)

summary(opr.1)

#######Interpretation (results are in t-value (SD) changes of the dependent variable):
##For every one-unit increase in socioeconomic status, the latent math achievement 
##score increases by 0.706 standard deviations.


##################################################################################################################
####################################Alternatives to the Proportional Odds Model###################################
##################################################################################################################

##Because the proportional odds assumption is very difficult to meet, researchers must frequently
##turn to other methods of handling ordinal outcomes.

##############################################
##Generalized Ordinal Logistic Regression:####
##############################################
##we will switch DVs to Expectations (7 levels) so that we can better illustrate the relaxed assumptions
golr1 <- svy_vglm(formula = Expectations_Ord ~ BYSES1 + Female, 
                  family = cumulative(link = "logitlink", parallel = FALSE), 
                  ##parallel = FALSE relaxes the proportional odds assumption so the IV effects vary across levels
                  design = els.TSL.survey)
summary(golr1)

##interpretation: negative coefficients in this svy_vglm framework indicates increases 
##in the probability of being in higher categories of the ordinal outcome.

##So, A negative SES coefficient indicates a higher SES increases the probability 
##of being in higher categories of the ordinal outcome.

# Quick code to show the varying effects
coef_df <- data.frame(
  threshold = 1:6,
  SES_effect = coef(golr1)[7:12], ##Grabbing the BYSES1 coefficients
  Female_effect = coef(golr1)[13:18] ##Grabbing the Female coefficients
)

## if parallel odds assumption held, these coefficients should be relatively close.
##remember, lower coefficients are higher likelihoods of transition to next higher category

##SES has biggest effect at bottom of expectation distribution
ggplot(coef_df, aes(x = threshold, y = SES_effect)) +
  geom_line() + geom_point() +
  labs(title = "The Non-Parallel Effect of SES")

##Gender has largest effect in middle of distribution
ggplot(coef_df, aes(x = threshold, y = Female_effect)) +
  geom_line() + geom_point() +
  labs(title = "The Non-Parallel Effect of SES")

#####################################
##Partial Proportional Odds Model####
#####################################
##There is also a middle ground between the the proportional odds model and the 
##generalized ordinal regression: the partial proportional odds model

##for computational efficiency, we will switch the DV back to mathematics groups (3 levels)

##To calculate this, we add the parallel argument within the cumulative function:
# FALSE means Female gets (~) separate coefficients (non-parallel)
# TRUE means Female gets one single coefficient (parallel/proportional)
ppo.1 <- svy_vglm(Math_Group ~ BYSES1 + Female + high_math_self_eff, 
                  design = els.TSL.survey, 
                  family = cumulative(link = "logitlink", parallel = FALSE ~ Female))

##Print the results;  BYSES1 and math self-efficacy will only have one coefficient 
##as it is assumed they met the met the 'parallel' odds assumption
summary(ppo.1) ##print results
exp(ppo.1$coef) ##get ORs

#######Interpretation:
##BYSES1:1: For every one-unit increase in SES, the odds of remaining in the lowest math group are reduced by 69% (OR = 0.31).

##High Math Self-Efficacy: Students with high self-efficacy have 67% lower odds 
##(OR = 0.33) of being in a lower math group compared to their peers with lower self-efficacy.

##Female students have  around 14% lower odds of being in Low (vs. at least Medium/High) compared to males.
##No statistically clear difference between females and males in the odds of being in High math group compared to middle and low groups.
##In other words, thee are gender differences in the bottom of th distribution but not at the top. 

########################################
##multinational logistic regression:####
########################################

multinom <- svy_vglm(factor(Math_Group) ~ BYSES1 + Female, 
                       design = els.TSL.survey, 
                       family = multinomial(refLevel = 1)) ##refLevel = 1 indicates that the lowest level of your variable will be the reference

##we can get our log-likelihood and deviance by calling the object name
multinom
##print results (log-odds) with hypothesis tests
summary(multinom)
##get odds ratios
exp(multinom$coef)

##interpretation--relative to the lowest math group (refLevel = 1)
##BYSES1:1: For every one-SD increase in SES, a student is 2.23 times more likely to be in the Middle math group rather than the Low math group.
##BYSES1:2: For every one-SD increase in SES, a student is 5.57 times more likely to be in the High math group rather than the Low math group.
##Female:1: Female students have 17% higher odds than males of being in the middle group compared to the lowest group
##Female:2:There is no statistically significant difference between female and male students in the odds of reaching the highest group compared to the lowest



##################################################################################################################
####################################Zero-inflated Poisson models##################################################
##################################################################################################################

## We use BYRISKFC (Number of Academic Risk Factors) as our outcome.
## ZIP models estimate two things simultaneously:
## 1. The "Certain Zero" group (Logistic: Who is at zero risk?)
## 2. The "Count" group (Poisson: Among those with risk, how many factors do they have?)

##plot a survey weighted histogram of our outcome
svyhist(~BYRISKFC, els.TSL.survey)

##run the ZIP model
zip1 <- svy_vglm(BYRISKFC ~ BYSES1 + Female + high_math_self_eff,
                 family = zipoisson(), ##this family argument denotes the outcome distribution (Zero Inflated Poisson)
                 design = els.TSL.survey, 
                 crit = "coef") ##this argument has to do with the estimation of the model; alternatives include: crit = "deviance"

# View coefficients for both the 'logit' (:1) and 'Poisson' (:2) components
summary(zip1)

##(The Logistic/Inflation Link): Predicts the probability of being an 
##:1 --"Always Zero" student (someone who has zero risk factors).
##A positive coefficient in the :1 section means a student is less likely to have 
##any risk factors (because they are more likely to be a "zero").
##:2 --(The Poisson/Count Link): Predicts the number of risk factors for students who are not in the "Always Zero" group.

##SES is not significantly associated with having 0 academic risk factors (B = 0.18, SE = 0.04, p > .05).
##However, SES is negatively and significantly associated with having greater number of academic risk factors (B = -0.73, SE = 0.04, p < .001)

##################################################################################################################
##############################Conditional Quantile Regression#####################################################
##################################################################################################################

##Lumley’s survey package does not have a direct conditional quantile regression function like svyglm(). 
##However, we can do survey-weighted quantile regression using a replicate (BRR) weight design and the 'quantreg' package.
##This works because withReplicates() computes design-consistent standard errors using BRR, jackknife, or bootstrap replicate weights.
library(quantreg)

##quote() prevents R from evaluating an expression immediately.
##coef() extracts the vector of estimated regression coefficients from the model.
##tau is the quantile you want estimate [0-1]
##the 'weights = .weights' simply uses dot notation to pass weights from design object to the rq() fucntion 

rq.1 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.1, weights=.weights))))

rq.2 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.2, weights=.weights))))

rq.3 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.3, weights=.weights))))

rq.4 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.4, weights=.weights))))

rq.5 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.5, weights=.weights))))

rq.6 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~  scale(BYSES1) + Female + high_expect, tau=0.6, weights=.weights))))

rq.7 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.7, weights=.weights))))

rq.8 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.8, weights=.weights))))

rq.9 <- withReplicates(els.BRR.survey, 
                       quote(coef(rq(scale(BYTXCSTD) ~ scale(BYSES1) + Female + high_expect, tau=0.9, weights=.weights))))

##Lets examine the output for the regression at the 10th, 50th and 90th percentiles; 
##Significance Check: We divide the effect (theta) by the error (SE) to see if our T-value for a heuristic hypothesis test
rq.1
rq.5
rq.9

###For fun, lets plot out output to make it more understandable
output.1 <- data.frame(rq.1) ##save the results as a data frame
output.2 <- data.frame(rq.2)
output.3 <- data.frame(rq.3)
output.4 <- data.frame(rq.4)
output.5 <- data.frame(rq.5)
output.6 <- data.frame(rq.6)
output.7 <- data.frame(rq.7)
output.8 <- data.frame(rq.8)
output.9 <- data.frame(rq.9)


##Create list of output dfs
output_list <- list(output.1, output.2, output.3, output.4, output.5,
                    output.6, output.7, output.8, output.9)

##create a vector of tau values for constructing df for plot
taus <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

##Extract SES coefficient and SE and save them as vectors using sapply()
ses.coef <- sapply(output_list, function(x) x$theta[2]) ##if you are using this code for you own purposes you will have to edit this
ses.se   <- sapply(output_list, function(x) x$SE[2]) ##if you are using this code for you own purposes you will have to edit this

# Combine into a tidy data frame and plot using tidyverse
data.frame( ##combine vectors we just made
  taus = taus,
  coef = ses.coef,
  se   = ses.se
) %>%
  mutate( ##calculate 95% CI for the coefficients 
    lower = coef - 1.96 * se, ##lower
    upper = coef + 1.96 * se ##upper
  ) %>% 
  ggplot(aes(x = taus, y = coef)) + ##plot the quantile regression outputs
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") + ##add 0 line
  labs(
    x = "Quantile (τ)",
    y = "Standardized SES Coefficient",
    title = "Quantile Regression",
    subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()


