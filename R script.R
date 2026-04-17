########################################################################################
####Quantitative and Qualitative Data Analysis Lab: Spring 2026 Workshop Series#########
########################################################################################
######################Propensity Score Matching in R####################################
########################QQDAL GA: Preston Martin########################################
########################################################################################

##Install packages as needed:
install.packages("tidyverse") ##data wrangling
install.packages("MatchIt") ##matching
install.packages("cobalt") ##covariate balance
install.packages("MatchThem") ##matching with MI 
install.packages("mice") ##MI
install.packages("survey") ##Working with complex surveys
install.packages("mitools") ##survey data and MI
install.packages("RSQLite") ##survey data and MI 
install.packages("patchwork") ##putting plots together
install.packages("marginaleffects") ##for AMEs

##Load the packages
library(tidyverse)
library(MatchIt)
library(cobalt)
library(MatchThem)
library(mice)
library(survey)
library(mitools)
library(RSQLite)
library(patchwork)

##set your working directory to where you saved the data I sent you:
setwd("~/Documents/QQDAL/Workshops/Propensity Score Matching")

##This is data I simulated for our initial matching models: an after school tutoring program with the aim of raising achievement (ATT)
data <- read_csv("PSM_sim_data.csv")

###Examine the data
data %>% 
  glimpse()

##Set categorical variables as factors
data <- data %>% 
  mutate(
    race = as.factor(race),
    female = as.factor(female),
    urbanicity = as.factor(urbanicity),
    treatment = as.factor(treatment)
  )

##Verify
data %>% 
  glimpse()

###Examine treatment proportions 
data %>% 
  count(treatment)


##Quick naive model to test fo differences
naive <- summary(lm(math_score_t1 ~ treatment, data = data))
naive


##Define formula for propensity score matching: '[Treatment] ~ [IV1] + [IV2] + ... + [IVx]'
##In the next step, we will pass this as an object to the matchit() to keep the code cleaner 
ps_formula <- treatment ~ race + ses_index + parent_ed + family_income + female + 
  urbanicity + math_score_t0 + read_score_t0 + gpa_baseline + school_quality + peer_support +
  expectations + grit_scale + absenteeism + motivation

##Nearest Neighbor with glm distance measure
m.out <- matchit(ps_formula, ##formula object we created above
                 data = data, ##data set used for matching
                 method = "nearest", ##nearest neighbor matching
                 distance = "logit", ##distance measure
                 ratio = 1) ##1:1 matching ratio (one-to-one)

###Examine the matching quality
##Basic table with only standardized and standardized mean differences
bal.tab(m.out, un = TRUE)

##We can add more more metrics to our outputs
bal.tab(m.out, 
        un = TRUE,  ##Include the "unmatched" (pre-match) stats for comparison
        stats = c("m", "s", "v"), ##Include standardized mean differences, and variance ratios
        thresholds = c(m = 0.1, v = 2), ##Set your "success" thresholds
        disp.v.ratio = TRUE, ##Explicitly display variance ratios
        binary = "std") ##Standardize differences for binary variables (like Race)

###Now lets turn to plotting our SMDs for an easier understanding
love.plot(m.out, binary = "std",thresholds = 0.1)

##We can customize out plots with the many options available: 
love.plot(m.out, 
          thresholds = c(m = 0.1), ##Sets the dashed vertical balance lines
          abs = TRUE,   ##Plots absolute differences (easier to read)
          var.order = "unadjusted", ##Sorts variables by their original bias
          line = TRUE, ##Connects the dots with a line
          stars = "raw", ##Puts a star next to variables that were already balanced
          sample.names = c("Original", "Matched"), ##Renames the legend
          colors = c("#E41A1C", "#377EB8"), ##Red for Original, Blue for Matched
          title = "Covariate Balance: Afterschool Tutoring Program")

##We can also look at specific variables to see matching:
bal.plot(m.out, var.name = "race") ##categorical variables
bal.plot(m.out, var.name = "family_income") ##continuous variables 

########Now that we established a decent match, lets extract our data and calculate our effect
#####Extract the matched data set
m.data <- match.data(m.out)


####Calculate the treatment effect using lm()
output <- lm(math_score_t1 ~ treatment, 
             data = m.data, 
             weights = m.data$weights) ##we keep this hear for posterity, but here they are all one so it doesn't change anything
summary(output)##print results
naive ##print naive results for comparison 

#Compared to otherwise similar students who did not attend the tutoring program, students who attended 
#the program had significantly higher mathematics test scores.
#Specifically, attending the tutoring program was associated with a 3.11 point increase in 
#mathematics achievement (B = 3.11, SE = 0.65, t = 4.77, p < .001)


####Calculate the treatment effect (Outcome regression adjusted for remaining imbalance)
robust.output <- lm(math_score_t1 ~ treatment + race + ses_index + parent_ed + family_income + female + 
                      urbanicity + math_score_t0 + read_score_t0 + gpa_baseline + school_quality + peer_support +
                      expectations + grit_scale + absenteeism + motivation, data = m.data, weights = m.data$weights)
summary(robust.output)


################################################################################
#########################now lets work with some real data#####################
################################################################################

##Read the data in ELS data
els <- read_csv("PSM_ELS_2002_Merged.csv") 

##Take a quick look at our data
glimpse(els)
##Count the missing values in the data (43455)
sum(is.na(els))

##Now lets set our categorical variables as factors:
els <- els %>%
  mutate(
    Enrolled = as.factor(Enrolled),
    Prep_Course = as.factor(Prep_Course),
    BYSEX = as.factor(BYSEX),
    BYRACE = as.factor(BYRACE),
    BYP54B = as.factor(BYP54B),
    BYSCTRL = as.factor(BYSCTRL),
    BYURBAN = as.factor(BYURBAN),
    BYREGION = as.factor(BYREGION),
    BYREGURB = as.factor(BYREGURB),
    BYREGCTL = as.factor(BYREGCTL),
    BY10FLP = as.ordered(BY10FLP), ##Ordered
    BYSTLNG2 = as.factor(BYSTLNG2),
    BYPLANG = as.factor(BYPLANG),
    BYFCOMP = as.factor(BYFCOMP),
    BYGNSTAT = as.factor(BYGNSTAT),
    BYSIBSTR = as.factor(BYSIBSTR),
    BYSIBHOM = as.factor(BYSIBHOM)
  ) 

######Note: We now estimate the effect of a college Prep_Course on the probability of college enrollment (log-odds scale)
##Treatment proportion: Attending a college preparatory course in 9th grade:
els %>% 
  group_by(Prep_Course) %>% 
  count()

##For demonstration purposes only!
##Remove NAs for this portion of our discussion becasue NAs are not allowed on DV or IVs
els.lw <- els %>%
  drop_na()

##Verify the observations were dropped 
els.lw %>% 
  group_by(Prep_Course, Enrolled) %>% 
  count()



##Define formula for propensity score matching: '[Treatment] ~ [IV1] + [IV2] + ... + [IVx]'
##In the next step, we will pass this as an object to the matchit() to keep the code cleaner 
ps_formula <- Prep_Course ~ BYSEX + BYRACE + BYP54B + BYTXMSTD + BYTXRSTD + BYTXCSTD + BYMATHSE + BYSES1 + 
  BYREGION + BYREGURB + BYREGCTL + BYACCLIM + BYSPANP + BY10FLP + BYSTLNG2 + 
  BYPLANG + BYFCOMP + BYGNSTAT + BYSIBSTR + BYSIBHOM 

##compare different matching strategies to compare outputs:

## 1. Nearest Neighbor with glm distance measure (Caliper + Ratio)
m.out <- matchit(ps_formula, ##formula object we created above
                 data = els.lw, ##data set used for matching
                 method = "nearest", ##nearest neighbor matching
                 distance = "logit", ##distance measure
                 caliper = 0.2, ##tolerance threshold used to define the maximum allowed difference in propensity scores between matched treated and control units
                 ratio = 1) ##1:1 matching ratio (one-to-one)

## 2. Nearest Neighbor with mahalanobis distance measure (here we will change out distance measure from glm)
m.out.mal <- matchit(ps_formula, ##formula object we created above
                     data = els.lw, ##data set used for matching
                     method = "nearest", ##nearest neighbor matching
                     distance = "mahalanobis") ##distance measure


## 3. Nearest Neighbor + Exact Matching
# This ensures sex and urbanicity are identical across groups
m.out.exact <- matchit(ps_formula, 
                       data = els.lw, 
                       method = "nearest", 
                       distance = "logit",
                       exact = ~ BYSEX + BY10FLP, ##match only students with identical values for these varibales
                       caliper = 0.2, 
                       ratio = 1)

######################################################################################
#################Examine the differences in mathcing using bal.tab####################
######################################################################################
##We want to examine the both the matched sample sizes and the mean differences between groups
##standard matching
bal.tab(m.out, un = TRUE,
        stats = c("m", "s", "v", "ks"), ##Include standardized mean differences, variance ratios, and Kolmogorov-Smirnov-test p-values
        thresholds = c(m = 0.1, v = 2), ##Set your "success" thresholds
        disp.v.ratio = TRUE, ##Explicitly display variance ratios
        binary = "std")

##mahalanobis distance measure
bal.tab(m.out.mal, un = TRUE,
        stats = c("m", "s", "v", "ks"), ##Include standardized mean differences, variance ratios, and Kolmogorov-Smirnov-test p-values
        thresholds = c(m = 0.1, v = 2), ##Set your "success" thresholds
        disp.v.ratio = TRUE, ##Explicitly display variance ratios
        binary = "std")

##some exact matching
bal.tab(m.out.exact, un = TRUE,
        stats = c("m", "s", "v", "ks"), ##Include standardized mean differences, variance ratios, and Kolmogorov-Smirnov-test p-values
        thresholds = c(m = 0.1, v = 2), ##Set your "success" thresholds
        disp.v.ratio = TRUE, ##Explicitly display variance ratios
        binary = "std")


###############################################################################
#################Plot the mean differences using Love plots####################
###############################################################################
##Love plot for Nearest Neighbor with glm distance measure (Caliper + Ratio)
glm.plot <- love.plot(
  m.out, ##'mathit' object; can also pass bal.tab object
  threshold = 0.1, ##where to put the dashed line (0.1 is relatively strict; Evidence for ESSA and WWC cut off is 0.2)
  abs = FALSE, ##whether to present the statistic in absolute value or not
  var.order = "unadjusted", ##how to order the variables in the plot
  title = "Standard NN Matching with Logit Distance" # This sets the title
)

##love plot for  Nearest Neighbor with mahalanobis distance measure
mal.plot <- love.plot(
  m.out.mal , 
  threshold = 0.1, 
  abs = FALSE,
  var.order = "unadjusted",
  title = "Standard Mahalanobis Matching"
)


##Love plot for Nearest Neighbor + Exact Matching
exact.plot <- love.plot(
  m.out.exact, 
  threshold = 0.1, 
  abs = FALSE,
  var.order = "unadjusted",
  title = "NN with Exact Matching and Logit Distance"
)


##Combine the four plots into a 2x2 grid (library(patchwork))
(glm.plot + mal.plot) / (exact.plot) + 
  plot_annotation(title = "Comparison of Propensity Score Matching Methods",
                  subtitle = "ELS:2002 Data - Balance Assessment",
                  tag_levels = 'A') # Labels plots as A, B, C, D

##Nearest Neighbor + Exact Matching seems like the best so we will proceed with that

##############################################################################
#################Extracting the data from the matched object##################
##############################################################################

##Extract the matched data from the Nearest Neighbor + Exact Matching object
matched_df <- match.data(m.out.exact)

##View the first few rows
matched_df %>% 
  glimpse()

##Compare N
nrow(els.lw) ##Original cleaned N
nrow(matched_df) ##Matched N

##############################################################################
#################Running our outcome model####################################
##############################################################################
##Final Outcome Model
##We can still include covariates to account for any minor remaining imbalance but the sample size precludes including all of them

res <- glm(factor(Enrolled) ~ Prep_Course+ BYSES1 + BYTXMSTD + BYSCTRL + BYURBAN + BY10FLP, 
              data = matched_df, 
              weights = matched_df$weights,
              family = binomial(link = "logit"))

##examine out put and compare it to the naive model output for above:
summary(res)


#######################################################################################
#################Now lets turn to PSM with missing data################################
#######################################################################################
library(mice)

##select a small subset of variables to speed up imputation and a random selection of 5,000 rows
els.mi <- els %>% 
  select(STU_ID, Enrolled, Prep_Course, BYSES1, BYTXMSTD, BYSCTRL, BYURBAN, BY10FLP) %>%
  sample_n(5000)

##write formula
ps_formula_imp <- Prep_Course ~ BYSES1 + BYTXMSTD + BYSCTRL + BYURBAN + BY10FLP

##Multiple Imputation using mice
##We will create 5 imputed datasets (m = 5)
els_imputed <- mice(els.mi, m = 5, print = TRUE)

##Matching across all imputed data sets using matchthem()
##This syntax mirrors matchit() almost exactly
m.imputed <- matchthem(
  ps_formula_imp,
  datasets = els_imputed,
  approach = "within",  ##within" matches within each imputed dataset
  method = "nearest",
  distance = "logit",
  ratio = 1
)

##Check balance across all imputed datasets
##Cobalt works natively with matchthem objects! 
##It will show the average balance across all imputations with bars for coverage.
love.plot(m.imputed, 
          abs = TRUE, 
          threshold = 0.1, 
          var.order = "unadjusted",
          title = "Balance Across Imputed Datasets")


###Estimate the Treatment Effect (Outcome Analysis)--we will use a glm() this time 
##as we will look at the treatment on college enrollment
##We use the with() from the mice package to pool our estimates across imputations:
results_imputed <- with(m.imputed, 
                        glm(Enrolled ~ Prep_Course + BYSES1 + BYTXMSTD + 
                              BYSCTRL + BYURBAN + BY10FLP,
                              family = binomial(link = "logit")))

##Pool the results using Rubin's Rules
pooled_results <- pool(results_imputed)
##Print the results with ORs and 95% CIs
summary(pooled_results, conf.int = TRUE, exponentiate = TRUE)
  

################################################################################################################
#####Now lets build a complex survey design with matched data (non-imputed; ill leave that fun up to you!)######
################################################################################################################
##First, This is how I imputed the data using the futuremice(), this enables parallel processing 
##and greatly speeds up the process (will take around 1 min)
##if you often have RAM issues on your computer, I don't recommend this.
#els.imp <- els %>% 
 # select(STU_ID, Prep_Course, BYSES1, BYTXMSTD, BYSCTRL, BYURBAN, BY10FLP, 
  #       BYSEX, BYRACE, Enrolled, F3BYPNLWT, PSU, STRAT_ID) %>% 
   # futuremice(m = 5, meth = "pmm", parallelseed = 12345) 

###Read in the imputed data:
imp_long <- read.csv("els_imp.csv")

##Now we need to make this data frame into a "mids" object
els.imp <- as.mids(imp_long)

##Examine treatment proportion by imputation
imp_long %>% 
  group_by(.imp, Prep_Course) %>% 
  count()

##Examine treatment proportion by imputation and enrollment status
imp_long %>% 
  group_by(.imp, Prep_Course, Enrolled) %>% 
  count() %>% 
  print(n = 50)

##Define formula for propensity score matching
formula.svy <- Prep_Course ~ BYSES1 + BYTXMSTD + BYSCTRL + BYURBAN + BY10FLP + BYSEX + BYRACE

##run the matching model within imputations with the sampling weight included
m.imputed <- matchthem(
  formula.svy,
  datasets = els.imp,
  approach = "within",  ##"within" matches within each imputed data set
  method = "nearest", 
  distance = "mahalanobis", ##we can also change our distance measure to Mahalanobis distance matching
  s.weights = "F3BYPNLWT" ##include out survey weight here
)

##examine the balance table:
bal.tab(m.imputed, un = TRUE,
        stats = c("m", "s", "v", "ks"), ##Include standardized mean differences, variance ratios, and Kolmogorov-Smirnov-test p-values
        thresholds = c(m = 0.1, v = 2), ##Set your "successful match" thresholds
        disp.v.ratio = TRUE, ##Explicitly display variance ratios
        binary = "std")


##examine the love plot
love.plot(m.imputed, 
          abs = TRUE, 
          threshold = 0.1, 
          var.order = "unadjusted",
          title = "Balance Across Imputed Datasets")



#Extracting the first imputed dataset; This will keep all rows, not just matched rows.
matched.dataset.1 <- complete(m.imputed, n = 1)
matched.dataset.2 <- complete(m.imputed, n = 2)
matched.dataset.3 <- complete(m.imputed, n = 3)
matched.dataset.4 <- complete(m.imputed, n = 4)
matched.dataset.5 <- complete(m.imputed, n = 5)

##Put those matched datasets into a list object
imp_list <- list(matched.dataset.1, matched.dataset.2, 
                 matched.dataset.3, matched.dataset.4, matched.dataset.5)

##We will need to multiply our sampling weight by our matching weight to "turn off" non-matched students
imp_list <- lapply(imp_list, function(df){
  
  df %>% 
    mutate(
      final_wt = F3BYPNLWT * weights
    )
})

##Load the needed packages for analyses
library(survey)
library(mitools)
library(RSQLite)
##create the imputation list to pass as data argument in the following survey design call
impdata <- imputationList(imp_list,
                          dbtype = "SQLite", 
                          ###You will need to change the line below this comment to a folder on your machine
                          dbname = "~/Documents/QQDAL/Workshops/Propensity Score Matching/imp_Surv_DB.db")

##create BRR survey deign object with imputation list data
els.svy <-  svydesign(ids = ~PSU, 
                      strata = ~STRAT_ID, 
                      weights = ~final_wt, 
                      data = impdata, 
                      nest=TRUE)

##Calculate the effect using a design-based logistic regression
mod1 <- with(els.svy,
             svyglm(Enrolled ~ Prep_Course + BYSES1 + BYTXMSTD + 
                      BYSCTRL + BYURBAN + BY10FLP + BYSEX + BYRACE, 
                    family = quasibinomial("logit")))

#build up out outputs
mod1.tab <- summary(MIcombine(mod1), digits = 3) ##pool
mod1.tab$z <- mod1.tab$results / mod1.tab$se ##b/SE
mod1.tab$OR <- exp(mod1.tab$results) ##OR
mod1.tab ##print


#Compared to otherwise similar students who did not attend, students who enrolled in 
#the college preparatory class had significantly higher odds of enrolling in college. 
#Specifically, attending the preparatory class was associated with a 49% increase in 
#the odds of college enrollment (B = 0.42, SE = 0.10, z = 3.86, OR = 1.49)


##For fun, lets calculate average marginal effects:
mod1.ame <- lapply(mod1, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("Prep_Course"),
                              type = "response")   ##puts it on probability scale
  
})

mod1.pooled.ame <- MIcombine(mod1.ame) ##13 & 14: high SES, Low Math (1; 0)
summary(mod1.pooled.ame)

##On the probability scale, this corresponds to an average increase of approximately 
##4 percentage points in the likelihood of enrolling in college among students who 
##attended the preparatory class (AME = 0.040, SE = 0.01, 95% CI: [0.02, 0.06]).
