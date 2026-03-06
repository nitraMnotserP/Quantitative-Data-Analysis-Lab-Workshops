#################################################################################
###########Data Wrangling with dplyr & HLM & LPA & CART##########################
################Quantitative and Qualitative Data Analysis Lab###################
##########################Preston Martin#########################################

# Install and load required packages
install.packages("tidyverse")
library(tidyverse)

# Read in the data as a .csv using the readr package
pisa <- readr::read_csv("WorkshopData.csv")


############################Filtering Data######################################

# Example 1: Keep only students from the USA
df.filter.usa <- pisa %>%
  filter(CNT == "USA")

# Example 2: Keep only students from the USA and Mexico
df.filter.multi <- pisa %>%
  filter(CNT %in% c("USA", "MEX"))

# Example 3: Keep only male students from the USA and Mexico
df.filter.male <- pisa %>%
  filter(CNT %in% c("USA", "MEX") & ST004D01T == 2)


############################Selecting Variables##################################

##Select only a subset of relevant student-level variables
pisa.2 <- pisa %>%
  select(
    CNT,          # Country ID
    CNTSCHID,     # School ID
    CNTSTUID,     # Student ID
    ESCS,         # Socioeconomic status
    DISCLIM,      # Student-reported disciplinary climate
    ANXMAT,       # Math anxiety
    SCHLTYPE,     # School type (public/private)
    ST004D01T,    # Gender
    PV1MATH,      # Math score
    W_FSTUWT,     # Student weight
    W_SCHGRNRABWT # School weight
  )

############################Recoding and Cleaning Variables############################


#Recode variables for analysis and assign correct data types
pisa.recode <- pisa.2 %>%
  mutate(
    # Recode school type: 1/2 = Private, else = Public
    Private = if_else(SCHLTYPE %in% c(1, 2), 1, 0, missing = NA_real_),
    
    # Recode gender: 1 = Female, 2 = Male
    Male = case_when(
      ST004D01T == 2 ~ 1,
      ST004D01T == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Convert data types for analysis
    CNT = as.factor(CNT),
    CNTSCHID = as.factor(CNTSCHID),
    Private = as.factor(Private),
    Male = as.factor(Male),
    ESCS = as.numeric(ESCS),
    DISCLIM = as.numeric(DISCLIM),
    ANXMAT = as.numeric(ANXMAT),
    PV1MATH = as.numeric(PV1MATH)
  )


##############################Descriptive Statistics############################

##Explore means and SDs for continuous variables
pisa.recode %>%
  summarise(
    MEAN_math = mean(PV1MATH, na.rm = TRUE),
    SD_math   = sd(PV1MATH, na.rm = TRUE),
    MEAN_ESCS = mean(ESCS, na.rm = TRUE),
    SD_ESCS   = sd(ESCS, na.rm = TRUE),
    MEAN_ANXMAT = mean(ANXMAT, na.rm = TRUE),
    SD_ANXMAT   = sd(ANXMAT, na.rm = TRUE),
    MEAN_DISCLIM = mean(DISCLIM, na.rm = TRUE),
    SD_DISCLIM   = sd(DISCLIM, na.rm = TRUE)
  )

#Examine means and SDs by gender
pisa.recode %>%
  group_by(Male) %>%
  summarise(
    MEAN_math = mean(PV1MATH, na.rm = TRUE),
    SD_math   = sd(PV1MATH, na.rm = TRUE),
    MEAN_ESCS = mean(ESCS, na.rm = TRUE),
    SD_ESCS   = sd(ESCS, na.rm = TRUE),
    MEAN_ANXMAT = mean(ANXMAT, na.rm = TRUE),
    SD_ANXMAT   = sd(ANXMAT, na.rm = TRUE),
    MEAN_DISCLIM = mean(DISCLIM, na.rm = TRUE),
    SD_DISCLIM   = sd(DISCLIM, na.rm = TRUE)
  )


############################Exploring Categorical Variables#####################

# Count number of male and female students
pisa.recode %>%
  count(Male)

# Cross-tabulation of gender by school type
pisa.recode %>%
  count(Male, Private)

############################Full Data Preparation Pipeline############################

##Clean and prepare dataset for multilevel and predictive modeling
pisa.full <- pisa %>%
  ##Select only variables needed for analysis
  select(
    CNT, CNTSCHID, CNTSTUID,
    ESCS, DISCLIM, ANXMAT,
    SCHLTYPE, SCHSEL,
    ST004D01T, PV1MATH,
    W_FSTUWT, W_SCHGRNRABWT
  ) %>%
  ###Create and recode variables
  mutate(
    Private = if_else(SCHLTYPE %in% c(1, 2), 1, 0, missing = NA_real_),
    Male = case_when(
      ST004D01T == 2 ~ 1,
      ST004D01T == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    #Create binary outcome: High math achievers
    High_Math = if_else(PV1MATH >= 531.433, 1, 0, missing = NA_real_),
    
    ##Convert variable types
    CNT = as.factor(CNT),
    CNTSCHID = as.factor(CNTSCHID),
    High_Math = as.factor(High_Math),
    Private = as.factor(Private),
    Male = as.factor(Male),
    
    ##Standardize SES and ensure numeric predictors
    ESCS = scale(as.numeric(ESCS)),
    DISCLIM = as.numeric(DISCLIM),
    ANXMAT = as.numeric(ANXMAT),
    PV1MATH = as.numeric(PV1MATH)
  ) %>%
  ##Compute school-level aggregates
  group_by(CNTSCHID) %>%
  mutate(
    school_ESCS = mean(ESCS, na.rm = TRUE),
    school_Math = mean(PV1MATH, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ##Create conditional weight and center variables
  mutate(
    cwgt.1 = W_FSTUWT / W_SCHGRNRABWT,  # Conditional student weight
    cwgt.2 = W_SCHGRNRABWT,             # School weight
    cwgt.3 = 1,                         # Level 3 weight placeholder
    school_ESCS.grand = school_ESCS - mean(school_ESCS, na.rm = TRUE) ##grand mean center
  ) %>%
  group_by(CNTSCHID) %>%
  mutate(
    ##Group mean centering: student's SES relative to thier school mean
    ESCS_groupm = ESCS - mean(ESCS, na.rm = TRUE)
  ) %>%
  ungroup()

#View structure of cleaned dataset
glimpse(pisa.full)

#################################################################################
#########################Multilevel Models (HLM) with lme4#######################
#################################################################################
# Install and load required packages
install.packages("lme4")       # Mixed effects modeling
install.packages("performance")# ICC calculation and diagnostics
install.packages("lmerTest")   # Adds p-values to fixed effects
library(lme4)
library(lmerTest)
library(performance)

##Create a USA-only subset for 2-level models (3-level models will use the full dataset)
pisa.usa <- pisa.full %>%
  filter(CNT == "USA")

###############################################
##Two-Level Models (Continuous Outcome)
###############################################

#Start with null model to estimate baseline variability
##Raudenbush & Bryk: Fully unconditional model (Random Intercepts)
mlm.1 <- lmer(PV1MATH ~ 1 + (1 | CNTSCHID), data = pisa.usa, REML = TRUE)
summary(mlm.1)

# Calculate Intraclass Correlation Coefficient (ICC)
performance::icc(mlm.1)  ##proportion of variance in PV1MATH attributable b/w schools


# Partially conditional model: Random Coefficients Model (Level-1 predictors)
mlm.2 <- lmer(PV1MATH ~ Male + ESCS_groupm + (ESCS_groupm | CNTSCHID),
  data = pisa.usa, REML = TRUE) # Random slopes for ESCS_groupm allow the SES effect to vary across schools

summary(mlm.2)

# Fully conditional model: Intercepts- and Slopes-as-Outcomes (with Cross-level interactions)
mlm.3 <- lmer(
  PV1MATH ~ Male + ESCS_groupm + Private + school_ESCS.grand + 
    school_ESCS.grand:Male + (ESCS | CNTSCHID), 
  data = pisa.usa, REML = TRUE)

summary(mlm.3)


##################################################
##Three-Level Models (Continuous Outcome)
##################################################

# Fully unconditional 3-level model (students nested in schools nested in countries)
mlm.4 <- lmer(PV1MATH ~ 1 + (1 | CNTSCHID) + (1 | CNT), data = pisa.full, REML = TRUE)

summary(mlm.4)

# Calculate ICC by level (country and school)
performance::icc(mlm.4, by_group = TRUE)

# Fully conditional 3-level model (with student- and school-level predictors)
mlm.5 <- lmer(PV1MATH ~ Male + ESCS_groupm + Private +
    school_ESCS.grand + school_ESCS.grand:Male + (1 | CNTSCHID) + (1 | CNT),
    data = pisa.full, REML = FALSE)

summary(mlm.5)

###############################################
##Two- and Three-Level HGLMs (Binary Outcome)
###############################################

##Estimate multilevel logistic regression for High_Math (binary outcome)
# Note: Computationally intensive; random slopes not included for simplicity

# Two-level HGLM with cross-level interaction
mlm.6 <- glmer(High_Math ~ Male + ESCS_groupm + Private + school_ESCS.grand + 
                 school_ESCS.grand:Male + (1 | CNTSCHID), family = binomial("logit"),
                 data = pisa.usa
)
summary(mlm.6)

# Three-level HGLM with cross-level interaction
mlm.7 <- glmer(High_Math ~ Male + ESCS_groupm + Private + school_ESCS.grand + 
    school_ESCS.grand:Male + (1 | CNTSCHID) + (1 | CNT), family = binomial("logit"),
    data = pisa.full)

summary(mlm.7)


#################################################################################
#######################Multilevel Models with WeMix##############################
#################################################################################
# Install and load required package
install.packages("WeMix")
library(WeMix)

###############################################
##Two-Level Continuous Outcome Model
###############################################

##Estimate a fully conditional two-level model (students nested in schools)
##Includes cross-level interaction and weighted data

mlm.9 <- WeMix::mix(PV1MATH ~ Male + ESCS_groupm + Private +
    school_ESCS.grand + school_ESCS.grand:Male +  (1 | CNTSCHID),
    data = pisa.usa,
    weights = c("cwgt.1", "cwgt.2"),   # student and school-level weights
    cWeights = TRUE)                     # indicate conditional weights

summary(mlm.9)
# Note: Warnings may appear due to missing data in covariates

###############################################
##Two-Level Binary Outcome Model (HGLM)
###############################################

##Estimate a two-level logistic HGLM
##Computationally intensive so only one predictor included
mlm.10 <- WeMix::mix(High_Math ~ Male + (1 | CNTSCHID),
  family = binomial(), data = pisa.usa,
  weights = c("cwgt.1", "cwgt.2"))

summary(mlm.10)
# Note: Warnings may appear due to missing data in covariates

###############################################
##Three-Level Continuous Outcome Model
###############################################

##Estimate a null three-level model (students in schools in countries)
##Computationally intensive; can be expanded with predictors and random slopes
mlm.11 <- WeMix::mix(PV1MATH ~ 1 + (1 | CNTSCHID) + (1 | CNT),
  data = pisa.full, weights = c("cwgt.1", "cwgt.2", "cwgt.3"))

summary(mlm.11)
# Note: Warnings may appear due to missing data

###############################################
##Three-Level HGLM (Optional, Not Run)
###############################################

##three-level HGLM with weights at all levels
##Computationally intensive; uncomment and modify as needed

# mlm.12 <- WeMix::mix(
#   factor(High_Math) ~ Male + (1 | CNTSCHID) + (1 | CNT),
#   data = pisa.full,
#   family = binomial(),
#   weights = c("cwgt.1", "cwgt.2", "cwgt.3")
# )
# summary(mlm.12)

# Detach packages to avoid conflicts with other modeling sections
detach("package:lmerTest", unload = TRUE)
detach("package:WeMix", unload = TRUE)
detach("package:lme4", unload = TRUE)


################################################################################
################Latent Profile Analysis (LPA) with tidyLPA#####################
################################################################################
install.packages("tidyLPA")
library(tidyLPA)


## The pisaUSA15 dataset is included in tidyLPA and contains 
## data from U.S. students in the 2015 PISA study.
data("pisaUSA15")
glimpse(pisaUSA15)  # similar to head(), but shows variable types

##take a random sample of 2000 to speed up our calculations and three of the rows 
data_subset <- pisaUSA15 %>% 
  select(broad_interest, self_efficacy, enjoyment) %>% 
  scale() ##put our variables all on the same scale

##fill in missing values with mean values 
data_subset <- single_imputation(data_subset)

##Run the latent profile analysis for models with 1 to 4 classes
results <- estimate_profiles(data_subset, 1:3)

##print the resulting fit indexes
## The output includes several information criteria:
##AIC, BIC, SABIC: lower = better fit
##Entropy: closer to 1 = better classification certainty
get_fit(results)

##select the best-fitting model (model 3) and estimate the profiles
results <- estimate_profiles(data_subset, 3)

#plot the results
plot_profiles(results, n_profiles = 3)

##get_data() returns the dataframe with a column indicating 
##each individual's most likely profile.
final <- get_data(results) %>%
  rename(profile = Class)

##Because tidyLPA maintains row order, we can safely bind profile data 
##back to the original dataset using cbind().
pisa.lpa <- cbind(pisaUSA15, final)

##now we can use them in a follow on analysis
output <- lm(instrumental_mot ~ factor(profile), data = pisa.lpa)
summary(output)


##tidyLPA functions are designed to work seamlessly with the pipe operator (%>%) 
##for compact, reproducible workflows.

##Quickly assess fit across models
pisaUSA15 %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(1:3) %>%
  get_fit()

##Plot a specific model’s profiles
pisaUSA15 %>%
  select(broad_interest, self_efficacy, enjoyment) %>%
  scale() %>%
  single_imputation() %>%
  estimate_profiles(3) %>%
  plot_profiles()

##Full workflow – from estimation to regression
lpa.output <- pisaUSA15 %>%
  select(broad_interest, self_efficacy, enjoyment) %>%
  scale() %>%
  single_imputation() %>%
  estimate_profiles(3) %>%
  get_data() %>%
  rename(profile = Class)

##Add the profile assignments back into the original data
data_with_profiles <- bind_cols(pisaUSA15, lpa.output %>% select(profile))

##Run a regression using the new profile variable
lm(instrumental_mot ~ factor(profile), data = data_with_profiles) %>%
  summary()

detach("package:tidyLPA", unload = TRUE)

#################################################################################
###Classification and Regression Trees (CART)####################################
###Adapted from Hvitfeldt (2024)#################################################
#################################################################################

#CART models are nonparametric methods that split data recursively 
#to predict either categorical outcomes (classification trees) 
#or continuous outcomes (regression trees).

install.packages("tidymodels")
install.packages("rpart")
install.packages("rpart.plot")
library(tidymodels)
library(rpart)
library(rpart.plot)

##We'll use variables from the pisa.full dataset we used prior. 
##Our classification target will be High_Math (binary: high vs. not high).

cart.data <- pisa.full %>%
  select(High_Math, ESCS, DISCLIM, ANXMAT, Private:school_Math, PV1MATH)

## Split the data into training (used to fit the model) 
## and testing (used to evaluate performance)
cart.split <- initial_split(cart.data, prop = 0.7)
cart.train <- training(cart.split)
cart.test <- testing(cart.split)

##Specify the Model
##In tidymodels, we define:
##- model type (decision_tree)
##- engine (the package doing the work)
##- mode (classification or regression)
## Classification Tree with rpart engine

class_tree_spec <- decision_tree() %>% ##model type
  set_engine("rpart") %>% ##engine
  set_mode("classification") ##mode

##Now we will fit the model:
##Predict whether a student is "High_Math" based on background variables.
##We exclude PV1MATH to avoid singular prediction.
class_tree_fit <- class_tree_spec %>%
  fit(High_Math ~ . - PV1MATH, data = cart.train)

class_tree_fit

##Now visualize tree:
##Each split represents a decision rule that separates students into more homogeneous subgroups.

class_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

##Now lets Evaluate Model Performance:
##the confusion matrix summarizes correct and incorrect classifications.
augment(class_tree_fit, new_data = cart.train) %>%
  conf_mat(truth = High_Math, estimate = .pred_class)

##Compute accuracy for training data
train_acc <- augment(class_tree_fit, new_data = cart.train) %>%
  accuracy(truth = High_Math, estimate = .pred_class)

##Compute accuracy for testing data
test_acc <- augment(class_tree_fit, new_data = cart.test) %>%
  accuracy(truth = High_Math, estimate = .pred_class)

train_acc
test_acc
##########################################################################################
##Now we will turn to regression trees using that same data set, but well split it again. 
##Regression trees work the same way but predict a continuous outcome 
##########################################################################################
##Split the data
cart.split.r <- initial_split(cart.data, prop = 0.7)
cart.train.r <- training(cart.split.r)
cart.test.r <- testing(cart.split.r)

##Lets star by setting out type, engine, and mode for classification trees
reg_tree_spec <- decision_tree() %>% ##type
  set_engine("rpart") %>% ##engine
  set_mode("regression") ##mode changed to regression

##Now we will fit the model:
##Predicting continuous math performance (PV1MATH) from student background characteristics.
reg_tree_fit <- reg_tree_spec %>%
  fit(PV1MATH ~ . -High_Math, data = cart.train.r) ##-High_Math tells R to skip that variable

reg_tree_fit ##print the results

##plot the regression tree the same way as the classification tree
reg_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

##Check RMSE of test data for training and testing data: Root Mean Squared Error, 
##is a metric used to quantify the difference  between predicted and actual values 
##in a regression model 

augment(reg_tree_fit, new_data = cart.train.r) %>%
  rmse(truth = PV1MATH, estimate = .pred) 

augment(reg_tree_fit, new_data = cart.test.r) %>%
  rmse(truth = PV1MATH, estimate = .pred)
