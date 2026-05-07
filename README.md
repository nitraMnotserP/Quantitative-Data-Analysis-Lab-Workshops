# QQDAL Workshop Materials

This repository contains materials for workshops I developed for the **University at Buffalo's Quantitative and Qualitative Data Analysis Lab (QQDAL) annual workshop series**.

## Workshops Included

- **Advanced Analyses in R**
- **Analyses of Complex Survey Data**
- **Data Visualization with ggplot2**
- **Introduction to R and RStudio**
- **Propensity Score Matching in R**

---

## Advanced Analyses in R

This workshop introduces a range of advanced statistical modeling techniques in R.

### Topics Covered

- Data manipulation with `dplyr`
- Two- and three-level **Hierarchical Linear Models (HLM)**
- **Hierarchical Generalized Linear Models (HGLM)** using `lmer`
- Two- and three-level **survey-weighted H(G)LMs** using `WeMix`
- **Latent Profile Analysis (LPA)** using `tidyLPA`
- **Classification and Regression Trees (CART)** using `tidymodels`

### Dataset Used

Public-use **2022 Programme for International Student Assessment (PISA)**.

---

## Analyses of Complex Survey Data

This workshop focuses on statistical methods for **complex survey designs** using the `survey` and `svyVGAM` packages (Lumley, 2011; 2024).

### Variance Estimation Methods

- **Taylor Series Linearization (TSL)**
- **Balanced Repeated Replication (BRR)**

### Topics Covered

- Defining complex survey designs
- Subsetting survey designs for correct variance estimation
- **Design-based logistic regression**
- **Predicted probabilities** and **average marginal effects** using `marginaleffects`
- **Linear Probability Models**
- **Ordinal logistic and probit models** (proportional odds models)
- **Generalized ordinal logistic regression**
- **Partial proportional odds models**
- **Multinomial logistic regression**
- **Zero-inflated Poisson models**
- **Conditional quantile regression**

### Dataset Used

Public-use **Education Longitudinal Study of 2002 (ELS:2002)**.

---

## Data Visualization with ggplot2

This workshop provides an introduction to **data visualization in R using `ggplot2`**.

### Visualizations Covered

- Histograms
- Boxplots
- Pie charts
- Univariate and bivariate bar plots
- Scatter plots
- Heat maps
- Regression diagnostic plots (linear and logistic models)
- Clustered slope plots for hierarchical linear models

---

## Introduction to R and RStudio

This workshop introduces the **fundamentals of programming and statistical analysis in R**.

### Topics Covered

- R syntax and operators
- Data types and data structures
- Working with data frames and data tables
- Basic descriptive statistics
- Correlations
- Simple linear regression

---

## Propensity Score Matching in R

This workshop provides a practical introduction to **propensity score methods for causal inference** using observational data. The workshop emphasizes applied implementation, diagnostics, and extensions for missing data and complex survey designs.

### Topics Covered

- Causal inference framework (counterfactuals, ATT vs. ATE)
- Selection bias and naive vs. adjusted treatment effects
- Propensity score estimation using logistic regression
- **Nearest neighbor matching**, including calipers and exact matching
- **Mahalanobis distance matching**
- Assessing covariate balance using standardized mean differences and variance ratios
- Visual balance diagnostics using **Love plots**
- Outcome analysis using matched samples and matching weights
- Regression adjustment for remaining imbalance

### Extensions Covered

- Propensity score matching with **multiple imputation** using `mice` and `MatchThem`
- Pooling treatment effect estimates across imputed datasets
- Incorporating **survey weights** into propensity score matching
- Design-based outcome models using `survey` and `mitools`
- Estimation of **odds ratios** and **average marginal effects**

### Data Sources

- Simulated data: Afterschool tutoring intervention
- Real data: Public-use **Education Longitudinal Study of 2002 (ELS:2002)**
