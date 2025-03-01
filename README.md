# Project Title:  
**Climate Change is Escalating Aflatoxin Contamination of Peanuts in China**

This directory contains the code used to support the analyses, models, and results presented in the associated manuscript. The scripts include data extraction from climate files, data preprocessing, Bayesian hierarchical modeling, logistic regression, posterior analysis, and more.

---

## Directory Structure
├── README.txt                               # This README file
├── 1_extract_climate_data_from_NetCDF.R     # Extracts climate data from multiple NetCDF files
├── 2_SMOTE_class_balancing.R                # Applies SMOTE to balance class distributions
├── 3_Bayesian_hierarchical_model_JAGS.R     # Bayesian hierarchical model using JAGS
├── 4_posterior_analysis_visualization.R     # Visualizes posterior distributions from the Bayesian model
├── 5_posterior_prediction_yearwise.R        # Uses posterior samples to predict AFB1 contamination by year
├── 6_nonlinear_logistic_regression.R        # Fits a nonlinear logistic model for exceedance rates vs. concentration

---

## Description of Each Script

### 1. **`1_extract_climate_data_from_NetCDF.R`**  
Extracts relevant climate variables (e.g., wind speed, temperature, precipitation) from multiple NetCDF files.  
Aggregates these variables into a CSV or other tabular format to be used in subsequent analyses.

### 2. **`2_SMOTE_class_balancing.R`**  
Uses the Synthetic Minority Over-sampling Technique (SMOTE) to handle imbalanced classes in the dataset (e.g., contaminated vs. non-contaminated samples).  
Outputs a balanced dataset for model training.

### 3. **`3_Bayesian_hierarchical_model_JAGS.R`**  
Implements a Bayesian hierarchical model in JAGS (Just Another Gibbs Sampler).  
Defines priors, likelihood functions, and runs Markov Chain Monte Carlo (MCMC) to estimate posterior distributions of key parameters.  
Saves posterior samples for further analysis.

### 4. **`4_posterior_analysis_visualization.R`**  
Loads the posterior samples generated in the previous script.  
Produces diagnostic plots (trace plots, histograms, etc.), convergence metrics (Gelman-Rubin), and summary statistics.  
Can also generate publication-quality figures for parameter estimates.

### 5. **`5_posterior_prediction_yearwise.R`**  
Uses stored posterior samples to make year-by-year predictions of Aflatoxin B1 (AFB1) contamination.  
Incorporates projected or observed climate data for each year to estimate future contamination levels.  
Outputs results as tables or figures.

### 6. **`6_nonlinear_logistic_regression.R`**  
Fits a nonlinear logistic model to describe the relationship between average AFB1 concentration and exceedance rates.  
Outputs regression parameters, predicted values, and correlation coefficients.  
Generates a plot showing observed data points and the fitted logistic curve.

---

## Requirements

- **R version**: 4.2.3
- **Required R packages**:
  - `tidyverse` (data manipulation and plotting)
  - `ncdf4` or `terra` (for reading NetCDF files)
  - `openxlsx` (Excel file I/O)
  - `rjags` or `R2jags` (Bayesian modeling via JAGS)
  - `SMOTE` / `DMwR` (for synthetic minority over-sampling)
  - `ggplot2` / `bayesplot` (visualization)
  - `nlstools` (nonlinear regression utilities)

### JAGS Installation:
JAGS must be installed on your system so that `rjags` or `R2jags` can function properly.  
Download and install JAGS from:  
[http://mcmc-jags.sourceforge.net/](http://mcmc-jags.sourceforge.net/)

---

## Data Availability

For access to the dataset and full code, please visit the GitHub repository:  
[https://github.com/canguo92/Code-for-AFB1-Climate-Change](https://github.com/canguo92/Code-for-AFB1-Climate-Change)

---
