# Credit Risk Scorecard Modeling

## Overview

This project develops a transparent and robust credit scoring model for home equity loans. The dataset includes 5,960 observations with borrower characteristics, loan attributes, credit history, and delinquency indicators. Loan default is the target variable.

After univariate analysis using default rates, Weight of Evidence (WoE), and Information Value (IV), a subset of predictive variables was selected. Continuous variables were discretized into equal‑frequency bins and categorical variables used natural groupings. Variables with low IV or high correlation were excluded to avoid instability and multicollinearity.

Using WoE‑transformed variables, a logistic regression model was trained and validated using repeated random splits of the dataset to evaluate discriminative power. Gini coefficients indicated strong stability (~76–77%) and no signs of overfitting. The final model was translated into a scorecard using standard banking conventions to deliver intuitive credit scores. Decile analysis showed a monotonic increase in default rates across score deciles, confirming the strong ranking ability of the scorecard.

## Methodology

* **Data exploration and cleaning** – handle missing values and assess data quality.
* **Univariate analysis** – assess explanatory power of individual variables using default rates, WoE, and IV to select meaningful predictors.
* **Variable binning** – discretize continuous variables into approximately equal‑frequency bins and group categorical variables where appropriate.
* **WoE transformation and variable selection** – transform predictors using WoE and exclude variables with low information value or high correlation.
* **Logistic regression modeling** – train a logistic regression model using the selected WoE‑transformed variables.
* **Model evaluation** – assess performance using Gini coefficient/AUC on repeated random train/test splits to check stability.
* **Scorecard creation** – convert logistic regression coefficients into a scorecard scale, yielding an intuitive measure of credit risk.
* **Decile analysis** – evaluate monotonicity by comparing default rates across scorecard deciles.

## Repository Contents

* `credit_risk_report.pdf` – full credit risk modeling report detailing data, methodology, results and conclusions.
* `README.md` – overview of the project and methodology (this document).
* `packages.R` – list of R packages used in the analysis.

## Technologies

The analysis was performed in **R** using packages such as `data.table`, `dplyr`, `ggplot2`, `pROC`, and other credit scoring utilities. You can install the required packages using the script in `packages.R`.