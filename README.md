# Credit Risk Scorecard

This repository contains a case study in developing a credit risk scorecard using logistic regression and Weight of Evidence (WoE) transformation.

The objective is to estimate the probability of default (PD) for home equity loan applicants and convert the resulting logistic regression model into an interpretable scorecard.

## Repository Structure

```text
.
├── data/
│   └── Home_Eq_Dataset.xlsx
├── report/
│   ├── credit_risk_report.doc
│   └── credit_risk_report.pdf
├── src/
│   ├── 01_univariate_analysis.R
│   └── 02_scorecard_model.R
├── packages.R
└── README.md
```

## Dataset

The project uses the Home Equity dataset containing 5,960 observations.

The target variable is:

- `BAD = 1` → default
- `BAD = 0` → non-default

The remaining variables describe borrower characteristics, loan information and credit history.

## Methodology

### 1. Univariate Analysis

The script `src/01_univariate_analysis.R` performs exploratory analysis of individual variables.

Main objectives:

- analyse default rates across categories and intervals,
- create initial binning schemes,
- inspect missing values,
- calculate grouped default-rate summaries,
- identify potentially useful predictors.

Continuous variables are sorted and divided into approximately equal-sized bins. Categorical variables are analysed by category, with selected grouped summaries prepared where relevant.

### 2. Scorecard Development

The script `src/02_scorecard_model.R` uses the selected binning logic to:

- apply WoE transformation,
- calculate Information Value (IV),
- fit a logistic regression model,
- estimate probabilities of default,
- evaluate model performance,
- generate scorecard points.

Model quality is assessed using:

- AUC,
- Gini coefficient,
- decile analysis,
- score distribution across validation deciles.

### 3. Score Scaling

The final logistic regression model is transformed into a scorecard using standard score-scaling conventions:

- reference score: 600,
- reference odds: 50:1,
- PDO (Points to Double the Odds): 20.

Higher scores correspond to lower estimated credit risk.

## Main Results

The final model achieved approximately:

| Metric | Value |
|---|---:|
| Training Gini | 77% |
| Validation Gini | 76% |

The validation decile analysis shows an overall increasing trend in observed default rates across risk groups, indicating good discriminatory power.

## Required Packages

Required R packages can be installed and loaded by running:

```r
source("packages.R")
```

## Running the Project

Run the scripts from the repository root in the following order:

```r
source("packages.R")
source("src/01_univariate_analysis.R")
source("src/02_scorecard_model.R")
```

The univariate analysis is intentionally separated from the final modelling script because exploratory binning review and final model estimation are distinct steps in a scorecard workflow.

## Report

A detailed description of the methodology, model development process and results is available in:

```text
report/credit_risk_report.pdf
```

The Word version of the report is also included in:

```text
report/credit_risk_report.doc
```

## Disclaimer

This project is provided for educational and portfolio purposes. It is not intended for production credit decisions.
