# Investigating the Synergistic Impact of Operational Factors and Equipment Attributes on Power Outage Duration within Australia's National Electricity Market (NEM) Grid 

This repository contains the R scripts and supporting files used to model outage durations in Queensland’s portion of Australia’s National Electricity Market (NEM). The analysis focuses on how operational reasons, equipment attributes, and their interactions influence outage length.

## Contents

* `MainScript.R` (Complete R script for data cleaning, EDA, SHAP analysis, linear and XGBoost modeling, and evaluation)


* `README.md` (This file)

## Project Overview

1. **Data Cleaning & Feature Engineering**

   * Parse timestamps, compute outage duration.
   * Standardize categorical fields (e.g., REASON, ISSECONDARY).
   * Derive numeric features: `VOLTAGE_kV`, `EQ_AGE_AT_OUTAGE`.

2. **Exploratory Data Analysis**

   * Histograms and boxplots of duration by reason, equipment type, voltage, and age.
   * Heatmaps of mean duration by reason–equipment combinations.
   * Correlation matrix among numeric predictors.

3. **Interaction Discovery (SHAP)**

   * Train an initial XGBoost model.
   * Compute SHAP values and rank top two‐way interactions.
   * Select highest‐scoring interactions for inclusion in a regression.

4. **Weighted Log‐Linear Regression**

   * Apply inverse‐frequency weights to correct class imbalance.
   * Fit regression on log(duration + 1) including main effects and selected interactions.
   * Extract coefficients, translate to percent‐change interpretations.

5. **Tree‐Based Benchmark (XGBoost)**

   * Tune hyperparameters via 5‐fold CV.
   * Compare RMSE and R² on train/test sets to the linear model.

6. **Evaluation & Recommendations**

   * Report adjusted R² and RMSE for both models.
   * Recommend an ensemble approach and additional feature integration (e.g., weather, geospatial data).

## Requirements

* R (≥ 4.0) with the following packages installed:

  * `tidyverse`
  * `lubridate`
  * `xgboost`
  * `shapviz`
  * `Metrics`
  * `broom`
  * `forcats`
  * `viridis`

You can install all dependencies at once by running:

```r
install.packages(c(
  "tidyverse", "lubridate", "xgboost", "shapviz", 
  "Metrics", "broom", "forcats", "viridis"
))
```

## How to Run

1. Clone this repository:

   ```bash
   git clone https://github.com/<your‐username>/NEM‐Outage‐Analysis.git
   cd NEM‐Outage‐Analysis
   ```

2. Open `MainScript.R` in RStudio (or any R IDE).

3. Update file paths if needed (by default, it expects `data/processed_df_final.csv`).

4. Run the script end‐to‐end. It will:

   * Load and clean the data.
   * Perform EDA and generate plots.
   * Compute SHAP interaction scores.
   * Fit the weighted linear model and produce coefficient tables.
   * Train and tune an XGBoost model.
   * Print evaluation metrics (R² and RMSE) for train/test splits.

## Output

* **Console**: Key model coefficients, percent‐change interpretations, and performance metrics.
* **Plots**: Generated EDA and diagnostic plots (histograms, boxplots, scatter plots, heatmaps, residual plots).
* **Tables**: Main‐effect and interaction coefficients with percent‐change calculated from log‐scale estimates.

## Notes & Next Steps

* The current analysis excludes weather or geospatial triggers. Adding those features is recommended for future work.
* Causal inferences are limited—models capture associations only.
* Ensure `REASON` coding is accurate; any mislabeling can bias results.

---

**Author:**  \[Wan Muhammad Syazwan Bin Ramli]
**Contact:**  \[[n11725575@qut.edu.au]
**Date:**  1 June 2025
