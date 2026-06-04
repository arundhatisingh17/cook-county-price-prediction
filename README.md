# Cook County Residential Sale Price Prediction

## Benchmarking GUIDE Trees Against CART and Ensemble Methods

### Overview
This project predicts residential property sale prices in Cook County using a 
dataset of 98,714 records from the Cook County Assessor's Computer Assisted Mass 
Appraisal system. The dataset contains every arm's-length residential sale and 
includes 83 columns of both qualitative and quantitative features. The richness 
of the dataset provides an excellent opportunity to benchmark regression tree 
algorithms by comparing not only their predictive performance but also the 
variable importance scores they produce.

Prior to modeling, a thorough exploratory data analysis was conducted to reduce 
redundancy and skewness. Missing values were examined and classified into MCAR, 
MAR, and MNAR categories. A large degree of structural missingness was revealed 
by grouping features according to missing percentage and the median sale price 
with versus without the feature — the large number of identical missing rates and 
notable sparsity of features within each modeling group further indicate 
structural missingness due to property-type-specific features.

Although standard CART algorithms frequently select continuous variables as the 
primary split points (because they offer far more candidate split values), this 
does not imply they are the most important predictors. This well-known selection 
bias is clearly visible in the variable importance scores produced by rpart, 
sklearn DecisionTreeRegressor, Random Forest, and LightGBM — all of which ranked 
latitude as the top predictor simply because it has a large number of distinct 
values. GUIDE directly addresses this through an unbiased variable selection 
procedure, providing importance scores that better reflect true predictive signal.

---

### Key Findings
- **LightGBM** achieved the lowest RMSE on the 2018–2019 temporal hold-out 
  (Log RMSE = 0.3963, dollar-scale RMSE = $124,703, MAE = $64,601)
- **GUIDE Random Forest** attained the highest Log R² (0.7938), demonstrating 
  strong variance explanation
- **GUIDE single trees** substantially outperformed classical CART 
  (Log R² = 0.6487 vs ~0.50–0.52), directly attributable to GUIDE's unbiased 
  variable selection procedure
- Classical CART implementations ranked latitude highest due to selection bias — 
  a flaw GUIDE directly addresses through chi-squared based variable selection
- Raw square footage features (v3) outperformed price-per-square-foot features 
  (v2) due to target leakage of training sale prices into the test set
- Stepwise-linear leaves narrowly outperformed multiple-linear leaves on the 
  temporal hold-out despite the latter producing more terminal nodes
- Linear and Ridge regression achieved identical performance (Log RMSE ≈ 0.595, 
  R² ≈ 0.463), confirming linear models cannot capture the non-linear 
  interactions present in the data

---

### Methodology

#### Data Cleaning & Feature Engineering
- Started with 98,714 rows and 83 columns; post cleaning: 80,743 rows, 61 columns
- Filtered to arm's-length transactions only using the Pure Market Filter, which 
  revealed dramatically different sale price distributions between market and 
  non-market transactions (median $272,778 vs $26,695)
- Removed mathematically derived columns: Age Squared, Age Decade, Lot Size 
  Squared, Improvement Size Squared, Sale Quarter, Garage Indicator, and others
- Dropped pure identifiers (PIN, Deed No., Property Address) and the Assessor's 
  own prior-year estimates to prevent circular prediction
- Log-transformed the top-3 skewed continuous features — Land Square Feet 
  (skew=7.56), Sale Price (skew=5.30), and Building Square Feet (skew=2.78)
- Classified missing data into MCAR, MAR, and MNAR — the majority fell into 
  MNAR due to structural, property-type-specific missingness confirmed by a 
  modeling group heatmap

#### Variable Selection & EDA
- Approximated GUIDE's variable selection using Chi-squared + Wilson-Hilferty 
  scoring under two target encodings: tertiles of log(Sale Price) and the sign 
  of residuals from the median log sale price
- Compared rankings against ANOVA F-statistic for categorical predictors
- Computed pairwise Pearson correlations and filtered pairs with |r| > 0.8, 
  cross-referencing with GUIDE importance scores to decide which variable to drop
- "Pure Market Filter" anomaly: ranked first under the tertile target but last 
  under the residual-based target — investigated and confirmed dramatically 
  different price distributions between subcategories

#### Train/Test Split
- Temporal hold-out: **2013–2017 training**, **2018–2019 test** (n = 23,810)
- Chosen over k-fold CV given the steady upward price trend across years, 
  which would allow look-ahead leakage under random splits
- MSE chosen as the error metric after confirming approximately normal residuals

#### Models Implemented
| Model | Log MSE | Log RMSE | Log R² |
|---|---|---|---|
| Linear Regression | 0.3537 | 0.5947 | 0.4633 |
| Ridge Regression (α=0.1) | 0.3534 | 0.5945 | 0.4637 |
| rpart (CART in R) | 0.3265 | 0.5714 | 0.5045 |
| sklearn DecisionTreeRegressor | 0.3167 | 0.5627 | 0.5194 |
| GUIDE, multiple-linear leaves | 0.2328 | 0.4825 | 0.6468 |
| GUIDE, stepwise-linear leaves (v3) | 0.2315 | 0.4811 | 0.6487 |
| sklearn Random Forest | 0.1933 | 0.4396 | 0.7067 |
| LightGBM (early stopping) | 0.1570 | 0.3963 | 0.7617 |
| GUIDE Random Forest (OOB) | 0.1970 | 0.4438 | 0.7682 |

---

### Report
The full 30-page report is available in `/report/Stat443_Report.pdf`, covering 
all modeling decisions, comparisons, and visual diagnostics in detail.

---

### Future Work
Adding binary indicator columns for features with high missingness percentages 
to create stronger signals — particularly useful given the degree of structural 
missingness in this dataset.

---

*STAT 443 — Spring 2026 | University of Wisconsin-Madison*
