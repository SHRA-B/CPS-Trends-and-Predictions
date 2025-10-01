# CPS-Trends-and-Predictions
Data-driven analysis of Crown Prosecution Service (CPS) case outcomes using R for trend exploration, predictive modeling, and strategic insights.
## Overview
This project presents a comprehensive analysis of conviction outcomes across Crown Prosecution Service (CPS) regions. The goal is to uncover patterns, evaluate regional performance, and understand the relationship between workload and conviction success rate using advanced data processing, statistical modeling, and machine learning techniques.

## Key Objectives
- Quantify the impact of case volume on conviction success rates.
- Predict CPS regions’ success rate categories (High/Low or Low/Medium/High workload).
- Identify hidden regional groupings and anomalies.
- Provide actionable insights for strategic evaluation and decision-making.

## Methodology

### 1. Descriptive Analysis
- Summary statistics for numerical and categorical variables.
- Visualization of distributions, trends, and correlations using `ggplot2` and `corrplot`.
- Outlier detection and temporal trend analysis.

### 2. Hypothesis Testing
- **ANOVA:** Tested differences in conviction success rates across CPS regions.
- **Chi-Square Test:** Examined relationships between categorical variables and regional outcomes.

### 3. Predictive Modeling
- **Regression:** Linear, Ridge, and Lasso regression to study the effect of case volume on success rates.
- **Binary Classification:** Logistic Regression and Random Forest to classify regions into 'High' or 'Low' success categories.
- **Multi-class Classification:** Multinomial Logistic Regression and Random Forest to categorize regions into Low, Medium, or High workload tiers.

### 4. Clustering Analysis
- **K-Means:** Centroid-based regional segmentation.
- **DBSCAN:** Density-based clustering and outlier detection.
- **Gaussian Mixture Model (GMM):** Probabilistic clustering capturing overlapping regional profiles.

## Key Findings
- High-case-volume regions tend to have lower conviction success rates.
- Random Forest outperformed Logistic Regression in classification tasks.
- Distinct clusters of CPS regions reveal operational similarities, disparities, and anomalies.
- Temporal trends show fluctuations in monthly conviction success and failure rates.
- Correlation and descriptive analysis highlighted key offense-specific and overall performance metrics.

## Tools & Libraries
- **Data Cleaning & Processing:** `dplyr`, `tidyr`, `naniar`
- **Visualization:** `ggplot2`, `corrplot`, `geom_tile`
- **Statistical Analysis:** `stats`, `car`
- **Machine Learning & Modeling:** `glmnet`, `randomForest`, `caret`, `mclust`, `dbscan`

## Recommendations
- Target high-workload regions to improve conviction success rates.
- Leverage clustering insights for better resource allocation and policy planning.
- Expand the dataset with additional temporal or socio-demographic features for improved modeling.
- Use predictive models for proactive monitoring and intervention.

## Project Structure
├── data/ # Raw and cleaned datasets
├── scripts/ # R scripts for cleaning, analysis, and modeling
├── figures/ # Visualizations and plots
├── README.md # Project overview and summary
└── report/ # Full analysis report with detailed findings

## License
This project is licensed under the MIT License.
