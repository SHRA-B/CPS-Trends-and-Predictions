# Load Required Libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(naniar)
library(stringr)
#Task-1 Cleaning and Intregation 
# set Dataset and csv fils Paths
CPS_dataset <- "C:/Users/Shree/OneDrive/Desktop/Data visualization/Dataset - Assignment"
selected_year_folders <- c("2014", "2015", "2016")
# Load & Merge All CSV Files
CPS_all_csv_files <- map(selected_year_folders, ~ list.files(
  path = file.path(CPS_dataset, .x),
  pattern = "\\.csv$",
  full.names = TRUE
)) %>% unlist()

read_metadata <- function(file_path) {
  file_name <- basename(file_path)
  year <- as.numeric(str_extract(file_name, "\\d{4}"))
  month_raw <- str_extract(file_name, "(?<=category_)[a-zA-Z]+")
  
  read_csv(file_path, show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(
      year = year,
      month = str_to_title(month_raw)
    )
}
safe_reader <- possibly(read_metadata, otherwise = tibble())
CPS_combined_data <- map_dfr(CPS_all_csv_files, safe_reader)
# Object Size
object.size(CPS_combined_data)
# shape of dataset
dim(CPS_combined_data)
#overview of dataset
glimpse(CPS_combined_data)
#Print Original Column Names BEFORE Renaming
cat("\n Display original Column Names:\n")
print(names(CPS_combined_data))
#first column contain 
CPS_combined_data <- CPS_combined_data %>%
  rename(
    CPS_region = x1,
    homicide_conviction_count = number_of_homicide_convictions,
    homicide_conviction_percent = percentage_of_homicide_convictions,
    homicide_failed_count = number_of_homicide_unsuccessful,
    homicide_failed_percent = percentage_of_homicide_unsuccessful,
    person_offence_conviction_count = number_of_offences_against_the_person_convictions,
    person_offence_conviction_percent = percentage_of_offences_against_the_person_convictions,
    person_offence_failed_count = number_of_offences_against_the_person_unsuccessful,
    person_offence_failed_percent = percentage_of_offences_against_the_person_unsuccessful,
    sexual_conviction_count = number_of_sexual_offences_convictions,
    sexual_conviction_percent = percentage_of_sexual_offences_convictions,
    sexual_failed_count = number_of_sexual_offences_unsuccessful,
    sexual_failed_percent = percentage_of_sexual_offences_unsuccessful,
    burglary_conviction_count = number_of_burglary_convictions,
    burglary_conviction_percent = percentage_of_burglary_convictions,
    burglary_failed_count = number_of_burglary_unsuccessful,
    burglary_failed_percent = percentage_of_burglary_unsuccessful,
    robbery_conviction_count = number_of_robbery_convictions,
    robbery_conviction_percent = percentage_of_robbery_convictions,
    robbery_failed_count = number_of_robbery_unsuccessful,
    robbery_failed_percent = percentage_of_robbery_unsuccessful,
    theft_conviction_count = number_of_theft_and_handling_convictions,
    theft_conviction_percent = percentage_of_theft_and_handling_convictions,
    theft_failed_count = number_of_theft_and_handling_unsuccessful,
    theft_failed_percent = percentage_of_theft_and_handling_unsuccessful,
    fraud_conviction_count = number_of_fraud_and_forgery_convictions,
    fraud_conviction_percent = percentage_of_fraud_and_forgery_convictions,
    fraud_failed_count = number_of_fraud_and_forgery_unsuccessful,
    fraud_failed_percent = percentage_of_fraud_and_forgery_unsuccessful,
    damage_conviction_count = number_of_criminal_damage_convictions,
    damage_conviction_percent = percentage_of_criminal_damage_convictions,
    damage_failed_count = number_of_criminal_damage_unsuccessful,
    damage_failed_percent = percentage_of_criminal_damage_unsuccessful,
    drugs_conviction_count = number_of_drugs_offences_convictions,
    drugs_conviction_percent = percentage_of_drugs_offences_convictions,
    drugs_failed_count = number_of_drugs_offences_unsuccessful,
    drugs_failed_percent = percentage_of_drugs_offences_unsuccessful,
    public_order_conviction_count = number_of_public_order_offences_convictions,
    public_order_conviction_percent = percentage_of_public_order_offences_convictions,
    public_order_failed_count = number_of_public_order_offences_unsuccessful,
    public_order_failed_percent = percentage_of_public_order_offences_unsuccessful,
    other_offence_conviction_count = number_of_all_other_offences_excluding_motoring_convictions,
    other_offence_conviction_percent = percentage_of_all_other_offences_excluding_motoring_convictions,
    other_offence_failed_count = number_of_all_other_offences_excluding_motoring_unsuccessful,
    other_offence_failed_percent = percentage_of_all_other_offences_excluding_motoring_unsuccessful,
    motoring_conviction_count = number_of_motoring_offences_convictions,
    motoring_conviction_percent = percentage_of_motoring_offences_convictions,
    motoring_failed_count = number_of_motoring_offences_unsuccessful,
    motoring_failed_percent = percentage_of_motoring_offences_unsuccessful,
    admin_failed_count = number_of_admin_finalised_unsuccessful,
    l_motoring_failed_percent = percentage_of_l_motoring_offences_unsuccessful,
    
  )

#Column Names AFTER Renaming
cat("Renamed Column Names:")
print(names(CPS_combined_data))
#Handle Placeholder Values and Convert Percentages
CPS_combined_data <- CPS_combined_data %>%
  mutate(across(where(is.character), ~ na_if(.x, "-"))) %>%
  mutate(across(where(is.character), ~ na_if(.x, "n/a"))) %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  mutate(across(where(is.character), ~ ifelse(str_detect(.x, "%"),
                                              str_remove(.x, "%"), .x))) %>%
  mutate(across(contains("percentage"), as.numeric))
#check which month data missing 
expected_months <- factor(month.name, levels = month.name)
missing_months_df <- CPS_combined_data %>%
  group_by(year) %>%
  summarise(actual_months = list(unique(month))) %>%
  rowwise() %>%
  mutate(missing_months = list(setdiff(expected_months, actual_months))) %>%
  select(year, missing_months) %>%
  unnest(missing_months)
print(missing_months_df)
#Check for duplicate rows
duplicate_count <- sum(duplicated(CPS_combined_data))
print(paste("Duplicate rows:", duplicate_count))
#Check Missing value count and percentage
missing_summary <- data.frame(
  column_name = names(CPS_combined_data),
  missing_count = colSums(is.na(CPS_combined_data)),
  missing_percent = round(colMeans(is.na(CPS_combined_data)) * 100, 2)
)
print(missing_summary %>% filter(missing_count > 0))
# Drop Percentage Columns (Redundant)
# Preview the columns being dropped
cat("\n Columns being dropped (ending with '_percent'):\n")
print(names(CPS_combined_data)[endsWith(names(CPS_combined_data), "_percent")])

# Drop all percentage columns (conviction_percent, failed_percent, etc.)
CPS_combined_data <- CPS_combined_data %>%
  select(-ends_with("_percent"))

# Confirm the dataset structure after dropping
cat("\n columns after dropping _percent variables:\n")
print(names(CPS_combined_data))
#Feature Engineering
CPS_combined_data <- CPS_combined_data %>%
  # Total convictions across all offence types
  mutate(
    total_convictions = rowSums(select(., ends_with("conviction_count")), na.rm = TRUE),
    total_unsuccessful = rowSums(select(., ends_with("failed_count")), na.rm = TRUE),
    overall_total_cases = total_convictions + total_unsuccessful,
    
    # Success rate as a percentage
    overall_success_rate = round((total_convictions / overall_total_cases) * 100, 2),
    
    # Combine year + month for time-series analysis
    year_month = paste0(year, "-", str_pad(match(month, month.name), 2, pad = "0")),
    
    # Categorize regions by volume of cases
    case_volume_tier = cut(
      overall_total_cases,
      breaks = quantile(overall_total_cases, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  )
# Display categorical (character or factor) columns
cat("Categorical Columns in crime dataset:\n")
categorical_columns_CPS <- names(select_if(CPS_combined_data, ~ is.character(.x) || is.factor(.x)))
print(categorical_columns_CPS)

#Display numerical columns
cat("\n Numerical Columns in crime dataset:\n")
numerical_columns_CPS <- names(select_if(CPS_combined_data, is.numeric))
print(numerical_columns_CPS)

# Convert categorical columns
CPS_combined_data <- CPS_combined_data %>%
  mutate(
    # Converted CPS_region and case_volume_tier to factor
    CPS_region = as.factor(CPS_region),
    case_volume_tier = as.factor(case_volume_tier),
    
    # converted 'month' to factor
    month = month %>%
      as.character() %>%        # for checking it's a character
      str_trim() %>%            # remove leading/trailing whitespace
      str_to_title() %>%        # ensure correct capitalization (e.g., "May", "June")
      factor(levels = month.name, labels = month.abb) # convert to abbreviated form
  )
cat("Column types after conversion:\n")
sapply(CPS_combined_data[c("CPS_region", "month", "case_volume_tier")], class)
#Final Dataset Summmary
#View structure (column types & sample values)
cat("\n Structure of Final Dataset:\n")
glimpse(CPS_combined_data)

#Shape number of rows and columns
cat("\n Dataset Dimensions:\n")
cat("Rows:", nrow(CPS_combined_data), "\n")
cat("Columns:", ncol(CPS_combined_data), "\n")

#Data types of each column
cat("\n Data Types of Columns:\n")
print(sapply(CPS_combined_data, class))

#Missing values summary
# Count missing values per column
missing_values <- colSums(is.na(CPS_combined_data))
cat(missing_values)

# Task 2-Descriptive Analysis
library(ggplot2)
library(dplyr)
library(naniar)

# Created summary with percentage
column_type_df <- data.frame(
  Type = c("Categorical", "Numerical"),
  Count = c(length(categorical_columns_CPS), length(numerical_columns_CPS))
) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1),
         Label = paste0(Percentage, "%"))

# Plot pie chart with percentage labels
ggplot(column_type_df, aes(x = "", y = Count, fill = Type)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5) +
  labs(title = "Column Type Distribution in CPS Dataset", fill = "Column Type") +
  theme_void()

#Numerical Feature Distribution and Central Tendencies.
# List of all your numerical column names
numerical_cols <- c(
  "homicide_conviction_count", "homicide_failed_count",
  "person_offence_conviction_count", "person_offence_failed_count",
  "sexual_conviction_count", "sexual_failed_count",
  "burglary_conviction_count", "burglary_failed_count",
  "robbery_conviction_count", "robbery_failed_count",
  "theft_conviction_count", "theft_failed_count",
  "fraud_conviction_count", "fraud_failed_count",
  "damage_conviction_count", "damage_failed_count",
  "drugs_conviction_count", "drugs_failed_count",
  "public_order_conviction_count", "public_order_failed_count",
  "other_offence_conviction_count", "other_offence_failed_count",
  "motoring_conviction_count", "motoring_failed_count",
  "admin_failed_count", "year",
  "total_convictions", "total_unsuccessful",
  "overall_total_cases", "overall_success_rate"
)

# Loop through and print summary for each
for (col in numerical_cols) {
  cat("\n\n Summary for:", col, "\n")
  print(summary(CPS_combined_data[[col]]))
}
# Define categorical columns
categorical_columns <- c("CPS_region", "month", "year_month", "case_volume_tier")

# Print unique value count for each
sapply(CPS_combined_data[categorical_columns], n_distinct)

#missing value Analysis
library(naniar)

gg_miss_var(CPS_combined_data) +
  labs(title = "Missing Values by Variable",
       x = "Variables",
       y = "Number of Missing Values") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Correlation Analysis
library(corrplot)

full_corr <- cor(select(CPS_combined_data, where(is.numeric)), use = "complete.obs")

corrplot(
  full_corr,
  method = "color",
  type = "lower",
  order = "hclust",        # cluster similar variables together
  tl.cex = 0.4,            # tiny text
  col = colorRampPalette(c("#B2182B","white","#2166AC"))(200),
  tl.col = "black",
  addgrid.col = "gray90",
  title = "CPS Correlation Matrix",
  mar = c(0,0,1,0)
)

#Distributaion of Numerical Columns
library(tidyr)

# Select all numeric columns
numeric_data <- select(CPS_combined_data, where(is.numeric))

# Convert to long format for faceted plotting
numeric_long <- numeric_data %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

# Plot histograms for each numeric feature
ggplot(numeric_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ feature, scales = "free", ncol = 3) +
  labs(
    title = "Distribution of Numerical Features",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  )

# Average total convictions by region
CPS_combined_data %>%
  group_by(CPS_region) %>%
  summarise(avg_convictions = mean(total_convictions, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(CPS_region, avg_convictions), y = avg_convictions)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Total Convictions by CPS Region",
    x = "CPS Region",
    y = "Average Convictions"
  ) +
  theme_minimal()

# Histogram of total unsuccessful outcomes
ggplot(CPS_combined_data, aes(x = total_unsuccessful)) +
  geom_histogram(bins = 30, fill = "firebrick", color = "black") +
  labs(
    title = "Distribution of Total Unsuccessful Cases",
    x = "Total Unsuccessful Outcomes",
    y = "Frequency"
  ) +
  theme_minimal()

# Convictions vs Failures by Region
CPS_combined_data %>%
  group_by(CPS_region) %>%
  summarise(
    avg_convictions = mean(total_convictions, na.rm = TRUE),
    avg_failures = mean(total_unsuccessful, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(avg_convictions, avg_failures), names_to = "Outcome", values_to = "Average") %>%
  ggplot(aes(x = reorder(CPS_region, Average), y = Average, fill = Outcome)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparison of Convictions vs Failures by CPS Region",
    x = "CPS Region",
    y = "Average Number of Cases"
  ) +
  theme_minimal()

# Monthly conviction success rate trend
CPS_combined_data %>%
  group_by(year_month) %>%
  summarise(avg_success_rate = mean(overall_success_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = year_month, y = avg_success_rate, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly Conviction Rate Trend",
    x = "Year-Month",
    y = "Average Success Rate (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly failure rate trend
CPS_combined_data %>%
  group_by(year_month) %>%
  summarise(avg_failure_rate = mean(100 - overall_success_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = year_month, y = avg_failure_rate, group = 1)) +
  geom_line(color = "green", size = 1.2) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Monthly Failure Rate Trend",
    x = "Year-Month",
    y = "Average Failure Rate (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly trend: convictions and failures
CPS_combined_data %>%
  group_by(year_month) %>%
  summarise(
    avg_convictions = mean(total_convictions, na.rm = TRUE),
    avg_failures = mean(total_unsuccessful, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(avg_convictions, avg_failures), names_to = "Outcome", values_to = "Count") %>%
  ggplot(aes(x = year_month, y = Count, color = Outcome, group = Outcome)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Trend of Convictions and Failures (2014–2016)",
    x = "Month-Year",
    y = "Average Number of Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Temporal Heatmap (Month × Year)
CPS_combined_data %>%
  group_by(year, month) %>%
  summarise(avg_success_rate = mean(overall_success_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = month, fill = avg_success_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "orange") +
  labs(
    title = "Temporal Heatmap of Conviction Success Rate",
    x = "Year",
    y = "Month",
    fill = "Success Rate (%)"
  ) +
  theme_minimal()

#outlier 
ggplot(CPS_combined_data, aes(x = "", y = overall_success_rate)) +
  geom_violin(fill = "pink") +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Distribution & Outliers in Success Rate") +
  theme_minimal()

# Convictions by offence Type
offence_success <- CPS_combined_data %>%
  select(ends_with("conviction_count")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "offence", values_to = "total")

ggplot(offence_success, aes(x = reorder(offence, -total), y = total)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Convictions by Offence Type", x = "Offence", y = "Total Cases") +
  theme_minimal()

# failed cases by offence type
offence_failed <- CPS_combined_data %>%
  select(ends_with("failed_count")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "offence", values_to = "total")

ggplot(offence_failed, aes(x = reorder(offence, -total), y = total)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Failed Cases by Offence Type",
    x = "Offence",
    y = "Total Failed Cases"
  ) +
  theme_minimal()

# Hypothesis Testing
# 1.ANOVA
anova_result <- aov(overall_success_rate ~ CPS_region, data = CPS_combined_data)
summary(anova_result)
# Identify which region pairs differ
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
#plot
ggplot(CPS_combined_data, aes(x = reorder(CPS_region, overall_success_rate, FUN = median), 
                              y = overall_success_rate, fill = CPS_region)) +
  geom_boxplot() +
  coord_flip() +  
  labs(
    title = "Anova test Conviction Success Rate by CPS Region",
    x = "CPS Region",
    y = "Success Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 2.chi-square test 
# Create binary outcome column
chisq_data <- CPS_combined_data %>%
  mutate(conviction_binary = ifelse(overall_success_rate >= median(overall_success_rate, na.rm = TRUE), "High", "Low"))

chisq_test_result <- chisq.test(table(chisq_data$CPS_region, chisq_data$conviction_binary))
print(chisq_test_result)

ggplot(chisq_data, aes(x = CPS_region, fill = conviction_binary)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "Chi-Square Test Conviction Outcome by Region",
    x = "CPS Region",
    y = "Proportion",
    fill = "Conviction Outcome"
  ) +
  theme_minimal()

# Task 3-Predictive Analysis
library(tidyverse)
library(caret)
library(glmnet)
library(Metrics)
library(broom)

#H1- An increase in the overall number of cases handled by CPS regions is significantly associated with a decrease in conviction success rate.
# Regression Analysis 
# Selected features according to H1
regression_data <- CPS_combined_data %>%
  filter(!is.na(overall_success_rate)) %>%
  select(
    overall_success_rate,       
    overall_total_cases,         
    total_convictions,          
    total_unsuccessful,         
    CPS_region, year, month     
  )
# 1.linear regression
# Fit linear regression model
success_rate_lm <- lm(overall_success_rate ~ ., data = regression_data)
# View model summary
summary(success_rate_lm)
# Predict and store results
regression_data$predicted_linear <- predict(success_rate_lm)
#plot
ggplot(regression_data, aes(x = predicted_linear, y = overall_success_rate)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Linear Regression: Actual vs Predicted",
    x = "Predicted Success Rate",
    y = "Actual Success Rate"
  ) +
  theme_minimal()

# 2.ridge model
# Prepare matrix input
x_matrix <- model.matrix(overall_success_rate ~ ., regression_data)[, -1]
y_vector <- regression_data$overall_success_rate

# Fit Ridge model (alpha = 0)
ridge_success_model <- cv.glmnet(x_matrix, y_vector, alpha = 0)
plot(ridge_success_model)

# Best lambda
best_lambda_ridge <- ridge_success_model$lambda.min

# Predict
regression_data$predicted_ridge <- predict(ridge_success_model, s = best_lambda_ridge, newx = x_matrix)
#plot
ggplot(regression_data, aes(x = predicted_ridge, y = overall_success_rate)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Ridge Regression: Actual vs Predicted",
    x = "Predicted Success Rate",
    y = "Actual Success Rate"
  ) +
  theme_minimal()
#3.Lasso Regression
# Fit Lasso Regression
lasso_success_model <- cv.glmnet(x_matrix, y_vector, alpha = 1)
plot(lasso_success_model)

# Best lambda
best_lambda_lasso <- lasso_success_model$lambda.min

# Predict outcome
regression_data$predicted_lasso <- predict(lasso_success_model, s = best_lambda_lasso, newx = x_matrix)
#plot
ggplot(regression_data, aes(x = predicted_lasso, y = overall_success_rate)) +
  geom_point(alpha = 0.6, color = "darkred") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Lasso Regression: Actual vs Predicted",
    x = "Predicted Success Rate",
    y = "Actual Success Rate"
  ) +
  theme_minimal()

#compare model
# Actual values
actual <- regression_data$overall_success_rate

# Performance metrics
cat("MODEL PERFORMANCE:\n")

cat("Linear Regression:\n")
cat("  RMSE:", round(rmse(actual, regression_data$predicted_linear), 2), "\n")
cat("  MAE :", round(mae(actual, regression_data$predicted_linear), 2), "\n")
cat("  R²  :", round(summary(success_rate_lm)$r.squared, 4), "\n\n")

cat("Ridge Regression:\n")
cat("  RMSE:", round(rmse(actual, regression_data$predicted_ridge), 2), "\n")
cat("  MAE :", round(mae(actual, regression_data$predicted_ridge), 2), "\n\n")

cat("Lasso Regression:\n")
cat("  RMSE:", round(rmse(actual, regression_data$predicted_lasso), 2), "\n")
cat("  MAE :", round(mae(actual, regression_data$predicted_lasso), 2), "\n")


# comaparison plot of all model
compare_models_df <- regression_data %>%
  select(actual = overall_success_rate,
         Linear = predicted_linear,
         Ridge = predicted_ridge,
         Lasso = predicted_lasso) %>%
  pivot_longer(cols = -actual, names_to = "Model", values_to = "Predicted")

# Plot Actual vs Predicted for each model
ggplot(compare_models_df, aes(x = Predicted, y = actual, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ Model) +
  labs(
    title = "Actual vs Predicted Success Rate: Regression Models",
    x = "Predicted Success Rate",
    y = "Actual Success Rate"
  ) +
  theme_minimal()

#H2- CPS regions can be grouped into distinct clusters based on patterns in their conviction counts, failure counts, and overall conviction success rate
# Clustering Analysis
install.packages(c("factoextra", "cluster", "dendextend", "dbscan"))

# Load libraries
library(factoextra)     # For clustering visualizations
library(cluster)        # Clustering methods and distance matrices
library(dendextend)     # For plotting dendrograms
library(dbscan)         # For DBSCAN clustering
library(dplyr)          # Data manipulation
#install.packages("mclust")
library(mclust)


# Selected features for clustering 
cluster_data <- CPS_combined_data %>%
  select(total_convictions, total_unsuccessful, overall_success_rate) %>%
  scale()

#Elbow Mmethod
fviz_nbclust(cluster_data, kmeans, method = "wss") +
  labs(
    title = "Number of Clusters",
    x = "Number of Clusters (k)",
    y = "Cluster Sum of Squares"
  ) +
  theme_minimal()
#1.k-means
set.seed(123) 
kmeans_model <- kmeans(cluster_data, centers = 4, nstart = 25)
kmeans_labels <- kmeans_model$cluster

fviz_cluster(kmeans_model, data = cluster_data, geom = "point") +
  labs(title = "K-Means Clustering (k = 4)") +
  theme_minimal()

#2.DBSCAN 

#plot
kNNdistplot(cluster_data, k = 3)
abline(h = 0.5, col = "red", lty = 2) 
#model
db_model <- dbscan(cluster_data, eps = 0.5, minPts = 5)
#Visualize clusters
fviz_cluster(list(data = cluster_data, cluster = db_model$cluster),
             stand = FALSE,
             geom = "point",
             ellipse = FALSE) +
  labs(title = "DBSCAN Clustering") +
  theme_minimal()

#3.GMM
gmm_model <- Mclust(cluster_data)

# Summary to view number of clusters and model type
summary(gmm_model)

fviz_cluster(list(data = cluster_data, cluster = gmm_model$classification),
             geom = "point", ellipse.type = "norm") +
  labs(title = "Gaussian Mixture Model Clustering") +
  theme_minimal()


fviz_cluster(list(data = cluster_data, cluster = gmm_model$classification),
             geom = "point", ellipse.type = "norm") +
  labs(title = "Gaussian Mixture Model Clustering") +
  theme_minimal()

# Classification 
#H3 - CPS regions with higher levels of unsuccessful prosecutions are more likely to fall into the ‘Low’ conviction success rate category
# 1.Binary Classification
# Load libraries
library(tidyverse)
library(caret)
library(pROC)
library(randomForest)
library(MLmetrics)
#install.packages("MLmetrics")

# Create binary target
CPS_combined_data <- CPS_combined_data %>%
  mutate(success_rate_binary = ifelse(overall_success_rate >= median(overall_success_rate, na.rm = TRUE), 
                                      "High", "Low")) %>%
  mutate(success_rate_binary = factor(success_rate_binary))

# Feature selection
selected_features_data <- CPS_combined_data %>%
  select(success_rate_binary,
         person_offence_conviction_count,
         theft_conviction_count,
         fraud_failed_count,
         motoring_failed_count,
         admin_failed_count,
         CPS_region,
         year,
         month)

#train test split
set.seed(123)
split <- createDataPartition(selected_features_data$success_rate_binary, p = 0.7, list = FALSE)
train_data <- selected_features_data[split, ]
test_data <- selected_features_data[-split, ]

#Logistic Regression
log_model <- glm(success_rate_binary ~ ., data = train_data, family = "binomial")

# Predict probabilities and classes
log_probs <- predict(log_model, newdata = test_data, type = "response")
log_pred <- ifelse(log_probs > 0.5, "High", "Low") %>% factor(levels = c("Low", "High"))

# Confusion matrix and metrics
log_cm <- confusionMatrix(log_pred, test_data$success_rate_binary)
log_acc <- Accuracy(log_pred, test_data$success_rate_binary)
log_prec <- Precision(log_pred, test_data$success_rate_binary, positive = "High")
log_f1 <- F1_Score(log_pred, test_data$success_rate_binary, positive = "High")
log_auc <- auc(roc(test_data$success_rate_binary, as.numeric(log_probs)))

# ROC Curve
log_roc <- roc(test_data$success_rate_binary, as.numeric(log_probs))
plot(log_roc, col = "darkred", lwd = 2, main = "ROC Curve - Logistic Regression")
abline(a = 0, b = 1, lty = 2)
legend("bottomright", legend = paste("AUC =", round(log_auc, 3)), col = "darkred", lwd = 2)

# Visual Confusion Matrix (Logistic)
library(ggplot2)
library(reshape2)
log_table <- table(Predicted = log_pred, Actual = test_data$success_rate_binary)
log_df <- as.data.frame(log_table)
ggplot(log_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6, color = "white") +
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  labs(title = "Confusion Matrix - Logistic Regression", x = "Actual", y = "Predicted") +
  theme_minimal()

# Random Forest
rf_model <- randomForest(success_rate_binary ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, "High"]

# Confusion matrix and metrics
rf_cm <- confusionMatrix(rf_pred, test_data$success_rate_binary)
rf_acc <- Accuracy(rf_pred, test_data$success_rate_binary)
rf_prec <- Precision(rf_pred, test_data$success_rate_binary, positive = "High")
rf_f1 <- F1_Score(rf_pred, test_data$success_rate_binary, positive = "High")
rf_auc <- auc(roc(test_data$success_rate_binary, rf_probs))

# ROC Curve
rf_roc <- roc(test_data$success_rate_binary, rf_probs)
plot(rf_roc, col = "blue", lwd = 2, main = "ROC Curve - Random Forest")
abline(a = 0, b = 1, lty = 2)
legend("bottomright", legend = paste("AUC =", round(rf_auc, 3)), col = "blue", lwd = 2)

# Visual Confusion Matrix (Random Forest)
rf_table <- table(Predicted = rf_pred, Actual = test_data$success_rate_binary)
rf_df <- as.data.frame(rf_table)
ggplot(rf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix - Random Forest", x = "Actual", y = "Predicted") +
  theme_minimal()

# Comparison
model_metrics <- tibble(
  Model = rep(c("Logistic", "Random Forest"), each = 3),
  Metric = rep(c("Accuracy", "Precision", "F1 Score"), times = 2),
  Value = c(log_acc, log_prec, log_f1, rf_acc, rf_prec, rf_f1)
)

# Summary Table for Binary Classification
binary_classification_summary <- tibble(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(log_acc, rf_acc),
  Precision = c(log_prec, rf_prec),
  F1_Score = c(log_f1, rf_f1),
  AUC = c(log_auc, rf_auc)
)

print(binary_classification_summary)

# Plot for Binary Models
binary_classification_summary %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score") %>%
  ggplot(aes(x = Metric, y = Score, fill = Model)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Score, 3)), position = position_dodge(0.9), vjust = -0.5) +
  labs(
    title = "Binary Classification: Model Performance",
    y = "Score", x = "Metric"
  ) +
  theme_minimal() +
  ylim(0, 1)

# Multi-Classification
library(nnet)

#H4-The volume of cases handled by CPS regions can be effectively classified into distinct workload tiers (Low, Medium, High) using conviction-related features such as offence counts, conviction outcomes, and regional attributes

# 1.model - Multinomial Logistic Regression
multi_data <- CPS_combined_data %>%
  filter(!is.na(case_volume_tier)) %>%
  select(case_volume_tier,
         person_offence_conviction_count,
         theft_conviction_count,
         fraud_failed_count,
         motoring_failed_count,
         admin_failed_count,
         CPS_region, year, month) %>%
  mutate(case_volume_tier = factor(case_volume_tier))

# Train-Test Split
set.seed(123)
split <- createDataPartition(multi_data$case_volume_tier, p = 0.7, list = FALSE)
train_multi <- multi_data[split, ]
test_multi <- multi_data[-split, ]

# Train model
multi_log_model <- multinom(case_volume_tier ~ ., data = train_multi)

# Predict
multi_log_pred <- predict(multi_log_model, newdata = test_multi)
multi_log_pred <- factor(multi_log_pred, levels = levels(test_multi$case_volume_tier))

# Confusion matrix
multi_log_cm <- confusionMatrix(multi_log_pred, test_multi$case_volume_tier)
print(multi_log_cm)

# Class-level metrics & Macro F1
log_metrics <- multi_log_cm$byClass
log_overall <- multi_log_cm$overall
log_f1_macro <- mean(log_metrics[, "F1"])

#2.model- random forest
# Train model
rf_multi_model <- randomForest(case_volume_tier ~ ., data = train_multi, ntree = 100)

# Predict
rf_multi_pred <- predict(rf_multi_model, newdata = test_multi)
rf_multi_pred <- factor(rf_multi_pred, levels = levels(test_multi$case_volume_tier))

# Confusion matrix
rf_multi_cm <- confusionMatrix(rf_multi_pred, test_multi$case_volume_tier)
print(rf_multi_cm)

# Class-level metrics & Macro F1
rf_metrics <- rf_multi_cm$byClass
rf_overall <- rf_multi_cm$overall
rf_f1_macro <- mean(rf_metrics[, "F1"])

# Multinomial Logistic Confusion Plot
log_df <- as.data.frame(table(Predicted = multi_log_pred, Actual = test_multi$case_volume_tier))
ggplot(log_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, color = "black") +
  scale_fill_gradient(low = "pink", high = "red") +
  labs(title = "Confusion Matrix – Multinomial Logistic Regression") +
  theme_minimal()

# Random Forest Confusion Plot
rf_df <- as.data.frame(table(Predicted = rf_multi_pred, Actual = test_multi$case_volume_tier))
ggplot(rf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix – Random Forest") +
  theme_minimal()

# Combined metrics
model_scores <- tibble(
  Model = c("Multinomial", "Random Forest"),
  Accuracy = c(log_overall["Accuracy"], rf_overall["Accuracy"]),
  Macro_F1 = c(log_f1_macro, rf_f1_macro)
)

#Summary Table for Multi-Class Classification
multi_classification_summary <- tibble(
  Model = c("Multinomial Logistic Regression", "Random Forest"),
  Accuracy = c(log_overall["Accuracy"], rf_overall["Accuracy"]),
  Macro_F1 = c(log_f1_macro, rf_f1_macro)
)

print(multi_classification_summary)

# Plot
model_scores %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score") %>%
  ggplot(aes(x = Metric, y = Score, fill = Model)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Score, 3)), position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "Model Comparison: Accuracy & Macro F1 Score") +
  ylim(0, 1) +
  theme_minimal()