# ==============================================================================
# LENDINGCLUB DATA CLEANING SCRIPT
# Goal: Reproduce exact output from Assignment 1 - Regression (Group 6)
# Input: Dataset-part-2.csv
# Output: cleaned_data (734825 rows, 19 columns)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(fastDummies)

# 1. Initial Load & Mapping (Correcting Delimiter to Comma)
# ------------------------------------------------------------------------------
# Based on your str(original), the separator is a comma
original <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/LCdata.csv", 
                     row.names = NULL, 
                     sep = ";", 
                     header = TRUE,
                     stringsAsFactors = FALSE)

# Predefined columns to drop (immediately after loading)
columns_to_drop <- c(
  "collection_recovery_fee", "installment", "funded_amnt", "funded_amnt_inv",
  "issue_d", "last_pymnt_amnt", "last_pymnt_d", "loan_status", "next_pymnt_d",
  "out_prncp", "out_prncp_inv", "pymnt_plan", "recoveries", "total_pymnt",
  "total_pymnt_inv", "total_rec_int", "total_rec_late_fee", "initial_list_status",
  "total_rec_prncp", "url"
)

original_drop <- original[, -match(columns_to_drop, names(original), nomatch = 0)]

# 2. Global Placeholder Handling
# ------------------------------------------------------------------------------
placeholders <- c(999, 9999.00, 9999999)
data <- original_drop %>%
  mutate(across(where(is.numeric), ~ ifelse(. %in% placeholders, NA, .)))

# 3. Categorical Transformations & Ordinal Variables
# ------------------------------------------------------------------------------

# term: Convert to numeric months
data$term <- trimws(as.character(data$term))
data$term <- ifelse(grepl("^36", data$term), 36, 60)
data$term <- as.numeric(data$term)

# emp_length: Clean "n/a" and set as ordered factor
data$emp_length[data$emp_length == "n/a"] <- NA
data$emp_length <- factor(
  data$emp_length,
  levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", 
             "6 years", "7 years", "8 years", "9 years", "10+ years"),
  ordered = TRUE
)

# home_ownership: Filter rare levels and set order
# Note: ANY, NONE, and OTHER are dropped due to missing info/low frequency
data <- data %>% filter(!home_ownership %in% c("ANY", "NONE", "OTHER"))
data$home_ownership <- factor(
  data$home_ownership,
  levels = c("RENT", "MORTGAGE", "OWN"),
  ordered = TRUE
)

# purpose: Drop 'educational' (only 1 observation after pre-proc)
data <- data[data$purpose != "educational", ]
data$purpose <- factor(data$purpose)
data$purpose <- droplevels(data$purpose)

# addr_state & title: Dropped due to cardinality/redundancy
data <- data %>% select(-any_of(c("addr_state", "title")))

# verification_status: Ordered factor
data$verification_status <- factor(
  data$verification_status,
  levels = c("Not Verified", "Source Verified", "Verified"),
  ordered = TRUE
)

# Dates: Drop date fields as they aren't available at application
data <- data %>% select(-any_of(c("earliest_cr_line", "last_credit_pull_d")))

# 4. Missing Value Filtering
# ------------------------------------------------------------------------------

# Drop high-missingness columns (>95%)
columns_to_drop_na <- c(
  "open_acc_6m", "open_il_6m", "open_il_12m", "open_il_24m", "mths_since_rcnt_il",
  "total_bal_il", "il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc",
  "all_util", "inq_fi", "total_cu_tl", "inq_last_12m"
)
cleaned_data <- data[, -match(columns_to_drop_na, names(data), nomatch = 0)]

# Identify and remove the systematic 25 rows with specific NAs
cleaned_data <- cleaned_data %>%
  filter(!is.na(delinq_2yrs), !is.na(inq_last_6mths), !is.na(open_acc))

# Remove rows where credit exposure features are all NA
cleaned_data <- cleaned_data %>%
  filter(!(is.na(tot_coll_amt) & is.na(tot_cur_bal) & is.na(total_rev_hi_lim)))

# 5. Joint Application Removal
# ------------------------------------------------------------------------------
joint_cols <- c("annual_inc_joint", "dti_joint", "verification_status_joint")
cleaned_data <- cleaned_data %>%
  select(-any_of(joint_cols)) %>%
  filter(application_type != "JOINT") %>%
  select(-application_type)

# 6. Outlier Handling & Logic
# ------------------------------------------------------------------------------

# annual_inc: Correction for values >500k (misplaced decimals)
cleaned_data <- cleaned_data %>%
  mutate(dti = ifelse(annual_inc > 500000, NA, dti),
         annual_inc = ifelse(annual_inc > 500000, annual_inc / 100, annual_inc))

# Log Transformations
cleaned_data <- cleaned_data %>%
  mutate(log_loan_amnt = log(loan_amnt + 1),
         log_annual_inc = log(annual_inc + 1))

# dti: Set implausible values (0 or >100) to NA
cleaned_data <- cleaned_data %>%
  mutate(dti = ifelse(dti > 100 | dti == 0, NA, dti))

# 7. Consistency Checks & Delinquency Feature Engineering
# ------------------------------------------------------------------------------

# Force consistency between delinq_2yrs and mths_since_last_delinq
cleaned_data <- cleaned_data %>%
  mutate(mths_since_last_delinq = ifelse(delinq_2yrs > 0 & mths_since_last_delinq > 24, NA, mths_since_last_delinq),
         delinq_2yrs = ifelse(!is.na(mths_since_last_delinq) & mths_since_last_delinq == 0 & delinq_2yrs == 0, 1, delinq_2yrs))

# Consistency with acc_now_delinq
cleaned_data <- cleaned_data %>%
  mutate(mths_since_last_delinq = ifelse(acc_now_delinq > 0 & mths_since_last_delinq > 0, 0, mths_since_last_delinq))

# Create Ordered Categorical Buckets
cleaned_data <- cleaned_data %>%
  mutate(delinq_2yrs_cat = factor(case_when(
    delinq_2yrs == 0 ~ "0",
    delinq_2yrs == 1 ~ "1",
    delinq_2yrs == 2 ~ "2",
    delinq_2yrs >= 3 ~ "3+"
  ), levels = c("3+", "2", "1", "0"), ordered = TRUE)) %>%
  mutate(delinq_recency = factor(case_when(
    is.na(mths_since_last_delinq) ~ "none",
    mths_since_last_delinq == 0 ~ "0",
    mths_since_last_delinq <= 6 ~ "1-6",
    mths_since_last_delinq <= 12 ~ "7-12",
    mths_since_last_delinq <= 24 ~ "13-24",
    mths_since_last_delinq > 24 ~ "24+"
  ), levels = c("0", "1-6", "7-12", "13-24", "24+", "none"), ordered = TRUE)) %>%
  mutate(acc_now_delinq_bin = factor(ifelse(acc_now_delinq > 0, "1", "0"), levels = c("0", "1")))

# 8. Revolving Credit & Major Derogatory Logic
# ------------------------------------------------------------------------------

# revol_bal logic vs total_rev_hi_lim
cleaned_data <- cleaned_data %>%
  mutate(revol_bal = ifelse(revol_bal > total_rev_hi_lim, NA, revol_bal))

# Force consistency for major derogatories based on pub_rec
cleaned_data <- cleaned_data %>%
  mutate(mths_since_last_major_derog = ifelse(pub_rec == 0, NA, mths_since_last_major_derog))

# major_derog_recency Buckets
cleaned_data <- cleaned_data %>%
  mutate(major_derog_recency = factor(case_when(
    is.na(mths_since_last_major_derog) ~ "none",
    mths_since_last_major_derog == 0 ~ "0",
    mths_since_last_major_derog <= 6 ~ "1-6",
    mths_since_last_major_derog <= 12 ~ "7-12",
    mths_since_last_major_derog <= 24 ~ "13-24",
    mths_since_last_major_derog <= 48 ~ "25-48",
    mths_since_last_major_derog <= 84 ~ "49-84",
    TRUE ~ ">84"
  ), levels = c("49-84", "25-48", "13-24", "7-12", "1-6", "0", "none"), ordered = TRUE))

# 9. Final Cleanup (Target Filtering and Feature Selection)
# ------------------------------------------------------------------------------

# Target validation: drop impossible int_rate
cleaned_data <- cleaned_data %>%
  filter(int_rate > 0 & int_rate <= 40)

# Final Drops to reach 19 columns:
# Drop redundant raw features after engineering their categorical versions
# Drop zero-inflated features identified as "useless" in Rmd
cleaned_data <- cleaned_data %>%
  select(-any_of(c(
    "id", "member_id", "desc", "zip_code", "emp_title", "policy_code", 
    "revol_util", "mths_since_last_delinq", "mths_since_last_major_derog", 
    "delinq_2yrs", "mths_since_last_record", "pub_rec", 
    "loan_amnt", "annual_inc", "tot_coll_amt", "acc_now_delinq",
    "collections_12_mths_ex_med"
  )))

# FINAL SUMMARY CHECK
message("Cleaning Complete.")
print(dim(cleaned_data))      # Should be 734825 x 19
print(summary(cleaned_data))  # Compare against Rmd summary

# =========================================================
# 12. MODEL EVALUATION (MSE)
# =========================================================

# 1. Load the saved model pipeline
# This contains both the XGBoost model and the preprocessing recipe
final_xgb_model <- readRDS("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/Assignment_1/final_xgb_model_FULL.rds")

# 2. Generate predictions on your cleaned data
# This returns a tibble with a column named '.pred'
predictions <- predict(final_xgb_model, new_data = cleaned_data)

# 3. Combine predictions with the actual target values
results <- cleaned_data %>%
  select(int_rate) %>%
  bind_cols(predictions)

# 4. Calculate Mean Squared Error (MSE)
# We calculate it manually or using yardstick's rmse() and squaring it
xgb_metrics <- results %>%
  metrics(truth = int_rate, estimate = .pred)

mse_value <- mean((results$int_rate - results$.pred)^2)

# 5. Output the results
message("--- Model Performance Summary ---")
print(xgb_metrics)
cat("Final Mean Squared Error (MSE):", mse_value, "\n")