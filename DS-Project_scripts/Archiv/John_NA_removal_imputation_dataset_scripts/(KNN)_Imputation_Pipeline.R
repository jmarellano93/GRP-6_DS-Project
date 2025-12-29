# Script: (KNN)_Imputation_Pipeline.R

# Load necessary libraries (install missing packages with install.packages() if needed)
library(readr)
library(dplyr)
library(lubridate)
library(VIM) # Recommended package for kNN imputation in R
library(tidyr)
library(caret) # For scaling (preProcess)
library(forcats)

# 1. Load Data and Pre-processing

# Assuming semicolon separator
raw_data <- read.csv2("C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/Full_dataset/LCdata.csv", 
                      stringsAsFactors = FALSE, na.strings = c("", "NA", "n/a"))

# Define reference date 
reference_date <- as.Date("2025-11-17") 

# Convert date strings to Date objects for derivation
data_imputation_prep <- raw_data
date_cols <- c("earliest_cr_line", "last_credit_pull_d", "issue_d")
for(col in date_cols) {
  data_imputation_prep[[col]] <- parse_date_time(data_imputation_prep[[col]], orders = c("b-Y", "Y-b"))
}

# Calculate critical time-elapsed metrics (in months)
data_imputation_prep <- data_imputation_prep %>%
  mutate(
    # Use ifelse for robust date handling
    credit_age_in_months = ifelse(is.na(earliest_cr_line), NA, 
                                  as.numeric(difftime(reference_date, earliest_cr_line, units = "days") / 30.44)),
    time_since_last_pull = ifelse(is.na(last_credit_pull_d), NA,
                                  as.numeric(difftime(reference_date, last_credit_pull_d, units = "days") / 30.44))
  )

# Mandatory Feature Exclusion
cols_to_drop <- c(
  "id", "member_id", "funded_amnt", "funded_amnt_inv", "installment", 
  "loan_status", "pymnt_plan", "url", "desc", "title", "out_prncp", 
  "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", 
  "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee", 
  "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "annual_inc_joint", 
  "dti_joint", "verification_status_joint", "open_acc_6m", "open_il_6m", 
  "open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", "il_util", 
  "open_rv_12m", "open_rv_24m", "max_bal_bc", "all_util", "inq_fi", 
  "total_cu_tl", "inq_last_12m", "policy_code", 
  "earliest_cr_line", "last_credit_pull_d", "issue_d"
)

data_imputation_prep <- data_imputation_prep %>% select(-one_of(cols_to_drop))

# 2. TYPE CONVERSION AND CARDINALITY REDUCTION

    # A. Type Conversion for Numerical Columns (Essential for Scaling/Distance)
data_imputation_prep$int_rate <- data_imputation_prep$int_rate %>% 
  as.character() %>% 
  gsub("%", "", .) %>% 
  as.numeric()

data_imputation_prep$annual_inc <- as.numeric(data_imputation_prep$annual_inc)
data_imputation_prep$dti <- as.numeric(data_imputation_prep$dti)
data_imputation_prep$revol_util <- as.numeric(data_imputation_prep$revol_util)


    # B. Convert remaining appropriate character columns to factors
char_cols <- names(data_imputation_prep)[sapply(data_imputation_prep, is.character)]
for (col in char_cols) {
  data_imputation_prep[[col]] <- as.factor(data_imputation_prep[[col]])
}

    # C. Cardinality Reduction for Factors (Essential for kNN Performance)
data_imputation_prep$emp_title <- data_imputation_prep$emp_title %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_EmpTitle")

data_imputation_prep$zip_code <- data_imputation_prep$zip_code %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_ZipCode")

data_imputation_prep$addr_state <- data_imputation_prep$addr_state %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_State")

# 3. Domain-Specific Imputation (MNA: Sentinel Imputation 999 + Flag)
mna_cols <- c("mths_since_last_delinq", "mths_since_last_record", "mths_since_last_major_derog")
SENTINEL_VALUE <- 999

for (col in mna_cols) {
  data_imputation_prep[[paste0(col, "_is_na")]] <- is.na(data_imputation_prep[[col]])
  data_imputation_prep[[col]][is.na(data_imputation_prep[[col]])] <- SENTINEL_VALUE
}

# 4. KNN IMPUTATION ON A 10,000 INSTANCE SAMPLE

    # A. Create the sample
SAMPLE_SIZE <- 10000
N_rows <- nrow(data_imputation_prep)
actual_sample_size <- min(SAMPLE_SIZE, N_rows)

set.seed(42) # Set seed for reproducible sampling
data_imputation_sample <- data_imputation_prep %>%
  sample_n(size = actual_sample_size)

    # B. Scaling and Preparation (Based ONLY on the Sample data for speed)
numeric_cols_for_scaling <- names(data_imputation_sample)[sapply(data_imputation_sample, is.numeric)]
data_knn_sample_prep <- data_imputation_sample

    # C. Scale the numeric features 
pre_obj_sample <- preProcess(data_knn_sample_prep[numeric_cols_for_scaling], method = c("center", "scale"))
data_knn_sample_prep[numeric_cols_for_scaling] <- predict(pre_obj_sample, data_knn_sample_prep[numeric_cols_for_scaling])

    # D. Apply kNN Imputation
set.seed(42)
data_imputed_knn_sample <- kNN(data_knn_sample_prep, k = 5) 

    # E. Cleanup and Rescale back to original values
data_imputed_knn_sample <- data_imputed_knn_sample %>% 
  select(-ends_with("_imp"))

data_imputed_knn_sample[numeric_cols_for_scaling] <- predict(pre_obj_sample, 
                                                             data_imputed_knn_sample[numeric_cols_for_scaling], 
                                                             inver = TRUE)

# Final output object for sample: data_imputed_knn_sample

# Check for NA, Inf, NaN values after imputation
na_summary <- data_imputed_knn_sample %>%
  summarise_all(~sum(is.na(.)))
print(na_summary)

total_na_count <- sum(na_summary)
print(paste("Total remaining NA values:", total_na_count))

inf_count <- sum(is.infinite(as.matrix(data_imputed_knn_sample)))
print(paste("Total Infinite values:", inf_count))

nan_count <- sum(is.nan(as.matrix(data_imputed_knn_sample)))
print(paste("Total NaN values:", nan_count))

# 5. FULL KNN IMPUTATION

# Identify columns needing scaling (all remaining numerics in the full dataset)
numeric_cols_for_scaling_full <- names(data_imputation_prep)[sapply(data_imputation_prep, is.numeric)]

data_knn_prep_full <- data_imputation_prep

# Scale the numeric features (Standardization is crucial for distance-based methods like KNN)
# NOTE: Scaling object is trained on the full dataset here.
pre_obj_full <- preProcess(data_knn_prep_full[numeric_cols_for_scaling_full], method = c("center", "scale"))
data_knn_prep_full[numeric_cols_for_scaling_full] <- predict(pre_obj_full, data_knn_prep_full[numeric_cols_for_scaling_full])

# Apply kNN Imputation (k=5 chosen as a robust starting point)
set.seed(42)
data_imputed_knn <- kNN(data_knn_prep_full, k = 5) 

# Remove the kNN indicator columns (e.g., column_imp) that VIM adds
data_imputed_knn <- data_imputed_knn %>% 
  select(-ends_with("_imp"))

# Rescale data back to original values after imputation (Invert the scaling)
data_imputed_knn[numeric_cols_for_scaling_full] <- predict(pre_obj_full, 
                                                           data_imputed_knn[numeric_cols_for_scaling_full], 
                                                           inver = TRUE)

# Final imputed output object: data_imputed_knn


# 6. Final Feature Transformation (One-Hot Encoding)