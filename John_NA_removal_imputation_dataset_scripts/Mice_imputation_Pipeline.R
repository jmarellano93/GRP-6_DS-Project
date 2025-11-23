# Script: Mice_imputation_Pipeline.R

# Load necessary libraries (install missing packages with install.packages() if needed)
library(readr)
library(dplyr)
library(lubridate)
library(mice)
library(tidyr)
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
    # Use ifelse to handle NA dates and prevent difftime errors
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

# 2.TYPE CONVERSION AND CARDINALITY REDUCTION

    # A. Convert Numerical Columns back to Numeric (resolves the thousands-of-levels error)

# int_rate: Must strip '%' and convert to numeric.
data_imputation_prep$int_rate <- data_imputation_prep$int_rate %>% 
  as.character() %>% 
  gsub("%", "", .) %>% 
  as.numeric()

# annual_inc, dti, revol_util: Convert misclassified factor/character columns to numeric.
data_imputation_prep$annual_inc <- as.numeric(data_imputation_prep$annual_inc)
data_imputation_prep$dti <- as.numeric(data_imputation_prep$dti)
data_imputation_prep$revol_util <- as.numeric(data_imputation_prep$revol_util)

    # B. Convert remaining appropriate character columns to factors
char_cols <- names(data_imputation_prep)[sapply(data_imputation_prep, is.character)]
for (col in char_cols) {
  data_imputation_prep[[col]] <- as.factor(data_imputation_prep[[col]])
}

    # C. Reduce cardinality for legitimate Factor Columns (to be safely below 50)

# emp_title (51 levels)
data_imputation_prep$emp_title <- data_imputation_prep$emp_title %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_EmpTitle")

# zip_code (101 levels)
data_imputation_prep$zip_code <- data_imputation_prep$zip_code %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_ZipCode")

# addr_state (51 levels)
data_imputation_prep$addr_state <- data_imputation_prep$addr_state %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_State")

# 3. Domain-Specific Imputation (MNA: Sentinel Imputation 999 + Flag)
mna_cols <- c("mths_since_last_delinq", "mths_since_last_record", "mths_since_last_major_derog")
SENTINEL_VALUE <- 999

for (col in mna_cols) {
  data_imputation_prep[[paste0(col, "_is_na")]] <- is.na(data_imputation_prep[[col]])
  data_imputation_prep[[col]][is.na(data_imputation_prep[[col]])] <- SENTINEL_VALUE
}

# 4. MICE IMPUTATION ON A 10,000 INSTANCE SAMPLE

    # A. Create a random sample of 10,000 instances
SAMPLE_SIZE <- 10000
N_rows <- nrow(data_imputation_prep)
actual_sample_size <- min(SAMPLE_SIZE, N_rows)

set.seed(42) # Set seed for reproducible sampling
data_imputation_sample <- data_imputation_prep %>%
  sample_n(size = actual_sample_size)

    # B. Check for missingness in the sample
initial_miss_sample <- data_imputation_sample %>% summarize_all(list(na_count = ~sum(is.na(.))))
cols_to_impute_sample <- names(initial_miss_sample)[initial_miss_sample > 0]

if(length(cols_to_impute_sample) > 0) {
  # Generate imputation model blueprint based on the SAMPLE data structure
  imputation_methods_sample <- make.method(data_imputation_sample)
  
  # Use Predictive Mean Matching (PMM) for numerical and mixed types
  imputation_methods_sample[sapply(data_imputation_sample, is.numeric)] <- "pmm"
  
  # Define base predictor matrix
  predictor_matrix_sample <- quickpred(data_imputation_sample, exclude = "int_rate", mincor = 0.1)
  
    # C. CRITICAL PREDICTOR MATRIX MODIFICATIONS
  
  # Based on logs (Rows 4-603), variables are highly collinear with HOME_OWNERSHIP & PURPOSE.
  # Set the problem predictors (columns) to 0 when imputing the problem variables (rows).
  
  problem_dep_vars <- c("open_acc", "total_acc", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim", "credit_age_in_months")
  
  # We cannot easily target the dummy variables (e.g., home_ownershipMORTGAGE), 
  # so we must turn off the entire source factor column (`home_ownership` and `purpose`) 
  # as predictors for the problem variables.
  
  problem_predictors <- c("home_ownership", "purpose")
  
  # Set predictor matrix cells to 0 for problem variables/predictors
  predictor_matrix_sample[problem_dep_vars, problem_predictors] <- 0
  
  # -------------------------------------------------------------
  
  # Run MICE (m=5 imputed datasets, max iterations=20)
  set.seed(42) # For reproducibility
  mice_model_sample <- mice(data_imputation_sample, 
                            m = 5, 
                            method = imputation_methods_sample, 
                            predictorMatrix = predictor_matrix_sample,
                            maxit = 20, 
                            printFlag = TRUE) 
  
  # Extract the first imputed dataset for evaluation/modeling
  data_imputed_mice_sample <- complete(mice_model_sample, 1)
} else {
  # If no further missingness is found in the sample
  data_imputed_mice_sample <- data_imputation_sample
}
# Final output object: data_imputed_mice_sample

# Check for NA, Inf, NaN values after imputation
na_summary <- data_imputed_mice_sample %>%
  summarise_all(~sum(is.na(.)))
print(na_summary)

total_na_count <- sum(na_summary)
print(paste("Total remaining NA values:", total_na_count))

inf_count <- sum(is.infinite(as.matrix(data_imputed_mice_sample)))
print(paste("Total Infinite values:", inf_count))

nan_count <- sum(is.nan(as.matrix(data_imputed_mice_sample)))
print(paste("Total NaN values:", nan_count))

# 5. Full MICE Imputation

# Initial check for missingness after MNA handling
initial_miss <- data_imputation_prep %>% summarize_all(list(na_count = ~sum(is.na(.))))
cols_to_impute <- names(initial_miss)[initial_miss > 0]

if(length(cols_to_impute) > 0) {
  # Generate imputation model blueprint
  imputation_methods <- make.method(data_imputation_prep)
  
  # Use Predictive Mean Matching (PMM) for numerical and mixed types
  imputation_methods[sapply(data_imputation_prep, is.numeric)] <- "pmm"
  
  # Define predictor matrix: Exclude target (int_rate) from predicting other variables
  predictor_matrix <- quickpred(data_imputation_prep, exclude = "int_rate", mincor = 0.1)
  
  # Run MICE (m=5 imputed datasets, max iterations=20 for convergence)
  set.seed(42) # For reproducibility
  mice_model <- mice(data_imputation_prep, 
                     m = 5, 
                     method = imputation_methods, 
                     predictorMatrix = predictor_matrix,
                     maxit = 20, 
                     printFlag = FALSE)
  
  # Extract the first imputed dataset for the predictive model (M=1 for simplicity)
  MICE_imputed_baseline_DS <- complete(mice_model, 1)
} else {
  # If no further missingness is found 
  MICE_imputed_baseline_DS <- data_imputation_prep
}