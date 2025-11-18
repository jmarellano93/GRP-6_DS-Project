# Script: Median-Mode_Imputation_Pipeline.R

# Load necessary libraries (install missing packages with install.packages() if needed)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(forcats)
library(fastDummies) # Added for Section 7: Optimized OHE

# 1. Define Utility Function (Mode Calculation)
# Function to calculate Mode for categorical imputation
calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  # Handle factors/characters: return the most frequent value
  tab <- table(x)
  return(names(tab)[which.max(tab)])
}

# 2. Load Data and Define Parameters
raw_data <- read.csv2("C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/Full_dataset/LCdata.csv", 
                      stringsAsFactors = FALSE, na.strings = c("", "NA", "n/a"))

# Define reference date (e.g., current date for prediction time)
reference_date <- as.Date("2025-11-17") 

# 3. Feature Engineering (Date Derivation)

# Convert date strings to Date objects
data_clean <- raw_data 
date_cols <- c("earliest_cr_line", "last_credit_pull_d", "issue_d")
for(col in date_cols) {
  # Robust date parsing (Lending Club uses 'Mon-YYYY' format, e.g., 'Jan-2016')
  data_clean[[col]] <- parse_date_time(data_clean[[col]], orders = c("b-Y", "Y-b"))
}

# Calculate critical time-elapsed metrics (in months)
data_clean <- data_clean %>%
  mutate(
    # FIX: Use ifelse to handle NA dates and prevent difftime errors
    # Credit Age: Time in months from earliest credit line opening to today (for applicant risk)
    credit_age_in_months = ifelse(is.na(earliest_cr_line), NA, 
                                  as.numeric(difftime(reference_date, earliest_cr_line, units = "days") / 30.44)),
    # Time Since Last Credit Pull (for recency)
    time_since_last_pull = ifelse(is.na(last_credit_pull_d), NA,
                                  as.numeric(difftime(reference_date, last_credit_pull_d, units = "days") / 30.44))
  )
# The NA values generated here will be handled correctly by the imputation step (Section 6)

# 4. Mandatory Feature Exclusion (Data Leakage, Sparsity, Metadata/NLP)

cols_to_drop <- c(
  "id", "member_id", "funded_amnt", "funded_amnt_inv", "installment", 
  "loan_status", "pymnt_plan", "url", "desc", "title", "out_prncp", 
  "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", 
  "total_rec_int", "total_rec_late_fee", 
  "recoveries", "collection_recovery_fee", 
  "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "annual_inc_joint", 
  "dti_joint", "verification_status_joint", "open_acc_6m", "open_il_6m", 
  "open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", "il_util", 
  "open_rv_12m", "open_rv_24m", "max_bal_bc", "all_util", "inq_fi", 
  "total_cu_tl", "inq_last_12m", "policy_code", 
  # Drop original date columns after derivation
  "earliest_cr_line", "last_credit_pull_d", "issue_d"
)

data_clean <- data_clean %>% select(-one_of(cols_to_drop))

# 5. Type Conversion (MUST BE DONE BEFORE IMPUTATION) 

# This converts misclassified numerical columns back to numeric, which prevents 
# incorrect factor imputation and ensures Median imputation works correctly.

# int_rate: Must remove the '%' character (if present) and convert to numeric.
data_clean$int_rate <- data_clean$int_rate %>% 
  as.character() %>% 
  gsub("%", "", .) %>% 
  as.numeric()

# annual_inc, dti, revol_util: Convert misclassified factor/character columns to numeric.
data_clean$annual_inc <- as.numeric(data_clean$annual_inc)
data_clean$dti <- as.numeric(data_clean$dti)
data_clean$revol_util <- as.numeric(data_clean$revol_util)

# 6. Domain-Specific Imputation (MNA: Missing Not Applicable)

mna_cols <- c("mths_since_last_delinq", "mths_since_last_record", "mths_since_last_major_derog")
SENTINEL_VALUE <- 999

for (col in mna_cols) {
  # 1. Create binary flag: TRUE if NA (clean history), FALSE otherwise
  data_clean[[paste0(col, "_is_na")]] <- is.na(data_clean[[col]])
  # 2. Impute NA with the high sentinel value (999)
  data_clean[[col]][is.na(data_clean[[col]])] <- SENTINEL_VALUE
}

# 7. Core Imputation (Median/Mode)

    # A. Factor and Character Handling (Type Conversion + Cardinality Reduction)

# Convert all remaining character columns to factors first
char_cols <- names(data_clean)[sapply(data_clean, is.character)]
for (col in char_cols) {
  data_clean[[col]] <- as.factor(data_clean[[col]])
}

    # B.CONSISTENT CARDINALITY REDUCTION (Essential for efficient OHE later)

# Handle 'emp_title' (reduce levels to 45 + 1 for 'Other')
data_clean$emp_title <- data_clean$emp_title %>% 
  forcats::fct_explicit_na(na_level = "Missing") %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_EmpTitle")

# Handle 'zip_code' (reduce levels to 45 + 1 for 'Other')
data_clean$zip_code <- data_clean$zip_code %>% 
  forcats::fct_explicit_na(na_level = "Missing") %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_ZipCode")

# Handle 'addr_state' (reduce levels to 45 + 1 for 'Other')
data_clean$addr_state <- data_clean$addr_state %>% 
  forcats::fct_explicit_na(na_level = "Missing") %>% 
  forcats::fct_lump_n(n = 45, other_level = "Other_State")

# Handle 'emp_length' (explicit NA handling only, since it was previously a character column)
data_clean$emp_length <- data_clean$emp_length %>% 
  forcats::fct_explicit_na(na_level = "Missing")

    # C. Numeric features: Use Median

numeric_cols <- names(data_clean)[sapply(data_clean, is.numeric)]
for (col in numeric_cols) {
  data_clean[[col]][is.na(data_clean[[col]])] <- median(data_clean[[col]], na.rm = TRUE)
}

    # D. Factor features: Use Mode (for any remaining factor NAs)

factor_cols <- names(data_clean)[sapply(data_clean, is.factor)]
for (col in factor_cols) {
  if(anyNA(data_clean[[col]])) {
    mode_val <- calculate_mode(data_clean[[col]])
    data_clean[[col]][is.na(data_clean[[col]])] <- mode_val
  }
}

# Set the final imputed baseline data frame
Median_Mode_imputed_baseline_DS <- data_clean

# Check for NA, Inf, NaN values after imputation
na_summary <- Median_Mode_imputed_baseline_DS %>%
  summarise_all(~sum(is.na(.)))
print(na_summary)

total_na_count <- sum(na_summary)
print(paste("Total remaining NA values:", total_na_count))

inf_count <- sum(is.infinite(as.matrix(Median_Mode_imputed_baseline_DS)))
print(paste("Total Infinite values:", inf_count))

nan_count <- sum(is.nan(as.matrix(Median_Mode_imputed_baseline_DS)))
print(paste("Total NaN values:", nan_count))

# 7. FINAL FEATURE TRANSFORMATION (Optimized OHE with fastDummies)