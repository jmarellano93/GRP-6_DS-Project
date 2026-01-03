# ==============================================================================
# SECTION 1: LIBRARY IMPORTS
# ==============================================================================
# Load necessary libraries for data manipulation, visualization, and modeling
library(tidyverse)
library(corrplot)
library(fastDummies)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(caret)
library(readr)

# ==============================================================================
# SECTION 2: DATA LOADING & INITIAL CLEANUP
# ==============================================================================

# Load the raw dataset
original <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/Assignment_1/LCdata.csv", 
                     row.names = NULL, 
                     sep = ";",
                     header = TRUE)

# Define columns that are not needed for analysis (e.g., post-origination features, URLs)
columns_to_drop <- c("collection_recovery_fee",
                     "installment",
                     "funded_amnt",
                     "funded_amnt_inv",
                     "issue_d",
                     "last_pymnt_amnt",
                     "last_pymnt_d",
                     "loan_status",
                     "next_pymnt_d",
                     "out_prncp",
                     "out_prncp_inv",
                     "pymnt_plan",
                     "recoveries",
                     "total_pymnt",
                     "total_pymnt_inv",
                     "total_rec_int",
                     "total_rec_late_fee",
                     "initial_list_status",
                     "total_rec_prncp",
                     "url")

# Remove the defined columns from the dataframe
original_drop <- original[, -match(columns_to_drop,  names(original))]

# ==============================================================================
# SECTION 3: DATA TYPE CASTING
# ==============================================================================

# Define a dictionary to map column names to their correct data types
dtype_dict <- list(
  acc_now_delinq = "integer",
  addr_state = "factor",
  all_util = "numeric",
  annual_inc = "numeric",
  annual_inc_joint = "numeric",
  application_type = "factor",
  collections_12_mths_ex_med = "integer",
  delinq_2yrs = "integer",
  desc = "factor",
  dti = "numeric",
  dti_joint = "numeric",
  earliest_cr_line = "str", # special case, will be converted to date later
  emp_length = "factor",
  emp_title = "factor",
  home_ownership = "factor",
  id = "integer",
  il_util = "numeric",
  inq_fi = "integer",
  inq_last_12m = "integer",
  inq_last_6mths = "integer",
  int_rate = "numeric",
  last_credit_pull_d = "str", # special case, will be converted to date later
  loan_amnt = "integer",
  max_bal_bc = "numeric",
  member_id = "integer",
  mths_since_last_delinq = "integer",
  mths_since_last_major_derog = "integer",
  mths_since_last_record = "integer",
  mths_since_rcnt_il = "integer",
  open_acc = "integer",
  open_acc_6m = "integer",
  open_il_12m = "integer",
  open_il_24m = "integer",
  open_il_6m = "integer",
  open_rv_12m = "integer",
  open_rv_24m = "integer",
  policy_code = "factor",
  pub_rec = "integer",
  purpose = "factor",
  revol_bal = "integer",
  revol_util = "numeric",
  term = "factor",
  title = "factor",
  tot_coll_amt = "numeric",
  tot_cur_bal = "numeric",
  total_acc = "integer",
  total_bal_il = "numeric",
  total_cu_tl = "integer",
  total_rev_hi_lim = "numeric",
  zip_code = "factor"
)

# Iterate through the dictionary and cast columns to correct types
for(col_name in names(dtype_dict)) {
  dtype <- dtype_dict[[col_name]]
  if(dtype == "factor") {
    original_drop[[col_name]] <- as.factor(original_drop[[col_name]])
  } else if(dtype == "integer") {
    original_drop[[col_name]] <- as.integer(original_drop[[col_name]])
  } else if(dtype == "numeric") {
    original_drop[[col_name]] <- as.numeric(original_drop[[col_name]])
  } else if(dtype == "date") {
    original_drop[[col_name]] <- as.Date(original_drop[[col_name]], format="%b-%Y")
  }
}

# Verify structure
str(original_drop)

# Create a working copy of the data
data <- original_drop

# ==============================================================================
# SECTION 4: HANDLING PLACEHOLDERS & CATEGORICAL ENCODING
# ==============================================================================

# Identify specific placeholder values (e.g., 999) and replace them with NA
placeholders <- c(999, 9999.00, 9999999)
data <- data %>%
  mutate(across(where(is.numeric),
                ~ ifelse(. %in% placeholders, NA, .)))


# --- Processing 'term' ---
# Convert 'term' string (e.g., "36 months") to numeric (36 or 60)
data$term <- trimws(as.character(data$term))
data$term <- ifelse(grepl("^36", data$term), 36, 60)
data$term <- as.numeric(data$term)

# --- Processing 'emp_length' ---
# Handle 'n/a' and convert to an ordered factor
data$emp_length[data$emp_length == "n/a"] <- NA
data$emp_length <- factor(
  data$emp_length,
  levels = c(
    "< 1 year",
    "1 year",
    "2 years",
    "3 years",
    "4 years",
    "5 years",
    "6 years",
    "7 years",
    "8 years",
    "9 years",
    "10+ years"
  ),
  ordered = TRUE
)

table(data$emp_length)

# --- Processing 'home_ownership' ---
# Filter out sparse categories and create ordered factor
data <- data %>%
  filter(!home_ownership %in% c("ANY", "NONE", "OTHER"))

data$home_ownership <- droplevels(data$home_ownership)

data$home_ownership <- factor(
  data$home_ownership,
  levels = c("RENT", "MORTGAGE", "OWN"),
  ordered = TRUE
)

# --- Processing 'purpose' ---
# Define levels for loan purpose
data$purpose <- factor(
  data$purpose,
  levels = c(
    "car",
    "credit_card",
    "debt_consolidation",
    "educational",
    "home_improvement",
    "house",
    "major_purchase",
    "medical",
    "moving",
    "other",
    "renewable_energy",
    "small_business",
    "vacation",
    "wedding"
  )
)

# Remove 'educational' purpose as per logic defined in preprocessing
data <- data[data$purpose != "educational", ]
data$purpose <- droplevels(data$purpose)

# Remove 'addr_state'
data <- data[, !(names(data) %in% "addr_state")]

# --- Processing 'verification_status' ---
# Convert to ordered factor representing increasing level of verification
data$verification_status <- factor(
  data$verification_status,
  levels = c(
    "Not Verified",
    "Source Verified",
    "Verified"
  ),
  ordered = TRUE
)

# Remove raw date columns and title (redundant or not usable)
data <- data %>% select(-earliest_cr_line, -last_credit_pull_d)
data <- data %>% select(-title)

# ==============================================================================
# SECTION 5: DROPPING HIGH-MISSINGNESS COLUMNS & ROWS
# ==============================================================================

# List of columns identified as having excessive missing values (>95%)
columns_to_drop_na <- c("open_acc_6m",
                        "open_il_6m",
                        "open_il_12m",
                        "open_il_24m",
                        "mths_since_rcnt_il",
                        "total_bal_il",
                        "il_util",
                        "open_rv_12m",
                        "open_rv_24m",
                        "max_bal_bc",
                        "all_util",
                        "inq_fi",
                        "total_cu_tl",
                        "inq_last_12m")

# Drop these columns
cleaned_data <- data[, -match(columns_to_drop_na, names(data))]
dim(cleaned_data)

# Identify rows that have NA in critical columns and remove them
na_rows <- data %>%
  filter(is.na(delinq_2yrs) |
           is.na(inq_last_6mths) |
           is.na(open_acc) |
           is.na(pub_rec) |
           is.na(total_acc) |
           is.na(acc_now_delinq))
print("Rows with NA values in specified columns:")
print(na_rows)

cleaned_data <- cleaned_data %>%
  filter(!id %in% na_rows$id)
dim(cleaned_data)

# Remove rows where all credit balance info is missing
cleaned_data <- cleaned_data %>%
  filter(
    !(is.na(tot_coll_amt) &
        is.na(tot_cur_bal) &
        is.na(total_rev_hi_lim))
  )

# ==============================================================================
# SECTION 6: REMOVING JOINT APPLICATIONS
# ==============================================================================

# Define joint application specific columns
joint_cols <- c(
  "annual_inc_joint",
  "dti_joint",
  "verification_status_joint"
)

# Only drop columns that exist
cols_to_drop <- intersect(joint_cols, names(cleaned_data))

# Remove joint columns and joint application rows (simplification strategy)
cleaned_data <- cleaned_data %>%
  select(-all_of(cols_to_drop))

cleaned_data <- cleaned_data %>%
  filter(application_type != "JOINT")

# Drop application_type column since it is now constant
cleaned_data <- cleaned_data %>%
  select(-application_type)
dim(cleaned_data) 

# ==============================================================================
# SECTION 7: FEATURE ENGINEERING & LOG TRANSFORMATIONS
# ==============================================================================

# Log transform loan amount
cleaned_data <- cleaned_data %>%
  mutate(log_loan_amnt = log(loan_amnt + 1)) 

# Fix outliers in annual income (> 500k likely data error)
cleaned_data <- cleaned_data %>%
  mutate(dti = ifelse(annual_inc > 500000, NA, dti),
         annual_inc = ifelse(annual_inc > 500000, annual_inc / 100, annual_inc)
  )

# Log transform annual income
cleaned_data <- cleaned_data %>%
  mutate(log_annual_inc = log(annual_inc + 1)) 

# Clean DTI outliers (implausible values > 100 or exactly 0)
cleaned_data <- cleaned_data %>%
  mutate(
    dti = ifelse(dti > 100, NA, dti),
    dti = ifelse(dti == 0, NA, dti)
  )

# ==============================================================================
# SECTION 8: CONSISTENCY CHECKS & FEATURE CLEANUP
# ==============================================================================

# --- Delinquency Consistency ---
# Fix contradictions between 'delinq_2yrs' and 'mths_since_last_delinq'
cleaned_data <- cleaned_data %>%
  mutate(mths_since_last_delinq = ifelse(delinq_2yrs > 0 &
                                           mths_since_last_delinq > 24,
                                         NA,
                                         mths_since_last_delinq))

cleaned_data <- cleaned_data %>%
  mutate(delinq_2yrs = ifelse(!is.na(mths_since_last_delinq) & mths_since_last_delinq == 0 &
                                delinq_2yrs == 0,
                              1,
                              delinq_2yrs))

# Check for contradictions
cleaned_data %>%
  filter(acc_now_delinq > 0 & delinq_2yrs == 0) # Should be 0 rows

# Fix contradiction between current delinquency and recency
cleaned_data <- cleaned_data %>%
  mutate(mths_since_last_delinq = ifelse(acc_now_delinq > 0 &
                                           mths_since_last_delinq > 0,
                                         0,
                                         mths_since_last_delinq))

# Create categorical bucket for delinquencies
cleaned_data <- cleaned_data %>%
  mutate(delinq_2yrs_cat = case_when(
    delinq_2yrs == 0 ~ "0",
    delinq_2yrs == 1 ~ "1",
    delinq_2yrs == 2 ~ "2",
    delinq_2yrs >= 3 ~ "3+",
    TRUE ~ NA_character_
  )) %>%
  mutate(delinq_2yrs_cat = factor(delinq_2yrs_cat,
                                  levels = c("3+", "2", "1", "0"),
                                  ordered = TRUE))

# Create categorical bucket for delinquency recency
cleaned_data <- cleaned_data %>%
  mutate(delinq_recency = case_when(
    is.na(mths_since_last_delinq) ~ "none",
    mths_since_last_delinq == 0 ~ "0",
    mths_since_last_delinq <= 6 ~ "1-6",
    mths_since_last_delinq <= 12 ~ "7-12",
    mths_since_last_delinq <= 24 ~ "13-24",
    mths_since_last_delinq > 24 ~ "24+",
    TRUE ~ NA_character_
  )) %>%
  mutate(delinq_recency = factor(delinq_recency,
                                 levels = c("0", "1-6", "7-12", "13-24", "24+", "none"),
                                 ordered = TRUE))

# Binarize 'acc_now_delinq'
cleaned_data <- cleaned_data %>%
  mutate(acc_now_delinq_bin = ifelse(acc_now_delinq > 0, 1, 0))

cleaned_data <- cleaned_data %>%
  mutate(acc_now_delinq_bin = factor(ifelse(acc_now_delinq > 0, "1", "0"),
                                     levels = c("0", "1")))

cleaned_data <- cleaned_data %>% select(-acc_now_delinq)

# --- Public Record Consistency ---
cleaned_data <- cleaned_data %>%
  mutate(pub_rec = ifelse(mths_since_last_record == 0 & pub_rec == 0, 1, pub_rec))

cleaned_data <- cleaned_data %>%
  filter(!(is.na(pub_rec) & !is.na(mths_since_last_record)))

# Inspect logic (optional checks)
cleaned_data %>%
  arrange(mths_since_last_record) %>%
  select(loan_amnt, mths_since_last_record, pub_rec, int_rate, term) %>%
  head(20)

cleaned_data %>%
  arrange(desc(mths_since_last_record)) %>%
  select(loan_amnt, mths_since_last_record, pub_rec, int_rate, term) %>%
  head(20)

cleaned_data %>%
  arrange(pub_rec) %>%
  select(loan_amnt, mths_since_last_record, pub_rec, int_rate, term) %>%
  head(20)

cleaned_data %>%
  arrange(desc(pub_rec)) %>%
  select(loan_amnt, mths_since_last_record, pub_rec, int_rate, term) %>%
  head(20)

# --- Account Consistency ---
cleaned_data <- cleaned_data %>%
  filter(!(is.na(open_acc) & is.na(total_acc)))

# Optional checks
cleaned_data %>%
  arrange(open_acc) %>%
  select(loan_amnt, open_acc, total_acc, int_rate, term) %>%
  head(20)

cleaned_data %>%
  arrange(desc(open_acc)) %>%
  select(loan_amnt, open_acc, total_acc, int_rate, term) %>%
  head(20)

cleaned_data %>%
  arrange(total_acc) %>%
  select(loan_amnt, open_acc, total_acc, int_rate, term) %>%
  head(20)

cleaned_data %>%
  arrange(desc(total_acc)) %>%
  select(loan_amnt, open_acc, total_acc, int_rate, term) %>%
  head(20)

# Clean revolving balance logic (balance cannot exceed limit)
cleaned_data <- cleaned_data %>%
  mutate(revol_bal = ifelse(revol_bal > total_rev_hi_lim, NA, revol_bal))

# Recalculate revolving utilization where possible
cleaned_data <- cleaned_data %>%
  mutate(revol_util = ifelse(!is.na(revol_bal) & !is.na(total_rev_hi_lim) & total_rev_hi_lim > 0,
                             pmin(100, (revol_bal / total_rev_hi_lim) * 100),
                             revol_util))

# Remove redundant collection columns
cleaned_data <- cleaned_data %>%
  select(-collections_12_mths_ex_med)

# Clean major derogatory events
cleaned_data <- cleaned_data %>%
  mutate(mths_since_last_major_derog =
           ifelse(pub_rec == 0, NA, mths_since_last_major_derog))

# Create buckets for major derogatory recency
cleaned_data <- cleaned_data %>%
  mutate(major_derog_recency = case_when(
    is.na(mths_since_last_major_derog) ~ "none",
    mths_since_last_major_derog == 0 ~ "0",
    mths_since_last_major_derog <= 6 ~ "1-6",
    mths_since_last_major_derog <= 12 ~ "7-12",
    mths_since_last_major_derog <= 24 ~ "13-24",
    mths_since_last_major_derog <= 48 ~ "25-48",
    mths_since_last_major_derog <= 84 ~ "49-84",
    TRUE ~ ">84"
  )) %>%
  mutate(major_derog_recency = factor(major_derog_recency,
                                      levels = c("49-84", "25-48", "13-24", "7-12", "1-6", "0", "none"),
                                      ordered = TRUE))

# Drop unused columns
cleaned_data <- cleaned_data %>%
  select(-tot_coll_amt)

cleaned_data <- cleaned_data %>%
  select(-c(id, member_id, desc, zip_code, emp_title, policy_code, revol_util, mths_since_last_delinq, mths_since_last_major_derog, delinq_2yrs, mths_since_last_record, pub_rec, loan_amnt, annual_inc))


print(dim(cleaned_data))

# ==============================================================================
# SECTION 9: SAVE & RELOAD CLEAN DATA
# ==============================================================================

# Save intermediate csv
#write.csv(cleaned_data,
#          file = "cleaned_data_test.csv",
#          row.names = FALSE)

# ==============================================================================
# SECTION 10: RE-ESTABLISH FACTOR LEVELS (POST-RELOAD)
# ==============================================================================
# Since CSV does not store factor metadata, we must re-declare ordered factors

# emp_length
data$emp_length <- factor(
  data$emp_length,
  levels = c(
    "< 1 year",
    "1 year",
    "2 years",
    "3 years",
    "4 years",
    "5 years",
    "6 years",
    "7 years",
    "8 years",
    "9 years",
    "10+ years"
  ),
  ordered = TRUE
)

# verification_status
data$verification_status <- factor(
  data$verification_status,
  levels = c(
    "Not Verified",
    "Source Verified",
    "Verified"
  ),
  ordered = TRUE
)

# home_ownership
data$home_ownership <- factor(
  data$home_ownership,
  levels = c("RENT", "MORTGAGE", "OWN"),
  ordered = TRUE
)

# delinq_2yrs_cat
data$delinq_2yrs_cat <- factor(
  data$delinq_2yrs_cat,
  levels = c("0", "1", "2", "3+"),
  ordered = TRUE
)

# delinq_recency
data$delinq_recency <- factor(
  data$delinq_recency,
  levels = c(
    "0",
    "1-6",
    "7-12",
    "13-24",
    "24+",
    "none"
  ),
  ordered = TRUE
)

# major_derog_recency
data$major_derog_recency <- factor(
  data$major_derog_recency,
  levels = c(
    "0",
    "1-6",
    "7-12",
    "13-24",
    "25-48",
    "49-84",
    "none"
  ),
  ordered = TRUE
)

# ==============================================================================
# SECTION 11: PREDICTION & EVALUATION
# ==============================================================================

library(tidymodels)

# Load the pre-trained XGBoost model
final_xgb_model <- readRDS("models/final_xgb_model.rds")

# Generate predictions on the new data
pred <- predict(final_xgb_model, new_data = data)

# Combine predictions with actuals
library(yardstick)
library(dplyr)

results <- bind_cols(data %>% select(int_rate), pred)

# Calculate metrics (RMSE, MAE, R-squared)
metrics(results, truth = int_rate, estimate = .pred)

# Calculate MSE manually
mse_value <- mean((results$int_rate - results$.pred)^2, na.rm = TRUE)
mse_value