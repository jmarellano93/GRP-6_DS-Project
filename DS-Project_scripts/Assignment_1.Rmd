library(tidyverse)
library(corrplot)
library(fastDummies)
library(dplyr)
library(ggplot2)

# Read the dataset
original <- read.csv("DS-Project_data/LCdata.csv", 
                 row.names = NULL, 
                 sep = ";",
                 header = TRUE)

# Predefined columns to drop
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
                     "total_rec_prncp")
original_drop <- original[, -match(columns_to_drop,  names(original))]

view(original_drop)

# -----------------------------
# Dtype dictionary
# -----------------------------
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
  earliest_cr_line = "date",
  emp_length = "factor",
  emp_title = "factor",
  home_ownership = "factor",
  id = "integer",
  il_util = "numeric",
  initial_list_status = "factor",
  inq_fi = "integer",
  inq_last_12m = "integer",
  inq_last_6mths = "integer",
  int_rate = "numeric",
  last_credit_pull_d = "date",
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
  url = "factor",
  zip_code = "factor"
)

# -----------------------------
# Function to convert columns
# -----------------------------
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

# -----------------------------
# Quick check
# -----------------------------
str(original_drop)

# -----------------------------
# Make Copy to work with
# -----------------------------

data <- original_drop


# -----------------------------
# Basic Exploration
# -----------------------------
dim(data) # The shape is 798641:54

# Display the structure of the dataset
str(data)
view(data)

# Display the first few rows of the dataset
head(data)

# Display summary statistics of the dataset
summary(data)

# Search for duplicated rows
dup_ids <- data[duplicated(data$id), ]
dup_ids # There are no duplicated rows

# -----------------------------
# Identify NA and Missingness
# -----------------------------
# Which columns has NA
na_counts <- colSums(is.na(data))
na_counts[na_counts > 0]

# Drop rows which ONLY have NA
  # 1. Get the total number of columns in the dataframe
  num_cols <- ncol(data)
  
  # 2. Calculate the number of NAs per row
  na_count_per_row <- rowSums(is.na(data))
  
  # 3. Filter the dataframe to keep only rows where the NA count is LESS THAN the total number of columns (i.e., not ALL are NA)
  data_clean <- data[na_count_per_row < num_cols, ]
  
  # To check the result:
  print(paste("Original rows:", nrow(data)))
  print(paste("Rows remaining:", nrow(data_clean)))
  
  data <- data_clean


# Features which can be removed right away
pre_select_feat_drop <- c("member_id", # already one identifier called id, id's do not have predictive power
                          "url", # linking to the loan listing; provides no predictive power
                          "zip_code" # too granular and often redundant with addr_state
                          )
feat_to_drop <- c(pre_select_feat_drop)


# ===============================================================
# Inspect missingness in installment loan-related features
# ===============================================================

# 1. Define columns of interest
rct_il_features <- c("id", "il_util", "total_bal_il",
                     "open_il_12m", "open_il_24m", "open_il_6m", "mths_since_rcnt_il")

il_cols <- c("open_il_12m", "open_il_24m", "open_il_6m", "mths_since_rcnt_il")

# 2. Calculate NA count per column
na_summary <- colSums(is.na(data[, rct_il_features]))
na_percent <- round(na_summary / nrow(data) * 100, 2)

na_overview <- data.frame(
  column = names(na_summary),
  na_count = na_summary,
  na_percent = na_percent
)

cat("=== NA Summary per Installment-Related Column ===\n")
print(na_overview)

# 3. Calculate how many IL columns are NA per row
is_na_matrix <- is.na(data[, il_cols])
na_count_per_row <- rowSums(is_na_matrix)

# 4. Count how many rows have 0–6 NAs in these columns
row_na_distribution <- table(na_count_per_row)

# 5. Compute percentages
row_na_percentage <- round(100 * row_na_distribution / nrow(data), 2)

na_pattern_summary <- data.frame(
  n_na_columns = names(row_na_distribution),
  n_rows = as.numeric(row_na_distribution),
  percent_of_total = as.numeric(row_na_percentage)
)

cat("\n=== NA Pattern Across Rows (Installment Features) ===\n")
print(na_pattern_summary)

# 6. Identify rows that could be imputed (1–4 NAs among IL columns)
il_rows_for_impute <- data[na_count_per_row >= 1 & na_count_per_row < length(il_cols), 
                           c("id", il_cols)]
nr_il_rows_for_impute <- nrow(il_rows_for_impute)
cat("\nRows with partial installment data (1–4 NAs):", nr_il_rows_for_impute, "\n")

# 7. Identify rows with all IL columns NA (no installment info at all)
n_rows_all_il_na <- sum(na_count_per_row == length(il_cols))
cat("Rows with all installment-related columns NA:", n_rows_all_il_na, "\n")

# 8.1 Rows where all IL columns NA but il_util is NOT NA
rows_all_il_na_ilutil_notna <- data[na_count_per_row == length(il_cols) & 
                                      !is.na(data$il_util), 
                                    c("id", "il_util", il_cols)]
n_rows_all_il_na_ilutil_notna <- nrow(rows_all_il_na_ilutil_notna)
cat("Rows with all IL columns NA but il_util present:", n_rows_all_il_na_ilutil_notna, "\n")

# 8.2. Rows where all IL columns NA but total_bal_il is NOT NA
rows_all_il_na_totalbalil_notna <- data[na_count_per_row == length(il_cols) & 
                                      !is.na(data$total_bal_il), 
                                    c("id", "total_bal_il", il_cols)]
n_rows_all_il_na_totalbalil_notna <- nrow(rows_all_il_na_totalbalil_notna)
cat("Rows with all IL columns NA but total_bal_il present:", n_rows_all_il_na_totalbalil_notna, "\n")

# 9. Add a flag column to data (1 = at least one IL feature available, 0 = all missing)
data$has_installment_data <- ifelse(na_count_per_row < length(il_cols), 1, 0)

cat("\nNew flag column 'has_installment_data' created.\n")
cat("Rows with installment data:", sum(data$has_installment_data == 1), "\n")
cat("Rows without installment data:", sum(data$has_installment_data == 0), "\n")

# 10. Total number of rows
total_rows <- nrow(data)
cat("\nTotal rows in dataset:", total_rows, "\n")

# Add installment loan columns to drop list
feat_to_drop <- c(feat_to_drop, il_cols)

# ===============================================================

na_df <- data.frame(
  column = names(data),
  na_percent = round(colMeans(is.na(data)) * 100,2)
)
print(na_df)

# ===============================================================
# Distribution
# ===============================================================

# Identify columns with > 90% NA and drop it
cols_to_remove <- na_df$column[na_df$na_percent > 90]
cols_to_remove

data_filtered <- data[ , !names(data) %in% cols_to_remove]

# Ensure Dtypes are correct
# Make Histograms / Density plot (numeric)
# Make barplots (categorical)
# Make Boxplots
# Identify outliers
# Identify imputed values / wrong values
# Bivariate exploration: Scatterplots: numeric vs numeric | Boxplots: numeric vs categorical | Contingency tables: categorical vs categorical | Correlation matrix (very helpful)

view(data_filtered)


