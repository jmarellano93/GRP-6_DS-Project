# ==============================================================================
# COMBINED SCRIPT: DATA CLEANING, EDA, AND NEURAL NETWORK TRAINING
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: ENVIRONMENT SETUP
# ------------------------------------------------------------------------------

# 1.1 Working Directory and Libraries
# -----------------------------------
# Set personal working directory (Adjust as needed)
# setwd("~/Documents/Repos/GRP-6_DS-Project")
# renv::activate()

# --- Automated Dependency Management ---
# Ensures reproducibility across different grading environments.
installed_pkgs <- installed.packages()[, "Package"]
req_pkgs <- c("tidyverse",    # Collection of packages (ggplot2, dplyr, etc.)
              "caret",        # ML training helper (nearZeroVar, findCorrelation)
              "corrplot",     # Visualizing correlation matrices
              "naniar",       # Visualizing missing values
              "gridExtra",    # Arranging multiple plots
              "e1071",        # Statistical functions (skewness, kurtosis)
              "Hmisc",        # Data analysis helper functions
              "tensorflow",   # Interface to TensorFlow (needed for seeding)
              "reticulate",   # Python interface (needed for backend configuration)
              "rpart",        # Recursive Partitioning (Decision Trees)
              "fastDummies",  # Fast One-Hot Encoding
              "keras3",       # Deep Learning API
              "here")         # Path management

new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if(length(new_pkgs)) install.packages(new_pkgs)

# Load necessary libraries for Data Cleaning and EDA
library(tidyverse)    # Loads ggplot2, dplyr, tidyr, readr, etc.
library(caret)        # Feature selection (nearZeroVar)
library(rpart)        # Decision trees
library(here)         # Path management
library(Hmisc)        # Data analysis helpers
library(fastDummies)  # One-hot encoding
library(gridExtra)    # Plot arrangement
library(corrplot)     # Correlation plots
library(naniar)       # Missing value visualization
library(e1071)        # Skewness calculation

# Load necessary libraries for Modeling
library(keras3)       # Neural Networks
library(tensorflow)   # TensorFlow backend (for seeding)
# library(reticulate) # Loaded automatically by keras3/tensorflow, but installed above

# Verify Project Root
cat(">> Project Root Detected at:", here(), "\n")

# ------------------------------------------------------------------------------
# 1.2 Backend Connection Strategy (TensorFlow)
# ------------------------------------------------------------------------------
# Explicit Environment Binding: Attempts to locate 'r-reticulate' Conda environment.
backend_configured <- FALSE

# Check 1: Is a Conda environment named 'r-reticulate' available?
# We use tryCatch to prevent the script from crashing if Conda is not installed.
if (tryCatch("r-reticulate" %in% reticulate::conda_list()$name, error = function(e) FALSE)) {
  try({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    backend_configured <- TRUE
    cat(">> CONNECTION SUCCESS: Activated 'r-reticulate' Conda environment.\n")
  }, silent = TRUE)
}

# Check 2: Fallback to any valid Python with TF available
if (!backend_configured) {
  if (reticulate::py_module_available("tensorflow")) {
    backend_configured <- TRUE
    cat(">> CONNECTION SUCCESS: Found TensorFlow in the current active Python environment.\n")
  }
}

# ------------------------------------------------------------------------------
# 1.3 Seeding and Reproducibility
# ------------------------------------------------------------------------------
# Ensure TensorFlow is available for seeding
library(tensorflow)

# REPRODUCIBILITY GUARANTEE:
# 1. Seed R (Frontend)
set.seed(123)

# 2. Seed TensorFlow/Python (Backend)
if (backend_configured && reticulate::py_module_available("tensorflow")) {
  tryCatch({
    tf$random$set_seed(123L)
    cat(">> SETUP SUCCESS: TensorFlow backend seeded (Reproducibility Guaranteed).\n")
  }, error = function(e) {
    cat(">> WARNING: TensorFlow found but seeding failed. Error:", e$message, "\n")
  })
} else {
  # Note: Keras 3 can use JAX/Torch backends, so we log a warning rather than a hard stop
  # unless you are strictly enforcing TensorFlow.
  cat("\n>> NOTICE: TensorFlow Python Backend not explicitly detected.\n") 
  cat("   If you intended to use TensorFlow, run reticulate::install_miniconda() and install_tensorflow().\n")
  cat("   If using JAX or PyTorch backends via Keras 3, you can ignore this warning.\n")
}

# ------------------------------------------------------------------------------
# SECTION 2: DATA INGESTION AND PRELIMINARY EDA
# ------------------------------------------------------------------------------

cat("--- Loading and Examining Data ---\n")

# 2.1 Load Raw Data
# -----------------
# Load the dataset using relative paths via 'here'
path <- here("DS-Project_data", "Dataset-part-2.csv")

# Optional: Safety check
if(!file.exists(path)) stop("CRITICAL ERROR: File not found at: ", path)

data <- read.csv(path)

# ==============================================================================
# 2.2 Initial Exploratory Data Analysis
# ==============================================================================
# PURPOSE: Identify Data Quality issues (Anomalies, Skew, MNAR) prior to preprocessing.

# Mapping 'data' to 'df'
df <- data 
# -------------------------

# ------------------------------------------------------------------------------
# 2.2.1 Setup PDF Output
# ------------------------------------------------------------------------------
plot_output_dir <- here("DS-Project_Part2_Scripts", "Saved_Outputs")
if(!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

pdf(file = file.path(plot_output_dir, "Plots_&_Visualizations.pdf"), width = 11, height = 8.5)
cat(">> PDF Graphics Device Opened. Plots will be saved to:", plot_output_dir, "\n")

# ------------------------------------------------------------------------------
# 2.2.2 Structure & Content Inspection
# ------------------------------------------------------------------------------
cat("\n[Phase 1] Structure & Content Inspection\n")
print(dim(df))
dplyr::glimpse(df)

# ------------------------------------------------------------------------------
# 2.2.3 Data Quality Checks
# ------------------------------------------------------------------------------
cat("\n[Phase 2] Data Quality Checks\n")

# 1. Statistical Summary
print(summary(dplyr::select(df, where(is.numeric))))

# 2. Missing Values (Focus on OCCUPATION_TYPE)
cat("\nMissing Values Count:\n")
miss_counts <- colSums(is.na(df))
print(miss_counts[miss_counts > 0])

# 3. Duplicate Rows
dup_count <- sum(duplicated(df))
cat("\nDuplicate Rows Detected:", dup_count, "\n")

# 4. Variance Check
if("FLAG_MOBIL" %in% names(df)) {
  cat("\nVariance Check for FLAG_MOBIL:\n")
  print(table(df$FLAG_MOBIL))
}

# ------------------------------------------------------------------------------
# 2.2.4 Univariate Visualization
# ------------------------------------------------------------------------------

# 1. Target Variable Distribution
if("status" %in% names(df)) {
  p_target <- ggplot(df, aes(x = as.factor(status))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Target Distribution (Class Imbalance Check)", x = "Status Code", y = "Count") +
    theme_minimal()
  print(p_target)
}

# 2. Status Variable Breakdown (Imbalance Confirmation)
cat("\n[Analysis Verification] Detailed Status Variable Breakdown:\n")
status_counts <- table(df$status)
print(status_counts)
cat("\nRelative Frequencies (%):\n")
print(round(prop.table(status_counts) * 100, 2))

# 3. Numeric Distributions (Income Skew)
num_cols <- names(dplyr::select(df, where(is.numeric)))
for(col in num_cols) {
  p_hist <- ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "coral", color = "white") +
    labs(title = paste("Distribution:", col)) + theme_minimal()
  
  # Updated deprecated aes_string to .data[[]]
  p_box <- ggplot(df, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Outliers:", col)) + theme_minimal()
  
  grid.arrange(p_hist, p_box, ncol = 2)
}

# 4. Discrete Variable Analysis
if("CNT_CHILDREN" %in% names(df)) {
  cat("\n[Analysis Verification] Discrete Count Distribution: CNT_CHILDREN\n")
  print(table(df$CNT_CHILDREN))
}

# 5. Skewness Verification (Income)
if("AMT_INCOME_TOTAL" %in% names(df)) {
  cat("\n[Analysis Verification] Income Distribution Statistics:\n")
  skew_val <- e1071::skewness(df$AMT_INCOME_TOTAL, na.rm = TRUE)
  cat(paste("Skewness Coefficient:", round(skew_val, 4), "\n"))
  cat("Quantiles (0% to 100%):\n")
  print(quantile(df$AMT_INCOME_TOTAL, probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.99, 1)))
}

# ------------------------------------------------------------------------------
# 2.2.5 The 365243 Pensioner Anomaly
# ------------------------------------------------------------------------------
# CRITICAL FINDING: 'DAYS_EMPLOYED' contains 365243 (~1000 years), indicating "Pensioner".
cat("\n Handling Anomalies and Engineering Features\n")

if("DAYS_EMPLOYED" %in% names(df) && "NAME_INCOME_TYPE" %in% names(df)) {
  anomaly_check <- df %>%
    mutate(is_magic_val = ifelse(DAYS_EMPLOYED == 365243, 1, 0)) %>%
    group_by(NAME_INCOME_TYPE) %>%
    summarise(
      Total_Count = n(),
      Magic_Val_Count = sum(is_magic_val),
      Percentage = (sum(is_magic_val) / n()) * 100
    )
  print("365243 Anomaly Overlap Analysis:")
  print(anomaly_check)
}

# ------------------------------------------------------------------------------
# 2.2.6 MNAR (Missing Not At Random) Analysis
# ------------------------------------------------------------------------------
# Logic Check: Do 100% of Pensioners have missing occupation?
if("OCCUPATION_TYPE" %in% names(df) && "NAME_INCOME_TYPE" %in% names(df)) {
  cat("\n[Analysis Verification] MNAR Check: Occupation Missingness vs. Income Type\n")
  
  mnar_check <- df %>%
    mutate(OCCUPATION_TYPE = ifelse(OCCUPATION_TYPE == "", NA, OCCUPATION_TYPE)) %>%
    group_by(NAME_INCOME_TYPE) %>%
    summarise(
      Total_Count = n(),
      Missing_Occupation = sum(is.na(OCCUPATION_TYPE)),
      Missing_Rate_Pct = round((sum(is.na(OCCUPATION_TYPE)) / n()) * 100, 2)
    )
  print(mnar_check)
}

# MNAR Evidence Visualization
p_mnar <- df %>%
  mutate(Has_Occupation = ifelse(is.na(OCCUPATION_TYPE) | OCCUPATION_TYPE == "", "Missing", "Present")) %>%
  ggplot(aes(x = NAME_INCOME_TYPE, fill = Has_Occupation)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Evidence of MNAR: Occupation Missingness by Income Type",
       subtitle = "Pensioners have nearly 100% missing occupation data",
       x = "Income Type", y = "Proportion", fill = "Occupation Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_mnar)

# ------------------------------------------------------------------------------
# SECTION 3: DATA TYPE DEFINITION AND LOGICAL CORRECTIONS
# ------------------------------------------------------------------------------

# 3.1 Define Data Types and Reload
# --------------------------------
# 1. Define Data Types
col_types <- c(
  ID = "character",                  
  CODE_GENDER = "factor",
  FLAG_OWN_CAR = "character",        
  FLAG_OWN_REALTY = "character",     
  CNT_CHILDREN = "integer",
  AMT_INCOME_TOTAL = "numeric",
  NAME_INCOME_TYPE = "character",
  NAME_EDUCATION_TYPE = "factor",
  NAME_FAMILY_STATUS = "factor",
  NAME_HOUSING_TYPE = "factor",
  DAYS_BIRTH = "integer",
  DAYS_EMPLOYED = "integer",
  FLAG_MOBIL = "integer",            
  FLAG_WORK_PHONE = "integer",
  FLAG_PHONE = "integer",
  FLAG_EMAIL = "integer",
  OCCUPATION_TYPE = "factor",        
  CNT_FAM_MEMBERS = "integer",
  status = "factor"
)

# 2. Reload the CSV with strict types
# Note: 'path' here now refers to the dynamic path created with here() in Section 2.1
data <- read.csv(path, colClasses = col_types, na.strings = c("NA", ""))

# [QA 3.1] Strict Reload Check
cat("[QA 3.1] Verifying Strict Types... ")
if(is.numeric(data$AMT_INCOME_TOTAL) && is.factor(data$NAME_INCOME_TYPE)) {
  cat("PASS: Data reloaded with strict types correctly.\n")
} else {
  cat("FAIL: Types mismatch after reload.\n")
}

# 3.2 Initial Formatting
# ----------------------
# 3. Post-Load Logic (Conversions to Integer)
data$FLAG_OWN_CAR <- as.integer(data$FLAG_OWN_CAR == "Y")
data$FLAG_OWN_REALTY <- as.integer(data$FLAG_OWN_REALTY == "Y")

# Convert 0/1 integers to Logical Booleans
flags_numeric <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")
data[flags_numeric] <- lapply(data[flags_numeric], as.integer)

# Drop columns with no variance (FLAG_MOBIL)
data <- data %>% select(-FLAG_MOBIL)

# [QA 3.2] Formatting Check
cat("[QA 3.2] Verifying Boolean Conversions... ")
if(is.integer(data$FLAG_OWN_CAR) && !"FLAG_MOBIL" %in% names(data)) {
  cat("PASS: Booleans converted and zero-variance column removed.\n")
} else {
  cat("FAIL: Boolean conversion failed.\n")
}

cat("--- Correcting Age and Employment Data ---\n")

# 3.3 Age and Employment Logic
# ----------------------------
# Correct DAYS_BIRTH to positive years
data$AGE <- abs(data$DAYS_BIRTH) / 365.25

# Pensioner and Employment corrections
# Fix pensioners who are actually working
data <- data %>%
  mutate(
    NAME_INCOME_TYPE = ifelse(
      NAME_INCOME_TYPE == "Pensioner" & DAYS_EMPLOYED != 365243,
      "Working",
      NAME_INCOME_TYPE
    )
  )

# Reclassify NA values in OCCUPATION_TYPE
data <- data %>%
  mutate(
    OCCUPATION_TYPE = as.character(OCCUPATION_TYPE),
    OCCUPATION_TYPE = case_when(
      DAYS_EMPLOYED == 365243 ~ "Retired",
      is.na(OCCUPATION_TYPE)  ~ "Unknown",
      TRUE ~ OCCUPATION_TYPE
    ),
    OCCUPATION_TYPE = as.factor(OCCUPATION_TYPE)
  )

# Set DAYS_EMPLOYED to 0-equivalent for pensioned people (sentinel value replacement)
data$DAYS_EMPLOYED[data$DAYS_EMPLOYED == 365243] <- 21915 

# Calculate ACTIVE_EMPLOYMENT_YEARS
data$ACTIVE_EMPLOYMENT_YEARS <- abs(data$DAYS_EMPLOYED) / 365.25
data$ACTIVE_EMPLOYMENT_YEARS_LOG <- log1p(data$ACTIVE_EMPLOYMENT_YEARS)

# [QA 3.3] Logic Corrections Check
cat("[QA 3.3] Verifying Age/Employment Logic... ")
neg_age <- any(data$AGE < 0)
sentinel_remains <- any(data$DAYS_EMPLOYED == 365243)
cat(sprintf("Negative Age Found: %s. Sentinel Remains: %s.\n", neg_age, sentinel_remains))
if(!neg_age && !sentinel_remains) {
  cat("PASS: Age positive and sentinels replaced.\n")
} else {
  cat("FAIL: Logic corrections incomplete.\n")
}

# 3.4 Family Size and Outliers
# ----------------------------
# Family Size Outliers
cat("--- Handling Family and Income Outliers ---\n")
data <- data[data$CNT_FAM_MEMBERS <= 10, ]

# Correct Family Member counts (Logic: Family Members >= Children + Adults)
anomaly <- data$CNT_FAM_MEMBERS < data$CNT_CHILDREN
data$CNT_FAM_MEMBERS[anomaly & data$NAME_FAMILY_STATUS %in% c("Married", "Civil marriage")] <-
  data$CNT_CHILDREN[anomaly & data$NAME_FAMILY_STATUS %in% c("Married", "Civil marriage")] + 2

data$CNT_FAM_MEMBERS[anomaly & data$NAME_FAMILY_STATUS %in% c("Separated", "Widow", "Single / not married")] <-
  data$CNT_CHILDREN[anomaly & data$NAME_FAMILY_STATUS %in% c("Separated", "Widow", "Single / not married")] + 1

# [QA 3.4] Family Size Check
cat("[QA 3.4] Verifying Family Logic... ")
if(any(data$CNT_FAM_MEMBERS > 10) || any(data$CNT_FAM_MEMBERS < data$CNT_CHILDREN)) {
  cat("FAIL: Family anomalies still present.\n")
} else {
  cat("PASS: Family size anomalies resolved.\n")
}

# 3.5 Income Outliers
# -------------------
# Income Outliers
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", 
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")

# Remove unlikely high incomes for specific jobs
data <- data %>%
  filter( !(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs) )

# Log transform Income
data$AMT_INCOME_TOTAL_LOG <- log1p(data$AMT_INCOME_TOTAL)

# [QA 3.5] Income Logic Check
cat("[QA 3.5] Verifying Income Outliers... ")
bad_income_rows <- nrow(data %>% filter(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs))
if(bad_income_rows == 0) {
  cat("PASS: Impossible high incomes removed.\n")
} else {
  cat("FAIL: High income outliers remain.\n")
}

# ------------------------------------------------------------------------------
# SECTION 4: FEATURE ENGINEERING
# ------------------------------------------------------------------------------

cat("--- Feature Engineering ---\n")

# 4.1 Creating Ratio Features
# ---------------------------
data$INCOME_PER_FAMILY_MEMBER <- data$AMT_INCOME_TOTAL / data$CNT_FAM_MEMBERS
data$INCOME_PER_FAMILY_MEMBER_LOG <- log1p(data$INCOME_PER_FAMILY_MEMBER)

data$EMPLOYMENT_RATIO <- data$ACTIVE_EMPLOYMENT_YEARS / data$AGE
data$CREDIT_MATURITY <- data$AGE - data$ACTIVE_EMPLOYMENT_YEARS
data$INCOME_PER_AGE <- data$AMT_INCOME_TOTAL / data$AGE
data$INCOME_PER_AGE_LOG <- log1p(data$INCOME_PER_AGE)

# [QA 4.1] Feature Creation Check
cat("[QA 4.1] Verifying New Features... ")
new_feats <- c("INCOME_PER_FAMILY_MEMBER", "EMPLOYMENT_RATIO", "CREDIT_MATURITY", "INCOME_PER_AGE")
if(all(new_feats %in% names(data))) {
  cat("PASS: All engineered features present.\n")
} else {
  cat("FAIL: Missing engineered features.\n")
}

# ------------------------------------------------------------------------------
# SECTION 5: FINAL PRE-PROCESSING AND ENCODING
# ------------------------------------------------------------------------------

cat("--- Encoding Variables for Modeling ---\n")

# 5.1 Target Variable Encoding
# ----------------------------
# NOTE: We retain rows with target 'X' as required (8 classes total).

# TARGET ENCODING
data$status <- as.character(data$status)

# Map status levels to class IDs 0..7 (unordered)
# Mapping: C=0, X=1, 0=2, 1=3, 2=4, 3=5, 4=6, 5=7
data$target_class <- recode(data$status,
                            "C" = 0,
                            "X" = 1,
                            "0" = 2,
                            "1" = 3,
                            "2" = 4,
                            "3" = 5,
                            "4" = 6,
                            "5" = 7
) |> as.numeric()

# [QA 5.1] Target Encoding Check
cat("[QA 5.1] Verifying Target Encoding... ")
if(any(data$status == "X") && any(data$target_class == 1)) {
  cat("PASS: Status 'X' retained and encoded as class 1.\n")
} else {
  cat("WARNING: Status 'X' not found or not encoded correctly.\n")
}

# [EDA SNAPSHOT] Save dataset with categorical columns before they are encoded/dropped
# We will use this specific object for the Risk Profiling in Section 6.6
df_eda_categorical <- data

# 5.2 Feature Encoding
# --------------------
# BOOLEAN FLAGS -> NUMERIC
bool_cols <- c("FLAG_OWN_CAR", "FLAG_OWN_REALTY", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")
data[bool_cols] <- lapply(data[bool_cols], as.numeric)

# EDUCATION (ORDINAL)
data$EDUCATION_ENCODED <- case_when(
  data$NAME_EDUCATION_TYPE == "Lower secondary" ~ 0,
  data$NAME_EDUCATION_TYPE == "Secondary / secondary special" ~ 1,
  data$NAME_EDUCATION_TYPE == "Incomplete higher" ~ 2,
  data$NAME_EDUCATION_TYPE == "Higher education" ~ 3,
  data$NAME_EDUCATION_TYPE == "Academic degree" ~ 4,
  TRUE ~ NA_real_
)
data <- data %>% select(-NAME_EDUCATION_TYPE)

# [QA 5.2] Feature Encoding Check
cat("[QA 5.2] Verifying Feature Encoding... ")
has_dummies <- any(grepl("CODE_GENDER_", names(data)))
edu_dropped <- !"NAME_EDUCATION_TYPE" %in% names(data)
if(has_dummies && edu_dropped) {
  cat("PASS: One-hot encoding applied and original column dropped.\n")
} else {
  cat("FAIL: Encoding incomplete.\n")
}

# 5.3 Cleanup and Save
# --------------------
# --------------------
# DROP UNUSED COLUMNS
# Removed '-Employment_Status' because it was never created in this script
data <- data %>% select(-ID, -DAYS_BIRTH, -DAYS_EMPLOYED, -AMT_INCOME_TOTAL, 
                        -ACTIVE_EMPLOYMENT_YEARS, -INCOME_PER_FAMILY_MEMBER, 
                        -INCOME_PER_AGE, -status)

# [QA 5.3] Cleanup Check
cat("[QA 5.3] Verifying Column Cleanup... ")
if(!"ID" %in% names(data) && !"status" %in% names(data)) {
  cat("PASS: Unused columns dropped successfully.\n")
} else {
  cat("FAIL: Unused columns still present.\n")
}

# 5.4 Feature Variance and Collinearity Filtering
# -----------------------------------------------
cat("--- Performing Feature Variance and Collinearity Filtering ---\n")

# Zero Variance Filtering (caret::nearZeroVar)
# Identifies predictors that have one unique value or very few unique values 
# relative to the number of samples.
# Adjusted to be less aggressive (99/1 instead of 95/5) in order to 
# Avoid dropping a relevant rare category (e.g., "Academic degree") but still return indices.
nzv_cols <- nearZeroVar(data, freqCut = 99/1, saveMetrics = FALSE)
if (length(nzv_cols) > 0) {
  # Get names before removing
  nzv_names <- names(data)[nzv_cols]
  cat(sprintf(">> Dropping %d Near-Zero Variance features: %s\n", 
              length(nzv_names), paste(nzv_names, collapse = ", ")))
  data <- data[, -nzv_cols]
} else {
  cat(">> No Near-Zero Variance features found.\n")
}

# Collinearity Elimination (caret::findCorrelation)
# Calculates correlation matrix and finds attributes that are highly corrected
# (ideally > 0.90) and removes them to reduce redundancy.

# Select only numeric columns for correlation check (OHE creates numeric binaries, so this works)
numeric_data <- data %>% select_if(is.numeric)

# Exclude target class from correlation removal check
numeric_data_features <- numeric_data %>% select(-target_class)

# Calculate Correlation Matrix
cor_matrix <- cor(numeric_data_features, use = "pairwise.complete.obs")

# Find attributes that are highly corrected (cutoff = 0.9)
high_corr_cols <- findCorrelation(cor_matrix, cutoff = 0.90)

if (length(high_corr_cols) > 0) {
  # Get names of columns to remove
  corr_names <- colnames(cor_matrix)[high_corr_cols]
  cat(sprintf(">> Dropping %d Highly Collinear features (>0.90): %s\n", 
              length(corr_names), paste(corr_names, collapse = ", ")))
  
  # Remove them from the main dataframe
  data <- data %>% select(-all_of(corr_names))
} else {
  cat(">> No highly collinear features (>0.90) found.\n")
}

# [QA 5.4] Variance/Collinearity Check
cat("[QA 5.4] Verifying Variance/Collinearity Filtering... ")
cat(sprintf("PASS: Filtering complete. Current column count: %d\n", ncol(data)))

# SAVE INTERMEDIATE DATA
write.csv(
  data,
  file = here("DS-Project_Part2_Scripts", "Saved_Outputs", "cleaned_dataset.csv"),
  row.names = FALSE
)

cat("Data cleaning complete. Starting Cleaned Data EDA...\n")

# ==============================================================================
# SECTION 6: CLEANED DATA EXPLORATION
# ==============================================================================
# PURPOSE: Verification of Cleaning Steps

# --- Integration Setup ---
# Mapping 'data' to 'df_clean_processed'
df_clean_processed <- data
# Crucial: Module 4 visualization expects a "TARGET" column, but Section 5.3 
# renamed/removed "status". We map "target_class" to "TARGET" for visualization.
df_clean_processed$TARGET <- df_clean_processed$target_class
# -------------------------

# ------------------------------------------------------------------------------
# 6.1 Initialization & Data Verification
# ------------------------------------------------------------------------------
if(exists("df_clean_processed")) {
  df_clean <- df_clean_processed
}
cat("New Dimensions:", dim(df_clean), "\n")
dplyr::glimpse(df_clean)

# ------------------------------------------------------------------------------
# 6.2 Data Quality & Sanity Assurance
# ------------------------------------------------------------------------------
cat("\n[Section 6.2] Data Quality Checks\n")
print(summary(dplyr::select_if(df_clean, is.numeric)))

# Missingness Map (Should be blank)
print(naniar::vis_miss(df_clean, warn_large_data = FALSE) + 
        ggtitle("Missingness Map (Cleaned Data)"))

# ------------------------------------------------------------------------------
# 6.3 Variance Analysis & Target Visualization
# ------------------------------------------------------------------------------
nzv_metrics <- caret::nearZeroVar(df_clean, saveMetrics = TRUE)
zero_var_cols <- rownames(nzv_metrics[nzv_metrics$zeroVar == TRUE, ])
if(length(zero_var_cols) > 0) {
  df_clean <- df_clean[, !names(df_clean) %in% zero_var_cols]
}

if("TARGET" %in% names(df_clean)) {
  p_target <- ggplot(df_clean, aes(x = as.factor(TARGET))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Final Target Distribution", 
         subtitle = "0=Clean, 1=Minor Delay, 2=Severe Delinquency",
         x = "Target Group", y = "Count") +
    theme_minimal()
  print(p_target)
}

# ------------------------------------------------------------------------------
# 6.4 Numeric Univariate Distributions
# ------------------------------------------------------------------------------
num_cols <- names(dplyr::select_if(df_clean, is.numeric))
num_cols <- num_cols[num_cols != "ID"]
for(col in num_cols) {
  # Updated deprecated aes_string to .data[[]]
  p_hist <- ggplot(df_clean, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "coral", color = "white") +
    labs(title = paste("Distribution:", col)) + theme_minimal()
  
  # Updated deprecated aes_string to .data[[]]
  p_box <- ggplot(df_clean, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Outliers:", col)) + theme_minimal()
  
  grid.arrange(p_hist, p_box, ncol = 2)
}

# ------------------------------------------------------------------------------
# 6.5 Correlations
# ------------------------------------------------------------------------------
# Correlation Matrix
num_df <- dplyr::select_if(df_clean, is.numeric)
if(ncol(num_df) > 1) {
  cor_mat <- cor(num_df, use = "pairwise.complete.obs")
  cor_mat[is.na(cor_mat)] <- 0
  corrplot(cor_mat, method = "circle", type = "lower", title = "Correlation Matrix (Cleaned)", mar = c(0,0,2,0), tl.cex = 0.7)
}

# ==============================================================================
# 6.6 Correlations & Structural Missingness (Extended: Granular Risk Profiling)
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.6.1 Create Granular Risk Categories (Using EDA Snapshot)
# ------------------------------------------------------------------------------
# MENTOR NOTE: We define strict mappings for all 8 classes to ensure 
# the Neural Network weights can be tuned for specific minority classes (3, 4, 5).

df_diagnostics <- df_eda_categorical %>%
  mutate(
    # TARGET maps: C=0, X=1, 0=2, 1=3, 2=4, 3=5, 4=6, 5=7
    TARGET = target_class, 
    Risk_Label = case_when(
      TARGET == 0 ~ "Status_C_Paid",        # Good Standing
      TARGET == 1 ~ "Status_X_NoLoan",      # No History
      TARGET == 2 ~ "Status_0_1-29DPD",     # Past Due 1-29 Days
      TARGET == 3 ~ "Status_1_30-59DPD",    # Past Due 30-59 Days
      TARGET == 4 ~ "Status_2_60-89DPD",    # Past Due 60-89 Days
      TARGET == 5 ~ "Status_3_90-119DPD",   # Past Due 90-119 Days
      TARGET == 6 ~ "Status_4_120-149DPD",  # Past Due 120-149 Days
      TARGET == 7 ~ "Status_5_Over150DPD"   # Past Due >150 Days
    ),
    # Binary Flag: Is this *any* form of bad debt (Status 2, 3, 4, or 5)?
    Is_Serious_Delinquency = ifelse(TARGET >= 4, 1, 0)
  )

cat("--- Granular Risk Category Distribution ---\n")
print(prop.table(table(df_diagnostics$Risk_Label)) * 100)

# ------------------------------------------------------------------------------
# 6.6.2 Interaction Heatmap (Focus: Serious Delinquency > 60 Days)
# ------------------------------------------------------------------------------
# Visualizing where the "Serious" defaults (Status 2+) cluster
p_interaction <- df_diagnostics %>%
  group_by(NAME_FAMILY_STATUS, NAME_HOUSING_TYPE) %>%
  summarise(
    Risk_Rate = mean(Is_Serious_Delinquency, na.rm=TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  filter(Count > 30) %>% # Filter low-sample bins to avoid noise
  ggplot(aes(x = NAME_HOUSING_TYPE, y = NAME_FAMILY_STATUS, fill = Risk_Rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Risk_Rate * 100, 1)), color = "white", size = 3) +
  scale_fill_viridis_c(option = "inferno", name = "Serious Delinq %", labels = scales::percent) +
  labs(title = "Risk Heatmap: Serious Delinquency (Status 2, 3, 4, 5)",
       subtitle = "% of Applicants >60 Days Past Due",
       x = "Housing Type", y = "Family Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_interaction)

# ------------------------------------------------------------------------------
# 6.6.3 Risk Density Analysis (Violin Plots by Granular Status)
# ------------------------------------------------------------------------------
# We use the raw Status labels to see if Income differentiates severity
p_violin <- ggplot(df_diagnostics, aes(x = Risk_Label, y = AMT_INCOME_TOTAL, fill = Risk_Label)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  scale_y_log10(labels = scales::dollar) +
  coord_flip() + # Flip for readability of long labels
  labs(title = "Income Distribution by Granular Status", 
       subtitle = "Income variance across specific delinquency stages",
       x = "Status", y = "Income (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_violin)

# ------------------------------------------------------------------------------
# 6.6.4 Detailed Subgroup Analysis Table
# ------------------------------------------------------------------------------
if("Risk_Label" %in% names(df_diagnostics)) {
  
  # Function to calculate rates for the specific minority classes we want to weight
  calc_detailed_risk <- function(data, var_name) {
    data %>%
      group_by(!!sym(var_name)) %>%
      summarise(
        Count = n(),
        # We focus on the progression of delinquency
        Rate_Stat_0 = sprintf("%.1f%%", mean(Risk_Label == "Status_0_1-29DPD") * 100),
        Rate_Stat_1 = sprintf("%.1f%%", mean(Risk_Label == "Status_1_30-59DPD") * 100),
        Rate_Stat_2 = sprintf("%.1f%%", mean(Risk_Label == "Status_2_60-89DPD") * 100),
        Rate_Stat_5 = sprintf("%.1f%%", mean(Risk_Label == "Status_5_Over150DPD") * 100)
      ) %>%
      arrange(desc(Rate_Stat_5)) # Sort by worst offenders
  }
  
  # Bin continuous variables for analysis
  df_risk <- df_diagnostics %>%
    mutate(
      DAYS_EMPLOYED_BIN = ntile(DAYS_EMPLOYED, 5),
      INCOME_BIN = ntile(AMT_INCOME_TOTAL, 5)
    )
  
  risk_vars <- c("NAME_INCOME_TYPE", "NAME_FAMILY_STATUS", "OCCUPATION_TYPE")
  
  for(var in risk_vars) {
    if(var %in% names(df_risk)) {
      cat(paste("\n>>> Granular Risk Breakdown by:", var, "\n"))
      print(calc_detailed_risk(df_risk, var))
    }
  }
}

# ------------------------------------------------------------------------------
# 6.6.5 Multivariate Risk Profiling (Decision Tree)
# ------------------------------------------------------------------------------
cat("\n[Analysis] Generating Decision Tree Rules for Specific Status Classes...\n")

# Prepare data for Tree
tree_data <- df_diagnostics %>% 
  select(-TARGET, -Is_Serious_Delinquency, -ID, -status)

# MENTOR NOTE: 
# There are 8 classes. We set 'prior' to uniform (1/8 each) to force the tree 
# to pay attention to the rare classes (Status 3, 4, 5).
# If we used default priors, the tree would ignore Status 5 entirely due to class imbalance.
uniform_priors <- rep(1/8, 8) 

tree_model <- rpart(as.factor(Risk_Label) ~ ., 
                    data = tree_data, 
                    method = "class", 
                    parms = list(prior = uniform_priors), 
                    control = rpart.control(cp = 0.001, minbucket = 10))

# Extract Rules logic
class_levels <- levels(as.factor(df_diagnostics$Risk_Label))

get_tree_rules <- function(target_label) {
  col_idx <- which(class_levels == target_label)
  
  if(length(col_idx) > 0) {
    # Get probability of this specific class in the leaf nodes
    df_diagnostics$Node_Prob <- predict(tree_model, df_diagnostics)[, col_idx]
    df_diagnostics$Leaf_Node <- tree_model$where
    
    profiles <- df_diagnostics %>%
      group_by(Leaf_Node) %>%
      summarise(
        Count = n(),
        Target_Prob = mean(Risk_Label == target_label),
        # Identify the most common job in this risk node
        Dominant_Job = if("OCCUPATION_TYPE" %in% names(.)) names(sort(table(OCCUPATION_TYPE), decreasing=T))[1] else "N/A"
      ) %>%
      arrange(desc(Target_Prob)) %>%
      head(5)
    
    cat(sprintf("\n>>> Top Profiles for Group: %s\n", target_label))
    node_ids <- profiles$Leaf_Node
    
    if(length(node_ids) > 0) {
      rules_list <- path.rpart(tree_model, nodes = node_ids, pretty = 0, print.it = FALSE)
      for(i in 1:length(rules_list)) {
        rule <- rules_list[[i]][-1] 
        prob <- profiles$Target_Prob[i] * 100
        count <- profiles$Count[i]
        cat(sprintf("   Profile %d (Prob: %.1f%% | n=%d): %s\n", i, prob, count, paste(rule, collapse = " AND ")))
      }
    } else {
      cat("   No specific rules found for this class (Try adjusting CP/Priors).\n")
    }
  }
}

# Generate profiles for the critical delinquency classes
# These rules will help you adjust weights for the Neural Network
get_tree_rules("Status_0_1-29DPD")
get_tree_rules("Status_1_30-59DPD")
get_tree_rules("Status_2_60-89DPD")
get_tree_rules("Status_3_90-119DPD")
get_tree_rules("Status_4_120-149DPD")
get_tree_rules("Status_5_Over150DPD")

# Cleanup
rm(df_diagnostics, tree_model, p_interaction, p_violin, tree_data)

# ------------------------------------------------------------------------------
# 6.7 Final Artifact Export
# ------------------------------------------------------------------------------
dev.off()
cat(">> PDF Graphics Device Closed. Visualization file saved.\n")

# Set path using here()
data_output_dir <- here("DS-Project_Part2_Scripts", "Saved_Outputs")
if(!dir.exists(data_output_dir)) dir.create(data_output_dir, recursive = TRUE)

# Save the CSV
clean_data_path <- file.path(data_output_dir, "final_cleaned_dataset.csv")
write.csv(df_clean, file = clean_data_path, row.names = FALSE)

cat(">> SUCCESS: Final cleaned dataset saved to:", clean_data_path, "\n")

# ------------------------------------------------------------------------------
# SECTION 7: MODEL CONFIGURATION AND ARCHITECTURE
# ------------------------------------------------------------------------------

# 7.1 Data Preparation for Keras
# ------------------------------
# Assign cleaned data to 'df'
df <- data

# Define Status Levels (Mapped 0 to 7)
status_levels <- c("C", "X", "0", "1", "2", "3", "4", "5")
num_classes <- 8 

# Convert to R matrix
mat_data <- data.matrix(df)

# Scale inputs to [0, 1]
scale_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

X <- mat_data[, -ncol(mat_data)]
y_numeric <- mat_data[, ncol(mat_data)] # This contains levels 0 to 7

X_scaled <- apply(X, 2, scale_01)

# One-Hot Encoding for Target
y_onehot <- to_categorical(y_numeric, num_classes = num_classes)

# 7.2 Calculate Class Weights
# ---------------------------
# --- Calculate Class Weights ---
target_counts <- table(y_numeric)
class_weights <- list()

for (i in names(target_counts)) {
  val <- sqrt(max(target_counts) / target_counts[i])
  class_weights[[i]] <- as.numeric(val)
}

# 7.3 Evaluation Metrics Function
# -------------------------------
# --- Evaluation Function ---
evaluate_detailed <- function(y_true_numeric, y_pred_onehot) {
  y_pred_numeric <- apply(y_pred_onehot, 1, which.max) - 1
  
  pred_factor <- factor(status_levels[y_pred_numeric + 1], levels = status_levels)
  true_factor <- factor(status_levels[y_true_numeric + 1], levels = status_levels)
  
  cm <- table(Actual = true_factor, Predicted = pred_factor)
  stats <- data.frame(Class = status_levels, Precision = NA, Recall = NA, F1 = NA)
  
  for (i in 0:(num_classes - 1)) {
    tp <- sum(y_true_numeric == i & y_pred_numeric == i)
    fp <- sum(y_true_numeric != i & y_pred_numeric == i)
    fn <- sum(y_true_numeric == i & y_pred_numeric != i)
    
    idx <- i + 1 
    stats[idx, "Precision"] <- if (tp + fp > 0) tp / (tp + fp) else 0
    stats[idx, "Recall"] <- if (tp + fn > 0) tp / (tp + fn) else 0
    stats[idx, "F1"] <- if (stats[idx, "Precision"] + stats[idx, "Recall"] > 0) 
      2 * (stats[idx, "Precision"] * stats[idx, "Recall"]) / (stats[idx, "Precision"] + stats[idx, "Recall"]) else 0
  }
  return(list(ConfusionMatrix = cm, Stats = stats, MacroF1 = mean(stats$F1), BalancedAcc = mean(stats$Recall)))
}

# 7.4 Model Architecture Definition
# ---------------------------------
# --- Define Model Architecture (Functional API) ---
create_model <- function(input_dim) {
  # 1. Define the Input Node explicitly
  inputs <- layer_input(shape = c(input_dim))
  
  # 2. Flow the tensor through the layers
  outputs <- inputs %>%
    layer_dense(units = 1024, activation = "relu") %>% 
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.4) %>%
    
    layer_dense(units = 512, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.3) %>%
    
    layer_dense(units = 256, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = num_classes, activation = "softmax")
  
  # 3. Instantiate the Model
  model <- keras_model(inputs = inputs, outputs = outputs)
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.0001),
    metrics = c("accuracy")
  )
  return(model)
}

# ------------------------------------------------------------------------------
# SECTION 8: MODEL EXECUTION AND TRAINING
# ------------------------------------------------------------------------------

# 8.1 Cross-Validation Loop
# -------------------------
# --- 10-Fold Cross-Validation ---
cat("\nStarting 10-fold cross-validation...\n")
k <- 10
folds <- sample(rep(1:k, length.out = nrow(X))) # X is now unscaled
cv_accuracies <- numeric(k)

# Scaling Helper Function to prevent leakage
scale_col <- function(x, min_val, max_val) {
  if (max_val - min_val == 0) return(rep(0, length(x)))
  return((x - min_val) / (max_val - min_val))
}

for (i in 1:k) {
  cat(sprintf("Fold %d/%d...", i, k))
  v_idx <- which(folds == i)
  t_idx <- which(folds != i)
  
  # A. Split Raw Data
  X_train_raw <- X[t_idx, ]
  X_val_raw   <- X[v_idx, ]
  y_train_fold <- y_onehot[t_idx, ]
  y_val_fold   <- y_onehot[v_idx, ]
  
  # --- Apply Transformations on Split Data ---
  
  # 1. Skewness Correction
  # Iterate through columns to check skew on TRAIN data
  for(col_idx in 1:ncol(X_train_raw)) {
    # Check skewness on TRAIN only
    skew_val <- skewness(X_train_raw[, col_idx], na.rm = TRUE)
    
    if(!is.na(skew_val) && (skew_val > 1 || skew_val < -1)) {
      # Ensure non-negative before log (using min from Train)
      if(min(X_train_raw[, col_idx], na.rm=TRUE) >= 0) {
        # Transform Train
        X_train_raw[, col_idx] <- log1p(X_train_raw[, col_idx])
        # Transform Val (Blindly apply same transformation)
        X_val_raw[, col_idx] <- log1p(X_val_raw[, col_idx])
      }
    }
  }
  
  # 2. Outlier Clipping (Winsorization)
  # Calculate caps on TRAIN only
  for(col_idx in 1:ncol(X_train_raw)) {
    quantiles <- quantile(X_train_raw[, col_idx], probs = c(0.01, 0.99), na.rm = TRUE)
    min_cap <- quantiles[1]
    max_cap <- quantiles[2]
    
    # Clip Train
    X_train_raw[, col_idx] <- pmax(X_train_raw[, col_idx], min_cap)
    X_train_raw[, col_idx] <- pmin(X_train_raw[, col_idx], max_cap)
    
    # Clip Val (using Train caps)
    X_val_raw[, col_idx] <- pmax(X_val_raw[, col_idx], min_cap)
    X_val_raw[, col_idx] <- pmin(X_val_raw[, col_idx], max_cap)
  }
  
  # B. Calculate Scaling Params on TRAIN only
  # We iterate over columns to scale
  X_train_scaled <- X_train_raw
  X_val_scaled   <- X_val_raw
  
  for(col_idx in 1:ncol(X_train_raw)) {
    min_v <- min(X_train_raw[, col_idx], na.rm = TRUE)
    max_v <- max(X_train_raw[, col_idx], na.rm = TRUE)
    
    # Apply to Train
    X_train_scaled[, col_idx] <- scale_col(X_train_raw[, col_idx], min_v, max_v)
    # Apply to Val (using Train params)
    X_val_scaled[, col_idx]   <- scale_col(X_val_raw[, col_idx], min_v, max_v)
  }
  
  m_cv <- create_model(ncol(X_train_scaled))
  
  h_cv <- fit(
    object = m_cv,
    x = X_train_scaled, 
    y = y_train_fold,
    epochs = 50, 
    batch_size = 1024,
    validation_data = list(X_val_scaled, y_val_fold),
    class_weight = class_weights,
    verbose = 0
  )
  
  cv_accuracies[i] <- tail(h_cv$metrics$val_accuracy, 1)
  cat(sprintf(" Val Acc: %.4f\n", cv_accuracies[i]))
}

cat(sprintf("Mean CV Accuracy: %.4f\n", mean(cv_accuracies)))

# 8.2 Final Model Training
# ------------------------
# --- Final Training ---
cat("\nTraining final model (max 5000 epochs) with Early Stopping...\n")

# 1. Final Train/Validation Split
# We use a fresh split to ensure we have a validation set to monitor for early stopping
split_idx <- sample(1:nrow(X), 0.8 * nrow(X))
X_train_raw <- X[split_idx, ]
y_train <- y_onehot[split_idx, ]
X_val_raw <- X[-split_idx, ]
y_val <- y_onehot[-split_idx, ]

# --- Apply Transformations on Split Data (Final Model) ---

# 2. Skewness Correction
# Calculate skewness on TRAIN only, apply to both
for(col_idx in 1:ncol(X_train_raw)) {
  skew_val <- skewness(X_train_raw[, col_idx], na.rm = TRUE)
  
  if(!is.na(skew_val) && (skew_val > 1 || skew_val < -1)) {
    if(min(X_train_raw[, col_idx], na.rm=TRUE) >= 0) {
      X_train_raw[, col_idx] <- log1p(X_train_raw[, col_idx])
      X_val_raw[, col_idx] <- log1p(X_val_raw[, col_idx])
    }
  }
}

# 3. Outlier Clipping
# Calculate caps on TRAIN only, apply to both
for(col_idx in 1:ncol(X_train_raw)) {
  quantiles <- quantile(X_train_raw[, col_idx], probs = c(0.01, 0.99), na.rm = TRUE)
  min_cap <- quantiles[1]
  max_cap <- quantiles[2]
  
  X_train_raw[, col_idx] <- pmax(X_train_raw[, col_idx], min_cap)
  X_train_raw[, col_idx] <- pmin(X_train_raw[, col_idx], max_cap)
  
  X_val_raw[, col_idx] <- pmax(X_val_raw[, col_idx], min_cap)
  X_val_raw[, col_idx] <- pmin(X_val_raw[, col_idx], max_cap)
}

# 4. Robust Scaling (Train-to-Val)
X_train_final <- X_train_raw
X_val_final   <- X_val_raw

for(col_idx in 1:ncol(X_train_raw)) {
  min_v <- min(X_train_raw[, col_idx], na.rm = TRUE)
  max_v <- max(X_train_raw[, col_idx], na.rm = TRUE)
  
  X_train_final[, col_idx] <- scale_col(X_train_raw[, col_idx], min_v, max_v)
  X_val_final[, col_idx]   <- scale_col(X_val_raw[, col_idx], min_v, max_v)
}

# 5. Define Model and Early Stopping
final_model <- create_model(ncol(X_train_final))

# Define Early Stopping Callback
# - monitor: Watch "val_loss"
# - patience: Stop if no improvement for 20 epochs
# - restore_best_weights: Revert model to the epoch with the lowest val_loss (CRITICAL)
early_stop <- callback_early_stopping(
  monitor = "val_loss", 
  patience = 30, 
  restore_best_weights = TRUE,
  verbose = 1
)

# 6. Fit Model
final_history <- fit(
  object = final_model,
  x = X_train_final, 
  y = y_train,
  epochs = 5000,           # High ceiling, but early_stop will cut it short
  batch_size = 1024,
  validation_data = list(X_val_final, y_val),
  class_weight = class_weights,
  callbacks = list(early_stop), # Added callback here
  verbose = 1
)

cat(sprintf("\nTraining stopped at epoch %d due to early stopping.\n", length(final_history$metrics$loss)))

# ------------------------------------------------------------------------------
# SECTION 9: EVALUATION AND REPORTING
# ------------------------------------------------------------------------------

# 9.1 Detailed Predictions and Metrics
# ------------------------------------
# --- Detailed Evaluation ---
cat("\n--- Final Model Detailed Metrics ---\n")
y_pred_val <- final_model %>% predict(X_val_final)
y_true_val <- apply(y_val, 1, which.max) - 1

results <- evaluate_detailed(y_true_val, y_pred_val)

cat("\nConfusion Matrix:\n")
print(results$ConfusionMatrix)

cat("\nClass-Specific Metrics (Precision, Recall, F1):\n")
print(results$Stats)

cat(sprintf("\nMacro-Average F1 Score: %.4f\n", results$MacroF1))
cat(sprintf("Balanced Accuracy: %.4f\n", results$BalancedAcc))

train_loss <- tail(final_history$metrics$loss, 1)
val_loss <- tail(final_history$metrics$val_loss, 1)
cat(sprintf("\nFinal Training Loss: %.4f\n", train_loss))
cat(sprintf("Final Validation Loss: %.4f\n", val_loss))

# 9.2 Requirements Verification
# -----------------------------
# --- Performance Requirements Verification Table ---
cat("\n--- Performance Requirements Verification ---\n")

# 1. Retrieve/Calculate Actual Metrics
actual_val_loss <- tail(final_history$metrics$val_loss, 1)
actual_train_loss <- tail(final_history$metrics$loss, 1)
loss_gap <- actual_val_loss - actual_train_loss

# Calculate Average Recall for Minority Classes (2, 3, 4, 5)
minority_classes <- c("2", "3", "4", "5")
minority_rows <- results$Stats[results$Stats$Class %in% minority_classes, ]
avg_minority_recall <- mean(minority_rows$Recall, na.rm = TRUE)

# Calculate Standard Accuracy to compare against Balanced Accuracy
total_samples <- sum(results$ConfusionMatrix)
correct_preds <- sum(diag(results$ConfusionMatrix))
standard_acc <- correct_preds / total_samples
bias_gap <- standard_acc - results$BalancedAcc

# 2. Define Logic for Pass/Fail Status
# Loss Status
if (actual_val_loss < 0.4) {
  loss_status <- "EXCELLENT (< 0.4)"
} else if (actual_val_loss < 0.84) {
  loss_status <- "ACCEPTABLE (< 0.84)"
} else {
  loss_status <- "FAIL (> Entropy Baseline)"
}

# Overfitting Status
overfit_status <- ifelse(loss_gap > 0.1, "FAIL (High Overfitting)", "PASS (Tracking Closely)")

# Minority Recall Status
if (avg_minority_recall > 0.5) {
  recall_status <- "PASS (> 50%)"
} else if (avg_minority_recall > 0.1) {
  recall_status <- "WARNING (Low Detection)"
} else {
  recall_status <- "FAIL (Ignoring Risk)"
}

# Macro F1 Status
if (results$MacroF1 > 0.6) {
  f1_status <- "EXCELLENT (> 0.6)"
} else if (results$MacroF1 > 0.3) {
  f1_status <- "ACCEPTABLE (> 0.3)"
} else {
  f1_status <- "FAIL (Model is Biased)"
}

# Balanced Accuracy Status
bias_status <- ifelse(bias_gap > 0.2, "FAIL (High Bias)", "PASS (Consistent)")

# 3. Generate Comparison Table
benchmark_table <- data.frame(
  Metric_Category = c(
    "Loss (Cross-Entropy)",
    "Overfitting Check", 
    "Minority Recall (Classes 2-5)", 
    "Macro-F1 Score", 
    "Bias Check (Std Acc - Bal Acc)"
  ),
  Target_Benchmark = c(
    "< 0.84 (Baseline)", 
    "Train approx. Val", 
    "> 0.50", 
    "> 0.30", 
    "Small Gap"
  ),
  Actual_Value = c(
    sprintf("%.4f", actual_val_loss),
    sprintf("Gap: %.4f", loss_gap),
    sprintf("%.4f", avg_minority_recall),
    sprintf("%.4f", results$MacroF1),
    sprintf("Gap: %.4f", bias_gap)
  ),
  Status = c(
    loss_status,
    overfit_status,
    recall_status,
    f1_status,
    bias_status
  )
)

# Print the formatted table
print(benchmark_table, row.names = FALSE, right = FALSE)

# 9.3 Saving the Model
# --------------------
# --- Save Model ---
# Define the path with .keras extension to ensure optimized single-file saving
# Add the filename and extension
save_model_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "final_model.keras")

save_model(final_model, save_model_path, overwrite = TRUE)