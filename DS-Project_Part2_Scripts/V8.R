# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Perfectly cleaned script with variable factor X removed and all regularization
# and normalization handled by my keras neural network.Includes categorical encoding,
# Validation of "in-model" normalization, and target variable inspection.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ------------------------------------------------------------------------------
# SECTION 1: ENVIRONMENT SETUP
# ------------------------------------------------------------------------------
# [NO CHANGES - PRESERVED]

# ==============================================================================
# 1.1: Working Directory and Libraries
# ==============================================================================
# Set personal working directory (Adjust as needed)
# setwd("~/Documents/Repos/GRP-6_DS-Project")
# renv::activate()

# 1.1.1: Automated Dependency Management
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
              "here",         # Path management
              "recipes",      # Data Preprocessing
              "MLmetrics"     # Metrics
)

new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if (length(new_pkgs)) install.packages(new_pkgs)

# 1.1.2: Load necessary libraries for Data Cleaning and EDA
library(tidyverse)
library(caret)
library(corrplot)
library(naniar)
library(gridExtra)
library(e1071)
library(Hmisc)
library(tensorflow)
library(reticulate)
library(rpart)
library(fastDummies)
library(keras3)
library(here)
library(recipes)
library(MLmetrics)

# 1.1.3: Verify Project Root
cat(">> Project Root Detected at:", here(), "\n")
cat(">> Libraries loaded. Environment ready.\n")

# ==============================================================================
# 1.2: Backend Connection Strategy (TensorFlow)
# ==============================================================================
# 1.2.1: Explicit Environment Binding: Attempts to locate 'r-reticulate' Conda environment.
backend_configured <- FALSE

# 1.2.2: Check - Is a Conda environment named 'r-reticulate' available?
if (tryCatch("r-reticulate" %in% reticulate::conda_list()$name,
             error = function(e) FALSE)) {
  
  try({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    backend_configured <- TRUE
    cat(">> CONNECTION SUCCESS: Activated 'r-reticulate' Conda environment.\n")
  }, silent = TRUE)
}

# 1.2.3: Check - Fallback to any valid Python with TF available
if (!backend_configured) {
  if (reticulate::py_module_available("tensorflow")) {
    backend_configured <- TRUE
    cat(">> CONNECTION SUCCESS: Found TensorFlow in the current active Python environment.\n")
  }
}

# ==============================================================================
# 1.3: Seeding and Reproducibility
# ==============================================================================

# 1.3.1: Ensure TensorFlow is available for seeding
library(tensorflow)

# REPRODUCIBILITY GUARANTEE:
# 1.3.2: Seed R (Frontend)
set.seed(123)

# 1.3.3: Seed TensorFlow/Python (Backend)
if (backend_configured && reticulate::py_module_available("tensorflow")) {
  tryCatch({
    tf$random$set_seed(123L)
    cat(">> SETUP SUCCESS: TensorFlow backend seeded (Reproducibility Guaranteed).\n")
  }, error = function(e) {
    cat(">> WARNING: TensorFlow found but seeding failed. Error:", e$message, "\n")
  })
} else {
  cat("\n>> NOTICE: TensorFlow Python Backend not explicitly detected.\n") 
}

# ------------------------------------------------------------------------------
# SECTION 2: DATA INGESTION AND PRELIMINARY EDA
# ------------------------------------------------------------------------------
# [NO CHANGES - PRESERVED]

# ==============================================================================
# 2.1: Load Raw Data
# ==============================================================================
# 2.1.1: Load the dataset using relative paths via 'here'
cat("--- Loading and Examining Data ---\n")

path <- here("DS-Project_data", "Dataset-part-2.csv")

# Optional: Safety check
if (!file.exists(path)) stop("CRITICAL ERROR: File not found at: ", path)

data <- read.csv(path)
cat(sprintf(">> RAW DATA IMPORTED: %d rows and %d columns.\n", nrow(data), ncol(data)))

# ==============================================================================
# 2.2: Initial Exploratory Data Analysis
# ==============================================================================
df <- data 

# ==============================================================================
# 2.3: Setup PDF Output
# ==============================================================================
plot_output_dir <- here("DS-Project_Part2_Scripts", "Saved_Outputs")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

pdf(file = file.path(plot_output_dir, "generated_plots_and_visualizations.pdf"),
    width = 11, height = 8.5)
cat(">> PDF Graphics Device Opened. Plots will be saved to:",
    plot_output_dir, "\n")

# ==============================================================================
# 2.4: Structure & Content Inspection
# ==============================================================================
cat("\n[Phase 1] Structure & Content Inspection\n")
print(dim(df))
dplyr::glimpse(df)

# ==============================================================================
# 2.5: Data Quality Checks
# ==============================================================================
cat("\n[Phase 2] Data Quality Checks\n")

# 2.5.1: Statistical Summary
print(summary(dplyr::select(df, where(is.numeric))))

# 2.5.2: Missing Values (Focus on OCCUPATION_TYPE)
cat("\nMissing Values Count:\n")
miss_counts <- colSums(is.na(df))
print(miss_counts[miss_counts > 0])

# 2.5.3: Duplicate Rows
dup_count <- sum(duplicated(df))
cat("\nDuplicate Rows Detected:", dup_count, "\n")

# 2.5.4: Remove Duplicates
if (dup_count > 0) {
  df <- df %>% distinct()
  cat(">> ACTION: Removed", dup_count, "duplicate rows from analysis dataframe.\n")
  # Also clean the main data object to be safe
  data <- data %>% distinct()
  cat(">> NEW ROW COUNT after deduplication:", nrow(data), "\n")
}

# 2.5.5: Variance Check
if ("FLAG_MOBIL" %in% names(df)) {
  cat("\nVariance Check for FLAG_MOBIL:\n")
  print(table(df$FLAG_MOBIL))
}

# ==============================================================================
# 2.6: Univariate Visualization
# ==============================================================================
# 2.6.1: Target Variable Distribution

if ("status" %in% names(df)) {
  p_target <- ggplot(df, aes(x = as.factor(status))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Target Distribution (Class Imbalance Check)",
         x = "Status Code", y = "Count") +
    theme_minimal()
  print(p_target)
  
  # 2.6.2: Status Variable Breakdown (Imbalance Confirmation)
  cat("\n[Analysis Verification] Detailed Status Variable Breakdown:\n")
  status_counts <- table(df$status)
  print(status_counts)
  cat("\nRelative Frequencies (%):\n")
  print(round(prop.table(status_counts) * 100, 2))
}

# 2.6.3: Numeric Distributions (Income Skew)
num_cols <- names(dplyr::select(df, where(is.numeric)))
for (col in num_cols) {
  p_hist <- ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "coral", color = "white") +
    labs(title = paste("Distribution:", col)) +
    theme_minimal()
  p_box <- ggplot(df, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Outliers:", col)) +
    theme_minimal()
  grid.arrange(p_hist, p_box, ncol = 2)
}

# 2.6.4: Discrete Variable Analysis
if ("CNT_CHILDREN" %in% names(df)) {
  cat("\n[Analysis Verification] Discrete Count Distribution: CNT_CHILDREN\n")
  print(table(df$CNT_CHILDREN))
}

# 2.6.5: Skewness Verification (Income)
if ("AMT_INCOME_TOTAL" %in% names(df)) {
  cat("\n[Analysis Verification] Income Distribution Statistics:\n")
  skew_val <- e1071::skewness(df$AMT_INCOME_TOTAL, na.rm = TRUE)
  cat(paste("Skewness Coefficient:", round(skew_val, 4), "\n"))
  cat("Quantiles (0% to 100%):\n")
  print(quantile(df$AMT_INCOME_TOTAL,
                 probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.99, 1)))
}

# ==============================================================================
# 2.7: The 365243 Pensioner Anomaly
# ==============================================================================

# CRITICAL FINDING: 'DAYS_EMPLOYED' contains 365243 (~1000 years), indicating "Pensioner".
cat("\n Handling Anomalies and Engineering Features\n")

if ("DAYS_EMPLOYED" %in% names(df) && "NAME_INCOME_TYPE" %in% names(df)) {
  anomaly_check <- df %>%
    mutate(is_magic_val = ifelse(DAYS_EMPLOYED == 365243, 1, 0)) %>%
    group_by(NAME_INCOME_TYPE) %>%
    summarise(
      Total_Count = n(),
      Magic_Val_Count = sum(is_magic_val),
      Percentage = (sum(is_magic_val) / n()) * 100)
  print("365243 Anomaly Overlap Analysis:")
  print(anomaly_check)
}

# ==============================================================================
# 2.8: MNAR (Missing Not At Random) Analysis
# ==============================================================================

# Logic Check: Do 100% of Pensioners have missing occupation?
if ("OCCUPATION_TYPE" %in% names(df) &&
    "NAME_INCOME_TYPE" %in% names(df)) {
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
  
  p_mnar <- df %>%
    mutate(Has_Occupation = ifelse(is.na(OCCUPATION_TYPE) |
                                     OCCUPATION_TYPE == "", "Missing", "Present")) %>%
    ggplot(aes(x = NAME_INCOME_TYPE, fill = Has_Occupation)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Evidence of MNAR: Occupation Missingness by Income Type",
         subtitle = "Pensioners have nearly 100% missing occupation data",
         x = "Income Type", y = "Proportion", fill = "Occupation Status") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_mnar)
}

# ------------------------------------------------------------------------------
# SECTION 3: DATA TYPE DEFINITION AND LOGICAL CORRECTIONS
# ------------------------------------------------------------------------------
# [NO CHANGES - PRESERVED]

# ==============================================================================
# 3.1: Define Data Types and Reload
# ==============================================================================
# 3.1.1: Define Data Types
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

# 3.1.2: Reload the CSV with strict types
# Note: 'path' here now refers to the dynamic path created with here() in Section 2.1)
data <- read.csv(path, colClasses = col_types, na.strings = c("NA", ""))

# 3.1.3: [QA - Strict Reload Check]
cat("[QA - Strict Reload Check] Verifying Strict Types... ")
if (is.numeric(data$AMT_INCOME_TOTAL) && is.factor(data$NAME_INCOME_TYPE)) {
  cat("PASS: Data reloaded with strict types correctly.\n")
} else {
  cat("FAIL: Types mismatch after reload.\n")
}

# ==============================================================================
# 3.2: Initial Formatting
# ==============================================================================
# 3.2.1: Performs Binary Encoding (converting categorical "Yes/No" values into numeric 1/0).
data$FLAG_OWN_CAR <- as.integer(data$FLAG_OWN_CAR == "Y")
data$FLAG_OWN_REALTY <- as.integer(data$FLAG_OWN_REALTY == "Y")
flags_numeric <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")

# 3.2.2: Post-Load Logic (Conversions to Integer)
data[flags_numeric] <- lapply(data[flags_numeric], as.integer)
data <- data %>% select(-FLAG_MOBIL)

# 3.2.3: [QA - Formatting Check]
cat("[QA - Formatting Check] Verifying Boolean Conversions... ")
if (is.integer(data$FLAG_OWN_CAR) && !"FLAG_MOBIL" %in% names(data)) {
  cat("PASS: Booleans converted and zero-variance column removed.\n")
} else {
  cat("FAIL: Boolean conversion failed.\n")
}

cat(">> BINARY CONVERSION COMPLETE. Preview of encoded flags:\n")
print(head(data %>% select(FLAG_OWN_CAR, FLAG_OWN_REALTY, all_of(setdiff(flags_numeric, "FLAG_MOBIL"))), 3))

# ==============================================================================
# 3.3: Age and Employment Logic
# ==============================================================================
cat("--- Correcting Age and Employment Data ---\n")

# 3.3.1: Correct DAYS_BIRTH to positive years
data$AGE <- abs(data$DAYS_BIRTH) / 365.25

# 3.3.2: Pensioner and Employment corrections
# Fix pensioners who are actually working
data <- data %>%
  mutate(
    NAME_INCOME_TYPE = ifelse(
      NAME_INCOME_TYPE == "Pensioner" & DAYS_EMPLOYED != 365243,
      "Working",
      NAME_INCOME_TYPE
    )
  )

# 3.3.3: Reclassify NA values in OCCUPATION_TYPE
data <- data %>%
  mutate(
    OCCUPATION_TYPE = as.character(OCCUPATION_TYPE),
    OCCUPATION_TYPE = case_when(
      DAYS_EMPLOYED == 365243 ~ "Retired",
      is.na(OCCUPATION_TYPE) ~ "Unknown",
      TRUE ~ OCCUPATION_TYPE
    ),
    OCCUPATION_TYPE = as.factor(OCCUPATION_TYPE)
  )

# 3.3.4: Set DAYS_EMPLOYED to 0-equivalent for pensioned people (sentinel value replacement)
data$DAYS_EMPLOYED[data$DAYS_EMPLOYED == 365243] <- 21915

# 3.3.5: Calculate ACTIVE_EMPLOYMENT_YEARS
data$ACTIVE_EMPLOYMENT_YEARS <- abs(data$DAYS_EMPLOYED) / 365.25
data$ACTIVE_EMPLOYMENT_YEARS_LOG <- log1p(data$ACTIVE_EMPLOYMENT_YEARS)

# 3.3.6: [QA - Logic Corrections Check]
cat("[QA - Logic Corrections Check] Verifying Age/Employment Logic... ")
neg_age         <- any(data$AGE < 0)
sentinel_remain <- any(data$DAYS_EMPLOYED == 365243)
cat(sprintf("Negative Age Found: %s. Sentinel Remains: %s.\n",
            neg_age, sentinel_remain))
if (!neg_age && !sentinel_remain) {
  cat("PASS: Age positive and sentinels replaced.\n")
} else {
  cat("FAIL: Logic corrections incomplete.\n")
}

cat(">> AGE & EMPLOYMENT RE-CALCULATED. Stats:\n")
print(summary(data$AGE))
cat(">> Occupation Types after imputation:\n")
print(table(data$OCCUPATION_TYPE))

# ==============================================================================
# 3.4: Family Size and Outliers
# ==============================================================================
# 3.4.1: Family Size Outliers
cat("--- Handling Family and Income Outliers ---\n")
initial_rows <- nrow(data)
data <- data[data$CNT_FAM_MEMBERS <= 10, ]
cat(">> Filtered Families > 10 members. Rows removed:", initial_rows - nrow(data), "\n")

# 3.4.2: Correct Family Member counts (Logic: Family Members >= Children + Adults)
anomaly <- data$CNT_FAM_MEMBERS < data$CNT_CHILDREN
data$CNT_FAM_MEMBERS[anomaly &
                       data$NAME_FAMILY_STATUS %in%
                       c("Married", "Civil marriage")] <-
  data$CNT_CHILDREN[anomaly &
                      data$NAME_FAMILY_STATUS %in%
                      c("Married", "Civil marriage")] + 2
data$CNT_FAM_MEMBERS[anomaly &
                       data$NAME_FAMILY_STATUS %in%
                       c("Separated", "Widow", "Single / not married")] <-
  data$CNT_CHILDREN[anomaly &
                      data$NAME_FAMILY_STATUS %in%
                      c("Separated", "Widow", "Single / not married")] + 1

# 3.4.3: [QA - Family Size Check]
cat("[QA - Family Size Check] Verifying Family Logic... ")
if (any(data$CNT_FAM_MEMBERS > 10) ||
    any(data$CNT_FAM_MEMBERS < data$CNT_CHILDREN)) {
  cat("FAIL: Family anomalies still present.\n")
} else {
  cat("PASS: Family size anomalies resolved.\n")
}

# ==============================================================================
# 3.5: Income Outliers
# ==============================================================================
# 3.5.1: Income Outliers
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries",
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")

# 3.5.2: Remove unlikely high incomes for specific jobs
data <- data %>%
  filter(!(AMT_INCOME_TOTAL > 1000000 &
             OCCUPATION_TYPE %in% impossible_jobs))

# 3.5.3: Log transform Income
data$AMT_INCOME_TOTAL_LOG <- log1p(data$AMT_INCOME_TOTAL)

# 3.5.4: [QA - Income Logic Check]
cat("[QA 3.5] Verifying Income Outliers... ")
bad_income_rows <- nrow(data %>% filter(AMT_INCOME_TOTAL > 1000000 &
                                          OCCUPATION_TYPE %in% impossible_jobs))
if (bad_income_rows == 0) {
  cat("PASS: Impossible high incomes removed.\n")
} else {
  cat("FAIL: High income outliers remain.\n")
}

cat(">> LOG TRANSFORMATION APPLIED. Income Log Summary:\n")
print(summary(data$AMT_INCOME_TOTAL_LOG))

# ------------------------------------------------------------------------------
# SECTION 4: FEATURE ENGINEERING
# ------------------------------------------------------------------------------
# [NO CHANGES - PRESERVED]

# ==============================================================================
# 4.1: Creating Ratio Features
# ==============================================================================
cat("--- Feature Engineering ---\n")

# 4.1.1: Creating new engineered features:
data$INCOME_PER_FAMILY_MEMBER <- data$AMT_INCOME_TOTAL / data$CNT_FAM_MEMBERS
data$INCOME_PER_FAMILY_MEMBER_LOG <- log1p(data$INCOME_PER_FAMILY_MEMBER)
data$EMPLOYMENT_RATIO <- data$ACTIVE_EMPLOYMENT_YEARS / data$AGE
data$CREDIT_MATURITY <- data$AGE - data$ACTIVE_EMPLOYMENT_YEARS
data$INCOME_PER_AGE <- data$AMT_INCOME_TOTAL / data$AGE
data$INCOME_PER_AGE_LOG <- log1p(data$INCOME_PER_AGE)

# 4.1.2: [QA 4.1 - Feature Creation Check]
cat("[QA 4.1 - Feature Creation Check] Verifying New Features... ")
new_feats <- c("INCOME_PER_FAMILY_MEMBER", "EMPLOYMENT_RATIO",
               "CREDIT_MATURITY", "INCOME_PER_AGE")
if (all(new_feats %in% names(data))) {
  cat("PASS: All engineered features present.\n")
} else {
  cat("FAIL: Missing engineered features.\n")
}

cat(">> NEW RATIO FEATURES CREATED. Preview:\n")
print(head(data[, new_feats], 3))

# ------------------------------------------------------------------------------
# SECTION 5: FINAL PREPROCESSING PIPELINE (REVISED)
# ------------------------------------------------------------------------------

# REVISION NOTE:
# This section has been completely rewritten to prevent Data Leakage.
# Previous version calculated global statistics (mean, variance) on the whole dataset.
# This version uses a 'Split-then-Recipe' architecture:
# 1. Split Data (Train/Val/Test) by ID.
# 2. Define Recipe (Blueprint for processing).
# 3. Prep Recipe (Calculate stats on Train ONLY).
# 4. Bake (Apply stats to Train/Val/Test).

# ==============================================================================
# 5.1: TARGET VARIABLE CLEANING & MAPPING
# ==============================================================================
# 5.1.1: We retain 'target_class' for the Neural Network.
# We drop status code "X" (No Loan Data) as it is noise.
data <- data %>% filter(status != "X")

# 5.1.2: Map status levels to class IDs 0..6
data$target_class <- recode(as.character(data$status),
                            "C" = 0, "0" = 1, "1" = 2, "2" = 3, 
                            "3" = 4, "4" = 5, "5" = 6
) |> as.numeric()

# 5.1.3: QA Check
if (max(data$target_class, na.rm=T) == 6) {
  cat("PASS: Target mapped to 0-6 range.\n")
}

# ==============================================================================
# 5.2: CREATE EDA SNAPSHOT (Crucial for Section 6)
# ==============================================================================
# Reason: We create the snapshot NOW, before splitting or destroying factors.
# This object is strictly for Section 6 visualization and is NOT used for training.
cat(">> Creating 'eda_snapshot' for human-readable risk profiling...\n")
eda_snapshot <- data
# Ensure 'TARGET' exists for Section 6 compatibility
eda_snapshot$TARGET <- eda_snapshot$target_class 

# ==============================================================================
# 5.3: ID-BASED DATA SPLITTING
# ==============================================================================
# 5.3.1: Drop Raw Columns (Cleanup)
cols_to_drop <- c("DAYS_BIRTH", "DAYS_EMPLOYED", "AMT_INCOME_TOTAL", "status")
df_modeling <- data %>% select(-any_of(cols_to_drop))

cat("--- Performing ID-Based Split (Preventing Data Leakage) ---\n")
all_ids <- unique(df_modeling$ID)

set.seed(123)
# 70% Train IDs
train_ids <- sample(all_ids, size = 0.70 * length(all_ids))

# Remaining 30% IDs
remaining_ids <- setdiff(all_ids, train_ids)

# 15% Val, 15% Test
val_ids <- sample(remaining_ids, size = 0.50 * length(remaining_ids))
test_ids <- setdiff(remaining_ids, val_ids)

# Filter Dataframes by ID
train_raw <- df_modeling %>% filter(ID %in% train_ids)
val_raw   <- df_modeling %>% filter(ID %in% val_ids)
test_raw  <- df_modeling %>% filter(ID %in% test_ids)

cat(sprintf(">> Split Complete: Train=%d, Val=%d, Test=%d (Rows)\n", 
            nrow(train_raw), nrow(val_raw), nrow(test_raw)))

# ==============================================================================
# 5.4: DEFINE PREPROCESSING RECIPE
# ==============================================================================
cat("--- Building Preprocessing Recipe (Stats calculated on Train ONLY) ---\n")
# Define the blueprint
rec_obj <- recipe(target_class ~ ., data = train_raw) %>%
  # A. Define ID as an identifier (not a predictor)
  update_role(ID, new_role = "id") %>%
  
  # B. Impute Categorical Modes (if any missing remain)
  step_impute_mode(all_nominal_predictors()) %>%
  
  # C. Lump Rare Levels (Replaces global fct_lump_min)
  # Calculates threshold based on Train counts only
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  
  # D. Zero Variance Filter (Replaces global nearZeroVar)
  step_nzv(all_predictors()) %>%
  
  # E. Correlation Filter (Replaces global findCorrelation)
  step_corr(all_numeric_predictors(), threshold = 0.90) %>%
  
  # F. Min-Max Scaling (Replaces global min_max_norm)
  step_range(all_numeric_predictors(), min = 0, max = 1) %>%
  
  # G. One-Hot Encoding (Replaces fastDummies)
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# ==============================================================================
# 5.5: TRAIN RECIPE (PREP) & APPLY (BAKE)
# ==============================================================================
cat(">> Prepping Recipe (Learning stats from Training Set)...\n")
# This step calculates the means, maxes, and factor levels from Train
rec_trained <- prep(rec_obj, training = train_raw)

cat(">> Baking Data (Applying learned stats to all sets)...\n")
# Apply the exact same transformation to all sets
train_baked <- bake(rec_trained, new_data = train_raw)
val_baked   <- bake(rec_trained, new_data = val_raw)
test_baked  <- bake(rec_trained, new_data = test_raw)

# Convert to Matrices for Keras (Dropping ID and Target)
x_train <- train_baked %>% select(-ID, -target_class) %>% data.matrix()
y_train <- train_baked$target_class %>% as.numeric()

x_val   <- val_baked %>% select(-ID, -target_class) %>% data.matrix()
y_val   <- val_baked$target_class %>% as.numeric()

x_test  <- test_baked %>% select(-ID, -target_class) %>% data.matrix()
y_test  <- test_baked$target_class %>% as.numeric()

cat(sprintf(">> PROCESSING COMPLETE. Final Input Shape: %d Features.\n", ncol(x_train)))

# ------------------------------------------------------------------------------
# SECTION 6: CLEANED DATA EXPLORATION & RISK PROFILING
# ------------------------------------------------------------------------------
# [NO CHANGES - PRESERVED]
# Note: This section uses 'eda_snapshot' which we correctly created in Section 5.2

# 6.1: Initialize Exploration Data
if (exists("eda_snapshot")) {
  df_clean <- eda_snapshot
  cat(">> Loaded 'eda_snapshot' (Factors preserved for Profiling).\n")
} else {
  warning("Snapshot not found. Using processed data (Risk Profiling may fail).")
  df_clean <- data
}

# Ensure TARGET exists (created in 5.2)
cat("\n==============================================================================\n")
cat(" STARTING SECTION 6: EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("==============================================================================\n")
cat("New Dimensions for EDA:", dim(df_clean)[1], "Rows x", dim(df_clean)[2], "Cols\n")

# ==============================================================================
# 6.2: Visualizing Missingness & Distributions
# ==============================================================================
cat("\n[Section 6.2] Generating Data Quality Visualizations...\n")

if (dev.cur() == 1) {
  # Safety check: If dev.cur() is 1, it means no file is open (something went wrong).
  warning(">> WARNING: No active PDF device found. Plots may print to screen instead of file.")
} else {
  cat(">> Appending Section 6 plots to the active PDF file...\n")
}

# 6.2.2: Missingness Map
if(require(naniar)) {
  # Vis_miss creates a heatmap of missing values
  p_miss <- naniar::vis_miss(df_clean, warn_large_data = FALSE) + 
    ggtitle("Missingness Map (Cleaned Data)")
  print(p_miss)
}

# 6.2.3: Target Distribution Bar Chart
if ("TARGET" %in% names(df_clean)) {
  p_target <- ggplot(df_clean, aes(x = as.factor(TARGET))) +
    geom_bar(fill = "steelblue") +
    labs(
      title = "Final Target Distribution",
      subtitle = "0-1: Safe/Neutral | 2-3: Early Delinq | 4-6: Serious Default (90+ DPD)",
      x = "Target Group (0-6)", y = "Count"
    ) +
    theme_minimal()
  print(p_target)
}

# ==============================================================================
# 6.3: Numeric Distributions & Correlations
# ==============================================================================
cat("\n[Section 6.3] Analyzing Numeric Variables...\n")

# 6.3.1: Histograms & Boxplots for all numeric columns (excluding target)
num_cols <- names(select(df_clean, where(is.numeric)))
num_cols <- setdiff(num_cols, c("TARGET", "target_class"))

if(require(gridExtra)) {
  for (col in num_cols) {
    p_hist <- ggplot(df_clean, aes(x = .data[[col]])) +
      geom_histogram(bins = 30, fill = "coral", color = "white") +
      labs(title = paste("Distribution:", col)) + theme_minimal()
    
    p_box <- ggplot(df_clean, aes(y = .data[[col]])) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Outliers:", col)) + theme_minimal()
    
    grid.arrange(p_hist, p_box, ncol = 2)
  }
}

# 6.3.2: Correlation Matrix
if(require(corrplot)) {
  num_df <- select(df_clean, where(is.numeric))
  if (ncol(num_df) > 1) {
    cor_mat <- cor(num_df, use = "pairwise.complete.obs")
    cor_mat[is.na(cor_mat)] <- 0
    corrplot(
      cor_mat, method = "circle", type = "lower",
      title = "Correlation Matrix",
      mar = c(0, 0, 2, 0), tl.cex = 0.7
    )
  }
}

# ==============================================================================
# 6.4: Granular Risk Profiling
# ==============================================================================
cat("\n[Section 6.4] Generating Granular Risk Profiles...\n")

# 6.4.1: Create Human-Readable Risk Labels
df_diagnostics <- df_clean %>%
  mutate(
    Risk_Label = case_when(
      TARGET == 0 ~ "Status_C_Paid",
      TARGET == 1 ~ "Status_0_1-29DPD",
      TARGET == 2 ~ "Status_1_30-59DPD",
      TARGET == 3 ~ "Status_2_60-89DPD",
      TARGET == 4 ~ "Status_3_90-119DPD",
      TARGET == 5 ~ "Status_4_120-149DPD",
      TARGET >= 6 ~ "Status_5_Over150DPD",
      TRUE ~ "Unknown"
    ),
    # Serious Delinquency defined here as 90+ Days Past Due (Target 4, 5, 6)
    Is_Serious_Delinquency = ifelse(TARGET >= 4, 1, 0)
  )

# 6.4.2: Print Distribution of Risk Labels
cat("--- Risk Category Distribution ---\n")
print(prop.table(table(df_diagnostics$Risk_Label)) * 100)

# ==============================================================================
# 6.5: Factor Variable Risk Analysis (The 7 Factors)
# ==============================================================================
# 6.5.1: Function to calculate risk metrics
calc_detailed_risk <- function(data, var_name) {
  if(!var_name %in% names(data)) return(NULL)
  
  summary_stats <- data %>%
    group_by(!!sym(var_name)) %>%
    summarise(
      Total_Count    = n(),
      Serious_Delinq_Rate = mean(Is_Serious_Delinquency, na.rm = TRUE),
      Rate_Stat_90_Plus = mean(TARGET >= 4, na.rm=TRUE)
    ) %>%
    arrange(desc(Serious_Delinq_Rate)) %>%
    mutate(
      Serious_Delinq_Pct = sprintf("%.2f%%", Serious_Delinq_Rate * 100)
    ) %>%
    select(!!sym(var_name), Total_Count, Serious_Delinq_Pct)
  
  return(summary_stats)
}

# 6.5.2: List of Variables
risk_factors <- c(
  "CODE_GENDER", 
  "NAME_INCOME_TYPE", 
  "NAME_EDUCATION_TYPE", 
  "NAME_FAMILY_STATUS", 
  "NAME_HOUSING_TYPE", 
  "OCCUPATION_TYPE",
  "FLAG_OWN_REALTY"
)

all_risk_profiles <- list()

# 6.5.3: Main Loop (FIXED)
for (var in risk_factors) {
  if (var %in% names(df_diagnostics)) {
    cat(paste("\n>>> Generating Risk Profile for:", var, "\n"))
    
    # Calculate profile
    profile <- calc_detailed_risk(df_diagnostics, var)
    print(profile)
    
    # Visualization
    p_factor <- ggplot(df_diagnostics, aes(x = .data[[var]], fill = as.factor(Is_Serious_Delinquency))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste("Risk by", var), y = "Proportion", fill = "Serious Delinq (90+ DPD)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_factor)
    
    # Store "Riskiest Group" 
    highest_risk_group <- profile %>% slice(1)
    
    # Standardize column names
    colnames(highest_risk_group)[1] <- "Category_Value"
    
    # CRITICAL FIX: Convert to Character so Factors and Numbers can mix
    highest_risk_group$Category_Value <- as.character(highest_risk_group$Category_Value)
    
    highest_risk_group$Factor_Name <- var
    all_risk_profiles[[var]] <- highest_risk_group
  }
}

# ==============================================================================
# 6.6: Interaction Heatmap
# ==============================================================================
if("NAME_HOUSING_TYPE" %in% names(df_diagnostics) & "NAME_FAMILY_STATUS" %in% names(df_diagnostics)) {
  p_interaction <- df_diagnostics %>%
    group_by(NAME_FAMILY_STATUS, NAME_HOUSING_TYPE) %>%
    summarise(Risk_Rate = mean(Is_Serious_Delinquency, na.rm = TRUE), Count = n(), .groups = "drop") %>%
    filter(Count > 30) %>%
    ggplot(aes(x = NAME_HOUSING_TYPE, y = NAME_FAMILY_STATUS, fill = Risk_Rate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Risk_Rate * 100, 1)), color = "white", size = 3) +
    scale_fill_viridis_c(option = "inferno", name = "Risk %") +
    labs(title = "Risk Heatmap: Family vs Housing", x = "Housing", y = "Family") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_interaction)
}

# Close PDF
dev.off()
cat(">> PDF Graphics Device Closed. Visualization file saved.\n")


# ==============================================================================
# 6.7: Risk Profile Summary Report
# ==============================================================================

# Combine the stored high-risk groups into one table
if(length(all_risk_profiles) > 0) {
  summary_table <- bind_rows(all_risk_profiles) %>%
    select(Factor_Name, Category_Value, Total_Count, Serious_Delinq_Pct) %>%
    rename(
      `Factor Variable` = Factor_Name,
      `Highest Risk Group` = Category_Value,
      `Group Size` = Total_Count,
      `Delinquency Rate (90+ DPD)` = Serious_Delinq_Pct
    )
  
  # Print the table neatly
  print(knitr::kable(summary_table, format = "simple", caption = "Highest Risk Segments by Variable"))
  
  # Optional: Save this specific table to CSV
  write.csv(summary_table, file = here("DS-Project_Part2_Scripts", "Saved_Outputs", "risk_profile_summary.csv"), row.names = FALSE)
  cat("\n>> Summary table saved to 'risk_profile_summary.csv'.\n")
} else {
  cat("No risk profiles could be generated (Check variable names).\n")
}

# ------------------------------------------------------------------------------
# SECTION 7: [DELETED/MERGED]
# ------------------------------------------------------------------------------
# NOTE: The logic previously contained here (Simple Preprocessing & Late Split) 
# was fundamentally flawed and caused data leakage.
# It has been entirely removed and replaced by the correct Architecture in Section 5.

# ------------------------------------------------------------------------------
# SECTION 8: SIMPLE DENSE MLP (No Embeddings, Pure Numeric [0,1])
# ------------------------------------------------------------------------------

# Define Input shape dynamically based on the recipe output
num_features <- ncol(x_train)
# Define classes (7 classes: 0-6)
num_classes <- length(unique(y_train))

cat(sprintf("\n=== BUILDING MODEL: Input Dim = %d | Output Classes = %d ===\n", num_features, num_classes))

model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", 
              input_shape = c(num_features)) %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.1) %>%
  layer_dense(units = 64, activation = "relu") %>%
  # UPDATED: Units set to num_classes (7)
  layer_dense(units = num_classes, activation = "softmax") 

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "sparse_categorical_crossentropy",
  metrics = c("sparse_categorical_accuracy")
)

model %>% summary()

# ------------------------------------------------------------------------------
# SECTION 9: LONG TRAINING WITH CLASS WEIGHTS
# ------------------------------------------------------------------------------

cat("\n=== TRAINING WITH CLASS IMBALANCE HANDLING ===\n")

# Calculate Class Weights (Balanced)
# Weight = Total_Samples / (Num_Classes * Count_in_Class)
total_count <- nrow(train_baked)
class_counts <- table(train_baked$target_class)
class_keys <- as.numeric(names(class_counts))

weights_vec <- total_count / (length(class_keys) * class_counts)
class_weight_list <- as.list(weights_vec)
names(class_weight_list) <- class_keys

cat(">> Calculated Class Weights (Penalizing missed Defaults):\n")
print(class_weight_list)

# Training Configuration
callbacks <- list(
  callback_early_stopping(
    monitor = "val_sparse_categorical_accuracy",
    mode = "max",
    patience = 50,  # Adjusted patience for weighted convergence
    restore_best_weights = TRUE,
    verbose = 1
  ),
  callback_reduce_lr_on_plateau(
    monitor = "val_sparse_categorical_accuracy",
    mode = "max",
    factor = 0.5,
    patience = 15,
    min_lr = 1e-7,
    verbose = 1
  )
)

# Fit Model with Weights
history <- model %>% fit(
  x = x_train,
  y = y_train,
  validation_data = list(x_val, y_val),
  class_weight = class_weight_list,  
  epochs = 4000,            
  batch_size = 256,         
  callbacks = callbacks,
  verbose = 1
)

# Save Training History Plot
png(here("DS-Project_Part2_Scripts", "Saved_Outputs", "training_history_plot.png"), width=800, height=600)
plot(history)
dev.off()
cat(">> Training plot saved to 'Saved_Outputs/training_history_plot.png'\n")

# ------------------------------------------------------------------------------
# SECTION 10: COMPLETE EVALUATION & VISUALIZATION SUITE
# ------------------------------------------------------------------------------
# Includes: Fundamental Metrics, Class-Specific Stats, Probability Scoring,
# Credit Risk Metrics (KS, Gini, Lift), and 7+ Visualizations.

cat("\n=== INITIALIZING FINAL EVALUATION ===\n")

# 10.0: Dependency Check for Advanced Metrics
if(!require(pROC)) install.packages("pROC"); library(pROC)
if(!require(MLmetrics)) install.packages("MLmetrics"); library(MLmetrics)

# 10.1: Prepare Prediction Objects
# ----------------------------------------------------------
# Get Probability Matrix
pred_probs <- model %>% predict(x_test)

# Get Predicted Classes (Indices 0-6)
pred_classes <- apply(pred_probs, 1, which.max) - 1

# Define Class Map (Strictly 0-6 per cleaning)
class_map <- c("0"="Status_C_Paid", 
               "1"="Status_0_1-29d", 
               "2"="Status_1_30-59d", 
               "3"="Status_2_60-89d",
               "4"="Status_3_90-119d", 
               "5"="Status_4_120-149d", 
               "6"="Status_5_Over150d")

# Create Factors
actual_factor <- factor(y_test, levels = 0:6, labels = class_map)
pred_factor   <- factor(pred_classes, levels = 0:6, labels = class_map)

# 10.2: Open PDF Graphics Device for Report
# ----------------------------------------------------------
eval_pdf_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "Detailed_Model_Evaluation_Report.pdf")
pdf(eval_pdf_path, width = 11, height = 8.5)
cat(sprintf(">> Report initiated: %s\n", eval_pdf_path))

# ==============================================================================
# PART 1: FUNDAMENTAL & CLASS-SPECIFIC METRICS
# ==============================================================================

# A. Confusion Matrix & Basic Stats
cm <- confusionMatrix(pred_factor, actual_factor, mode = "everything")

# B. Calculate MCC (Matthews Correlation Coefficient) - Multiclass approximation
# We use a custom calculation or MLmetrics if available
mcc_score <- MLmetrics::MCC(y_test, pred_classes)

cat("\n>>> FUNDAMENTAL METRICS <<<\n")
cat(sprintf("Overall Accuracy:   %.2f%%\n", cm$overall['Accuracy'] * 100))
cat(sprintf("Kappa Statistic:    %.4f\n", cm$overall['Kappa']))
cat(sprintf("Matthews Corr (MCC): %.4f\n", mcc_score))

# C. Class-Specific Metrics Table
class_stats <- cm$byClass %>%
  as.data.frame() %>%
  select(Sensitivity, Specificity, Precision, Recall, F1) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

# D. Macro Metrics (Balanced)
macro_f1 <- mean(class_stats$F1, na.rm = TRUE)
macro_recall <- mean(class_stats$Recall, na.rm = TRUE)
cat(sprintf("Balanced Acc (Macro Recall): %.4f\n", macro_recall))
cat(sprintf("Macro F1 Score:              %.4f\n", macro_f1))

# VISUALIZATION 1: Confusion Matrix Heatmap
cm_melt <- as.data.frame(cm$table)
p_cm <- ggplot(cm_melt, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
  labs(title = "Confusion Matrix Heatmap", 
       subtitle = "Darker squares = Higher density (Diagonal is Good)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_cm)

# VISUALIZATION 2: Class-Specific Performance Bar Chart
class_stats %>%
  rownames_to_column("Class") %>%
  pivot_longer(cols = c(Sensitivity, Precision, F1), names_to = "Metric", values_to = "Score") %>%
  ggplot(aes(x = Class, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Class-Specific Performance Metrics", 
       subtitle = "Check Recall (Sensitivity) for Status 4, 5, 6") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") -> p_perf
print(p_perf)

# ==============================================================================
# PART 2: PROBABILITY & RANKING METRICS
# ==============================================================================

cat("\n>>> PROBABILITY METRICS <<<\n")

# A. Log Loss (Categorical Crossentropy)
log_loss_val <- MLmetrics::MultiLogLoss(y_true = model.matrix(~ actual_factor - 1), 
                                        y_pred = pred_probs)
cat(sprintf("Categorical Log Loss: %.4f (Lower is better)\n", log_loss_val))

# B. Multi-Class AUC-ROC
roc_multi <- multiclass.roc(y_test, pred_probs)
cat(sprintf("Multi-Class AUC-ROC:  %.4f\n", auc(roc_multi)))

# C. Brier Score (Mean Squared Error of Probabilities)
# We calculate this component-wise
brier_score <- mean(rowSums((model.matrix(~ actual_factor - 1) - pred_probs)^2))
cat(sprintf("Brier Score:          %.4f (Measures Calibration)\n", brier_score))

# VISUALIZATION 3: Prediction Confidence Histogram
# Are we confident? Plot max probability per prediction.
max_probs <- data.frame(Max_Prob = apply(pred_probs, 1, max),
                        Correct = (pred_classes == y_test))

p_conf <- ggplot(max_probs, aes(x = Max_Prob, fill = Correct)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Model Confidence Histogram",
       subtitle = "Peaks near 1.0 indicate high confidence. Peaks near 0.5 indicate guessing.",
       x = "Max Predicted Probability", y = "Count") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()
print(p_conf)

# VISUALIZATION 4: ROC Curves (One-vs-Rest)
# We loop through classes to create ROC curves for each
roc_data <- data.frame()
for(i in 0:6) {
  # Binary outcome for class i
  bin_y <- ifelse(y_test == i, 1, 0)
  # ROC calc
  r <- roc(bin_y, pred_probs[, i+1], quiet=TRUE)
  roc_data <- rbind(roc_data, data.frame(
    FPR = 1 - r$specificities,
    TPR = r$sensitivities,
    Class = class_map[as.character(i)]
  ))
}

p_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(linewidth = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves (One-vs-Rest)",
       subtitle = "Measures separation power for each status level",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()
print(p_roc)

# ==============================================================================
# PART 3: CREDIT RISK SPECIFIC METRICS (SERIOUS DELINQUENCY)
# ==============================================================================
# Business Goal: Catch "Bad" Debt (Status 4, 5, 6 - 90+ DPD) vs "Good/Indet" (0-3)

cat("\n>>> CREDIT RISK METRICS (Binary: 90+ DPD vs Rest) <<<\n")

# Define "Risk" as the Sum of Probabilities of Classes 4, 5, and 6
risk_probs <- rowSums(pred_probs[, 5:7]) # Columns 5,6,7 correspond to class 4,5,6
binary_truth <- ifelse(y_test >= 4, 1, 0) # 1 = Bad, 0 = Good

# A. Binary AUC for Serious Delinquency
roc_risk <- roc(binary_truth, risk_probs, quiet=TRUE)
auc_risk <- auc(roc_risk)
cat(sprintf("Binary AUC (Serious Risk): %.4f\n", auc_risk))

# B. Gini Coefficient (Credit Scorecard Standard)
gini_coeff <- 2 * auc_risk - 1
cat(sprintf("Gini Coefficient:          %.4f (Target > 0.40)\n", gini_coeff))

# C. KS Statistic (Kolmogorov-Smirnov)
# Max separation between CDF of Goods and Bads
ks_stat <- max(roc_risk$sensitivities + roc_risk$specificities - 1)
cat(sprintf("KS Statistic:              %.4f\n", ks_stat))

# VISUALIZATION 5: Cumulative Gain Chart
# "If we target X% of population by risk, what % of defaulters do we catch?"
lift_obj <- caret::lift(factor(binary_truth, labels=c("Good","Bad")) ~ risk_probs)
p_gain <- ggplot(lift_obj, plot = "gain") +
  labs(title = "Cumulative Gain Chart (Serious Delinquency)",
       subtitle = "Steeper curve = Better prioritization of risk") +
  theme_minimal()
print(p_gain)

# VISUALIZATION 6: Lift Chart
p_lift <- ggplot(lift_obj, plot = "lift") +
  labs(title = "Lift Chart",
       subtitle = "How much better than random guessing at top deciles?") +
  theme_minimal()
print(p_lift)

# VISUALIZATION 7: Calibration Plot (Reliability Diagram) for Risk
# Checks if "80% Risk" actually means 80% default rate
cal_data <- data.frame(prob = risk_probs, truth = binary_truth) %>%
  mutate(bin = ntile(prob, 10)) %>%
  group_by(bin) %>%
  summarise(mean_prob = mean(prob),
            actual_rate = mean(truth))

p_cal <- ggplot(cal_data, aes(x = mean_prob, y = actual_rate)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Calibration Plot (Reliability Diagram)",
       subtitle = "Points should lie on the red dashed line",
       x = "Predicted Risk Probability", y = "Actual Default Rate") +
  theme_minimal()
print(p_cal)

# VISUALIZATION 8: Training History (Recalled from Section 9)
# Assuming 'history' object exists from previous section
if(exists("history")) {
  # Note: plot(history) in keras typically outputs to viewer, 
  # but inside PDF() it might need conversion.
  # We construct a ggplot version manually to be safe:
  hist_df <- as.data.frame(history)
  p_hist <- ggplot(hist_df, aes(x = epoch, y = value, color = data)) +
    geom_line() +
    facet_wrap(~metric, scales = "free_y") +
    labs(title = "Training History Recalled") +
    theme_minimal()
  print(p_hist)
}

# 10.3: Close PDF and Save Data
# ----------------------------------------------------------
dev.off()
cat(sprintf(">> Evaluation Report Saved: %s\n", eval_pdf_path))

# Save Test Predictions with Risk Scores
results_df <- data.frame(
  actual_class = y_test,
  predicted_class = pred_classes,
  max_prob_confidence = apply(pred_probs, 1, max),
  risk_score_90plus = risk_probs,
  actual_label = as.character(actual_factor),
  predicted_label = as.character(pred_factor)
)

write.csv(results_df, here("DS-Project_Part2_Scripts", "Saved_Outputs", "comprehensive_test_predictions.csv"), row.names = FALSE)
cat(">> Predictions and Risk Scores exported to CSV.\n")
cat("=== EVALUATION COMPLETE ===\n")