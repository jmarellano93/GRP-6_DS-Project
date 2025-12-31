# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FINAL SCRIPT V4: HIERARCHICAL THREE-STAGE MLP (X -> Risk -> Severity)
# Refactored by: Senior Data Science Mentor
# Purpose: Implements a hierarchical classification system for Credit Risk.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ------------------------------------------------------------------------------
# SECTION 1: ENVIRONMENT SETUP
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 1
#
# 1.1: WORKING DIRECTORY & LIBRARY MANAGEMENT
# Reason: Dynamically checks for required packages (tidyverse, caret, keras3, etc.)
#       and installs them automatically if they are missing. It also establishes
#       a relative project root using the 'here' package.
# Function:  Ensures the script is "portable." It prevents crashes when running on a
#       different machine (e.g., a grader's computer) that might lack specific
#       libraries or have different file paths.
#
# 1.2: BACKEND CONNECTION STRATEGY (R-PYTHON BRIDGE)
# Reason: Manages the interface between R (the frontend) and Python (the backend 
#       engine required for TensorFlow/Keras).
#       - Step 1: Looks for a specific Conda environment named 'r-reticulate'.
#       - Step 2: Falls back to the system's active Python if Conda is absent.
# Function:  Deep Learning in R is not native; it relies on Python. This block prevents
#       "Python not found" errors by explicitly hunting for a valid TensorFlow
#       installation before the code attempts to build any models.
#
# 1.3: SEEDING & REPRODUCIBILITY (THE "DOUBLE-LOCK")
# Reason: Sets random seeds in TWO places:
#       1. R Environment (set.seed): Controls data splitting and R-based logic.
#       2. TensorFlow Graph (tf$random$set_seed): Controls neural network weight
#          initialization and tensor operations.
# Function:  Neural networks are non-deterministic by default. Without seeding BOTH
#       environments, two runs of the same script would yield slightly different
#       accuracy scores, making scientific comparison and grading impossible.

# ==============================================================================
# 1.1: Working Directory and Libraries
# ==============================================================================
# Set personal working directory (Adjust as needed)
# setwd("~/Documents/Repos/GRP-6_DS-Project")
# renv::activate()

# FORCE CLOSE ALL GRAPHICS DEVICES
# This prevents "file already open" errors from previous crashes
try(graphics.off(), silent = TRUE)

# 1.1.1: Automated Dependency Management
installed_pkgs <- installed.packages()[, "Package"]
# Added "tfdatasets" to the list
req_pkgs <- c("tidyverse", "caret", "corrplot", "naniar", "gridExtra", 
              "e1071", "tensorflow", "reticulate", "keras3", "here", 
              "recipes", "MLmetrics", "pROC", "vip", "tfdatasets")

new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if (length(new_pkgs)) install.packages(new_pkgs)

# 1.1.2: Load General Data Science Libraries (NO TF/KERAS YET)
library(tidyverse)
library(caret)
library(corrplot)
library(naniar)
library(gridExtra)
library(e1071)
library(here)
library(recipes)
library(MLmetrics)
library(pROC)
library(vip)
library(tfdatasets)

# NOTE: Deep Learning libs are deferred to 1.3 to protect backend config

# 1.1.3: Verify Project Root
cat(">> Project Root Detected at:", here(), "\n")
cat(">> General libraries loaded. Deep Learning libs pending configuration.\n")

# ==============================================================================
# 1.2: Backend Connection Strategy (TensorFlow)
# ==============================================================================
library(reticulate)

# 1.2.1: Explicit Environment Binding
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

# 1.3.1: NOW Safe to load Deep Learning Libraries
library(tensorflow)
library(keras3)

# REPRODUCIBILITY GUARANTEE:
# 1.3.2: Seed R (Frontend)
set.seed(1)

# 1.3.3: Seed TensorFlow/Python (Backend)
if (backend_configured) {
  tryCatch({
    tf$random$set_seed(1L)
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

# SCRIPT ARCHITECTURE SUMMARY: SECTION 2
#
# 2.1: DATA INGESTION & OUTPUT MANAGEMENT
# Reason: Safely loads the dataset using relative paths and prepares a dedicated
#         graphics device (PDF) for capturing multiple visualization plots.
# Function: Uses 'here' to locate the raw CSV regardless of the working directory.
#         It also initializes a PDF file connection so that all subsequent EDA
#         charts are saved into a single report rather than overwriting each other
#         in the RStudio plot pane.
#
# 2.2: DATA HYGIENE & QUALITY CONTROL
# Reason: Raw data often contains redundancies or non-informative features that
#         can distort statistical analysis.
# Function: Performs a "health check" on the dataframe:
#         1. Removes exact duplicate rows (deduplication).
#         2. Identifies missing values to prioritize cleaning steps.
#         3. Checks for zero-variance features (e.g., columns with only one value).
#
# 2.3: TARGET & FEATURE DISTRIBUTION ANALYSIS
# Reason: Establishes the statistical baseline of the dataset to identify
#         skewness, outliers, and class imbalance.
# Function: Generates visual and statistical summaries:
#         - Confirms severe class imbalance in the target variable ('status').
#         - highlights skewness in financial variables (Income).
#         - Uses boxplots to visually flag potential outliers.
#
# 2.4: ANOMALY DETECTION & MNAR FORENSICS
# Reason: Investigates specific domain oddities that standard imputation would mishandle,
#         specifically the relationship between Pensioners and missing data.
# Function: 
#         1. Identifies the "Magic Number" 365243 in 'DAYS_EMPLOYED' (approx 1000 years),
#            confirming it acts as a placeholder for "Pensioner".
#         2. Proves that missing 'OCCUPATION_TYPE' data is Missing Not At Random (MNAR)
#            by correlating it with Pensioner status (Pensioners legitimately lack jobs).

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
  p_combined <- grid.arrange(p_hist, p_box, ncol = 2)
  print(p_combined)}

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

# SCRIPT ARCHITECTURE SUMMARY: SECTION 3
#
# 3.1: STRICT SCHEMA ENFORCEMENT & RELOAD
# Reason: R's default CSV importer often misinterprets categorical data as strings
#         or integers. Relying on defaults causes downstream crashing.
# Function: 
#         1. Defines a rigid dictionary of data types (Factors vs Characters).
#         2. Reloads the dataset using these strict types.
#         3. Executes an *immediate* deduplication (distinct) to ensure the
#            cleaning pipeline starts with unique entities only.
#
# 3.2: BINARY ENCODING & NOISE REDUCTION
# Reason: Machine Learning models require numerical input. "Y/N" strings must
#         be converted to 1/0 integers.
# Function: 
#         - Converts FLAG_OWN_CAR and FLAG_OWN_REALTY to binary integers.
#         - Drops 'FLAG_MOBIL' (identified as zero-variance/useless in Section 2)
#           to reduce dataset width.
#
# 3.3: DOMAIN-SPECIFIC FEATURE ENGINEERING
# Reason: Raw data contains confusing artifacts (e.g., negative days for age,
#         the "365243" magic number for pensioners) that confuse models.
# Function: 
#         1. Humanizes Time: Converts negative 'DAYS_BIRTH' into positive 'AGE'.
#         2. Solves the MNAR Issue: Replaces missing 'OCCUPATION_TYPE' with 
#            "Retired" if the magic number is present, turning a missing value
#            problem into a valid signal.
#         3. Log Scaling: Applies log-transformation to employment years to 
#            compress the range and handle skewness.
#
# 3.4 & 3.5: LOGICAL CORRECTION & STRATEGIC DEFERRAL (ANTI-LEAKAGE)
# Reason: We found data anomalies (impossible family counts) and outliers. 
#         However, removing outliers *globally* (before splitting data) causes 
#         Data Leakage, biasing the test set.
# Function: 
#         1. Fixes Logic: Mathematically corrects rows where Family Members < Children
#            (impossible) by enforcing a logical floor based on marital status.
#         2. Defers Deletion: explicitly *comments out* row deletion steps. 
#            Outliers will be removed in Section 5 only from the TRAINING set, 
#            preserving the integrity of the validation/test sets.

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

# 3.1.2: Reload the CSV with strict types AND IMMEDIATE DEDUPLICATION
# The 'distinct()' call here is CRITICAL. It prevents the re-injection of duplicates.
data <- read.csv(path, colClasses = col_types, na.strings = c("NA", "")) %>% 
  distinct()

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
# 3.2.1: Binary Encoding
data$FLAG_OWN_CAR <- as.integer(data$FLAG_OWN_CAR == "Y")
data$FLAG_OWN_REALTY <- as.integer(data$FLAG_OWN_REALTY == "Y")
flags_numeric <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")

# 3.2.2: Post-Load Logic
data[flags_numeric] <- lapply(data[flags_numeric], as.integer)
data <- data %>% select(-FLAG_MOBIL)

# ==============================================================================
# 3.3: Age and Employment Logic
# ==============================================================================
cat("--- Correcting Age and Employment Data ---\n")

# 3.3.1: Correct DAYS_BIRTH
data$AGE <- abs(data$DAYS_BIRTH) / 365.25

# 3.3.2: Pensioner and Employment corrections
data <- data %>%
  mutate(NAME_INCOME_TYPE = ifelse(NAME_INCOME_TYPE == "Pensioner" & DAYS_EMPLOYED != 365243,
                                   "Working", NAME_INCOME_TYPE))

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

# 3.3.4: Fix Sentinel Value
data$DAYS_EMPLOYED[data$DAYS_EMPLOYED == 365243] <- 0

# 3.3.5: Calculate ACTIVE_EMPLOYMENT_YEARS
data$ACTIVE_EMPLOYMENT_YEARS <- abs(data$DAYS_EMPLOYED) / 365.25
data$ACTIVE_EMPLOYMENT_YEARS_LOG <- log1p(data$ACTIVE_EMPLOYMENT_YEARS)

# ==============================================================================
# 3.4: Family Size and Outliers
# ==============================================================================
cat("--- Handling Family Logic ---\n")

# We will filter outliers in Section 5.3 (Training Set Only) to avoid data leakage.

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

# ==============================================================================
# 3.5: Income Outliers
# ==============================================================================
# 3.5.1: Define Impossible Jobs
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", 
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")

# 3.5.2: Log transform Income (Safe to apply globally)
data$AMT_INCOME_TOTAL_LOG <- log1p(data$AMT_INCOME_TOTAL)

cat(">> LOG TRANSFORMATION APPLIED. Global outlier filtering deferred to Training Split.\n")

# ------------------------------------------------------------------------------
# SECTION 4: FEATURE ENGINEERING
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 4
#
# 4.1: RATIO-BASED FEATURE ENGINEERING
# Reason: Raw demographic numbers (like absolute Income or Age) often have weak 
#         predictive power on their own. Credit risk is better defined by 
#         *capacity* (wealth relative to dependents) and *stability* (work history 
#         relative to age).
# Function: 
#         1. Synthesizes Derivative Features: Creates interaction variables to 
#            expose relationships hidden in the raw data.
#            - Income Capacity: 'INCOME_PER_FAMILY_MEMBER' reveals actual 
#              disposable income better than total income.
#            - Lifecycle Stability: 'EMPLOYMENT_RATIO' contextualizes experience
#              (e.g., 5 years of work means something very different for a 
#              23-year-old vs a 50-year-old).
#         2. Logarithmic Smoothing: Immediately applies log transformations to 
#            these new high-variance financial ratios to prevent them from 
#            dominating the model gradients later.
#         3. QA Validation: Programmatically verifies the new columns were 
#            successfully appended before allowing the script to proceed.

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
# SECTION 5: FINAL PREPROCESSING & THREE-STAGE SPLIT STRATEGY
# ------------------------------------------------------------------------------

# 5.1: TARGET MAPPING (Original Global Map)
# We keep this for Reference and EDA, but we will generate specific targets later.
# 0=C, 1=0, 2=1, 3=2, 4=3, 5=4, 6=5, 7=X
data$target_class <- recode(as.character(data$status),
                            "C" = 0, "0" = 1, "1" = 2, "2" = 3, 
                            "3" = 4, "4" = 5, "5" = 6, "X" = 7
) |> as.numeric()

# 5.2: EDA Snapshot
eda_snapshot <- data
eda_snapshot$TARGET <- eda_snapshot$target_class 

# 5.3: ID-BASED SPLITTING (Consistent with Original)
cols_to_drop <- c("DAYS_BIRTH", "DAYS_EMPLOYED", "status") 
df_modeling <- data %>% select(-any_of(cols_to_drop))

all_ids <- unique(df_modeling$ID)
set.seed(1)
train_ids <- sample(all_ids, size = 0.70 * length(all_ids))
remaining_ids <- setdiff(all_ids, train_ids)
val_ids <- sample(remaining_ids, size = 0.50 * length(remaining_ids))
test_ids <- setdiff(remaining_ids, val_ids)

train_raw <- df_modeling %>% filter(ID %in% train_ids)
val_raw   <- df_modeling %>% filter(ID %in% val_ids)
test_raw  <- df_modeling %>% filter(ID %in% test_ids)

# --- OUTLIER REMOVAL (Train Only) ---
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", "Low-skill Laborers", "Drivers", "Waiters/barmen staff")
train_raw <- train_raw %>% filter(!(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs))
train_raw <- train_raw %>% filter(CNT_FAM_MEMBERS <= 10)

# 5.4: RECIPE (Standard Scaling applied to ALL data)
cat("--- Building Preprocessing Recipe ---\n")
raw_parents <- c("AMT_INCOME_TOTAL", "ACTIVE_EMPLOYMENT_YEARS", "INCOME_PER_FAMILY_MEMBER", "INCOME_PER_AGE")

rec_obj <- recipe(target_class ~ ., data = train_raw) %>%
  update_role(ID, new_role = "id") %>%
  step_rm(all_of(raw_parents)) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.90) %>%
  step_range(all_numeric_predictors(), min = 0, max = 1)

rec_trained <- prep(rec_obj, training = train_raw)

train_baked <- bake(rec_trained, new_data = train_raw)
val_baked   <- bake(rec_trained, new_data = val_raw)
test_baked  <- bake(rec_trained, new_data = test_raw)

# 5.5: SUBSET GENERATION FOR 3 STAGES
# We need to extract the matrices, but we must handle the targets hierarchically.

cat("--- Preparing Hierarchical Datasets (Stages 1, 2, 3) ---\n")

prepare_stage_data <- function(baked_df) {
  x_mat <- baked_df %>% select(-ID, -target_class) %>% data.matrix()
  y_orig <- baked_df$target_class
  
  # Stage 1: Active (0-6) vs No Loan (7/X)
  # Target 0 = No Loan (X), 1 = Active
  y_s1 <- ifelse(y_orig == 7, 0, 1) 
  
  # Stage 2: Paid (0/C) vs Delinquent (1-6) | Filter: Not X
  # Note: "Status 0" (1-29 DPD) is grouped with Delinquent (1) here to preserve it for Stage 3
  mask_s2 <- y_orig != 7
  x_s2    <- x_mat[mask_s2, , drop = FALSE]
  # Target 0 = Paid (C), 1 = Delinquent (Any severity)
  y_s2    <- ifelse(y_orig[mask_s2] == 0, 0, 1)
  
  # Stage 3: Delinquency Severity (1-6) | Filter: Not X AND Not C
  mask_s3 <- y_orig != 7 & y_orig != 0
  x_s3    <- x_mat[mask_s3, , drop = FALSE]
  # Remap targets: 1->0, 2->1, ... 6->5
  y_s3    <- y_orig[mask_s3] - 1 
  
  list(
    x = x_mat, y_orig = y_orig,
    y_s1 = y_s1,
    x_s2 = x_s2, y_s2 = y_s2,
    x_s3 = x_s3, y_s3 = y_s3
  )
}

# Apply to all splits
d_train <- prepare_stage_data(train_baked)
d_val   <- prepare_stage_data(val_baked)
d_test  <- prepare_stage_data(test_baked)

cat(sprintf(">> STAGE 1 (Active/X): %d samples\n", nrow(d_train$x)))
cat(sprintf(">> STAGE 2 (Paid/Delinq): %d samples\n", nrow(d_train$x_s2)))
cat(sprintf(">> STAGE 3 (Severity): %d samples\n", nrow(d_train$x_s3)))

# ------------------------------------------------------------------------------
# SECTION 6: CLEANED DATA EXPLORATION (UNCHANGED logic, uses Global Map)
# ------------------------------------------------------------------------------

# Note: We rely on the original 'target_class' (0-7) present in train_baked/eda_snapshot
# for the risk profiling logic. This section remains valid for business insights.
if (exists("eda_snapshot") && exists("train_ids")) {
  df_clean <- eda_snapshot %>% filter(ID %in% train_ids)
  df_clean$TARGET <- df_clean$target_class
} else {
  df_clean <- train_raw
  df_clean$TARGET <- df_clean$target_class
}

cat("\n==============================================================================\n")
cat(" STARTING SECTION 6: EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("==============================================================================\n")

# ==============================================================================
# 6.2: Visualizing Missingness & Distributions
# ==============================================================================
cat("\n[Section 6.2] Generating Data Quality Visualizations...\n")
if (dev.cur() != 1) cat(">> Appending Section 6 plots to the active PDF file...\n")

if(require(naniar)) {
  print(naniar::vis_miss(df_clean, warn_large_data = FALSE) + ggtitle("Missingness Map"))
}

# ==============================================================================
# 6.3: Numeric Distributions & Correlations
# ==============================================================================
num_cols <- setdiff(names(select(df_clean, where(is.numeric))), c("TARGET", "target_class"))
if(require(gridExtra)) {
  for (col in num_cols) {
    p_hist <- ggplot(df_clean, aes(x = .data[[col]])) + geom_histogram(bins = 30, fill = "coral", color = "white") + theme_minimal()
    p_box <- ggplot(df_clean, aes(y = .data[[col]])) + geom_boxplot(fill = "lightblue") + theme_minimal()
    grid.arrange(p_hist, p_box, ncol = 2)
  }
}

# ==============================================================================
# 6.4: Granular Risk Profiling
# ==============================================================================
cat("\n[Section 6.4] Generating Granular Risk Profiles...\n")
# MODIFICATION: Included label for Status X (Target 7)
df_diagnostics <- df_clean %>%
  mutate(
    Risk_Label = case_when(
      TARGET == 0 ~ "Status_C_Paid", TARGET == 1 ~ "Status_0_1-29DPD",
      TARGET == 2 ~ "Status_1_30-59DPD", TARGET == 3 ~ "Status_2_60-89DPD",
      TARGET == 4 ~ "Status_3_90-119DPD", TARGET == 5 ~ "Status_4_120-149DPD",
      TARGET == 6 ~ "Status_5_Over150DPD", TARGET == 7 ~ "Status_X_NoLoan", 
      TRUE ~ "Unknown"
    ),
    Is_Serious_Delinquency = ifelse(TARGET >= 4 & TARGET <= 6, 1, 0) # X (7) is not a default
  )
print(prop.table(table(df_diagnostics$Risk_Label)) * 100)

# ==============================================================================
# 6.5: Factor Variable Risk Analysis
# ==============================================================================
calc_detailed_risk <- function(data, var_name) {
  if(!var_name %in% names(data)) return(NULL)
  data %>% group_by(!!sym(var_name)) %>%
    summarise(Total_Count = n(), Serious_Delinq_Rate = mean(Is_Serious_Delinquency, na.rm = TRUE)) %>%
    mutate(Serious_Delinq_Pct = sprintf("%.2f%%", Serious_Delinq_Rate * 100))
}

risk_factors <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "FLAG_OWN_REALTY")
all_risk_profiles <- list()

for (var in risk_factors) {
  if (var %in% names(df_diagnostics)) {
    cat(paste("\n>>> Generating Risk Profile for:", var, "\n"))
    profile <- calc_detailed_risk(df_diagnostics, var)
    print(profile)
    
    p_factor <- ggplot(df_diagnostics, aes(x = .data[[var]], fill = as.factor(Is_Serious_Delinquency))) +
      geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent) +
      labs(title = paste("Risk by", var), fill = "Serious Delinq") + theme_minimal()
    print(p_factor)
    
    # Store Summary
    highest_risk <- profile %>% slice(which.max(Serious_Delinq_Rate))
    highest_risk$Category_Value <- as.character(highest_risk[[var]])
    highest_risk$Factor_Name <- var
    all_risk_profiles[[var]] <- highest_risk
  }
}

# ==============================================================================
# 6.6: Risk Profile Summary Report
# ==============================================================================
dev.off() # Close PDF
cat(">> PDF Graphics Device Closed. Visualization file saved.\n")

if(length(all_risk_profiles) > 0) {
  summary_table <- bind_rows(all_risk_profiles) %>% select(Factor_Name, Category_Value, Total_Count, Serious_Delinq_Pct)
  print(knitr::kable(summary_table, format = "simple", caption = "Highest Risk Segments by Variable"))
  write.csv(summary_table, file = here("DS-Project_Part2_Scripts", "Saved_Outputs", "risk_profile_summary.csv"), row.names = FALSE)
}

# ------------------------------------------------------------------------------
# SECTION 7: MODEL ARCHITECTURE (REFACTORED FOR 3 STAGES)
# ------------------------------------------------------------------------------

# Helper function to build consistent MLPs
build_stage_model <- function(input_dim, output_dim, name_prefix) {
  model <- keras_model_sequential(name = name_prefix) %>%
    layer_dense(units = 256, activation = "relu", input_shape = c(input_dim)) %>%
    layer_dropout(0.10) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(0.05) %>%
    layer_dense(units = output_dim, activation = ifelse(output_dim == 1, "sigmoid", "softmax"))
  
  # Compile
  loss_fn <- ifelse(output_dim == 1, "binary_crossentropy", "sparse_categorical_crossentropy")
  metric  <- ifelse(output_dim == 1, "binary_accuracy", "sparse_categorical_accuracy")
  
  model %>% compile(
    optimizer = optimizer_sgd(learning_rate = 0.005, momentum = 0.9, nesterov = TRUE),
    loss = loss_fn,
    metrics = c(metric)
  )
  return(model)
}

input_cols <- ncol(d_train$x)

# Model 1: Is Active? (Binary)
model_s1 <- build_stage_model(input_cols, 1, "Stage1_ActiveVsX")
# Model 2: Is Delinquent? (Binary)
model_s2 <- build_stage_model(input_cols, 1, "Stage2_PaidVsDelinq")
# Model 3: Severity? (Multi-class: Classes 0-5 representing orig 1-6)
model_s3 <- build_stage_model(input_cols, 6, "Stage3_Severity")

cat(">> Three-Stage Architecture Initialized.\n")

# ------------------------------------------------------------------------------
# SECTION 8: TRAINING WITH STRATIFIED SAMPLING (REFACTORED)
# ------------------------------------------------------------------------------

# Helper to create Stratified/Weighted Datasets
create_weighted_pipeline <- function(x_mat, y_vec, batch_size=256) {
  
  # Count classes
  classes <- unique(y_vec)
  n_total <- length(y_vec)
  
  # Create list of datasets per class
  ds_list <- list()
  weights <- c()
  
  for (cls in sort(classes)) {
    idx <- which(y_vec == cls)
    w   <- length(idx) / n_total # Natural proportion
    
    ds_cls <- tensor_slices_dataset(list(x_mat[idx, ], y_vec[idx])) %>%
      dataset_shuffle(buffer_size = length(idx)) %>%
      dataset_repeat()
    
    ds_list <- append(ds_list, ds_cls)
    weights <- c(weights, w)
  }
  
  # Interleave
  balanced_ds <- sample_from_datasets(ds_list, weights = weights) %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(1)
  
  return(balanced_ds)
}

# --- TRAINING STAGE 1: ACTIVE vs X ---
cat("\n=== TRAINING STAGE 1: Loan Active (1) vs No Loan (0) ===\n")
ds_s1 <- create_weighted_pipeline(d_train$x, d_train$y_s1)
hist_s1 <- model_s1 %>% fit(
  ds_s1, validation_data = list(d_val$x, d_val$y_s1),
  epochs = 1500, steps_per_epoch = floor(nrow(d_train$x)/256),
  callbacks = list(callback_early_stopping(patience=100, restore_best_weights=TRUE)),
  verbose = 0
)
cat(">> Stage 1 Complete.\n")

# --- TRAINING STAGE 2: PAID vs DELINQUENT ---
cat("\n=== TRAINING STAGE 2: Paid (0) vs Delinquent (1) ===\n")
# Note: Input is subset x_s2
ds_s2 <- create_weighted_pipeline(d_train$x_s2, d_train$y_s2)
hist_s2 <- model_s2 %>% fit(
  ds_s2, validation_data = list(d_val$x_s2, d_val$y_s2),
  epochs = 1500, steps_per_epoch = floor(nrow(d_train$x_s2)/256),
  callbacks = list(callback_early_stopping(patience=100, restore_best_weights=TRUE)),
  verbose = 0
)
cat(">> Stage 2 Complete.\n")

# --- TRAINING STAGE 3: SEVERITY (Classes 1-6) ---
cat("\n=== TRAINING STAGE 3: Severity Level ===\n")
# Note: Input is subset x_s3
ds_s3 <- create_weighted_pipeline(d_train$x_s3, d_train$y_s3)
hist_s3 <- model_s3 %>% fit(
  ds_s3, validation_data = list(d_val$x_s3, d_val$y_s3),
  epochs = 1500, steps_per_epoch = floor(nrow(d_train$x_s3)/256),
  callbacks = list(callback_early_stopping(patience=100, restore_best_weights=TRUE)),
  verbose = 0
)
cat(">> Stage 3 Complete.\n")

# ------------------------------------------------------------------------------
# SECTION 9: INFERENCE AGGREGATION & EVALUATION
# ------------------------------------------------------------------------------

cat("\n=== EXECUTING MULTI-STAGE INFERENCE ===\n")

# 9.1: Generate Raw Predictions for ALL test rows
# Note: We apply models S2 and S3 to the full x_test, even though they were trained on subsets.
# The mathematical combination later filters out the irrelevance.

p_active  <- predict(model_s1, d_test$x)           # Shape (N, 1) -> Prob(Active)
p_delinq  <- predict(model_s2, d_test$x)           # Shape (N, 1) -> Prob(Delinquent | Active)
p_severity <- predict(model_s3, d_test$x)          # Shape (N, 6) -> Prob(Class K | Delinquent)

# 9.2: Reconstruct Final 8-Class Probability Matrix
# Original Mapping:
# 0=C, 1=0, 2=1, 3=2, 4=3, 5=4, 6=5, 7=X

n_test <- nrow(d_test$x)
final_probs <- matrix(0, nrow = n_test, ncol = 8)

# Col 7 (Index 8 in R): Status X (No Loan)
# P(X) = 1 - P(Active)
final_probs[, 8] <- 1 - p_active

# Col 0 (Index 1 in R): Status C (Paid)
# P(C) = P(Active) * P(Not Delinquent)
# P(Not Delinquent) = 1 - P(Delinquent)
final_probs[, 1] <- p_active * (1 - p_delinq)

# Cols 1-6 (Indices 2-7 in R): Delinquency Classes
# P(Class k) = P(Active) * P(Delinquent) * P(Severity k)
p_act_del <- p_active * p_delinq # Shape (N, 1)

# Broadcast multiplication
for (k in 1:6) {
  final_probs[, k+1] <- p_act_del * p_severity[, k]
}

# 9.3: Validation & Normalization
# Due to floating point math, sums might slightly deviate from 1.0. Normalize rows.
row_sums <- rowSums(final_probs)
final_probs <- final_probs / row_sums

# 9.4: Standard Evaluation (Compatible with original script)
class_map <- c("0"="Status_C_Paid", 
               "1"="Status_0_1-29d", 
               "2"="Status_1_30-59d", 
               "3"="Status_2_60-89d",
               "4"="Status_3_90-119d", 
               "5"="Status_4_120-149d", 
               "6"="Status_5_Over150d", 
               "7"="Status_X_NoLoan")

colnames(final_probs) <- levels(factor(names(class_map), levels = 0:7, labels = class_map))
pred_classes <- apply(final_probs, 1, which.max) - 1

eval_pdf_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "three_stage_model_report.pdf")
pdf(eval_pdf_path, width = 11, height = 8.5)

# --- Confusion Matrix ---
actual_factor <- factor(d_test$y_orig, levels = 0:7, labels = class_map)
pred_factor   <- factor(pred_classes, levels = 0:7, labels = class_map)

cm <- confusionMatrix(pred_factor, actual_factor, mode = "everything")
print(cm$table)
cat(sprintf("Three-Stage Model Accuracy: %.2f%%\n", cm$overall['Accuracy'] * 100))

# --- ROC Curves (One vs Rest) ---
# This proves X and C are handled explicitly alongside others
roc_data_list <- list()
for(cls_idx in 0:7) {
  curr_binary_truth <- as.numeric(d_test$y_orig == cls_idx)
  curr_prob <- final_probs[, cls_idx + 1]
  r <- roc(curr_binary_truth, curr_prob, quiet = TRUE)
  roc_data_list[[cls_idx + 1]] <- data.frame(FPR = 1 - r$specificities, TPR = r$sensitivities, Class = class_map[as.character(cls_idx)])
}
roc_plot_data <- bind_rows(roc_data_list)
p_roc <- ggplot(roc_plot_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(size = 0.8) + labs(title = "ROC Curves (Three-Stage Hierarchical Reconstruction)") + theme_minimal()
print(p_roc)

dev.off()
cat(sprintf(">> Evaluation Report Saved: %s\n", eval_pdf_path))

# 9.5 Save Models
save_model(model_s1, here("DS-Project_Part2_Scripts", "Saved_Outputs", "model_stage1_active.keras"))
save_model(model_s2, here("DS-Project_Part2_Scripts", "Saved_Outputs", "model_stage2_delinq.keras"))
save_model(model_s3, here("DS-Project_Part2_Scripts", "Saved_Outputs", "model_stage3_severity.keras"))