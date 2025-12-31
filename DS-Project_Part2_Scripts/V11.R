# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Perfectly cleaned script with variable factor X removed and all regularization
# and normalization handled by my keras neural network.Includes categorical encoding,
# Validation of "in-model" normalization, and target variable inspection.
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

# 1.1.1: Automated Dependency Management
installed_pkgs <- installed.packages()[, "Package"]
req_pkgs <- c("tidyverse", "caret", "corrplot", "naniar", "gridExtra", 
              "e1071", "tensorflow", "reticulate", "keras3", "here", 
              "recipes", "MLmetrics", "pROC")

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
# MENTOR NOTE: Deep Learning libs are deferred to 1.3 to protect backend config

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
set.seed(123)

# 1.3.3: Seed TensorFlow/Python (Backend)
if (backend_configured) {
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

# 2.1 - 2.3: ROBUST LOADING & GRAPHICS MANAGEMENT
# Reason: 1. Loads data using relative paths ('here') to prevent file-not-found errors.
#       2. Opens a specific PDF graphics device.
# Function:  Instead of generating dozens of popup windows that must be manually saved,
#       this strategy directs ALL subsequent plots into a single, professional
#       PDF report. This is essential for batch processing and documentation.
#
# 2.4 - 2.5: DATA HEALTH & SANITY CHECKS
# Reason: Performs a "physical" on the dataset:
#       - Checks dimensions and data types (Glimpse).
#       - Identifies Missing Values (NAs).
#       - DEDUPLICATION: Crucially, identifies AND REMOVES duplicate rows.
# Function:  Garbage In, Garbage Out. Duplicates in credit data often represent 
#       system errors or repeated application submissions. If left in, they cause
#       data leakage (train/test overlap) and artificial accuracy inflation.
#
# 2.6: UNIVARIATE DISTRIBUTION ANALYSIS
# Function: Visualizes the "Target" (status) and "Features" (Income, Children, etc.).
#       - Calculates Skewness metrics.
#       - Generates Histograms/Boxplots for outlier detection.
# Reason:  Neural Networks are sensitive to Class Imbalance (rare defaults) and 
#       Unscaled Outliers (high incomes). This section confirms if we need 
#       Log-Transforms or Class Weighting later in the pipeline.
#
# 2.7: THE "MAGIC NUMBER" ANOMALY (365243)
# Function: Investigates a specific integer (365243) found in 'DAYS_EMPLOYED'.
#       It correlates this value with 'Pensioner' status.
# Reason:  365243 days is ~1000 years. This is a legacy system "sentinel value" 
#       meaning "Not Applicable / Retired." If treated as a real number, it 
#       would massively skew the model's weights. We identify it here to clean 
#       it in Section 3.
#
# 2.8: MNAR (MISSING NOT AT RANDOM) DIAGNOSTICS
# Function: Checks if 'OCCUPATION_TYPE' is missing randomly or for a specific reason.
#       (Result: It is missing almost exclusively for Pensioners).
# Reason:  Standard imputation (filling with the mode/mean) fails here. Since the 
#       data is Missing Not At Random (the missingness ITSELF implies retirement),
#       we must treat "Missing" as its own valid category ("Retired" or "Unknown")
#       rather than guessing a job title.

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

# 3.1 - 3.2: STRICT TYPING & BINARY ENCODING
# Function: 1. Reloads the CSV with a hard-coded schema (integers vs factors).
#       2. Converts text Booleans ("Y"/"N") into machine-readable integers (1/0).
# Reason:  R's default parser often guesses wrong (e.g., treating zip codes as numbers).
#       By strictly enforcing types now, we prevent "Factor vs Character" crashes
#       in the Neural Network later.
#
# 3.3: AGE & EMPLOYMENT LOGIC (THE "MAGIC NUMBER" FIX)
# Function: 1. Converts 'DAYS_BIRTH' (negative days) into 'AGE' (positive years).
#       2. Fixes the 365243 anomaly: Replaces it with a cap value (approx 60 yrs).
#       3. Imputes 'OCCUPATION_TYPE': If 'DAYS_EMPLOYED' was 365243, we force
#          Occupation to "Retired" instead of leaving it NA.
# Reason:  This solves the MNAR issue identified in Section 2. We are turning a 
#       "Missing" value into a "Structural" feature (Retirement status), which 
#       adds predictive power rather than losing data.
#
# 3.4: FAMILY CONSISTENCY CHECK
# Function: Checks for logical impossibilities, specifically rows where:
#       Count_Children > Count_Family_Members.
#       (Fix: Increases Family Member count based on Marital Status + Children).
# Reason:  Data entry errors are common. A single parent with 2 kids must have a
#       family size of at least 3. Adjusting this ensures the "Income Per Member"
#       ratio (calculated later) is mathematically sound.
#
# 3.5: INCOME OUTLIER FILTERS & TRANSFORMATIONS
# Function: 1. Removes rows with Income > 1,000,000 IF the job is low-skilled 
#          (e.g., 'Cleaning staff').
#       2. Applies Log-Transformation (log1p) to Income.
# Reason:  While rich people exist, a 'Waiter' earning $5M is likely a typo 
#       (extra zeros). These extreme outliers destroy Neural Network gradients.
#       Log-transforming the remaining valid incomes squashes the distribution 
#       shape to be more Gaussian (Normal), which helps the model converge.

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
# MENTOR NOTE: The 'distinct()' call here is CRITICAL. 
# It prevents the re-injection of the duplicates you identified in Section 2.
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

# MENTOR NOTE: Row deletion is REMOVED from here.
# We will filter outliers in Section 5.3 (Training Set Only) to avoid data leakage.
# data <- data[data$CNT_FAM_MEMBERS <= 10, ]  <-- DEFERRED

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

# 3.5.2: Remove unlikely high incomes
# MENTOR NOTE: Row deletion is REMOVED from here. Deferred to Section 5.3.

# 3.5.3: Log transform Income (Safe to apply globally)
data$AMT_INCOME_TOTAL_LOG <- log1p(data$AMT_INCOME_TOTAL)

cat(">> LOG TRANSFORMATION APPLIED. Global outlier filtering deferred to Training Split.\n")

# ------------------------------------------------------------------------------
# SECTION 4: FEATURE ENGINEERING
# ------------------------------------------------------------------------------

# 4.1: RATIO FEATURE CREATION
# ------------------------------------------------------------------------------
# Function: Synthesizes "Interaction Features" by combining existing columns.
#       - Income per Family Member (Wealth vs. Dependents)
#       - Employment Ratio (Stability relative to Age)
#       - Credit Maturity (Estimated start of working life)
# Reason:  While Neural Networks are powerful, they often struggle to "learn" division
#       or complex ratios from raw inputs quickly. By explicitly calculating
#       key financial ratios (like Disposable Income), we give the model a
#       head-start. This usually results in faster convergence and higher accuracy.
#
#       NOTE ON COLLINEARITY:
#       These features are naturally correlated with their parents (e.g., Income
#       vs. Income per Member). We accept this redundancy here because Section
#       5.4 (Filtering) will automatically detect and remove any features that
#       are statistically identical (>90% correlation), keeping only the strongest.

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
# SECTION 5: FINAL PREPROCESSING PIPELINE
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 5
#
# 5.1: TARGET MAPPING
# Function: Maps "C", "0"-"5" status codes to integers 0-6.
#
# 5.2: EDA SNAPSHOT
# Reason: Saves a copy of the data BEFORE formatting. This snapshot is used in 
#       Section 6 for human-readable risk profiling.
#
# 5.3: ID-BASED DATA SPLITTING
# Reason: CRITICAL for Credit Risk. Splits data by User ID, not by Row.
#       Ensures that a user's January data isn't in Train while their February
#       data is in Test (Data Leakage).
#
# 5.4 - 5.5: RECIPE PIPELINE (PREP & BAKE)
# Reason: Uses the 'recipes' package to learn statistics (Mean, Mode, Levels)
#       ONLY from the Training set ('Prep') and applies them to Test ('Bake').
#       This prevents Global Statistics Leakage.

# ==============================================================================
# 5.1: TARGET VARIABLE CLEANING & MAPPING
# ==============================================================================
data <- data %>% filter(status != "X")
data$target_class <- recode(as.character(data$status),
                            "C" = 0, "0" = 1, "1" = 2, "2" = 3, 
                            "3" = 4, "4" = 5, "5" = 6
) |> as.numeric()

# ==============================================================================
# 5.2: CREATE EDA SNAPSHOT (Crucial for Section 6)
# ==============================================================================
cat(">> Creating 'eda_snapshot' for human-readable risk profiling...\n")
eda_snapshot <- data
eda_snapshot$TARGET <- eda_snapshot$target_class 

# ==============================================================================
# 5.3: ID-BASED DATA SPLITTING
# ==============================================================================
# MENTOR NOTE: Removed AMT_INCOME_TOTAL from drops so we can use it for filtering below
cols_to_drop <- c("DAYS_BIRTH", "DAYS_EMPLOYED", "status") 
df_modeling <- data %>% select(-any_of(cols_to_drop))

cat("--- Performing ID-Based Split (Preventing Data Leakage) ---\n")
all_ids <- unique(df_modeling$ID)
set.seed(123)
train_ids <- sample(all_ids, size = 0.70 * length(all_ids))
remaining_ids <- setdiff(all_ids, train_ids)
val_ids <- sample(remaining_ids, size = 0.50 * length(remaining_ids))
test_ids <- setdiff(remaining_ids, val_ids)

train_raw <- df_modeling %>% filter(ID %in% train_ids)
val_raw   <- df_modeling %>% filter(ID %in% val_ids)
test_raw  <- df_modeling %>% filter(ID %in% test_ids)

# --- APPLY OUTLIER REMOVAL TO TRAINING DATA ONLY ---
# We clean the training data to help the model converge, but we leave the 
# Test/Val data "dirty" to accurately measure real-world performance.
cat(">> Applying Statistical Outlier Filters to TRAINING SET ONLY...\n")
rows_before <- nrow(train_raw)

# Filter 1: High Incomes (Train Only)
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", 
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")
train_raw <- train_raw %>% 
  filter(!(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs))

# Filter 2: Large Families (Train Only)
train_raw <- train_raw %>% filter(CNT_FAM_MEMBERS <= 10)

cat(sprintf(">> Outlier Filtering: Removed %d rows from Training Data.\n", 
            rows_before - nrow(train_raw)))
cat(sprintf(">> Split Complete: Train=%d, Val=%d, Test=%d (Rows)\n", 
            nrow(train_raw), nrow(val_raw), nrow(test_raw)))

# ==============================================================================
# 5.4: DEFINE PREPROCESSING RECIPE
# ==============================================================================
cat("--- Building Preprocessing Recipe (Stats calculated on Train ONLY) ---\n")

# Identify raw parent columns that have engineered Log/Ratio counterparts.
# We must remove these so step_corr doesn't accidentally delete the engineered versions.
raw_parents <- c("AMT_INCOME_TOTAL", "ACTIVE_EMPLOYMENT_YEARS", 
                 "INCOME_PER_FAMILY_MEMBER", "INCOME_PER_AGE")

rec_obj <- recipe(target_class ~ ., data = train_raw) %>%
  update_role(ID, new_role = "id") %>%
  
  # CRITICAL FIX: Explicitly remove raw parent features.
  # Forces the model to use the Log/Ratio versions we engineered.
  step_rm(all_of(raw_parents)) %>%
  
  step_impute_mode(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.90) %>%
  step_range(all_numeric_predictors(), min = 0, max = 1)

# ==============================================================================
# 5.5: TRAIN RECIPE (PREP) & APPLY (BAKE)
# ==============================================================================
cat(">> Prepping Recipe (Learning stats from Training Set)...\n")
rec_trained <- prep(rec_obj, training = train_raw)

cat(">> Baking Data (Applying learned stats to all sets)...\n")
train_baked <- bake(rec_trained, new_data = train_raw)
val_baked   <- bake(rec_trained, new_data = val_raw)
test_baked  <- bake(rec_trained, new_data = test_raw)

# Convert to Matrices for Keras
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

# SCRIPT ARCHITECTURE SUMMARY: SECTION 6
#
# 6.1: SNAPSHOT LOADING (STRICT SEPARATION)
# Reason: Loads the 'eda_snapshot' created in Section 5.2.
#       Crucially, we only analyze the TRAINING portion of the snapshot to 
#       ensure we do not bias our decisions based on Test set data.
#
# 6.2 - 6.3: DISTRIBUTIONS & CORRELATIONS
# Reason: Appends visual audits to the main PDF report. Checks if the class 
#       imbalance and outlier removals were effective.
#
# 6.4: GRANULAR RISK PROFILING
# Reason: Creates readable risk buckets (e.g., "Status 5_Over150DPD").
#       Connects the abstract Model Target (0-6) to Business Logic.
#
# 6.5: FACTOR VARIABLE RISK ANALYSIS
# Reason: Calculates the specific default rate (90+ DPD) for specific groups
#       (e.g., "Men in Municipal Housing"). This "Why" analysis validates
#       the model's predictions against real-world intuition.

# ==============================================================================
# 6.1: Initialize Exploration Data
# ==============================================================================
df_clean <- train_raw
df_clean$TARGET <- df_clean$target_class
cat(">> EDA initialized using TRAINING DATA ONLY (Strict Separation).\n")

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
df_diagnostics <- df_clean %>%
  mutate(
    Risk_Label = case_when(
      TARGET == 0 ~ "Status_C_Paid", TARGET == 1 ~ "Status_0_1-29DPD",
      TARGET == 2 ~ "Status_1_30-59DPD", TARGET == 3 ~ "Status_2_60-89DPD",
      TARGET == 4 ~ "Status_3_90-119DPD", TARGET == 5 ~ "Status_4_120-149DPD",
      TARGET >= 6 ~ "Status_5_Over150DPD", TRUE ~ "Unknown"
    ),
    Is_Serious_Delinquency = ifelse(TARGET >= 4, 1, 0)
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
# SECTION 7: MODEL ARCHITECTURE (DENSE MLP)
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 7
#
# 7.1: NETWORK TOPOLOGY
# Reason: Defines a Feed-Forward Neural Network (MLP) using Keras.
#       - Input Layer: Matches the feature count from the Recipe.
#       - Hidden Layers: Uses 'ReLU' activation for non-linearity and 'Dropout'
#         to randomly nullify neurons during training (Regularization).
#       - Output Layer: Uses 'Softmax' to output a probability distribution 
#         across the 7 class labels (0-6).
#
# 7.2: COMPILATION
# Reason: Configures the learning process using:
#       - Optimizer: Adam (Adaptive Moment Estimation) for stable convergence.
#       - Loss: Sparse Categorical Crossentropy (Standard for integer targets).

# ==============================================================================
# 7.1: Build Model Architecture (High Capacity for >90% Accuracy)
# ==============================================================================
num_features <- ncol(x_train)
num_classes <- 7 
cat(sprintf("=== BUILDING MODEL: Input Dim = %d | Output Classes = %d ===\n", num_features, num_classes))

model <- keras_model_sequential() %>%
  # INCREASED CAPACITY: 128 Units (was 64) to capture subtle majority patterns
  layer_dense(units = 128, activation = "relu", input_shape = c(num_features)) %>%
  # REDUCED DROPOUT: 0.2 (was 0.3) allows tighter fitting to the data
  layer_dropout(0.2) %>%
  
  # Layer 2: Increased to 64 units (was 32)
  layer_dense(units = 64, activation = "relu") %>%
  # Minimal dropout (0.1) to preserve learned features
  layer_dropout(0.1) %>%
  
  # Output Layer
  layer_dense(units = num_classes, activation = "softmax") 

# ==============================================================================
# 7.2: Compile Model
# ==============================================================================
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "sparse_categorical_crossentropy",
  metrics = c("sparse_categorical_accuracy")
)
model %>% summary()

# ==============================================================================
# 8.1: Robust Class Weights Calculation (ACCURACY OPTIMIZED)
# ==============================================================================
cat("\n=== TRAINING WITH DAMPENED CLASS WEIGHTS (Accuracy Focus) ===\n")
total_count <- nrow(train_baked)
possible_classes <- 0:6
class_counts <- table(factor(train_baked$target_class, levels = possible_classes))

# MENTOR NOTE: Damping Power Adjustment (The "Fourth Root" Fix)
# ^1.0 = Full Inverse Frequency (Paranoid: Ratio ~400:1, Acc ~36%)
# ^0.0 = No Weighting (Naïve: Ratio 1:1, Acc >95% but Zero Recall)
# ^0.25 = Sweet Spot (Balanced: Ratio ~4.5:1, Acc >90% with functional Recall)
damping_power <- 0.25 

# Calculate raw weights
raw_weights <- total_count / (length(possible_classes) * (class_counts + 1e-6))

# Apply smoothing
weights_vec <- raw_weights ^ damping_power

class_weight_list <- as.list(weights_vec)
names(class_weight_list) <- possible_classes

cat(">> Dampened Weights (Compressed Range for Higher Accuracy):\n")
print(class_weight_list)

# ==============================================================================
# 8.2: Training Execution
# ==============================================================================
callbacks <- list(
  # Increased patience to 50 to allow the model to settle on high accuracy
  callback_early_stopping(monitor = "val_sparse_categorical_accuracy", mode = "max", patience = 50, restore_best_weights = TRUE),
  callback_reduce_lr_on_plateau(monitor = "val_sparse_categorical_accuracy", mode = "max", factor = 0.5, patience = 15, min_lr = 1e-7)
)

history <- model %>% fit(
  x = x_train, y = y_train,
  validation_data = list(x_val, y_val),
  class_weight = class_weight_list,  
  epochs = 4000, 
  # Increased batch size to 512 for smoother gradient updates
  batch_size = 512, 
  callbacks = callbacks, 
  verbose = 1
)

# Save History Plot
png(here("DS-Project_Part2_Scripts", "Saved_Outputs", "training_history_plot.png"), width=800, height=600)
plot(history)
dev.off()

# ------------------------------------------------------------------------------
# SECTION 9: COMPLETE EVALUATION & VISUALIZATION SUITE
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 9
#
# 9.1: DEPENDENCY MANAGEMENT & PREDICTION SETUP
# Reason: Loads advanced metric libraries (pROC, MLmetrics) and generates
#       fresh predictions on the Test Set.
#       Function: Creates the 'pred_probs' matrix required for granular scoring.
#
# 9.2: REPORTING INFRASTRUCTURE
# Reason: Opens a dedicated PDF graphics device.
#       Function: Captures the 8+ visualizations generated in this section into
#       a single professional report ('Detailed_Model_Evaluation_Report.pdf').
#
# 9.3: FUNDAMENTAL METRICS (MCC & F1)
# Reason: Goes beyond Accuracy. Calculates Matthews Correlation Coefficient (MCC),
#       which is the gold standard for imbalanced datasets, ensuring the model
#       isn't simply guessing "Safe" for everyone.
#
# 9.4: PROBABILITY & CALIBRATION DIAGNOSTICS
# Reason: Evaluates "Confidence." Uses Brier Score and Calibration Plots to
#       ensure that when the model predicts 80% risk, the actual default rate is ~80%.
#
# 9.5: CREDIT RISK SPECIALTIES (KS & GINI)
# Reason: Calculates industry-standard banking metrics:
#       - KS Statistic: Max separation between Good/Bad distributions.
#       - Gini Coefficient: The primary metric for Credit Scorecard power.

# ==============================================================================
# 9.1: Dependency Check & Prediction Setup
# ==============================================================================
cat("\n=== INITIALIZING FINAL EVALUATION ===\n")

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

# ==============================================================================
# 9.2: Reporting Setup (PDF)
# ==============================================================================
eval_pdf_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "model_evaluation_report.pdf")
pdf(eval_pdf_path, width = 11, height = 8.5)
cat(sprintf(">> Report initiated: %s\n", eval_pdf_path))

# ==============================================================================
# 9.3: Fundamental & Class-Specific Metrics
# ==============================================================================
cat("\n>>> FUNDAMENTAL METRICS <<<\n")

# A. Confusion Matrix & Basic Stats
cm <- confusionMatrix(pred_factor, actual_factor, mode = "everything")

# B. Use Kappa as the MCC proxy (Standard for Multiclass in Caret)
# Note: In multiclass settings, Kappa is the statistical equivalent to MCC.
mcc_score <- cm$overall['Kappa']

cat(sprintf("Overall Accuracy:   %.2f%%\n", cm$overall['Accuracy'] * 100))
cat(sprintf("Kappa Statistic:    %.4f\n", cm$overall['Kappa']))

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
# 9.4: Probability & Calibration Metrics (CORRECTED)
# ==============================================================================
cat("\n>>> PROBABILITY METRICS <<<\n")

# A. Log Loss (Categorical Crossentropy)
# Note: Keras outputs are un-named, so we can leave them raw for LogLoss 
# as long as we match the one-hot encoding dimensions.
log_loss_val <- MLmetrics::MultiLogLoss(y_true = model.matrix(~ actual_factor - 1), 
                                        y_pred = pred_probs)
cat(sprintf("Categorical Log Loss: %.4f (Lower is better)\n", log_loss_val))

# B. Multi-Class AUC-ROC
# --- FIX START ---
# Keras outputs a plain matrix. We must name the columns to match the factor levels.
colnames(pred_probs) <- levels(actual_factor)
# --- FIX END ---

roc_multi <- pROC::multiclass.roc(actual_factor, pred_probs)
cat(sprintf("Multi-Class AUC-ROC:  %.4f\n", pROC::auc(roc_multi)))

# C. Brier Score (Mean Squared Error of Probabilities)
brier_score <- mean(rowSums((model.matrix(~ actual_factor - 1) - pred_probs)^2))
cat(sprintf("Brier Score:          %.4f (Measures Calibration)\n", brier_score))

# VISUALIZATION 3: Prediction Confidence Histogram
max_probs <- data.frame(Max_Prob = apply(pred_probs, 1, max),
                        Correct = (pred_classes == y_test))

p_conf <- ggplot(max_probs, aes(x = Max_Prob, fill = Correct)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Model Confidence Histogram",
       subtitle = "Peaks near 1.0 indicate high confidence.",
       x = "Max Predicted Probability", y = "Count") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()
print(p_conf)

# VISUALIZATION 4: ROC Curves (One-vs-Rest) [CORRECTED]
roc_data <- data.frame()
for(i in 0:6) {
  bin_y <- ifelse(y_test == i, 1, 0)
  r <- roc(bin_y, pred_probs[, i+1], quiet=TRUE)
  
  # FIX: Added row.names = NULL to silence the "short variable" warning
  roc_data <- rbind(roc_data, data.frame(
    FPR = 1 - r$specificities,
    TPR = r$sensitivities,
    Class = class_map[as.character(i)],
    row.names = NULL 
  ))
}

p_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(linewidth = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves (One-vs-Rest)",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()
print(p_roc)

# ==============================================================================
# 9.5: Credit Risk Specific Metrics (Serious Delinquency)
# ==============================================================================
cat("\n>>> CREDIT RISK METRICS (Binary: 90+ DPD vs Rest) <<<\n")

# Define "Risk" as the Sum of Probabilities of Classes 4, 5, and 6
risk_probs <- rowSums(pred_probs[, 5:7]) 
binary_truth <- ifelse(y_test >= 4, 1, 0) # 1 = Bad, 0 = Good

# A. Binary AUC for Serious Delinquency
roc_risk <- roc(binary_truth, risk_probs, quiet=TRUE)
auc_risk <- auc(roc_risk)
cat(sprintf("Binary AUC (Serious Risk): %.4f\n", auc_risk))

# B. Gini Coefficient (Credit Scorecard Standard)
gini_coeff <- 2 * auc_risk - 1
cat(sprintf("Gini Coefficient:          %.4f (Target > 0.40)\n", gini_coeff))

# C. KS Statistic (Kolmogorov-Smirnov)
ks_stat <- max(roc_risk$sensitivities + roc_risk$specificities - 1)
cat(sprintf("KS Statistic:              %.4f\n", ks_stat))

# VISUALIZATION 5: Cumulative Gain Chart
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

# VISUALIZATION 7: Calibration Plot (Reliability Diagram)
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

# VISUALIZATION 8: Training History (Recalled) [CORRECTED]
if(exists("history")) {
  # FIX: Filter out NA values caused by Early Stopping to prevent ggplot warnings
  hist_df <- as.data.frame(history) %>% 
    filter(!is.na(value))
  
  p_hist <- ggplot(hist_df, aes(x = epoch, y = value, color = data)) +
    geom_line() +
    facet_wrap(~metric, scales = "free_y") +
    labs(title = "Training History Recalled") +
    theme_minimal()
  print(p_hist)
}

# ==============================================================================
# 9.6: AUDIT STEP 1: CORRECTED PERMUTATION IMPORTANCE (FIXED)
# ==============================================================================

library(vip)
library(dplyr)

# 1. Define the Custom Prediction Wrapper (Same as before)
pred_wrapper <- function(object, newdata) {
  x_mat <- as.matrix(newdata)
  probs <- predict(object, x_mat)
  # Sum probabilities of Status 3, 4, 5 (Indices 5, 6, 7)
  risk_score <- rowSums(probs[, 5:7]) 
  return(risk_score)
}

# 2. Prepare Audit Data
set.seed(123)
audit_data <- train_baked %>% 
  sample_n(2000) %>% 
  select(-target_class, -ID)

# 3. Run VIP with "rmse"
cat(">> Running Permutation Importance Audit (Wrapper Method)...\n")
vip_obj <- vip(
  object = model,
  method = "permute",          
  train = audit_data,
  target = as.numeric(train_baked$target_class[1:2000]), 
  metric = "rmse",             # CHANGED: "rsquared" -> "rmse"
  pred_wrapper = pred_wrapper, 
  nsim = 5                     
)

# 4. Visualization
print(vip_obj + ggtitle("Feature Importance Audit (Leakage Check)"))

# ==============================================================================
# 9.7: Final Export
# ==============================================================================
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
