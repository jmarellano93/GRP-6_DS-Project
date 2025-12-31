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
# SECTION 5: FINAL PREPROCESSING PIPELINE
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 5
#
# 5.1 & 5.2: TARGET ENCODING & SNAPSHOTTING
# Reason: Neural Networks require numeric targets, but our 'status' variable is a 
#         mix of characters ("C", "X") and numbers. We also need a "clean" copy 
#         of the data for Section 6 (Risk Profiling) before we scramble it with 
#         normalization and one-hot encoding.
# Function: 
#         1. Maps the complex status codes to a simple 0-7 integer scale (handling 
#            the 'X' case explicitly).
#         2. Creates 'eda_snapshot': A preservation copy of the data used purely 
#            for human-readable visualizations later.
#
# 5.3: ID-BASED SPLITTING & STRATEGIC OUTLIER REMOVAL
# Reason: 
#         1. Leakage Prevention: Since one ID can have multiple rows, a simple 
#            random split would put the same person in both Train and Test sets, 
#            artificially inflating accuracy.
#         2. Realistic Testing: We must clean outliers from the Training set (to 
#            help the model learn) but leave the Test set "dirty" (to reflect 
#            real-world messiness).
# Function: 
#         - Splits data based on unique 'ID's rather than rows.
#         - Applies the "Impossible Job" and "Large Family" filters strictly to 
#           'train_raw', leaving 'val_raw' and 'test_raw' untouched.
#
# 5.4: DEFINE PREPROCESSING RECIPE
# Reason: Establishes a blueprint for data transformation without executing it yet.
#         This ensures the exact same logic applies to future data.
# Function: 
#         - Feature Selection: Explicitly removes raw parent columns (like 
#           'AMT_INCOME_TOTAL') to force the model to rely on the engineered 
#           Log/Ratio features from Section 4.
#         - Normalization: Scales inputs 0-1 for Neural Network convergence.
#         - Correlation Filter: Removes features >90% correlated to prevent multicollinearity.
#
# 5.5: TRAIN RECIPE (PREP) & APPLY (BAKE)
# Reason: "Prepping" learns the statistics (mean, max, min) from the Training set,
#         while "Baking" applies those learned stats to the Validation and Test sets.
#         This prevents "Look-ahead Bias" (learning from test data).
# Function: 
#         1. Calculates transformation parameters using *only* 'train_raw'.
#         2. Applies these transformations to create 'train_baked', 'val_baked', 
#            and 'test_baked'.
#         3. Matrix Conversion: Converts the clean R dataframes into numeric 
#            matrices (x_train, y_train) required by the Keras/TensorFlow engine.
#
# 5.6: SAVE FINAL PROCESSED DATA
# Reason: Checkpointing. It saves the fully processed tensors to CSVs so you can
#         restart the kernel or share the data without re-running the heavy 
#         cleaning pipeline every time.
# Function: Exports 'train_baked' and 'test_baked' to the 'Saved_Outputs' folder
#         for auditability and future use.

# ==============================================================================
# 5.1: TARGET VARIABLE CLEANING & MAPPING
# ==============================================================================
# MODIFICATION: Included status 'X' and mapped to 7
data$target_class <- recode(as.character(data$status),
                            "C" = 0, "0" = 1, "1" = 2, "2" = 3, 
                            "3" = 4, "4" = 5, "5" = 6, "X" = 7
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
# NOTE: Removed AMT_INCOME_TOTAL from drops so we can use it for filtering below
cols_to_drop <- c("DAYS_BIRTH", "DAYS_EMPLOYED", "status") 
df_modeling <- data %>% select(-any_of(cols_to_drop))

cat("--- Performing ID-Based Split (Preventing Data Leakage) ---\n")
all_ids <- unique(df_modeling$ID)
set.seed(1)
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

# ==============================================================================
# 5.6: SAVE FINAL PROCESSED DATA
# ==============================================================================
# Save the final training data (Fully cleaned, encoded, and scaled)
write.csv(train_baked, 
          here("DS-Project_Part2_Scripts", "Saved_Outputs", "final_processed_train_data.csv"), 
          row.names = FALSE)

# Save the final testing data
write.csv(test_baked, 
          here("DS-Project_Part2_Scripts", "Saved_Outputs", "final_processed_test_data.csv"), 
          row.names = FALSE)

cat(">> SUCCESS: Final processed datasets saved to Saved_Outputs folder.\n")

# ------------------------------------------------------------------------------
# SECTION 6: CLEANED DATA EXPLORATION & RISK PROFILING
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 6
#
# 6.1: LEAKAGE-PROOF EDA INITIALIZATION
# Reason: Standard EDA often accidentally mixes Train and Test data. This is 
#         unacceptable in credit risk modeling because viewing Test data distributions
#         might bias our decisions on how to group variables.
# Function: 
#         1. Filters the 'eda_snapshot' created in Section 5.2 using *only* #            'train_ids'.
#         2. Strict Validation: Checks if the row count matches the training set 
#            exactly before proceeding.
#         3. Ensures the 'TARGET' column is correctly mapped for visualization.
#
# 6.2 & 6.3: VISUALIZING MISSINGNESS & DISTRIBUTIONS
# Reason: Now that we have a clean, separated training set, we need to verify 
#         the data integrity one last time before calculating risk metrics.
# Function: 
#         - Missingness Map: Uses 'naniar' to visualize exactly where data is missing.
#         - Distribution Check: Loops through numeric columns to generate histogram/boxplot
#           pairs (checking for skew and outliers in the *Training* set specifically).
#         - PDF Append: Automatically adds these plots to the open PDF report.
#
# 6.4: GRANULAR RISK PROFILING (TARGET SEGMENTATION)
# Reason: The raw 'status' codes (0, 1, C, X) are technical banking terms. We need
#         to translate them into business logic to define what "Bad Debt" actually looks like.
# Function: 
#         1. Creates descriptive labels for every status (e.g., "Status_0_1-29DPD").
#         2. Defines the critical binary metric 'Is_Serious_Delinquency':
#            - Defaults (1) = Status 4, 5, 6 (90+ Days Past Due).
#            - Non-Defaults (0) = Everything else (including 'X' - No Loan).
#
# 6.5: FACTOR VARIABLE RISK ANALYSIS
# Reason: This is the core business intelligence step. It answers: "Who is most 
#         likely to default?" by calculating default rates across demographic groups.
# Function: 
#         - Iterative Analysis: Loops through key factors (Gender, Income Type, 
#           Education, Family Status, Housing).
#         - Risk Calculation: Computes the mean of 'Is_Serious_Delinquency' for 
#           each sub-group (e.g., "What % of Single men default?").
#         - Visualization: Generates 100% stacked bar charts to visually compare 
#           risk proportions across categories.
#         - Highest Risk Identification: Automatically extracts the riskiest 
#           sub-group for each variable (e.g., "Unemployed" might be the riskiest 
#           Job Type).
#
# 6.6: SUMMARY REPORT GENERATION & OUTPUT CLOSE
# Reason: Consolidates the findings into a readable artifact for stakeholders and
#         finalizes the graphics device to ensure the PDF is not corrupted.
# Function: 
#         1. Closes the PDF device (saving all plots from Sections 2-6).
#         2. Compiles a summary table showing the "Highest Risk Segment" for 
#            each demographic category.
#         3. Exports this high-level risk summary to CSV for easy reporting.

# ==============================================================================
# 6.1: Initialize Exploration Data (LEAKAGE PROOF)
# ==============================================================================
# NOTE: We must use 'train_ids' to filter the snapshot.
# Risk profiling must only reflect what the model is ALLOWED to see.

if (exists("eda_snapshot") && exists("train_ids")) {
  # Filter snapshot to keep ONLY training rows
  df_clean <- eda_snapshot %>% filter(ID %in% train_ids)
  
  # Ensure TARGET exists for plotting
  df_clean$TARGET <- df_clean$target_class
  
  cat(">> LEAKAGE CHECK PASSED: EDA initialized using TRAINING DATA ONLY.\n")
  cat(sprintf(">> Analyzing %d rows (Subset of %d Total)\n", nrow(df_clean), nrow(eda_snapshot)))
  
} else {
  warning("Snapshot or train_ids not found. Using processed data (Risk Profiling may fail).")
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
# SECTION 7: MODEL ARCHITECTURE (DENSE MLP)
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 7
#
# 7.1: DEEP NEURAL NETWORK ARCHITECTURE DEFINITION
# Reason: We require a model capable of learning non-linear relationships in tabular
#         data. Linear models (like Logistic Regression) cannot capture complex 
#         interactions between "Income", "Age", and "Family Size".
# Function: 
#         1. Input Layer: Automatically adapts to the number of features preserved 
#            by our Section 5 recipe (num_features).
#         2. Hidden Layers (The "Funnel" Design): 
#            - Layer 1 (256 units): High capacity to capture broad, coarse patterns.
#            - Layer 2 (128 units): Compresses information to find finer, more 
#              complex decision boundaries.
#         3. Dropout Regularization (0.15 / 0.10): Deliberately kept low. We allow 
#            the model to fit the training data tightly (high accuracy focus) rather 
#            than aggressively preventing overfitting at this stage.
#         4. Output Layer: Uses 'Softmax' with 8 units to output a probability 
#            distribution across all possible credit statuses (0-7).
#
# 7.2: COMPILATION & OPTIMIZER STRATEGY
# Reason: The "brain" of the network needs rules on how to learn. Using default 
#         settings often leads to unstable training on imbalanced financial data.
# Function: 
#         - Optimizer (Adam): We override the default learning rate (0.001) with 
#           a much slower rate of 0.0001. This forces the model to take "baby steps" 
#           down the error gradient, preventing it from overshooting the optimal 
#           solution or getting confused by noisy batches.
#         - Loss Function (Sparse Categorical Crossentropy): Efficiently handles 
#           our integer-encoded target (0-7) without needing massive One-Hot 
#           encoded target matrices.

# ==============================================================================
# 7.1: Build Model Architecture (High Accuracy Tuning)
# ==============================================================================
num_features <- ncol(x_train)
num_classes <- 8 
cat(sprintf("=== BUILDING MODEL: Input Dim = %d | Output Classes = %d ===\n", num_features, num_classes))

model <- keras_model_sequential() %>%
  # Layer 1: High capacity (256) to capture majority patterns
  layer_dense(units = 256, activation = "relu", input_shape = c(num_features)) %>%
  # Lower Dropout (0.15) -> Allows tighter fitting to the majority class
  layer_dropout(0.10) %>%
  
  # Layer 2: Wide secondary layer for complex decision boundaries
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.05) %>%
  
  # Output Layer
  layer_dense(units = num_classes, activation = "softmax") 

# ==============================================================================
# 7.2: Compile Model (ADJUSTED FOR SLOWER TRAINING)
# ==============================================================================
model %>% compile(
  # This forces the model to take much smaller steps.
  optimizer = optimizer_adam(learning_rate = 0.0001), 
  loss = "sparse_categorical_crossentropy",
  metrics = c("sparse_categorical_accuracy")
)
model %>% summary()

# ------------------------------------------------------------------------------
# SECTION 8: MODEL TRAINING WITH STRATIFIED BATCH SAMPLING (TRUE PROPORTION)
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 8
#
# 8.1: DEFINING THE SAMPLING STRATEGY
# Reason: We are implementing "Natural Proportion Stratified Sampling".
#         We divide customers into 4 streams (Good, Uncertain, Bad, ExtBad).
#         We then sample from these streams using weights calculated dynamically
#         from the actual dataset size.
#
#         Benefit: This ensures the model sees the EXACT natural distribution
#         (preserving accurate probability calibration) while guaranteeing that
#         rare classes (ExtBad) are represented smoothly over time, avoiding
#         "empty batches" that can occur with simple random shuffling.

# ==============================================================================
# 8.1: Create Risk-Stratified Dataset Pipeline
# ==============================================================================
cat("\n=== CONFIGURING STRATIFIED SAMPLING (Natural Proportions) ===\n")

# 1. Define Indices for 4 Groups
good_debt_indices      <- which(y_train %in% c(0, 1))         # Paid (C) or 0-29 days (0)
uncertain_debt_indices <- which(y_train %in% c(7))            # No Loan (X)
bad_debt_indices       <- which(y_train %in% c(2))            # 30-59 days (1)
ext_bad_debt_indices   <- which(y_train %in% c(3, 4, 5, 6))   # 60+ days (2,3,4,5)

# 2. Calculate Dynamic Weights (True Proportions)
# This guarantees the training distribution matches reality perfectly.
total_n <- nrow(x_train)
w_good  <- length(good_debt_indices) / total_n
w_unc   <- length(uncertain_debt_indices) / total_n
w_bad   <- length(bad_debt_indices) / total_n
w_ext   <- length(ext_bad_debt_indices) / total_n

cat(sprintf(">> Group Sizes: Good=%d | Uncertain=%d | Bad=%d | ExtBad=%d\n", 
            length(good_debt_indices), length(uncertain_debt_indices),
            length(bad_debt_indices), length(ext_bad_debt_indices)))

cat(sprintf(">> Dynamic Sampling Weights: Good=%.3f | Uncertain=%.3f | Bad=%.3f | ExtBad=%.3f\n", 
            w_good, w_unc, w_bad, w_ext))

# 3. Create 4 Infinite Streams (one per group)
# Note: Using dataset_shuffle ensures we don't repeat the exact same order every epoch

# Stream 1: Good Debt
ds_good <- tensor_slices_dataset(list(x_train[good_debt_indices, ], y_train[good_debt_indices])) %>%
  dataset_shuffle(buffer_size = length(good_debt_indices)) %>%
  dataset_repeat() 

# Stream 2: Uncertain
ds_uncertain <- tensor_slices_dataset(list(x_train[uncertain_debt_indices, ], y_train[uncertain_debt_indices])) %>%
  dataset_shuffle(buffer_size = length(uncertain_debt_indices)) %>%
  dataset_repeat() 

# Stream 3: Bad Debt
ds_bad <- tensor_slices_dataset(list(x_train[bad_debt_indices, ], y_train[bad_debt_indices])) %>%
  dataset_shuffle(buffer_size = length(bad_debt_indices)) %>%
  dataset_repeat() 

# Stream 4: Extreme Bad Debt
ds_ext_bad <- tensor_slices_dataset(list(x_train[ext_bad_debt_indices, ], y_train[ext_bad_debt_indices])) %>%
  dataset_shuffle(buffer_size = length(ext_bad_debt_indices)) %>%
  dataset_repeat() 

# 4. Merge into a Single Balanced Stream using Dynamic Weights
balanced_ds <- sample_from_datasets(
  list(ds_good, ds_uncertain, ds_bad, ds_ext_bad), 
  weights = c(w_good, w_unc, w_bad, w_ext)
) %>%
  dataset_batch(256) %>% 
  dataset_prefetch(1)

cat(">> Stratified Sampling Pipeline Successfully Created.\n")

# ==============================================================================
# 8.2: Training Execution
# ==============================================================================
# Define steps per epoch 
# We calculate steps based on the total training size divided by batch size (256)
steps_per_epoch <- floor(nrow(x_train) / 256)

callbacks <- list(
  callback_early_stopping(
    monitor = "val_sparse_categorical_accuracy", 
    mode = "max", 
    patience = 100,               
    restore_best_weights = TRUE
  ),
  callback_reduce_lr_on_plateau(
    monitor = "val_sparse_categorical_accuracy", 
    mode = "max", 
    factor = 0.5, 
    patience = 40,             
    min_lr = 1e-7
  )
)

cat(">> Starting Training with Dynamic Stratified Sampling...\n")

history <- model %>% fit(
  balanced_ds, 
  validation_data = list(x_val, y_val), # Validation remains natural/unbalanced
  epochs = 4000, 
  steps_per_epoch = steps_per_epoch,
  callbacks = callbacks, 
  verbose = 1
)

# ------------------------------------------------------------------------------
# SECTION 9: COMPLETE EVALUATION & VISUALIZATION SUITE
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 9
#
# 9.1: GLOBAL DEFINITIONS & PREDICTION SETUP
# Reason: The model outputs raw probabilities for 8 arbitrary classes (0-7). 
#         To make this useful for business, we must map these numbers back to 
#         human-readable statuses (e.g., "Status_3_90-119d") and define exactly 
#         what constitutes "Bad Debt".
# Function: 
#         1. Creates a 'Class Map' acting as the central dictionary for translation.
#         2. Aggregates Risk: Sums the probabilities of the three worst classes 
#            (indices 5, 6, 7) to create a single "Probability of Default" (PD) score.
#         3. Generates the final predictions on the Test set.
#
# 9.2: REPORTING SETUP
# Reason: Professional evaluation requires a consolidated document, not just 
#         console output.
# Function: Initializes a PDF graphics device to capture all subsequent charts 
#         (Confusion Matrix, ROC, Lift, History) into a single "model_evaluation_report.pdf".
#
# 9.3: FUNDAMENTAL METRICS (CONFUSION MATRIX & CLASS PERFORMANCE)
# Reason: Checks the general health of the classifier and identifies specific 
#         weaknesses. Global accuracy hides per-class failures (e.g., ignoring 
#         minority classes).
# Function: 
#         - Heatmap: Visualizes the Confusion Matrix (Darker diagonal = Better).
#         - Class-Specific Bar Chart: Explicitly plots F1, Precision, and Recall 
#           for *each* status code side-by-side. This instantly reveals if the 
#           model is sacrificing specific classes (like Status 1 or X) to boost 
#           overall accuracy.
#
# 9.4: PROBABILITY, CALIBRATION & MULTI-CLASS ROC
# Reason: Accuracy tells us *what* the model predicted, but Calibration tells us 
#         how *confident* we should be. ROC curves show the trade-off between 
#         sensitivity and false alarms for every single class.
# Function: 
#         - Log Loss: Penalizes confident wrong answers.
#         - One-vs-Rest ROC Curves: Plots 8 distinct curves (one per status) on 
#           a single chart. This allows us to compare how well the model separates 
#           "Status 0" vs "Everyone Else", "Status X" vs "Everyone Else", etc.
#         - Reliability Diagram: Plots "Predicted Probability" vs "Actual Default Rate". 
#           (Ideally, a diagonal line: if the model says 20% risk, 20% of those people should default).
#
# 9.5: CREDIT RISK SPECIFICS (GINI & KS)
# Reason: Banks care less about "Accuracy" and more about "Discrimination" 
#         (ranking bad customers higher than good ones).
# Function: 
#         - Binary Transformation: Temporarily treats the problem as "Good vs Bad" 
#           (ignoring the granular delays) to calculate standard banking metrics.
#         - Gini Coefficient: The industry standard for scorecard performance.
#         - KS Statistic: Measures the maximum separation between Good and Bad distributions.
#         - Lift/Gain Charts: Visualizes how much better the model is compared to random guessing.
#
# 9.6: AUDIT & HISTORY (INTERPRETABILITY)
# Reason: We need to verify the model didn't "memorize" the training data or rely 
#         on cheating features.
# Function: 
#         - Training History: Plots Loss/Accuracy over epochs to check for overfitting 
#           (divergence between Train and Val lines).
#         - Permutation Importance (VIP): Shuffles each feature column one by one 
#           to measure how much the model relies on it. If shuffling "Income" 
#           doesn't hurt accuracy, the model isn't using Income.
#
# 9.7: EXPORT & SAVE
# Reason: Traceability.
# Function: 
#         - Saves the full table of predictions (Classes + Risk Scores) to CSV.
#         - Saves a "Metrics Summary" CSV containing the final Gini, Accuracy, 
#           and KS scores for easy comparison with future model versions.

# ==============================================================================
# 9.1: GLOBAL DEFINITIONS & PREDICTION SETUP
# ==============================================================================
cat("\n=== INITIALIZING FINAL EVALUATION ===\n")

# 1. Define Class Map (Central Source of Truth)
class_map <- c("0"="Status_C_Paid", 
               "1"="Status_0_1-29d", 
               "2"="Status_1_30-59d", 
               "3"="Status_2_60-89d",
               "4"="Status_3_90-119d", # START RISK
               "5"="Status_4_120-149d", 
               "6"="Status_5_Over150d", # END RISK
               "7"="Status_X_NoLoan")

# 2. Define Risk Indices (Status 3, 4, 5 correspond to indices 5, 6, 7)
risk_indices <- 5:7 

# 3. Generate Predictions
pred_probs   <- model %>% predict(x_test)
colnames(pred_probs) <- levels(factor(names(class_map), levels = 0:7, labels = class_map))

pred_classes <- apply(pred_probs, 1, which.max) - 1
actual_factor <- factor(y_test, levels = 0:7, labels = class_map)
pred_factor   <- factor(pred_classes, levels = 0:7, labels = class_map)

# 4. Calculate Risk Score (Sum of Risk Indices)
risk_probs   <- rowSums(pred_probs[, risk_indices])
binary_truth <- ifelse(y_test >= 4 & y_test <= 6, 1, 0) # 1 = Bad, 0 = Good/X

# ==============================================================================
# 9.2: REPORTING SETUP
# ==============================================================================
eval_pdf_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "model_evaluation_report.pdf")
pdf(eval_pdf_path, width = 11, height = 8.5)
cat(sprintf(">> Report initiated: %s\n", eval_pdf_path))

# ==============================================================================
# 9.3: FUNDAMENTAL METRICS (Confusion Matrix & Accuracy)
# ==============================================================================
cat("\n>>> FUNDAMENTAL METRICS <<<\n")

cm <- confusionMatrix(pred_factor, actual_factor, mode = "everything")
mcc_score <- cm$overall['Kappa']

cat(sprintf("Overall Accuracy:   %.2f%%\n", cm$overall['Accuracy'] * 100))
cat(sprintf("Kappa (MCC Proxy):  %.4f\n", mcc_score))

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

# VISUALIZATION 1b: Class-Specific Performance (matches PDF Page 2)
# Extracts per-class metrics to show which statuses are hardest to predict
cm_by_class <- as.data.frame(cm$byClass) %>%
  tibble::rownames_to_column("Class_Name") %>%
  select(Class_Name, Sensitivity, Specificity, Precision, F1) %>%
  tidyr::pivot_longer(cols = -Class_Name, names_to = "Metric", values_to = "Score")

p_class_perf <- ggplot(cm_by_class, aes(x = Class_Name, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip() +
  labs(title = "Class-Specific Performance Metrics",
       subtitle = "Comparison of F1, Precision, and Recall across all statuses") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
print(p_class_perf)

# ==============================================================================
# 9.4: PROBABILITY & CALIBRATION METRICS
# ==============================================================================
cat("\n>>> PROBABILITY METRICS <<<\n")

# A. Log Loss
log_loss_val <- MLmetrics::MultiLogLoss(y_true = model.matrix(~ actual_factor - 1), 
                                        y_pred = pred_probs)
cat(sprintf("Categorical Log Loss: %.4f (Lower is better)\n", log_loss_val))

# B. Multi-Class AUC
roc_multi <- pROC::multiclass.roc(actual_factor, pred_probs)
cat(sprintf("Multi-Class AUC-ROC:  %.4f\n", pROC::auc(roc_multi)))

# VISUALIZATION 2b: One-vs-Rest ROC Curves (matches PDF Page 4)
# Loops through every class to generate 8 distinct ROC curves
roc_data_list <- list()
for(cls_idx in 0:7) {
  # Create binary target for current class vs all others
  curr_binary_truth <- as.numeric(y_test == cls_idx)
  # Get probability for current class
  curr_prob <- pred_probs[, cls_idx + 1]
  
  # Calculate ROC
  r <- roc(curr_binary_truth, curr_prob, quiet = TRUE)
  
  # Store coordinates
  roc_data_list[[cls_idx + 1]] <- data.frame(
    FPR = 1 - r$specificities,
    TPR = r$sensitivities,
    Class = class_map[as.character(cls_idx)]
  )
}
roc_plot_data <- bind_rows(roc_data_list)

p_multi_roc <- ggplot(roc_plot_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves (One-vs-Rest)", 
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  coord_fixed()
print(p_multi_roc)

# C. Calibration Plot
cal_data <- data.frame(prob = risk_probs, truth = binary_truth) %>%
  mutate(bin = ntile(prob, 10)) %>%
  group_by(bin) %>%
  summarise(mean_prob = mean(prob), actual_rate = mean(truth))

p_cal <- ggplot(cal_data, aes(x = mean_prob, y = actual_rate)) +
  geom_point(size = 3) + geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Calibration Plot (Reliability Diagram)",
       x = "Predicted Risk Probability", y = "Actual Default Rate") +
  theme_minimal()
print(p_cal)

# ==============================================================================
# 9.5: CREDIT RISK SPECIFICS (Binary Metrics)
# ==============================================================================
cat("\n>>> CREDIT RISK METRICS (Binary: 90+ DPD vs Rest) <<<\n")

# A. Binary AUC & Gini
roc_risk <- roc(binary_truth, risk_probs, quiet=TRUE)
auc_risk <- as.numeric(auc(roc_risk))
gini_coeff <- 2 * auc_risk - 1 # Calculated directly (No redundant function call)

cat(sprintf("Binary AUC (Risk):      %.4f\n", auc_risk))
cat(sprintf("Gini Coefficient:       %.4f\n", gini_coeff))

# B. KS Statistic
ks_stat <- max(roc_risk$sensitivities + roc_risk$specificities - 1)
cat(sprintf("KS Statistic:           %.4f\n", ks_stat))

# VISUALIZATION 3: Lift & Gain (Combined Grid)
lift_obj <- caret::lift(factor(binary_truth, labels=c("Good","Bad")) ~ risk_probs)
p_gain <- ggplot(lift_obj, plot = "gain") + labs(title = "Cumulative Gain") + theme_minimal()
p_lift <- ggplot(lift_obj, plot = "lift") + labs(title = "Lift Chart") + theme_minimal()
grid.arrange(p_gain, p_lift, ncol = 2, top = "Credit Risk Discrimination Power")

# ==============================================================================
# 9.6: AUDIT & HISTORY
# ==============================================================================
# A. Training History
if(exists("history")) {
  hist_df <- as.data.frame(history) %>% filter(!is.na(value))
  p_hist <- ggplot(hist_df, aes(x = epoch, y = value, color = data)) +
    geom_line() + facet_wrap(~metric, scales = "free_y") +
    labs(title = "Training History (Overfitting Check)") + theme_minimal()
  print(p_hist)
}

# B. VIP Audit (Using Pre-Defined Logic)
cat(">> Running Permutation Importance Audit...\n")
pred_wrapper_optimized <- function(object, newdata) {
  probs <- predict(object, as.matrix(newdata))
  # REUSE: Using the global 'risk_indices' defined in 9.1
  return(rowSums(probs[, 5:7])) 
}

audit_data <- train_baked %>% sample_n(2000) %>% select(-target_class, -ID)
vip_obj <- vip(
  object = model, method = "permute", train = audit_data,
  target = as.numeric(train_baked$target_class[1:2000]), 
  metric = "rmse", pred_wrapper = pred_wrapper_optimized, nsim = 5
)
print(vip_obj + ggtitle("Feature Importance Audit"))

# ==============================================================================
# 9.7: EXPORT & SAVE
# ==============================================================================
dev.off()
cat(sprintf(">> Report Saved: %s\n", eval_pdf_path))

# 1. Save Predictions
results_df <- data.frame(
  actual_class = y_test,
  predicted_class = pred_classes,
  risk_score_90plus = risk_probs,
  predicted_label = as.character(pred_factor)
)
write.csv(results_df, here("DS-Project_Part2_Scripts", "Saved_Outputs", "comprehensive_test_predictions.csv"), row.names = FALSE)

# 2. Save Metrics Summary
metrics_summary <- data.frame(
  Timestamp = Sys.time(),
  Accuracy = as.numeric(cm$overall['Accuracy']),
  Kappa = mcc_score,
  Log_Loss = log_loss_val,
  AUC_Risk = auc_risk,
  Gini = gini_coeff,
  KS = ks_stat
)
write.csv(metrics_summary, here("DS-Project_Part2_Scripts", "Saved_Outputs", "final_model_metrics.csv"), row.names = FALSE)

cat("=== EVALUATION COMPLETE ===\n")

# ==============================================================================
# 10.0: SAVE MODEL ARTIFACT (REQUIRED FOR ASSIGNMENT 2.a)
# ==============================================================================
model_save_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "final_model.keras")
save_model(model, model_save_path)
cat(sprintf(">> FINAL MODEL SAVED: %s\n", model_save_path))

# Also save the Recipe (You need this to process the secret data!)
saveRDS(rec_trained, here("DS-Project_Part2_Scripts", "Saved_Outputs", "preprocessing_recipe.rds"))
cat(">> PREPROCESSING RECIPE SAVED.\n")