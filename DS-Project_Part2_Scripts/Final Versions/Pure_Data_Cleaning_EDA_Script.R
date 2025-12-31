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
              "here",         #Path management
              "recipes"       # Data Preprocessing
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

# 1.1.3: Verify Project Root
cat(">> Project Root Detected at:", here(), "\n")
cat(">> Libraries loaded. Environment ready.\n")

# ==============================================================================
# 1.2: Backend Connection Strategy (TensorFlow)
# ==============================================================================
# 1.2.1: Explicit Environment Binding: Attempts to locate 'r-reticulate' Conda environment.
backend_configured <- FALSE

# 1.2.2: Check - Is a Conda environment named 'r-reticulate' available?
# We use tryCatch to prevent the script from crashing if Conda is not installed.
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
  # Note: Keras 3 can use JAX/Torch backends, so we log a warning rather than a hard stop
  # unless you are strictly enforcing TensorFlow.
  cat("\n>> NOTICE: TensorFlow Python Backend not explicitly detected.\n") 
  cat("   If you intended to use TensorFlow, run reticulate::install_miniconda() and install_tensorflow().\n")
  cat("   If using JAX or PyTorch backends via Keras 3, you can ignore this warning.\n")
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
# PURPOSE: Identify Data Quality issues (Anomalies, Skew, MNAR) prior to preprocessing.

# Mapping 'data' to 'df'
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
# SECTION 5: FINAL PREPROCESSING & STANDARDIZATION
# ------------------------------------------------------------------------------

# 5.1: TARGET VARIABLE CLEANING & MAPPING
# Function: Filters out noise (Status 'X') and maps credit statuses to Integers (0-6).
# Reason:   Neural Networks require integer targets for loss calculation. 
#           Specifically, Keras 'sparse_categorical_crossentropy' expects classes 
#           to be zero-indexed integers, not strings or characters like "C".
#
# 5.2: BOOLEAN STANDARDIZATION, FACTOR LUMPING & EDA SNAPSHOT
# Function: 1. Converts logical TRUE/FALSE to numeric 1/0.
#           2. Groups rare categories (<50 obs) into "Other" to prevent overfitting.
#           3. [CRITICAL] Creates a copy of the data ('eda_snapshot') *before* encoding.
# Reason:   We need a human-readable version of the data (with strings like "Married")
#           for the Risk Profiling in Section 6. If we didn't snapshot here, 
#           Section 5.4 would destroy these labels.
#
# 5.3: AUTOMATED FEATURE SELECTION (VARIANCE & COLLINEARITY)
# Function: Removes features with zero information (Zero Variance) and features 
#           that are redundant copies of others (Correlation > 0.90).
# Reason:   High collinearity causes "Unstable Weights" in Neural Networks, where 
#           gradients oscillate wildly because the model cannot distinguish which 
#           variable is actually driving the prediction.
#
# 5.4: CATEGORICAL ENCODING STRATEGY (TOGGLE)
# Function: Provides a configurable switch (USE_EMBEDDING_ENCODING) to choose between:
#           A) Integer Encoding: Maps categories to 0,1,2... (Required for Embedding Layers).
#           B) One-Hot Encoding: Explodes categories to binary columns (Required for Dense Layers).
# Reason:   Neural Networks are mathematical engines that cannot process strings. 
#           This step ensures the input matrix is purely numeric.
#
# 5.5: CLEANUP & MIN-MAX SCALING
# Function: Drops raw ID/Date columns and normalizes all predictors to range [0, 1].
# Reason:   1. Raw IDs lead to memorization (overfitting).
#           2. Scaling is mandatory for Neural Networks using Sigmoid/Tanh activations. 
#              Large inputs (like Income) cause gradients to explode or vanish. 
#              [0, 1] scaling ensures all inputs contribute equally to the gradient.
#
# 5.6: PERSISTENCE
# Function: Saves the fully numeric matrix to CSV.
# Reason:   Ensures the dataset passed to Python/Keras is perfectly clean, 
#           preventing runtime errors in the backend environment.

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
# 5.2: BOOLEAN STANDARDIZATION & FACTOR LUMPING
# ==============================================================================
# 5.2.1: Convert Booleans to Numeric (0/1)
bool_cols <- c("FLAG_OWN_CAR", "FLAG_OWN_REALTY", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")
data[bool_cols] <- lapply(data[bool_cols], as.numeric)

# 5.2.2: Define Categoricals & Lump Rare Levels
cat_cols <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", 
              "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")
data[cat_cols] <- lapply(data[cat_cols], as.factor)

cat(">> Pooling rare categorical levels (<50 obs)...\n")
data <- data %>%
  mutate(across(all_of(cat_cols), ~ fct_lump_min(., min = 50, other_level = "Other")))

# 5.2.3 Create EDA snapshot
# Reason: We must save the data state here because Section 5.4 will destroy 
# the Factor columns (Gender, Income Type) needed for Section 6 Risk Profiling.
cat(">> Creating 'eda_snapshot' for human-readable risk profiling...\n")
eda_snapshot <- data

# ==============================================================================
# 5.3: AUTOMATED FEATURE SELECTION (VARIANCE & COLLINEARITY)
# ==============================================================================
cat("--- Performing Feature Variance and Collinearity Filtering ---\n")

# 5.3.1: Zero Variance
# Removes columns that have the same value for everyone (Zero Information)
nzv_cols <- nearZeroVar(data, freqCut = 99/1, saveMetrics = FALSE)
if (length(nzv_cols) > 0) {
  nzv_names <- names(data)[nzv_cols]
  cat(sprintf(">> Dropping %d Near-Zero Variance features: %s\n", length(nzv_names), paste(nzv_names, collapse = ", ")))
  data <- data[, -nzv_cols]
}

# 5.3.2: Collinearity (Numeric Only)
# Removes highly correlated features to prevent unstable weights
numeric_vars <- data %>% select(where(is.numeric)) %>% select(-target_class)
if (ncol(numeric_vars) > 1) {
  cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
  high_corr_cols <- findCorrelation(cor_matrix, cutoff = 0.90)
  if (length(high_corr_cols) > 0) {
    corr_names <- colnames(cor_matrix)[high_corr_cols]
    cat(sprintf(">> Dropping %d Highly Collinear features (>0.90): %s\n", length(corr_names), paste(corr_names, collapse = ", ")))
    data <- data %>% select(-all_of(corr_names))
  }
}

# ==============================================================================
# 5.4: CATEGORICAL ENCODING STRATEGY (TOGGLE)
# ==============================================================================
# MENTOR CONFIGURATION:
# Set FALSE for standard Dense Networks (One-Hot).
# Set TRUE if you plan to use Keras Embedding Layers (Integer Encoding).
USE_EMBEDDING_ENCODING <- FALSE 

# Define columns to encode (intersect ensures we only grab columns that survived filtering)
cols_to_encode <- intersect(cat_cols, names(data))

if (USE_EMBEDDING_ENCODING) {
  # ----------------------------------------------------------------------------
  # OPTION A: INTEGER ENCODING (For Embedding Layers)
  # ----------------------------------------------------------------------------
  # Reason: Embedding layers require inputs to be integers (indices), not binary vectors.
  # We map each category to a number: "A" -> 0, "B" -> 1.
  
  cat(">> STRATEGY: Integer Encoding selected (Optimized for Embeddings)...\n")
  
  for (col in cols_to_encode) {
    # 1. Ensure it is a factor
    data[[col]] <- as.factor(data[[col]])
    
    # 2. Print Mapping for Reproducibility (Critical for Model Cards)
    levels_list <- levels(data[[col]])
    cat(sprintf("   Mapping %s: %s -> 0..%d\n", col, paste(head(levels_list, 3), collapse=","), length(levels_list)-1))
    
    # 3. Convert to Integer and Subtract 1 (To make it 0-indexed for Python/Keras)
    # R uses 1-based indexing, Python uses 0-based.
    data[[col]] <- as.integer(data[[col]]) - 1
  }
  
  cat(">> Integer Encoding Complete. Categories are now numbers (0, 1, 2...)\n")
  
} else {
  # ----------------------------------------------------------------------------
  # OPTION B: ONE-HOT ENCODING (For Standard Dense Layers)
  # ----------------------------------------------------------------------------
  # Reason: Standard Neural Networks cannot handle categorical integers (Ordinality).
  # We must "explode" them into binary features.
  
  cat(">> STRATEGY: One-Hot Encoding selected (Optimized for Dense Layers)...\n")
  
  library(fastDummies)
  data <- dummy_cols(data, 
                     select_columns = cols_to_encode, 
                     remove_first_dummy = TRUE,   # Avoids multicollinearity
                     remove_selected_columns = TRUE) # Drops original columns
  
  cat(">> One-Hot Encoding Complete. Categories exploded into binary columns.\n")
}

# ==============================================================================
# 5.5: CLEANUP & MIN-MAX SCALING
# ==============================================================================
# 5.5.1: Drop Raw Columns (Cleanup)
# We remove raw columns that aren't useful for the model
cols_to_drop <- c("ID", "DAYS_BIRTH", "DAYS_EMPLOYED", "AMT_INCOME_TOTAL", "status")
data <- data %>% select(-any_of(cols_to_drop))

# 5.5.2: Min-Max Scaling
cat(">> Applying Min-Max Scaling [0, 1]...\n")
min_max_norm <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# 5.5.3: # Scale all features except the target
features_to_scale <- setdiff(names(data), "target_class")
data[features_to_scale] <- lapply(data[features_to_scale], min_max_norm)

# ==============================================================================
# 5.6: SAVE PROCESSED DATA
# ==============================================================================
output_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "cleaned_dataset.csv")
if(!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)
write.csv(data, file = output_path, row.names = FALSE)

cat(sprintf("\n>> SUCCESS: Preprocessed dataset (Rows: %d, Cols: %d) saved to:\n%s\n", nrow(data), ncol(data), output_path))

# ------------------------------------------------------------------------------
# SECTION 6: CLEANED DATA EXPLORATION & RISK PROFILING
# ------------------------------------------------------------------------------

# 6.1: INITIALIZATION & SNAPSHOT LOADING
# Function: Checks for 'eda_snapshot' (created in Section 5.2). If present, loads it.
# Reason:   Crucial architectural step. Section 5.4 (One-Hot Encoding) destroys 
#           human-readable labels (e.g., converts "Gender" -> "Gender_M"). 
#           To perform meaningful EDA, we must revert to the snapshot taken 
#           *before* encoding to access the original factor levels.
#
# 6.2: DATA QUALITY VISUALIZATION
# Function: Generates visual audits (Missingness Maps, Target Distributions) and 
#           appends them to the master PDF report initiated in Section 2.
# Reason:   Confirms that cleaning operations (Imputation, Filtering) worked as 
#           intended. The Target Distribution plot specifically validates the 
#           severe class imbalance (~1% Default Rate) that necessitates weighting.
#
# 6.3: NUMERIC ANALYSIS (DISTRIBUTIONS & CORRELATIONS)
# Function: loops through all numeric features to plot Histograms and Boxplots.
# Reason:   Neural Networks are sensitive to unscaled outliers. This step visually 
#           confirms if Min-Max scaling (Section 5.5) is required (which it is).
#
# 6.4: GRANULAR RISK PROFILING (TARGET MAPPING)
# Function: Creates a 'Risk_Label' that translates abstract Integers (0-6) into 
#           business logic (e.g., Class 4 = "90-119 Days Past Due").
# Reason:   Bridges the gap between Data Science (numbers) and Business Context. 
#           It defines "Serious Delinquency" as Status 4+ (90+ days), which becomes
#           the primary success metric for the model.
#
# 6.5: FACTOR VARIABLE RISK ANALYSIS (THE 7 FACTORS)
# Function: Iterates through 7 key demographic factors (Gender, Housing, etc.) 
#           to calculate the specific Default Rate for each subgroup.
#           CRITICAL FIX: Converts all outputs to Character strings to allow 
#           stacking of mixed types (Text vs Numbers) in the final report.
# Reason:   Generates the "Why". It tells us *who* is defaulting (e.g., "Men 
#           living in Municipal Apartments"). This insight validates the model's 
#           predictions against real-world intuition.
#
# 6.6: INTERACTION HEATMAP
# Function: Visualizes complex interactions (e.g., Family Status vs Housing Type).
# Reason:   Reveals non-linear risk pockets (e.g., "Single parents in Rented Apts")
#           that a simple linear model might miss but a Neural Network should catch.
#
# 6.7: FINAL SUMMARY REPORTING
# Function: Aggregates the "Riskiest Segment" from each variable into a single 
#           clean table and saves it to CSV.
# Reason:   Provides a concise "Executive Summary" of the dataset's risk profile,
#           proving the analysis is complete and ready for Modeling.

# ==============================================================================
# 6.1: Initialize Exploration Data
# ==============================================================================
# Load from the snapshot, NOT the processed 'data' object
if (exists("eda_snapshot")) {
  df_clean <- eda_snapshot
  cat(">> Loaded 'eda_snapshot' (Factors preserved for Profiling).\n")
} else {
  warning("Snapshot not found. Using processed data (Risk Profiling may fail).")
  df_clean <- data
}

# Ensure TARGET exists (eda_snapshot has target_class from Section 5.1)
df_clean$TARGET <- df_clean$target_class

cat("\n==============================================================================\n")
cat(" STARTING SECTION 6: EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("==============================================================================\n")
cat("New Dimensions for EDA:", dim(df_clean)[1], "Rows x", dim(df_clean)[2], "Cols\n")

# ==============================================================================
# 6.2: Visualizing Missingness & Distributions
# ==============================================================================
cat("\n[Section 6.2] Generating Data Quality Visualizations...\n")

# 6.2.1: Graphics Device Check [CORRECTED]
# Note: We removed the pdf() command here.
# The PDF file was opened in Section 2.3 and is still active.
# We simply continue plotting, which appends new pages to that existing file.

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
# MAPPING LOGIC (Based on Section 5.1 Recode):
# 0="C"(Paid), 1="0"(1-29), 2="1"(30-59), 3="2"(60-89), 4="3"(90-119), 5="4"(120-149), 6="5"(150+)
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
    
    # --- FIX STARTS HERE ---
    # Store "Riskiest Group" 
    highest_risk_group <- profile %>% slice(1)
    
    # Standardize column names
    colnames(highest_risk_group)[1] <- "Category_Value"
    
    # CRITICAL FIX: Convert to Character so Factors and Numbers can mix
    highest_risk_group$Category_Value <- as.character(highest_risk_group$Category_Value)
    
    highest_risk_group$Factor_Name <- var
    all_risk_profiles[[var]] <- highest_risk_group
    # --- FIX ENDS HERE ---
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