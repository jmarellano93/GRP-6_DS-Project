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

# ==============================================================================
# SECTION 5: FINAL PRE-PROCESSING (PRESERVE FACTORS)
# ==============================================================================
# MENTOR NOTE: We now PRESERVE categorical variables as Factors.
# We do NOT manually encode them here. The 'recipes' pipeline in Section 7 
# will handle the integer encoding automatically for the Neural Network.

cat("--- Finalizing Data Structure (Preserving Factors) ---\n")

# 5.1 Target Variable Encoding
# ----------------------------
# We retain 'target_class' for the Neural Network, but we do not drop
# the predictors yet.

data$status <- as.character(data$status)

# Map status levels to class IDs 0..7
data$target_class <- recode(data$status,
                            "C" = 0, "X" = 1, "0" = 2, "1" = 3,
                            "2" = 4, "3" = 5, "4" = 6, "5" = 7
) |> as.numeric()

# [QA 5.1] Target Encoding Check
cat("[QA 5.1] Verifying Target Encoding... ")
if(any(data$status == "X") && any(data$target_class == 1)) {
  cat("PASS: Status 'X' encoded as class 1.\n")
} else {
  cat("WARNING: Status 'X' not found or not encoded correctly.\n")
}

# 5.2 Boolean Standardization & Factor Enforcement
# ------------------------------------------------
# 1. Convert Booleans to Numeric (0/1) - Safe for all model types
bool_cols <- c("FLAG_OWN_CAR", "FLAG_OWN_REALTY", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")
data[bool_cols] <- lapply(data[bool_cols], as.numeric)

# 2. Ensure Categoricals are Factors
# This is crucial for 'rpart' (EDA) and 'recipes' (Modeling) to recognize them correctly.
cat_cols <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", 
              "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")

# Apply factor conversion safely
data[cat_cols] <- lapply(data[cat_cols], as.factor)

# [QA 5.2] Factor Check
cat("[QA 5.2] Verifying Factor Retention... ")
if(is.factor(data$NAME_EDUCATION_TYPE)) {
  cat("PASS: Categorical variables preserved as Factors (Safe for Embeddings).\n")
} else {
  cat("FAIL: Categorical variables were prematurely encoded.\n")
}

# 5.3 Cleanup
# -----------
# Drop columns that are technically not needed for prediction
# Note: We keep 'target_class' as our Y, and all factors as X.
data <- data %>% select(-ID, -DAYS_BIRTH, -DAYS_EMPLOYED, -AMT_INCOME_TOTAL, 
                        -ACTIVE_EMPLOYMENT_YEARS, -INCOME_PER_FAMILY_MEMBER, 
                        -INCOME_PER_AGE, -status)

# 5.4 Feature Variance and Collinearity Filtering
# -----------------------------------------------
cat("--- Performing Feature Variance and Collinearity Filtering ---\n")

# A. Zero Variance (Works on both Factors and Numerics)
nzv_cols <- nearZeroVar(data, freqCut = 99/1, saveMetrics = FALSE)
if (length(nzv_cols) > 0) {
  nzv_names <- names(data)[nzv_cols]
  cat(sprintf(">> Dropping %d Near-Zero Variance features: %s\n", length(nzv_names), paste(nzv_names, collapse = ", ")))
  data <- data[, -nzv_cols]
} else {
  cat(">> No Near-Zero Variance features found.\n")
}

# B. Collinearity (Numeric Only)
# We must temporarily separate numeric data to calculate correlations,
# then apply the deletions to the main 'data' object.
numeric_vars <- data %>% select(where(is.numeric)) %>% select(-target_class)

if(ncol(numeric_vars) > 1) {
  cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
  high_corr_cols <- findCorrelation(cor_matrix, cutoff = 0.90)
  
  if (length(high_corr_cols) > 0) {
    corr_names <- colnames(cor_matrix)[high_corr_cols]
    cat(sprintf(">> Dropping %d Highly Collinear features (>0.90): %s\n", length(corr_names), paste(corr_names, collapse = ", ")))
    data <- data %>% select(-all_of(corr_names))
  } else {
    cat(">> No highly collinear features found.\n")
  }
}

# [QA 5.4] Final Dimension Check
cat(sprintf("PASS: Filtering complete. Final Dimensions: %d rows, %d columns.\n", nrow(data), ncol(data)))

# SAVE INTERMEDIATE DATA
write.csv(data, file = here("DS-Project_Part2_Scripts", "Saved_Outputs", "cleaned_dataset.csv"), row.names = FALSE)
cat("Data cleaning complete. Starting Cleaned Data EDA...\n")


# ==============================================================================
# SECTION 6: CLEANED DATA EXPLORATION & RISK PROFILING
# ==============================================================================
# PURPOSE: Verification and Granular Risk Analysis using the Cleaned Data.

# 6.1 Initialization
# ------------------
# We use the cleaned 'data' object directly. No need for 'df_eda_categorical'.
df_clean <- data
# Visualization scripts expect a 'TARGET' column
df_clean$TARGET <- df_clean$target_class

cat("New Dimensions:", dim(df_clean), "\n")
dplyr::glimpse(df_clean)

# 6.2 Data Quality & Sanity Assurance
# -----------------------------------
cat("\n[Section 6.2] Data Quality Checks\n")
print(summary(dplyr::select(df_clean, where(is.numeric))))

# Missingness Map
print(naniar::vis_miss(df_clean, warn_large_data = FALSE) + 
        ggtitle("Missingness Map (Cleaned Data)"))

# 6.3 Target Distribution
# -----------------------
if("TARGET" %in% names(df_clean)) {
  p_target <- ggplot(df_clean, aes(x = as.factor(TARGET))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Final Target Distribution", 
         subtitle = "0-1: Safe/Neutral | 2-3: Early Delinq | 4-7: Serious Default",
         x = "Target Group", y = "Count") +
    theme_minimal()
  print(p_target)
}

# 6.4 Numeric Univariate Distributions
# ------------------------------------
num_cols <- names(dplyr::select(df_clean, where(is.numeric)))
num_cols <- num_cols[num_cols != "TARGET" & num_cols != "target_class"]

for(col in num_cols) {
  p_hist <- ggplot(df_clean, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "coral", color = "white") +
    labs(title = paste("Distribution:", col)) + theme_minimal()
  
  p_box <- ggplot(df_clean, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Outliers:", col)) + theme_minimal()
  
  grid.arrange(p_hist, p_box, ncol = 2)
}

# 6.5 Correlations
# ----------------
num_df <- dplyr::select(df_clean, where(is.numeric))
if(ncol(num_df) > 1) {
  cor_mat <- cor(num_df, use = "pairwise.complete.obs")
  cor_mat[is.na(cor_mat)] <- 0
  corrplot(cor_mat, method = "circle", type = "lower", title = "Correlation Matrix", mar = c(0,0,2,0), tl.cex = 0.7)
}

# ==============================================================================
# 6.6 Correlations & Structural Missingness (Extended: Granular Risk Profiling)
# ==============================================================================

# 6.6.1 Create Granular Risk Categories
# -------------------------------------
# MENTOR NOTE: We define strict mappings for all 8 classes.
# Since 'df_clean' still contains factors (not integers), this works perfectly.

df_diagnostics <- df_clean %>%
  mutate(
    # TARGET maps: C=0, X=1, 0=2, 1=3, 2=4, 3=5, 4=6, 5=7
    Risk_Label = case_when(
      TARGET == 0 ~ "Status_C_Paid",        
      TARGET == 1 ~ "Status_X_NoLoan",      
      TARGET == 2 ~ "Status_0_1-29DPD",     
      TARGET == 3 ~ "Status_1_30-59DPD",    
      TARGET == 4 ~ "Status_2_60-89DPD",    
      TARGET == 5 ~ "Status_3_90-119DPD",   
      TARGET == 6 ~ "Status_4_120-149DPD",  
      TARGET == 7 ~ "Status_5_Over150DPD"   
    ),
    # Binary Flag: Is this *any* form of bad debt (Status 2, 3, 4, or 5)?
    Is_Serious_Delinquency = ifelse(TARGET >= 4, 1, 0)
  )

cat("--- Granular Risk Category Distribution ---\n")
print(prop.table(table(df_diagnostics$Risk_Label)) * 100)

# 6.6.2 Interaction Heatmap (Focus: Serious Delinquency > 60 Days)
# ------------------------------------------------------
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
  labs(title = "Risk Heatmap: Serious Delinquency (Status 2+)",
       subtitle = "% of Applicants >60 Days Past Due",
       x = "Housing Type", y = "Family Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_interaction)

# 6.6.3 Risk Density Analysis (Violin Plots by Granular Status)
# ------------------------------------------
# We use the raw Status labels to see if Income differentiates severity
p_violin <- ggplot(df_diagnostics, aes(x = Risk_Label, y = AMT_INCOME_TOTAL_LOG, fill = Risk_Label)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  coord_flip() + 
  labs(title = "Income (Log) Distribution by Granular Status", 
       subtitle = "Does income separate early delinquency from serious default?",
       x = "Status", y = "Log Income") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_violin)

# 6.6.4 Detailed Subgroup Analysis Table
# --------------------------------------
if("Risk_Label" %in% names(df_diagnostics)) {
  
  calc_detailed_risk <- function(data, var_name) {
    data %>%
      group_by(!!sym(var_name)) %>%
      summarise(
        Count = n(),
        Rate_Stat_0 = sprintf("%.1f%%", mean(Risk_Label == "Status_0_1-29DPD") * 100),
        Rate_Stat_2 = sprintf("%.1f%%", mean(Risk_Label == "Status_2_60-89DPD") * 100),
        Rate_Stat_5 = sprintf("%.1f%%", mean(Risk_Label == "Status_5_Over150DPD") * 100)
      ) %>%
      arrange(desc(Rate_Stat_5)) 
  }
  
  risk_vars <- c("NAME_INCOME_TYPE", "NAME_FAMILY_STATUS", "OCCUPATION_TYPE")
  
  for(var in risk_vars) {
    if(var %in% names(df_diagnostics)) {
      cat(paste("\n>>> Granular Risk Breakdown by:", var, "\n"))
      print(calc_detailed_risk(df_diagnostics, var))
    }
  }
}

# ------------------------------------------------------------------------------
# 6.6.5 Multivariate Risk Profiling (Decision Tree)
# ------------------------------------------------------------------------------
cat("\n[Analysis] Generating Decision Tree Rules for Specific Status Classes...\n")

# Prepare data for Tree (Remove identifiers and derived targets)
tree_data <- df_diagnostics %>% 
  select(-TARGET, -Is_Serious_Delinquency, -target_class)

# MENTOR NOTE: 
# We set 'prior' to uniform to force the tree to pay attention to rare classes.
# This logic dynamically adjusts to the number of risk labels present in the data.
uniform_priors <- rep(1/length(unique(tree_data$Risk_Label)), length(unique(tree_data$Risk_Label)))

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
    }
  }
}

# Generate profiles for the critical delinquency classes
get_tree_rules("Status_4_120-149DPD")
get_tree_rules("Status_5_Over150DPD")

# 6.7 Final Artifact Export
# -------------------------
dev.off()
cat(">> PDF Graphics Device Closed. Visualization file saved.\n")

# Save the final CSV (Ready for Section 7 Recipe)
data_output_dir <- here("DS-Project_Part2_Scripts", "Saved_Outputs")
if(!dir.exists(data_output_dir)) dir.create(data_output_dir, recursive = TRUE)

clean_data_path <- file.path(data_output_dir, "final_cleaned_dataset.csv")
write.csv(df_clean, file = clean_data_path, row.names = FALSE)

cat(">> SUCCESS: Final cleaned dataset (With Factors) saved to:", clean_data_path, "\n")

# ==============================================================================
# SECTION 7: ADVANCED PREPARATION (Multi-Class Setup)
# ==============================================================================
# MENTOR NOTE: We are now preparing for an 8-Class Classification problem.

# Ensure required libraries are loaded
if(!require(recipes)) install.packages("recipes"); library(recipes)
if(!require(keras3)) install.packages("keras3"); library(keras3)

cat("\n======================================================\n")
cat("   PHASE 1: MULTI-CLASS DATA PREPARATION\n")
cat("======================================================\n")

# 7.1 Data Recovery & Target Definition
# -------------------------------------
if(!exists("df_clean")) stop("Run Sections 1-6 first to generate 'df_clean'")

# We rely strictly on 'target_class' (Integers 0 to 7)
df_model <- df_clean %>%
  # Drop non-predictive columns, but KEEP target_class
  select(-any_of(c("ID", "status", "TARGET", "Risk_Label", "Is_Serious_Delinquency")))

# ------------------------------------------------------------------------------
# 7.2 Train / Validation / Test Split (70% / 15% / 15%)
# ------------------------------------------------------------------------------
set.seed(123)

# Step 1: Split Data into Train (70%) and Temporary (30%)
# We use stratified sampling to ensure rare classes exist in all sets
train_idx <- createDataPartition(df_model$target_class, p = 0.70, list = FALSE)
train_df <- df_model[train_idx, ]
temp_df  <- df_model[-train_idx, ]

# Step 2: Split Temporary into Validation (50% of 30% = 15%) and Test (Remaining 15%)
val_idx <- createDataPartition(temp_df$target_class, p = 0.50, list = FALSE)
val_df  <- temp_df[val_idx, ]
test_df <- temp_df[-val_idx, ]

cat(sprintf(">> Data Split Complete:\n   Train: %d rows\n   Val:   %d rows\n   Test:  %d rows\n", 
            nrow(train_df), nrow(val_df), nrow(test_df)))

# 7.3 Multi-Class Sample Weighting (Dampened)
# -------------------------------------------
calc_dampened_weights <- function(target_vec) {
  counts <- table(target_vec)
  n_classes <- length(counts)
  n_total <- length(target_vec)
  
  raw_weights <- n_total / (n_classes * counts)
  dampened_weights <- sqrt(raw_weights) # Square root smoothing
  dampened_weights <- dampened_weights / mean(dampened_weights)
  
  weights <- dampened_weights[as.character(target_vec)]
  return(as.numeric(weights))
}

train_weights <- calc_dampened_weights(train_df$target_class)
val_weights   <- calc_dampened_weights(val_df$target_class)
test_weights  <- calc_dampened_weights(test_df$target_class) # Added Test weights

cat(">> Multi-Class Weights Calculated for Train, Val, and Test sets.\n")

# 7.4 Recipe Definition (Preprocessing Pipeline)
# ----------------------------------------------
rec_obj <- recipe(target_class ~ ., data = train_df) %>%
  step_unknown(all_nominal_predictors()) %>%            
  step_integer(all_nominal_predictors()) %>% 
  step_mutate(AGE_INCOME_INTERACT = AGE * AMT_INCOME_TOTAL_LOG) %>%
  step_normalize(all_numeric_predictors()) %>%          
  prep(training = train_df)

# Bake (Transform) all three datasets
train_baked <- bake(rec_obj, new_data = train_df)
val_baked   <- bake(rec_obj, new_data = val_df)
test_baked  <- bake(rec_obj, new_data = test_df) # Added Test baking

# 7.5 Tensor Formatting for Keras Functional API
# ----------------------------------------------
prepare_tensors <- function(baked_df) {
  spec <- rec_obj$term_info
  pred_vars <- spec$variable[spec$role == "predictor"]
  all_preds <- intersect(names(baked_df), pred_vars)
  
  cat_col_names <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", 
                     "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")
  
  cat_vars <- intersect(all_preds, cat_col_names)
  num_vars <- setdiff(all_preds, cat_vars)
  
  tensor_list <- list(
    input_numeric = as.matrix(baked_df[, num_vars, drop = FALSE])
  )
  
  for(v in cat_vars) {
    tensor_list[[paste0("in_", v)]] <- as.matrix(baked_df[[v]])
  }
  
  # Return inputs and Y (Integers 0-7)
  list(inputs = tensor_list, y = as.numeric(baked_df$target_class), cat_vars = cat_vars, num_vars = num_vars)
}

train_tensors <- prepare_tensors(train_baked)
val_tensors   <- prepare_tensors(val_baked)
test_tensors  <- prepare_tensors(test_baked) # Added Test Tensors

# Vocabulary size calculation remains the same (based on max observed value)
vocab_sizes <- list()
for(v in train_tensors$cat_vars) {
  # We check all sets to ensure we don't crash on a new category in test
  vocab_sizes[[v]] <- max(c(train_baked[[v]], val_baked[[v]], test_baked[[v]]), na.rm = TRUE) + 1
}

# ==============================================================================
# SECTION 8: MODEL DEFINITION (Multi-Class RealMLP)
# ==============================================================================
cat("\n======================================================\n")
cat("   PHASE 2: MULTI-CLASS ARCHITECTURE (8 OUTPUTS)\n")
cat("======================================================\n")

# MENTOR UPDATE: Multi-Class Bias Initialization
# Formula: b_i = log(P_i)
# This sets the initial output probabilities to match the class distribution.
probs <- prop.table(table(train_df$target_class))
initial_bias <- log(as.numeric(probs) + 1e-7) # Add epsilon to avoid log(0)
cat(">> Calculated Output Bias Initialization (Log Probabilities):\n")
print(round(initial_bias, 4))

build_multiclass_mlp <- function(num_shape, cat_vars, vocab_sizes, output_bias) {
  
  # A. Inputs
  input_num <- layer_input(shape = c(num_shape), name = "input_numeric")
  
  input_cats <- list()
  embeddings <- list()
  
  for(v in cat_vars) {
    input_name <- paste0("in_", v)
    inp <- layer_input(shape = c(1), name = input_name)
    input_cats <- c(input_cats, list(inp))
    
    v_size <- vocab_sizes[[v]]
    emb_dim <- min(50, ceiling(v_size / 2))
    
    emb <- inp %>% 
      layer_embedding(input_dim = v_size + 1, output_dim = emb_dim) %>%
      layer_flatten()
    embeddings <- c(embeddings, list(emb))
  }
  
  # B. Concatenation
  x <- layer_concatenate(c(list(input_num), embeddings))
  
  # C. Residual Blocks (Deep & Wide for Complexity)
  # Block 1
  residual <- x
  x <- x %>% 
    layer_dense(units = 256, kernel_initializer = "he_normal") %>% 
    layer_layer_normalization() %>% 
    layer_activation("swish") %>% 
    layer_dropout(0.3)
  
  residual <- residual %>% layer_dense(units = 256, kernel_initializer = "he_normal")
  x <- layer_add(list(x, residual))
  
  # Block 2
  residual <- x
  x <- x %>% 
    layer_dense(units = 128, kernel_initializer = "he_normal") %>% 
    layer_layer_normalization() %>% 
    layer_activation("swish") %>% 
    layer_dropout(0.2)
  
  residual <- residual %>% layer_dense(units = 128, kernel_initializer = "he_normal")
  x <- layer_add(list(x, residual))
  
  # D. Output Head (Multi-Class)
  # 8 Units + Softmax Activation
  outputs <- x %>% layer_dense(
    units = 8, 
    activation = "softmax",
    bias_initializer = initializer_constant(value = output_bias) 
  )
  
  all_inputs <- c(list(input_num), input_cats)
  keras_model(inputs = all_inputs, outputs = outputs)
}

model <- build_multiclass_mlp(length(train_tensors$num_vars), train_tensors$cat_vars, vocab_sizes, initial_bias)

# ==============================================================================
# SECTION 9: COMPILATION AND TRAINING
# ==============================================================================

# 1. Optimizer
opt <- keras$optimizers$AdamW(learning_rate = 5e-4, weight_decay = 1e-4)

# 2. Loss Function: Sparse Categorical Crossentropy
# "Sparse" allows us to use Integers (0-7) as targets without one-hot encoding.
# We rely on sample_weights to handle the imbalance.
loss_fn <- keras$losses$SparseCategoricalCrossentropy()

model %>% compile(
  loss = loss_fn,
  optimizer = opt,
  metrics = c(
    "sparse_categorical_accuracy" # Standard accuracy for integer targets
  )
)

# 9.2 Training Loop
# -----------------
cat("\n======================================================\n")
cat("   PHASE 4: TRAINING (MULTI-CLASS)\n")
cat("======================================================\n")

callbacks_list <- list(
  # Monitor validation loss for multi-class
  callback_early_stopping(monitor = "val_loss", mode = "min", patience = 30, restore_best_weights = TRUE, verbose = 1),
  callback_reduce_lr_on_plateau(monitor = "val_loss", mode = "min", factor = 0.5, patience = 4, verbose = 1)
)

history <- model %>% fit(
  x = train_tensors$inputs,
  y = train_tensors$y,
  sample_weight = train_weights, # Crucial: Apply the inverse frequency weights
  validation_data = list(val_tensors$inputs, val_tensors$y, val_weights),
  epochs = 4000,
  batch_size = 512, 
  callbacks = callbacks_list,
  verbose = 1
)

# ==============================================================================
# SECTION 10: RIGOROUS EVALUATION (Unbiased Test Set)
# ==============================================================================

cat("\n======================================================\n")
cat("   PHASE 5: MULTI-CLASS EVALUATION (HELD-OUT TEST SET)\n")
cat("======================================================\n")

# 10.1 Generate Predictions on TEST Data
# --------------------------------------
# We use the Test set here, which the model has NEVER seen before.
pred_probs <- predict(model, test_tensors$inputs) 

# Convert probabilities to Class IDs (0-7)
pred_classes <- apply(pred_probs, 1, which.max) - 1 

# 10.2 Confusion Matrix
# ---------------------
class_map <- c("0"="Status_C", "1"="Status_X", "2"="Status_0", "3"="Status_1", 
               "4"="Status_2", "5"="Status_3", "6"="Status_4", "7"="Status_5")

# Compare Predicted vs Actual (Test Y)
actual_factor <- factor(test_tensors$y, levels = 0:7, labels = class_map)
pred_factor   <- factor(pred_classes, levels = 0:7, labels = class_map)

cm <- caret::confusionMatrix(pred_factor, actual_factor, mode = "everything")

cat("\n>>> Confusion Matrix (Rows=Pred, Cols=Actual [Test Set]) <<<\n")
print(cm$table)

# 10.3 Per-Class Performance Metrics
# ----------------------------------
stats <- cm$byClass %>% 
  as.data.frame() %>% 
  select(Precision, Recall, F1) %>%
  mutate(across(where(is.numeric), \(x) round(x, 4)))

print(stats)

# 10.4 Macro-Average Evaluation
# -----------------------------
macro_f1 <- mean(stats$F1, na.rm = TRUE)
macro_recall <- mean(stats$Recall, na.rm = TRUE)

cat(sprintf("\nMacro-Averaged F1 Score: %.4f\n", macro_f1))
cat(sprintf("Macro-Averaged Recall:   %.4f\n", macro_recall))

# 10.5 Requirements Verification Table
# ------------------------------------
val_loss <- tail(history$metrics$val_loss, 1)

benchmark_table <- data.frame(
  Metric_Category = c("Loss (Categorical)", "Macro F1 Score", "Status 5 Recall (Worst Risk)"),
  Target = c("Minimize", "> 0.15 (Hard Task)", "> 0.30"),
  Actual = c(
    sprintf("%.4f", val_loss),
    sprintf("%.4f", macro_f1),
    sprintf("%.4f", stats["Class: Status_5", "Recall"])
  ),
  Status = c("INFO", 
             ifelse(macro_f1 > 0.15, "PASS", "FAIL"),
             ifelse(stats["Class: Status_5", "Recall"] > 0.3, "PASS", "FAIL")
  )
)

print(benchmark_table, row.names = FALSE)

# 10.6 Save Model
# ---------------
save_model_path <- here("DS-Project_Part2_Scripts", "Saved_Outputs", "final_multiclass_model.keras")
save_model(model, save_model_path, overwrite = TRUE)
cat(">> Multi-Class Model Saved Successfully.\n")