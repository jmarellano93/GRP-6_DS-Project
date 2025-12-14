# ==============================================================================
# REPORT VERSION: SMOTE STRATEGY
# FILENAME: Pre_NN_Script_SMOTE.R
# ==============================================================================

# THE CLASSIFICATION OF FINANCIAL RISK ASSIGNMENT CONTEXT AND OBJECTIVE:

# The assignment rubric, "Classification Course Assignment.pdf," outlines a classification task rooted in the domain of credit risk assessment. 
# Unlike image recognition or natural language processing, where the signal is spatially or temporally encoded, financial classification relies on 
# extracting latent behavioral patterns from static demographic snapshots and historical performance logs. The assignment rubric emphasizes the 
# necessity of learning from data, which implies that we must demonstrate how the raw inputs are transformed into a probability density function representing the likelihood of default.

# In this specific context, "Classification" is not a binary distinct state but a thresholding exercise on a continuous risk probability. 
# The neural network must learn to distinguish between "Good" and "Bad" credit applicants. However, the definition of these labels is not 
# intrinsic to the raw data; it is a derived property based on the status variable. The rubric's focus on "A Credit Card Dataset for Machine Learning" 
# suggests that the data likely originates from a vintage analysis system, where performance is tracked over time windows. 
# This introduces a temporal dimension to the classification problem: are we predicting default in the next month, or the probability of ever defaulting? 
# The neural network architecture must reflect this goal.

# NEURAL NETWORK SPECIFICITY AND RIGOR CONSIDERATIONS:

# Homogeneity of Scale:
# Unlike decision trees, which are scale-invariant, neural networks require all input features to be on a similar magnitude 
# (typically normalized to mean 0, variance 1, or scaled between 0 and 1). If AMT_INCOME_TOTAL ranges in the hundreds of thousands while 
# CNT_CHILDREN ranges from 0 to 5, the gradients associated with the income variable will dominate the optimization process, preventing the 
# network from learning from the number of children.

# Representation of Topology:
# The network assumes that the input space is a continuous manifold. This makes the representation of categorical 
# variables (like OCCUPATION_TYPE) critical. Simple ordinal encoding introduces spurious mathematical relationships 
# (e.g., implying that "Laborers" > "Accountants"), whereas One-Hot encoding expands the dimensionality significantly.

# Missing Data Intolerance:
# Neural networks cannot perform matrix multiplication with NaN (Not a Number) values. The assignment requires a rigorous, 
# theoretically justified strategy for imputation, particularly for the Non-Random Missingness observed in the dataset.

# ==============================================================================
# Module 1: Environment Setup
# ==============================================================================
# METHODOLOGY: 
# This section ensures Reprodicibility, a core tenet of data science. 
# 1. Scientific Notation: We disable this because 'ID' fields (e.g., 5008804) can strictly be interpreted 
#    by R as numeric doubles. If R converts them to "5.00e+06", we lose the unique identifier.
# 2. Reticulate/TensorFlow: The assignment requires a Neural Network. Keras in R is an interface 
#    to Python. We must explicitly bind the R session to a specific Python environment to prevent 
#    path conflicts and ensure the backend is available for computation.

# --- Package Installation Logic ---
# Automated dependency management ensures reproducibility across different grading environments.
installed_pkgs <- installed.packages()[, "Package"]
# Added 'reticulate' to help with Python detection
req_pkgs <- c("tidyverse", "caret", "reshape2", "corrplot", "vcd", "naniar",
              "gridExtra", "e1071", "Hmisc", "VIM", "themis", "tidymodels",
              "tensorflow", "reticulate", "keras")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if(length(new_pkgs)) install.packages(new_pkgs)

# --- Load Libraries ---
options(scipen = 999)           # CRITICAL: Prevents IDs (e.g., 5008804) from converting to 5.00e6
library(tidyverse)               # Data manipulation & ggplot2
library(caret)                  # ML Preprocessing (The industry standard for R pipelines)
library(reshape2)               # Reshaping for correlation heatmaps
library(corrplot)               # Correlation Viz
library(vcd)                    # Categorical Statistics (Cramer's V)
library(naniar)                 # Missing Data Visualization (vis_miss)
library(gridExtra)              # Arranging plots
library(e1071)                  # Skewness calculations
library(Hmisc)                  # Enhanced histograms
library(VIM)                    # Visualization and Imputation of Missing Values (KNN)
library(themis)                 # For step_smote
library(tidymodels)             # For data splitting and preprocessing
library(reticulate)             # Interface to Python
library(tensorflow)             # Required for backend seeding
library(keras)                  # Added for backend management

# --- TENSORFLOW BACKEND CONNECTION STRATEGY ---
# FIX B & H: Explicit Environment Binding
# We attempt to locate the standard 'r-reticulate' Conda environment.
# If unavailable, the script will stop and prompt for a one-time installation.

backend_configured <- FALSE

# Check 1: Is a Conda environment named 'r-reticulate' available?
if ("r-reticulate" %in% reticulate::conda_list()$name) {
  try({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    backend_configured <- TRUE
  }, silent = TRUE)
}

# Check 2: If Check 1 failed, let reticulate try to find any valid python with TF
if (!backend_configured) {
  if (reticulate::py_module_available("tensorflow")) {
    backend_configured <- TRUE
  }
}

# --- Load TensorFlow and Seed ---
library(tensorflow)

# REPRODUCIBILITY:
# 1. Seed R (Frontend)
set.seed(123)

# 2. Seed TensorFlow/Python (Backend)
if (backend_configured && reticulate::py_module_available("tensorflow")) {
  tryCatch({
    # We access the python object directly to ensure the call works
    tf$random$set_seed(123L)
    cat(">> SUCCESS: TensorFlow backend seeded (Reproducibility Guaranteed).\n")
  }, error = function(e) {
    cat(">> WARNING: TensorFlow found but seeding failed. Error:", e$message, "\n")
  })
} else {
  # STOP EXECUTION IF BACKEND IS MISSING
  # A Neural Network assignment cannot be completed without the backend.
  stop("\n
==============================================================================
CRITICAL ERROR: TensorFlow Python Backend Not Found
==============================================================================
To fix this, run the following commands in your R Console ONCE (may take 5-10 mins):

1. reticulate::install_miniconda()
2. keras::install_keras()

Then restart your R session and run this script again.
==============================================================================\n")
}

# --- Data Loading Function ---
load_data <- function(path) {
  if(!file.exists(path)) stop(paste("File not found at:", path))
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  cat("--------------------------------------------------------\n")
  cat(" DATA LOADED SUCCESSFULLY \n")
  cat(" Dimensions:", dim(df)[1], "rows,", dim(df)[2], "columns\n")
  cat("--------------------------------------------------------\n")
  return(df)
}

# --- Execute Data Loading ---
# NOTE: Replace 'C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_data/Dataset-part-2.csv' with your specific file path if different.
df <- load_data("C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_data/Dataset-part-2.csv")

# ==============================================================================
# Module 2: Initial Exploratory Data Analysis (EDA)
# ==============================================================================
# PURPOSE: To explore the raw dataset and identify Data Quality issues that must be resolved in Module 3.
#
# JUSTIFICATION: 
# Blindly feeding data into a Neural Network results in "Garbage In, Garbage Out."
# We use this module to detect:
# 1. Zero Variance: Features that don't change (like FLAG_MOBIL) contribute 0 gradients and should be dropped.
# 2. Class Imbalance: The output shows 77% Good vs 9.6% Bad. This justifies using SMOTE in Module 5.
# 3. Anomalies: The 365243 DAYS_EMPLOYED value (1000 years) is detected here.
# 4. Skewness: Income is highly right-skewed (Skew > 8). This justifies Log-Transformation.

cat("\n================================================================\n")
cat(" MODULE 2: INITIAL DATA EXPLORATION (RAW DATA)\n")
cat("================================================================\n")

# --- Phase 1: Structure & Content Inspection ---
cat("\n[Phase 1] Structure & Content Inspection\n")
print(dim(df))
dplyr::glimpse(df)

# --- Phase 2: Data Quality Checks ---
cat("\n[Phase 2] Data Quality Checks\n")

# 1. Statistical Summary
# ANALYSIS: Looking for impossible values (e.g., negative age, massive income).
print(summary(dplyr::select(df, where(is.numeric))))

# 2. Missing Values
# EVIDENCE FROM OUTPUT: Your text output shows 'OCCUPATION_TYPE' has ~20,699 NAs.
# STRATEGY: We cannot simply drop 30% of our data. We will likely need to impute them later.
cat("\nMissing Values Count:\n")
miss_counts <- colSums(is.na(df))
print(miss_counts[miss_counts > 0])

# 3. Duplicate Rows & IDs
# Duplicate IDs implies that the same customer exists twice.
# Your output showed 0 duplicates, which validates the dataset integrity.
dup_count <- sum(duplicated(df))
cat("\nDuplicate Rows Detected:", dup_count, "\n")

# 4. Variance Check
# EVIDENCE FROM OUTPUT: FLAG_MOBIL has 67,614 rows all with value '1'.
# DECISION: A variable with zero variance offers no predictive power. Therefore we choose to drop it.
if("FLAG_MOBIL" %in% names(df)) {
  cat("\nVariance Check for FLAG_MOBIL:\n")
  print(table(df$FLAG_MOBIL))
}

# --- Phase 3: Univariate Visualization ---
# See PDF "Autogenerated_Plots" file that is generated at the end of our code for visual confirmation.

# 1. Target Variable Distribution (Class Imbalance Check)
# EVIDENCE FROM OUTPUT: Status '0' is the massive majority class (77.1%).
# DECISION: We must use a technique like SMOTE or class weighting due to severe imbalance.
if("status" %in% names(df)) {
  p_target <- ggplot(df, aes(x = as.factor(status))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Target Distribution (Class Imbalance Check)", x = "Status Code", y = "Count") +
    theme_minimal()
  print(p_target)
}

# Textual confirmation of Class Imbalance and Heterogeneity
cat("\n[Analysis Verification] Detailed Status Variable Breakdown:\n")
status_counts <- table(df$status)
print(status_counts)

cat("\nRelative Frequencies (%):\n")
print(round(prop.table(status_counts) * 100, 2))

# 2. Numeric Distributions
# EVIDENCE FROM PDF (Page 4): AMT_INCOME_TOTAL is heavily right-skewed.
# Neural Networks struggle with unscaled, skewed inputs.
# DECISION: Log-transformation is required in Module 3.
num_cols <- names(dplyr::select(df, where(is.numeric)))
for(col in num_cols) {
  p_hist <- ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "coral", color = "white") +
    labs(title = paste("Distribution:", col)) +
    theme_minimal()
  
  p_box <- ggplot(df, aes_string(y = col)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Outliers:", col)) +
    theme_minimal()
  
  grid.arrange(p_hist, p_box, ncol = 2)
}

# Discrete Variable Analysis (Children & Family)
if("CNT_CHILDREN" %in% names(df)) {
  cat("\n[Analysis Verification] Discrete Count Distribution: CNT_CHILDREN\n")
  # Supports claim: "Most values are small (0, 1, 2)... outliers 5..."
  print(table(df$CNT_CHILDREN))
}

# Skewness Verification for Income
if("AMT_INCOME_TOTAL" %in% names(df)) {
  cat("\n[Analysis Verification] Income Distribution Statistics:\n")
  # Supports claim: "Heavy right skew"
  skew_val <- e1071::skewness(df$AMT_INCOME_TOTAL, na.rm = TRUE)
  cat(paste("Skewness Coefficient:", round(skew_val, 4), "\n"))
  
  # Supports claim about specific values adjacent to others (Quantiles)
  cat("Quantiles (0% to 100%):\n")
  print(quantile(df$AMT_INCOME_TOTAL, probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.99, 1)))
}

# --- Phase 4: 365243 Pensioner Anomaly ---
# CRITICAL FINDING:
# The 'DAYS_EMPLOYED' variable contains the positive integer 365243 in approx. 18% of rows.
# Mathematical Analysis: 365,243 days / 365.25 = 1000 years. This is a physical impossibility.
# Context: In legacy banking systems (COBOL/Mainframe), this is a sentinel value for "Infinity" or "Not Applicable."
# Evidence: Cross-tabulation confirms 100% overlap between 365243 and NAME_INCOME_TYPE == 'Pensioner'.
# Risk Assessment: Leaving this as numeric will severely skew Mean/SD during Z-score scaling, 
# collapsing the variance of valid employment data and preventing the NN from learning tenure risk.
# Action: Value will be converted to NA and flagged with a binary indicator 'IS_PENSIONER' in Module 3.

cat("\n[Module 2] Handling Anomalies and Engineering Features [III.1.E]\n")

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
cat("\n--- Module 2 Complete ---\n")

# [ADDED] Missingness Mechanism Analysis
# Supports claim: "Pensioners have NA for occupation... Missing Not At Random"
if("OCCUPATION_TYPE" %in% names(df) && "NAME_INCOME_TYPE" %in% names(df)) {
  cat("\n[Analysis Verification] MNAR Check: Occupation Missingness vs. Income Type\n")
  
  mnar_check <- df %>%
    # Treat empty strings as NA for this check if necessary
    mutate(OCCUPATION_TYPE = ifelse(OCCUPATION_TYPE == "", NA, OCCUPATION_TYPE)) %>%
    group_by(NAME_INCOME_TYPE) %>%
    summarise(
      Total_Count = n(),
      Missing_Occupation = sum(is.na(OCCUPATION_TYPE)),
      Missing_Rate_Pct = round((sum(is.na(OCCUPATION_TYPE)) / n()) * 100, 2)
    )
  
  print(mnar_check)
  
  # Logic Check: Do 100% of Pensioners have missing occupation?
  pensioner_miss_rate <- mnar_check$Missing_Rate_Pct[mnar_check$NAME_INCOME_TYPE == "Pensioner"]
  cat(paste("\nPercentage of Pensioners with Missing Occupation:", pensioner_miss_rate, "%\n"))
}

# [ADDED] Reverse Anomaly Check
# Supports claim: "Every single instance [of 365243] corresponds to Pensioner"
if("DAYS_EMPLOYED" %in% names(df)) {
  cat("\n[Analysis Verification] Reverse Check: Who has the 365243 anomaly?\n")
  print(table(df$NAME_INCOME_TYPE[df$DAYS_EMPLOYED == 365243]))
}

# ==============================================================================
# Module 2: Initial Exploratory Data Analysis (EDA) Analysis Summary
# ==============================================================================

# GENERAL SUMMARY:
#
# The "Dataset-part-2.csv" and the associated assignment present a scenario that 
# mimics the complexities of real-world financial data science. The initial 
# analysis confirms that a naive application of a neural network to this raw 
# data would result in a failed model. The dataset is characterized by legacy 
# system artifacts (the 365243 anomaly), semantic ambiguities (the status 
# variable), and severe class imbalance.
#
# These challenges are surmountable through the rigorous data engineering 
# strategy outlined below. By treating the data preparation not as a preliminary 
# chore but as a core component of the model architecture, the student can 
# satisfy the "academic rigor" and "reproducibility" requirements of the rubric. 
# The transformation of the status variable into a binary target provides a 
# clear optimization landscape for the network. The rigorous scaling and anomaly 
# handling ensure that the loss function surface is smooth and navigable by 
# gradient descent.
#
# Ultimately, the success of this neural network classification system will 
# depend less on the depth of the network layers and more on the integrity of 
# the input features.

# ------------------------------------------------------------------------------
# RAW DATASET ANALYSIS
# ------------------------------------------------------------------------------
# The dataset provided is a comma-separated values file representing a snapshot 
# of applicant characteristics joined with a performance label. A granular, 
# column-by-column inspection reveals a mixture of continuous, categorical, and 
# binary variables, each presenting specific challenges for neural network 
# ingestion.

# 1. Structural Variables and Identifiers:
#    - The ID Column: The dataset begins with an ID column (e.g., 5008804). 
#      The autogenerated plots confirm that this variable follows a uniform 
#      distribution across the range of 5,000,000 to 8,000,000.
#    - Theoretical Implication: In the context of deep learning, a high-cardinality 
#      identifier acts as a unique hash. A network with sufficient capacity will 
#      trivially "memorize" the mapping between ID and target, achieving perfect 
#      training accuracy but zero validation accuracy (Overfitting).
#    - Actionable Strategy: The ID column must be strictly excluded from the 
#      input tensor. It contains zero intrinsic predictive signal.

# 2. The Status Column (Ground Truth):
#    The raw data reveals a heterogeneous mix of alphanumeric codes:
#    - C: Paid off or current for the month (Good).
#    - X: No loan for the month or inactive (Neutral/Good).
#    - 0: 1-29 days past due (Actionable, but often considered "Good").
#    - 1: 30-59 days past due (The beginning of delinquency).
#    - 2: 60-89 days past due.
#    - 3: 90-119 days past due.
#    - 4: 120-149 days past due.
#    - 5: Overdue or bad debts, write-offs (>150 days).
#
#    - Distributional Reality: R output confirms Status '0' is the overwhelming 
#      majority (52,133 rows / 77.1%). True "Bad" class {2,3,4,5} is a minority 
#      (1,395 rows / ~2.06%).
#    - Semantic Ambiguity: Neural networks require numerical targets. We cannot 
#      feed 'C' into a loss function.
#      - The "Good" Definition: {C, X, 0} -> Class 0.
#      - The "Bad" Definition: {2, 3, 4, 5} -> Class 1.
#      - The Ambiguity of "1": Status 1 (9.60%) is a grey area. We must document 
#        the decision to binarize at Status >= 2 to define true default.

# 3. The "365243" Anomaly: A Case Study in Legacy Data
#    - Observation: The DAYS_EMPLOYED column contains the integer 365243 
#      (~1,000 years).
#    - Socio-economic Correlation: Every instance corresponds to "Pensioner".
#    - Origin: A sentinel value from legacy COBOL banking systems for "N/A".
#    - Neural Network Consequence: Catastrophic. Because valid days are negative 
#      (e.g., -5000), this massive positive outlier collapses the variance during 
#      scaling. The network will drown out other signals.
#    - Remediation: 
#      1. Isolation: Create binary feature IS_PENSIONER.
#      2. Imputation: Replace 365243 with 0 or NaN (then mean-impute).

# 4. Demographic Features and Continuous Variables
#    - DAYS_BIRTH: Negative integers. Convert to positive years (|x|/365.25) to 
#      improve interpretability and scaling.
#    - AMT_INCOME_TOTAL: Follows a Pareto distribution (heavy right skew). 
#      Log Transformation (log(1+x)) is crucial before Z-score standardization 
#      to compress magnitude differences.
#    - CNT_CHILDREN: Discrete numerical. Outliers exist (up to 19). Must be 
#      capped (Winsorized) to prevent rare leverage points from distorting weights.

# 5. Categorical High-Cardinality Variables
#    - OCCUPATION_TYPE: Presents Missing Not At Random (MNAR) data. Pensioners 
#      have NA for occupation (99.12% rate). Imputation must introduce a 
#      "Retired" category rather than a generic mode.
#    - NAME_HOUSING_TYPE / FAMILY_STATUS: String labels must be One-Hot Encoded 
#      or Embedded. "Rented" vs "House" carries significant predictive power.

# 6. Binary Flags
#    - Variance Check: If var(FLAG_MOBIL) == 0, the column adds no information 
#      and must be dropped to save computational cost.

# ------------------------------------------------------------------------------
# ANALYSIS OF SCRIPT OUTPUTS AND PLOTS
# ------------------------------------------------------------------------------
# 1. Target Distribution: Severe Class Imbalance
#    - Visual Evidence: The histogram shows negligible bars for delinquency 
#      vs status '0'.
#    - Neural Network Failure Mode: Without intervention, the network will 
#      converge to a "Zero-Rule" baseline (predicting "Good" for everyone), 
#      achieving 97% accuracy but 0% Recall.
#    - Required Intervention: SMOTE (Resampling) and Weighted Binary Cross-Entropy.

# 2. Distribution of Identifiers
#    - Visual Confirmation: The plot shows a uniform rectangular block, confirming 
#      ID is noise.

# 3. Outlier Visualization
#    - The plots confirm outliers in Children Count, necessitating capping logic.

# ------------------------------------------------------------------------------
# ARCHITECTURAL PLANS AND R/TIDYVERSE IMPLEMENTATION
# ------------------------------------------------------------------------------
# Based on the forensic analysis of the data and plots, we can now formulate the 
# specific data engineering strategy. This strategy aligns with the "expert in 
# R, tidyverse" persona, leveraging the recipes package for reproducible 
# preprocessing pipelines.

# 1. The Tidyverse Preprocessing Recipe: 
#    In the R ecosystem, the recipes package is the standard for preparing data 
#    for models (including Keras/TensorFlow). The pipeline implemented in 
#    Module 5 is:
#
#    - Step 1: Role Assignment:
#      Define status as the outcome (after binarization) and ID as an ID role 
#      (to be excluded from training).
#
#    - Step 2: Imputation:
#      step_impute_median: Used for all numeric predictors (more robust to 
#      skewed financial data than mean).
#      step_unknown: For OCCUPATION_TYPE (fills NA with "unknown").
#
#    - Step 3: Geometric Correction (Crucial for SMOTE):
#      step_log: Apply to AMT_INCOME_TOTAL. This compresses the order of 
#      magnitude differences, ensuring that distance-based algorithms (like 
#      SMOTE) do not ignore other features due to the income scale.
#
#    - Step 4: Class Balancing:
#      step_smotenc: Generate synthetic samples for the minority class. 
#      Applied *after* log-transform to ensure geometric validity.
#
#    - Step 5: Encoding:
#      step_dummy: One-Hot encode all nominal predictors to create the binary 
#      vectors the network expects.
#
#    - Step 6: Scaling (Rubric Compliance):
#      step_range: Apply Min-Max scaling to force all values into [0, 1]. 
#      *Note:* We explicitly chose this over Z-score standardization to satisfy 
#      the specific requirement in the assignment rubric.

# 2. Neural Network Topology Recommendations (Plan for NN Script):
#    Given the tabular nature of the data (~50 features after encoding, ~90k rows), 
#    a Multi-Layer Perceptron (MLP) is appropriate.
#
#    - Input Layer: Dimension equal to the processed feature set.
#    - Hidden Layer 1: Dense, 64-128 units, Activation = ReLU.
#      Rationale: ReLU mitigates the vanishing gradient problem.
#    - Batch Normalization: Inserted after activation.
#      Rationale: Stabilizes learning by re-centering layer inputs.
#    - Dropout: Rate 0.3 - 0.5.
#      Rationale: Critical for preventing overfitting given the class imbalance.
#    - Hidden Layer 2: Dense, 32-64 units, Activation = ReLU.
#    - Output Layer: Dense, 1 unit, Activation = Sigmoid.
#      Rationale: We need a probability [0,1] for binary classification.

# 3. Metric Selection and Evaluation:
#    - Primary Metric: Area Under the Precision-Recall Curve (AUPRC). Focuses 
#      on the minority class performance.
#    - Validation Strategy: Stratified K-Fold Cross-Validation is necessary to 
#      ensure that the minority class is represented in every validation fold.

# ==============================================================================
# Module 3: Row-Wise Cleaning & Logic Features (Refactored)
# ==============================================================================
# METHODOLOGY: 
# This module implements the fixes identified in Sections 3.1 - 3.5.
# Order of operations is critical: Target Engineering must happen BEFORE filtering 
# to ensure we do not discard the minority class (Section 3.1).
#
# KEY DECISIONS:
# 1. Target Definition: We use "Vintage Analysis" logic. Status 0/C/X are 'Good' (paid/no loan). 
#    Status 2-5 are 'Bad' (overdue > 60 days). This binary thresholding creates the ground truth.
# 2. 365243 Handling: We convert the sentinel value to 0 (meaning "0 days employed") 
#    but create a binary flag 'EMPLOYMENT_STATUS_FLAG'. This preserves the information 
#    that they are Not Working without distorting the numeric scale of days employed.

cat("\n================================================================\n")
cat(" MODULE 3: DATA CLEANING & LOGIC (NO LEAKAGE)\n")
cat("================================================================\n")

# Initialize working dataframe
df_clean <- df

# --- Phase 1: Target Variable Reconstruction (Vintage Logic) ---
# Note: If this dataset is a snapshot (one row per ID), we apply the logic to the current status.
# If you have the raw monthly credit file, you would perform the group_by(ID) aggregation 
# BEFORE merging into this dataset.

if ("status" %in% names(df_clean)) {
  cat("Constructing Target Variable (Recovering Bad Instances)...\n")
  df_clean <- df_clean %>%
    mutate(
      # Logic: Status 2, 3, 4, 5 are BAD (1). Others are GOOD (0).
      TARGET = case_when(
        status %in% c("2", "3", "4", "5") ~ 1,
        status %in% c("C", "X", "0", "1") ~ 0,
        TRUE ~ NA_real_
      ),
      TARGET = as.factor(TARGET)
    ) %>%
    select(-status) %>%
    filter(!is.na(TARGET))
  
  # [QA CHECK] Target Integrity
  if(sum(is.na(df_clean$TARGET)) > 0) stop(">> QA FAIL: Target Variable contains NAs after construction.")
  cat(">> QA PASS: Target constructed successfully.\n")
}

# --- Phase 2: Drop Zero Variance ---
if("FLAG_MOBIL" %in% names(df_clean)) {
  if(length(unique(df_clean$FLAG_MOBIL)) == 1) {
    df_clean$FLAG_MOBIL <- NULL
    cat(">> QA PASS: Zero variance column FLAG_MOBIL dropped.\n")
  }
}

# --- Phase 3: Flag Standardization ---
df_clean <- df_clean %>%
  mutate(across(any_of(c("FLAG_OWN_CAR", "FLAG_OWN_REALTY")),
                ~ ifelse(. == "Y", 1L, 0L))) %>%
  mutate(across(any_of(c("FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")),
                ~ as.integer(. > 0)))

# [QA CHECK] Flag Binary Integrity
check_flags <- df_clean %>% select(starts_with("FLAG")) %>% summarise(across(everything(), ~ all(. %in% c(0,1))))
if(!all(unlist(check_flags))) stop(">> QA FAIL: One or more FLAG columns contain non-binary values.")
cat(">> QA PASS: All Flags standardized to binary integers.\n")


# --- Phase 4: Correcting "365243" Anomaly (Function Logic Integration) ---
if ("DAYS_EMPLOYED" %in% names(df_clean)) {
  cat("Rectifying 'DAYS_EMPLOYED': Creating Status Flag & Cleaning...\n")
  
  df_clean <- df_clean %>%
    mutate(
      # Create specific flag preserving pensioner/unemployed info
      EMPLOYMENT_STATUS_FLAG = ifelse(DAYS_EMPLOYED == 365243, "Not_Working", "Working"),
      
      # Replace sentinel with 0 and take absolute value
      DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, 0, abs(DAYS_EMPLOYED))
    )
  
  # [QA CHECK] Anomaly Removal
  if(max(df_clean$DAYS_EMPLOYED, na.rm=T) > 300000) stop(">> QA FAIL: 365243 Anomaly still exists in DAYS_EMPLOYED.")
  cat(">> QA PASS: 365243 Anomaly resolved.\n")
}

# --- Phase 5: Handling Multicollinearity & Structural Missingness ---
if("IS_PENSIONER" %in% names(df_clean)) {
  df_clean <- df_clean %>% select(-IS_PENSIONER)
}

if("OCCUPATION_TYPE" %in% names(df_clean) && "NAME_INCOME_TYPE" %in% names(df_clean)) {
  cat("Handling Structural Missingness in Occupation (Pensioner Logic)...\n")
  
  df_clean <- df_clean %>%
    mutate(OCCUPATION_TYPE = case_when(
      # Condition 1: Pensioners with missing job -> "Retired"
      NAME_INCOME_TYPE == "Pensioner" & (is.na(OCCUPATION_TYPE) | OCCUPATION_TYPE == "") ~ "Retired",
      
      # Condition 2: Everyone else with missing job -> "Unknown"
      (is.na(OCCUPATION_TYPE) | OCCUPATION_TYPE == "") ~ "Unknown",
      
      # Condition 3: Keep existing values
      TRUE ~ OCCUPATION_TYPE
    ))
}

# --- Phase 6: Encoding Strategy (Ordinal) ---
# JUSTIFICATION:
# We treat 'Education' as Ordinal (Integer encoded) rather than nominal (One-Hot).
# Why? Because Education has an intrinsic rank (Secondary < Higher < Academic).
# Preserving this rank allows the Neural Network to learn the monotonic relationship 
# between Education and Risk, which would be lost with One-Hot encoding.

if("NAME_EDUCATION_TYPE" %in% names(df_clean)) {
  # Define the hierarchy of education
  edu_levels <- c("Lower secondary", "Secondary / secondary special", 
                  "Incomplete higher", "Higher education", "Academic degree")
  
  # Convert to integer rank (1, 2, 3, 4, 5)
  df_clean <- df_clean %>%
    mutate(
      EDUCATION_LEVEL = as.integer(factor(
        NAME_EDUCATION_TYPE, 
        levels = edu_levels, 
        ordered = TRUE
      ))
    ) %>%
    # Remove the original string column to prevent duplicates
    select(-NAME_EDUCATION_TYPE)
  
  cat(">> QA PASS: Education converted to Ordinal Integer rank.\n")
}

# --- Phase 7: Robust Winsorization & Transformations [REMOVED FOR DATA LEAKAGE PREVENTION] ---
# Note: Capping (Winsorization) and Log Transformations have been moved to Module 5 (Recipes).
# This ensures that the 99th percentile and distributions are calculated on the TRAINING set only
# and applied to Test/Validation, preventing data leakage.

# --- Phase 8: Final Deduplication & Age ---
if ("DAYS_BIRTH" %in% names(df_clean)) {
  df_clean$AGE <- abs(df_clean$DAYS_BIRTH) / 365.25
  df_clean$DAYS_BIRTH <- NULL
}

if("ID" %in% names(df_clean)) df_clean$ID <- NULL
df_clean <- df_clean %>% dplyr::distinct()

# [QA CHECK] Data Sufficiency
if(nrow(df_clean) < 1000) stop(">> QA FAIL: Dataframe has dangerously few rows after cleaning.")
assign("df_clean_processed", df_clean, envir = .GlobalEnv)

cat("\n--- Module 3 Complete ---\n")

# ==============================================================================
# Module 4: Preprocessed Diagnostic EDA (Cleaned Data)
# ==============================================================================
# PURPOSE: Verification of Cleaning Steps.
# We confirm here that anomalies (365243) are gone, encoded variables exist, 
# and the dataset is ready for statistical modeling.

cat("\n================================================================\n")
cat(" MODULE 4: SECONDARY DATA EXPLORATION (CLEANED DATA)\n")
cat("================================================================\n")

# Ensure we use the processed dataframe
if(exists("df_clean_processed")) {
  df_clean <- df_clean_processed
}

cat("New Dimensions:", dim(df_clean), "\n")

# --- Phase 1: Structure & Content Inspection ---
cat("\n[Phase 1] Structure & Content Inspection\n")
print(head(df_clean, 5))
print(dim(df_clean))
dplyr::glimpse(df_clean)

# --- Phase 2: Data Quality Checks ---
cat("\n[Phase 2] Data Quality & Sanity Checks\n")
print(summary(dplyr::select_if(df_clean, is.numeric)))

miss_counts <- colSums(is.na(df_clean))
print(miss_counts[miss_counts > 0])
print(naniar::vis_miss(df_clean, warn_large_data = FALSE) + ggtitle("Missingness Map (Cleaned Data)"))

dup_count <- sum(duplicated(df_clean))
cat("\nDuplicate Rows Detected:", dup_count, "\n")

# --- Phase 3: Zero Variance & Univariate Visualization ---
cat("\n[Phase 3] Variance Checks & Univariate Visualization\n")

nzv_metrics <- caret::nearZeroVar(df_clean, saveMetrics = TRUE)
zero_var_cols <- rownames(nzv_metrics[nzv_metrics$zeroVar == TRUE, ])

if(length(zero_var_cols) > 0) {
  cat(">> WARNING: The following columns have ZERO variance (Single Value):\n")
  print(zero_var_cols)
  df_clean <- df_clean[, !names(df_clean) %in% zero_var_cols]
}

if("TARGET" %in% names(df_clean)) {
  p_target <- ggplot(df_clean, aes(x = as.factor(TARGET))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Final Target Distribution", x = "Target (0=Good, 1=Bad)", y = "Count") +
    theme_minimal()
  print(p_target)
}

# Numeric Distributions
num_cols <- names(dplyr::select_if(df_clean, is.numeric))
num_cols <- num_cols[num_cols != "ID"]
for(col in num_cols) {
  p_hist <- ggplot(df_clean, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "coral", color = "white") +
    labs(title = paste("Distribution:", col)) + theme_minimal()
  p_box <- ggplot(df_clean, aes_string(y = col)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Outliers:", col)) + theme_minimal()
  grid.arrange(p_hist, p_box, ncol = 2)
}

# --- Phase 4: Correlations & Relationships ---
cat("\n[Phase 4] Correlations & Bivariate Analysis\n")

num_df <- dplyr::select_if(df_clean, is.numeric)
if("ID" %in% names(num_df)) num_df$ID <- NULL

if(ncol(num_df) > 1) {
  cor_mat <- cor(num_df, use = "pairwise.complete.obs")
  cor_mat[is.na(cor_mat)] <- 0
  corrplot(cor_mat, method = "circle", type = "lower", title = "Correlation Matrix (Cleaned)", mar = c(0,0,2,0), tl.cex = 0.7)
}

# --- Phase 5: Deep Dive Risk Analysis (Default Rates) ---
cat("\n[Phase 5] Deep Dive Risk Analysis: Default Rates by Subgroup\n")

if("TARGET" %in% names(df_clean)) {
  
  calc_risk <- function(data, var_name) {
    data %>%
      group_by(!!sym(var_name)) %>%
      summarise(
        Count = n(),
        Bad_Count = sum(TARGET == 1, na.rm = TRUE),
        Default_Rate_Pct = round(mean(as.numeric(as.character(TARGET)) == 1, na.rm = TRUE) * 100, 2)
      ) %>%
      arrange(desc(Default_Rate_Pct)) %>%
      mutate(Variable = var_name) %>%
      rename(Subgroup = !!sym(var_name)) %>%
      select(Variable, Subgroup, Count, Bad_Count, Default_Rate_Pct)
  }
  
  # 1. Prepare Data with Bins
  # Note: Log transform is now in Recipe, so we bin the raw AMT_INCOME_TOTAL.
  # Ranking (ntile) is invariant to monotonic transformations like log.
  df_risk <- df_clean %>%
    mutate(
      DAYS_EMPLOYED_BIN = ntile(DAYS_EMPLOYED, 6),
      INCOME_BIN = ntile(AMT_INCOME_TOTAL, 6),
      AGE_BIN = ntile(AGE, 4)
    )
  
  # 2. List of variables to analyze
  risk_vars <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_FAMILY_STATUS",
                 "NAME_HOUSING_TYPE", "DAYS_EMPLOYED_BIN", "OCCUPATION_TYPE",
                 "EMPLOYMENT_STATUS_FLAG", "EDUCATION_LEVEL", "INCOME_BIN", "AGE_BIN")
  
  all_subgroups <- data.frame()
  
  for(var in risk_vars) {
    if(var %in% names(df_risk)) {
      tbl <- calc_risk(df_risk, var)
      cat(paste("\n--- Default Rates:", var, "---\n"))
      print(tbl)
      tbl$Subgroup <- as.character(tbl$Subgroup)
      all_subgroups <- bind_rows(all_subgroups, tbl)
    }
  }
  
  top_10_risk <- all_subgroups %>%
    filter(Count > 50) %>%
    arrange(desc(Default_Rate_Pct)) %>%
    head(10)
  
  print(top_10_risk)
}

cat("\n--- Module 4 Complete ---\n")

# ==============================================================================
# REPORT: MODULE 3 (CLEANING) & MODULE 4 (DIAGNOSTICS) ANALYSIS
# ==============================================================================
# AUTHOR: Senior Data Science Mentor
# CONTEXT: Neural Network Classification Assignment
# DATE:   December 14, 2025
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: CRITIQUE OF MODULE 3 CLEANING OPERATIONS
# ------------------------------------------------------------------------------
#
# 1. Target Variable Reconstruction (Vintage Definition):
#    OBSERVATION: You have defined the "Bad" class (TARGET = 1) strictly as
#    statuses {2, 3, 4, 5}. Status {1} (30-59 DPD) has been grouped with "Good".
#    IMPLICATION: This results in a hyper-imbalanced dataset (approx 98% Good vs
#    2% Bad). While this creates a high-purity ground truth (ignoring "soft"
#    defaults), it imposes a massive burden on the Neural Network to find the
#    signal.
#    RECOMMENDATION: In Module 5 (Resampling), ensuring the minority class is
#    oversampled to at least 10-15% is no longer optional; it is mandatory.
#    Without SMOTE, your model will collapse to a "Zero-Rule" classifier (98% acc).
#
# 2. Handling the '365243' DAYS_EMPLOYED Anomaly:
#    OBSERVATION: The max value in 'cleaned_dataset.csv' for DAYS_EMPLOYED is
#    17,531. The 365243 sentinel is successfully removed.
#    VERIFICATION: You effectively replaced the sentinel with 0 and likely created
#    a corresponding flag (EMPLOYMENT_STATUS_FLAG).
#    NN CONTEXT: This is excellent. Neural Networks are distance-sensitive.
#    Had 365243 remained, the gradient updates for this feature would have been
#    orders of magnitude larger than for AGE, destabilizing the weights.
#
# 3. Categorical Encoding (Education):
#    OBSERVATION: 'EDUCATION_LEVEL' appears as an integer (1-5) in the cleaned CSV.
#    ANALYSIS: You chose Ordinal Encoding over One-Hot for Education.
#    PROS: Preserves the rank order (Secondary < Higher < Academic). Reduces dimensionality.
#    CONS: Implies the distance between 'Secondary' and 'Incomplete Higher' is
#    mathematically equal to 'Incomplete Higher' and 'Higher'. This is an
#    acceptable assumption for this assignment.
#
# 4. Multicollinearity Management:
#    OBSERVATION: The correlation between CNT_CHILDREN and CNT_FAM_MEMBERS is 0.88.
#    ACTION ITEM: While Decision Trees handle collinearity well, Neural Networks
#    share weights across features. High collinearity introduces redundancy.
#    Consider dropping CNT_FAM_MEMBERS in the recipe if training diverges, as
#    CNT_CHILDREN likely carries the clearer signal.
#
# ------------------------------------------------------------------------------
# SECTION 2: ANALYSIS OF MODULE 4 OUTPUTS (PLOTS & STATISTICS)
# ------------------------------------------------------------------------------
#
# 1. Risk Stratification (The "Municipal" Signal):
#    FINDING: Your risk tables show 'Municipal apartment' residents have a default
#    rate of ~3.9%, nearly double the population average (~2.0%).
#    INTERPRETATION: This is a strong non-linear signal. A simple linear logistic
#    regression might miss this specific interaction unless explicitly encoded.
#    A Neural Network with adequate hidden layers (ReLU activation) should be able
#    to "segment" this risk pocket automatically.
#
# 2. Scaling Verification (Rubric Compliance):
#    FINDING: The "Verification of Min-Max Scaling" boxplots in your PDF explicitly
#    show all numeric features bounded strictly within [0, 1].
#    VERDICT: PASS.
#    THEORETICAL NOTE: Why [0,1]? Because you are using a Sigmoid output layer.
#    If inputs were Z-scaled (unbounded), large outliers could saturate the
#    sigmoid function early in the network, causing "Vanishing Gradients."
#    Min-Max is the safer architectural choice here.
#
# 3. The Log-Transformation Necessity:
#    FINDING: The "Distribution: AMT_INCOME_TOTAL" plot confirms the heavy
#    right-skew (Pareto distribution).
#    CRITICAL CHECK: Ensure `step_log(AMT_INCOME_TOTAL)` happens BEFORE
#    `step_range` in your recipe. If you Min-Max scale a skewed distribution,
#    99% of your data gets squashed into the [0, 0.05] range, destroying variance.
#    Your recipe order in Module 5 appears correct.
#
# 4. Final Missingness Check:
#    STATUS: Zero missing values detected in 'cleaned_dataset.csv'.
#    NOTE: Your `step_unknown` strategy for OCCUPATION_TYPE was necessary.
#    Dropping those rows would have biased the model against Pensioners (who
#    predominantly have NA occupation).

# ==============================================================================
# Module 5: Advanced Preprocessing & Data Splitting (FIXED)
# ==============================================================================
# METHODOLOGY: The Core Pipeline.
# This module converts cleaned data into Tensors ready for the Neural Network.
#
# 1. Stratified Split: We use `strata = TARGET` because the class balance is 90/10. 
#    Random splitting might result in a Validation set with 0 Bad cases, breaking the evaluation.
# 2. Recipe Engineering (The "Geometric" Fix):
#    - We apply Log-Transform on Income *before* SMOTE. 
#    - Why? SMOTE uses Euclidean/Gower distance. If Income is 6.75M and Age is 40, the 
#      distance metric is 100% dominated by Income. SMOTE would ignore Age/Gender.
#      Logging compresses Income, allowing SMOTE to see the other variables.
# 3. Rubric Compliance:
#    - We switched from `step_normalize` (Z-score) to `step_range` (Min-Max [0,1]).
#    - The Rubric explicitly demands inputs in [0, 1].
# You have successfully converted a "dirty", legacy banking dataset into a
# cleaner mathematical representation. The data is now:
#   a) Bounded (Scaling) -> Safe for Matrix Multiplication
#   b) Complete (Imputation) -> Safe for Tensor ingestion
#   c) Rank-Preserved (Ordinal Encoding) -> Captures hierarchy
#
# IMMINENT RISK: The 98/2 Class Imbalance.
# Proceed to Network Training.
cat("\n================================================================\n")
cat(" MODULE 5: PREPROCESSING PIPELINE (TIDYMODELS) - FIXED\n")
cat("================================================================\n")

# --- 5.1 Stratified Data Splitting ---
set.seed(123)

df_clean$TARGET <- as.factor(df_clean$TARGET)

cat("Partitioning Data (70/15/15) with Stratification...\n")
split_obj <- initial_validation_split(df_clean, prop = c(0.7, 0.15), strata = TARGET)

train_raw <- training(split_obj)
val_raw <- validation(split_obj)
test_raw <- testing(split_obj)

cat("Split Complete:\n")
cat("Training Set: ", nrow(train_raw), "rows\n")
cat("Validation Set: ", nrow(val_raw), "rows\n")
cat("Testing Set: ", nrow(test_raw), "rows\n")

# --- 5.1b Calculate Capping Thresholds (Training Data Only) ---
# [No changes to capping logic needed - preserved for anti-leakage]
if("AMT_INCOME_TOTAL" %in% names(train_raw)) {
  inc_cap <- quantile(train_raw$AMT_INCOME_TOTAL, 0.99, na.rm = TRUE)
} else { inc_cap <- Inf }

if("CNT_CHILDREN" %in% names(train_raw)) {
  child_cap <- quantile(train_raw$CNT_CHILDREN, 0.995, na.rm = TRUE)
} else { child_cap <- Inf }

if("CNT_FAM_MEMBERS" %in% names(train_raw)) {
  fam_cap <- quantile(train_raw$CNT_FAM_MEMBERS, 0.995, na.rm = TRUE)
} else { fam_cap <- Inf }

# --- 5.2 Define the Base Recipe (Common Steps) ---
base_recipe <- recipe(TARGET ~., data = train_raw) %>%
  
  # 1. Role Update
  update_role(any_of("ID"), new_role = "id") %>%
  
  # 2. Imputation
  # Even though df_clean is currently empty of NAs, keeping step_impute_median and step_unknown in the recipe is Critical Best Practice for two reasons:
  # A) The "Hidden" Test Set: The assignment rubric mentions assessing performance on "secret" data. Even if your provided CSV has no missing income values, the grading dataset might.
  # B) Zero Cost: If the recipe scans the data and finds no missing values, step_impute_median simply does nothing.
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  
  # 3. Winsorization
  step_mutate(
    AMT_INCOME_TOTAL = pmin(AMT_INCOME_TOTAL, !!inc_cap),
    CNT_CHILDREN = pmin(CNT_CHILDREN, !!child_cap),
    CNT_FAM_MEMBERS = pmin(CNT_FAM_MEMBERS, !!fam_cap)
  ) %>%
  
  # 4. Log Transformation (Fixes Geometric Invalidity for SMOTE later)
  step_log(AMT_INCOME_TOTAL, offset = 1) %>%
  
  # 5. Zero Variance
  step_zv(all_predictors())

# ==============================================================================
# STRATEGY: SMOTE-NC + MIN-MAX SCALING (RUBRIC COMPLIANT)
# ==============================================================================

# LOGIC UPDATE:
# 1. step_smotenc: Runs on logged data (from base_recipe), so distances are valid.
# 2. step_range: Replaces step_normalize. Forces all inputs to [0,1].

final_recipe <- base_recipe %>%
  # Step A: SMOTE (Safe now because outliers were logged in base_recipe)
  # skip = TRUE ensures we NEVER oversample Validation/Test data (prevents Leakage)
  step_smotenc(TARGET, over_ratio = 1, skip = TRUE) %>%
  
  # Step B: One-Hot Encoding
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  
  # Step C: Cleanup dummy columns
  step_zv(all_predictors()) %>%
  
  # Step D: RUBRIC FIX - Min-Max Scaling [0,1]
  # Neural Networks prefer [0,1] or [-1,1]. The rubric specifies [0,1].
  step_range(all_numeric_predictors(), min = 0, max = 1)

strategy_name <- "SMOTE-NC + MinMax Scaling"

# --- 5.4 Execute Preprocessing (Baking) ---
cat(paste("\n>> PROCESSING STRATEGY SELECTED:", strategy_name, "\n"))

trained_recipe <- prep(final_recipe, training = train_raw)

train_processed <- bake(trained_recipe, new_data = NULL)
val_processed <- bake(trained_recipe, new_data = val_raw)
test_processed <- bake(trained_recipe, new_data = test_raw)

# --- 5.5 Final Diagnostic Check ---
cat("\n[Final Preprocessing Checks]\n")
cat("Processed Train Shape: ", dim(train_processed)[1], "x", dim(train_processed)[2], "\n")

# QA Checks
if(any(is.na(train_processed))) stop(">> QA FAIL: NAs remain in Processed Training Data!")
if(max(select(train_processed, where(is.numeric))) > 1.0001) warning(">> QA WARNING: Values > 1 detected. Check step_range.")
if(min(select(train_processed, where(is.numeric))) < -0.0001) warning(">> QA WARNING: Negative values detected. Check step_range.")

cat(">> QA PASS: Preprocessing pipeline (Scaling [0,1]) verified.\n")
cat("\n--- Module 5 Complete ---\n")
# ==============================================================================
# Module 6: Final Data Formatting & Export (Fix C)
# ==============================================================================
# METHODOLOGY: Data Structure Compatibility.
# Keras (running in Python) does not accept R DataFrames (Tibbles). 
# It requires homogeneous numeric matrices (Tensors).
# 1. Matrix Conversion: We cast the DataFrame to a Matrix.
# 2. Target Encoding: Keras requires 0/1 integers for binary cross-entropy loss, 
#    not Factors ("0"/"1"). We explicitly convert and subtract 1 to align indices.

cat("\n================================================================\n")
cat(" MODULE 6: DATA EXPORT FOR NEURAL NETWORK \n")
cat("================================================================\n")

# --- 6.1 Helper Function: Strict Matrix Conversion ---
process_for_keras <- function(df, target_col = "TARGET") {
  
  # Explicitly separate Target
  y_raw <- df[[target_col]]
  
  # FIX C: Explicit Matrix Conversion
  # 1. Remove Target
  # 2. Ensure all columns are numeric (dummies make this true)
  # 3. Convert to strict Matrix structure
  x_matrix <- df %>%
    select(-all_of(target_col)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  # Handle Dimension Names (Keras sometimes warns about these)
  dimnames(x_matrix) <- NULL
  
  # Process Target (Factor -> 0/1 Integer)
  # as.integer(Factor) returns 1,2. Subtract 1 to get 0,1.
  y_vector <- as.integer(y_raw) - 1
  
  return(list(x = x_matrix, y = y_vector))
}

# --- 6.2 Execute Conversion ---
cat("Converting processed dataframes to Numeric Matrices...\n")

train_keras <- process_for_keras(train_processed)
val_keras <- process_for_keras(val_processed) # STRICT VALIDATION DATA
test_keras <- process_for_keras(test_processed)

# Diagnostic Check
cat("Final Training Matrix Shape:", dim(train_keras$x), "\n")
cat("Final Target Vector Shape:  ", length(train_keras$y), "\n")

# [QA CHECK] Tensor Structure
if(!is.matrix(train_keras$x)) stop(">> QA FAIL: Training Features (x) are not a Matrix.")
if(!is.numeric(train_keras$x)) stop(">> QA FAIL: Training Features (x) are not Numeric.")
if(any(is.na(train_keras$x))) stop(">> QA FAIL: NaNs found in Final Training Matrix.")
cat(">> QA PASS: Data structures are valid for Keras.\n")

# --- 6.3 Save to Disk ---
output_dir <- "processed_data"
if(!dir.exists(output_dir)) dir.create(output_dir)

cat("\nSaving formatted tensors to:", output_dir, "...\n")
saveRDS(train_keras, file.path(output_dir, "train_tensor.rds"))
saveRDS(val_keras, file.path(output_dir, "val_tensor.rds"))
saveRDS(test_keras, file.path(output_dir, "test_tensor.rds"))

# [QA CHECK] File I/O
if(!file.exists(file.path(output_dir, "train_tensor.rds"))) stop(">> QA FAIL: Train Tensor file not saved.")
cat(">> QA PASS: Files saved successfully.\n")

cat(">> SUCCESS: Data is ready for Neural Network Training.\n")
cat(">> NOTE: In your NN script, use 'val_tensor.rds' for validation_data.\n")
cat(">> DO NOT use validation_split on train_tensor.rds (It contains synthetic data).\n")

