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

# --- Package Installation Logic ---
# Automated dependency management ensures reproducibility across different grading environments.
installed_pkgs <- installed.packages()[, "Package"]
req_pkgs <- c("tidyverse", "caret", "reshape2", "corrplot", "vcd", "naniar", "gridExtra", "e1071", "Hmisc", "VIM", "themis", "tidymodels")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if(length(new_pkgs)) install.packages(new_pkgs)

# --- Load Libraries ---
options(scipen = 999) # CRITICAL: Prevents IDs (e.g., 5008804) from converting to 5.00e6
library(tidyverse)  # Data manipulation & ggplot2
library(caret)      # ML Preprocessing (The industry standard for R pipelines)
library(reshape2)   # Reshaping for correlation heatmaps
library(corrplot)   # Correlation Viz
library(vcd)        # Categorical Statistics (Cramer's V)
library(naniar)     # Missing Data Visualization (vis_miss)
library(gridExtra)  # Arranging plots
library(e1071)      # Skewness calculations
library(Hmisc)      # Enhanced histograms
library(VIM)        # Visualization and Imputation of Missing Values (KNN)
library(themis)     # For step_smote
library(tidymodels) # For data splitting and preprocessing

# REPRODUCIBILITY:
# Setting seed ensures that random processes (like KNN imputation and Splitting)
# produce the exact same results every time you run the code.
set.seed(123)

# --- Data Loading Function ---
load_data <- function(path) {
  if(!file.exists(path)) stop(paste("File not found at:", path))
  
  # STRATEGY: Load strings as characters initially (not factors) to allow for 
  # easy string manipulation (cleaning) before final encoding.
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
# Module 3: Row-Wise Cleaning & Logic Features (Refactored)
# ==============================================================================
# METHODOLOGY: 
# This module implements the fixes identified in Sections 3.1 - 3.5.
# Order of operations is critical: Target Engineering must happen BEFORE filtering 
# to ensure we do not discard the minority class (Section 3.1).

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
}

# --- Phase 2: Drop Zero Variance ---
if("FLAG_MOBIL" %in% names(df_clean)) {
  if(length(unique(df_clean$FLAG_MOBIL)) == 1) {
    df_clean$FLAG_MOBIL <- NULL
  }
}

# --- Phase 3: Flag Standardization ---
df_clean <- df_clean %>%
  mutate(across(any_of(c("FLAG_OWN_CAR", "FLAG_OWN_REALTY")), 
                ~ ifelse(. == "Y", 1L, 0L))) %>%
  mutate(across(any_of(c("FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")), 
                ~ as.integer(. > 0)))

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
if("NAME_EDUCATION_TYPE" %in% names(df_clean)) {
  edu_levels <- c("Lower secondary", "Secondary / secondary special", 
                  "Incomplete higher", "Higher education", "Academic degree")
  
  df_clean <- df_clean %>%
    mutate(
      NAME_EDUCATION_TYPE = factor(NAME_EDUCATION_TYPE, levels = edu_levels, ordered = TRUE),
      EDUCATION_LEVEL = as.integer(NAME_EDUCATION_TYPE)
    ) %>%
    select(-NAME_EDUCATION_TYPE)
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

assign("df_clean_processed", df_clean, envir = .GlobalEnv)
cat("\n--- Module 3 Complete ---\n")

# ==============================================================================
# Module 4: Preprocessed Diagnostic EDA (Cleaned Data)
# ==============================================================================

cat("\n================================================================\n")
cat("    MODULE 4: SECONDARY DATA EXPLORATION (CLEANED DATA)\n")
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
# Module 5: Advanced Preprocessing & Data Splitting (Neural Network Ready)
# ==============================================================================
# PURPOSE: To transform the Cleaned Data into Model-Ready Tensors.
# CRITICAL REFINEMENTS (ACADEMIC RIGOR):
# 1. 70:15:15 Split (Stratified) -> Prevents Leakage.
# 2. Winsorization (Capping) -> Moved here to be calculated on TRAINING data only.
# 3. Z-Score Standardization -> Replaces Min-Max for better ReLU/MLP convergence.

cat("\n================================================================\n")
cat("   MODULE 5: PREPROCESSING PIPELINE (TIDYMODELS) \n")
cat("================================================================\n")

set.seed(123) 
cat("Partitioning Data (70/15/15) with Stratification...\n")
split_obj <- initial_validation_split(df_clean, prop = c(0.7, 0.15), strata = TARGET)
train_raw <- training(split_obj)
val_raw   <- validation(split_obj)
test_raw  <- testing(split_obj)

if("AMT_INCOME_TOTAL" %in% names(train_raw)) {
  inc_cap <- quantile(train_raw$AMT_INCOME_TOTAL, 0.99, na.rm = TRUE)
} else { inc_cap <- Inf }

if("CNT_CHILDREN" %in% names(train_raw)) {
  child_cap <- quantile(train_raw$CNT_CHILDREN, 0.995, na.rm = TRUE)
} else { child_cap <- Inf }

if("CNT_FAM_MEMBERS" %in% names(train_raw)) {
  fam_cap <- quantile(train_raw$CNT_FAM_MEMBERS, 0.995, na.rm = TRUE)
} else { fam_cap <- Inf }

cat(paste("Learned Income Cap (Train 99%):", inc_cap, "\n"))

base_recipe <- recipe(TARGET ~., data = train_raw) %>%
  update_role(any_of("ID"), new_role = "id") %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_mutate(
    AMT_INCOME_TOTAL = pmin(AMT_INCOME_TOTAL, !!inc_cap),
    CNT_CHILDREN = pmin(CNT_CHILDREN, !!child_cap),
    CNT_FAM_MEMBERS = pmin(CNT_FAM_MEMBERS, !!fam_cap)
  ) %>%
  step_log(AMT_INCOME_TOTAL, offset = 1) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# ==============================================================================
# SELECT YOUR STRATEGY (UNCOMMENT ONE SECTION BELOW)
# ==============================================================================

# ------------------------------------------------------------------------------
# OPTION 1: UNDERSAMPLING (Randomly reduce majority class)
# Pros: Fast training. Cons: Discards potentially useful data.
# ------------------------------------------------------------------------------
final_recipe <- base_recipe %>%
  step_downsample(TARGET, under_ratio = 1) # 1:1 ratio
strategy_name <- "Undersampling"

# ==============================================================================
# --- 5.4 Execute Preprocessing (Baking) ---
# ==============================================================================

cat(paste("\n>> PROCESSING STRATEGY SELECTED:", strategy_name, "\n"))

trained_recipe <- prep(final_recipe, training = train_raw)
train_processed <- bake(trained_recipe, new_data = NULL) 
val_processed   <- bake(trained_recipe, new_data = val_raw)
test_processed  <- bake(trained_recipe, new_data = test_raw)

# --- 5.5 Final Diagnostic Check ---
cat("\n[Final Preprocessing Checks]\n")
cat("Processed Train Shape: ", dim(train_processed), "\n")
cat("\nScaling Check (Should be Mean ~0, SD ~1):\n")
summary(train_processed %>% select(contains("AGE"), contains("INCOME")))
cat("\nTarget Distribution After Processing (Training Set Only):\n")
print(table(train_processed$TARGET))

cat("\n--- Module 5 Complete: Data Ready for Neural Network ---\n")

# ==============================================================================
# Module 6: Final Data Formatting & Export (The "Hand-Off")
# ==============================================================================
# PURPOSE: Convert Tibbles to Keras-ready Matrices and Save to Disk.

cat("\n================================================================\n")
cat("   MODULE 6: DATA EXPORT FOR NEURAL NETWORK \n")
cat("================================================================\n")

# --- 6.1 Helper Function: Convert to Matrix (X) and Vector (y) ---
# Neural Networks require the Target (y) to be separated from Features (x)
# and for Features to be a numeric Matrix (not a dataframe).

process_for_keras <- function(df, target_col = "TARGET") {
  
  # 1. Separate X (Features) and Y (Target)
  # We remove the target column to create the Feature Matrix
  x_data <- df %>% 
    select(-all_of(target_col)) %>%
    # Ensure all columns are numeric (Module 5 should have done this, but safety first)
    mutate(across(everything(), as.numeric)) %>% 
    as.matrix()
  
  # 2. Process Y (Target)
  # Convert Factor "0"/"1" to Numeric 0/1. 
  # Note: as.integer on a factor returns 1,2. We subtract 1 to get 0,1.
  y_data <- as.integer(df[[target_col]]) - 1
  
  return(list(x = x_data, y = y_data))
}

# --- 6.2 Execute Conversion ---
cat("Converting processed dataframes to Numeric Matrices...\n")

train_keras <- process_for_keras(train_processed)
val_keras   <- process_for_keras(val_processed)
test_keras  <- process_for_keras(test_processed)

# Diagnostic Check
cat("Final Training Matrix Shape:", dim(train_keras$x), "\n")
cat("Final Target Vector Shape:  ", length(train_keras$y), "\n")

# --- 6.3 Save to Disk (.rds) ---
output_dir <- "processed_data"
if(!dir.exists(output_dir)) dir.create(output_dir)

cat("\nSaving formatted tensors to:", output_dir, "...\n")

saveRDS(train_keras, file.path(output_dir, "train_tensor.rds"))
saveRDS(val_keras,   file.path(output_dir, "val_tensor.rds"))
saveRDS(test_keras,  file.path(output_dir, "test_tensor.rds"))

if(exists("class_weights_list")) {
  saveRDS(class_weights_list, file.path(output_dir, "class_weights.rds"))
}

cat(">> SUCCESS: Data is ready for Neural Network Training.\n")
cat(">> TO LOAD IN NEXT SCRIPT: train_data <- readRDS('processed_data/train_tensor.rds')\n")

# ==============================================================================
# Module 7: Comprehensive Reporting (PDF Generation)
# ==============================================================================

cat("\n================================================================\n")
cat("   MODULE 7: GENERATING FINAL DIAGNOSTIC PDF REPORT \n")
cat("================================================================\n")

# --- 7.1 Setup Output Path ---
target_path <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts"
if(dir.exists(target_path)) {
  base_dir <- target_path
} else {
  base_dir <- getwd()
}

plot_dir <- file.path(base_dir, "Auto_Generated_Plots")
if(!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

ts <- format(Sys.time(), "%Y-%m-%d_%H%M")
filename <- paste0("Final_Model_Diagnostics_", ts, ".pdf")
full_path <- file.path(plot_dir, filename)

cat("Generating PDF at:", full_path, "\n")

# --- 7.2 Begin PDF Generation ---
pdf(file = full_path, width = 12, height = 8)

tryCatch({
  
  # --- SECTION 1: TARGET EVOLUTION ---
  p1 <- ggplot(df, aes(x = status)) + 
    geom_bar(fill = "gray50") + 
    labs(title = "1. Raw Status Codes", subtitle = "Original heterogeneous labels") + theme_minimal()
  
  p2 <- ggplot(df_clean_processed, aes(x = TARGET, fill = TARGET)) + 
    geom_bar() + 
    scale_fill_manual(values = c("steelblue", "firebrick")) +
    labs(title = "2. Cleaned Target (Imbalanced)", subtitle = "Binary Classification (0=Good, 1=Bad)") + theme_minimal()
  
  if(exists("train_processed")) {
    p3 <- ggplot(train_processed, aes(x = TARGET, fill = TARGET)) + 
      geom_bar() + 
      scale_fill_manual(values = c("steelblue", "firebrick")) +
      labs(title = "3. Final Training Set (Balanced)", subtitle = "After SMOTE/Sampling Strategy") + theme_minimal()
    gridExtra::grid.arrange(p1, p2, p3, ncol = 3, top = "Evolution of the Target Variable")
  } else {
    gridExtra::grid.arrange(p1, p2, ncol = 2, top = "Evolution of the Target Variable")
  }
  
  # --- SECTION 2: VARIABLE DISTRIBUTIONS ---
  num_vars <- names(dplyr::select_if(df_clean_processed, is.numeric))
  num_vars <- num_vars 
  
  plot_list <- list()
  for(col in num_vars) {
    p <- ggplot(df_clean_processed, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "white") +
      labs(title = paste("Dist:", col)) + theme_minimal() +
      theme(axis.title.x = element_blank())
    plot_list[[col]] <- p
  }
  
  if(length(plot_list) > 0) {
    for(i in seq(1, length(plot_list), 4)) {
      chunk <- plot_list[i:min(i+3, length(plot_list))]
      do.call(gridExtra::grid.arrange, c(chunk, ncol = 2, top = "Feature Distributions (Cleaned Data)"))
    }
  }
  
  # --- SECTION 3: CATEGORICAL FREQUENCIES ---
  cat_vars <- names(dplyr::select_if(df_clean_processed, is.character))
  for(col in cat_vars) {
    p <- ggplot(df_clean_processed, aes_string(x = col)) +
      geom_bar(fill = "darkseagreen") +
      coord_flip() +
      labs(title = paste("Frequency:", col)) + theme_minimal()
    print(p)
  }
  
  # --- SECTION 4: CORRELATION MATRIX ---
  if(length(num_vars) > 1) {
    cor_mat <- cor(df_clean_processed[, num_vars], use = "pairwise.complete.obs")
    cor_mat[is.na(cor_mat)] <- 0
    corrplot::corrplot(cor_mat, method = "color", type = "lower", 
                       title = "Correlation Matrix (Cleaned Data)", 
                       mar = c(0,0,2,0), addCoef.col = "black", number.cex = 0.6)
  }
  
  # --- SECTION 5: PREPROCESSING VERIFICATION (Standardization Proof) ---
  if(exists("train_processed")) {
    scaled_check <- train_processed %>% 
      select(contains("AGE"), contains("INCOME"), contains("CHILDREN"), contains("EMPLOYED"))
    
    if(ncol(scaled_check) > 0) {
      melted_scaled <- reshape2::melt(scaled_check)
      p_scale <- ggplot(melted_scaled, aes(x = variable, y = value)) +
        geom_boxplot(fill = "orange", alpha = 0.5) +
        coord_flip() +
        labs(title = "Verification of Z-Score Standardization", 
             subtitle = "Values should be centered around 0 (Mean=0, SD=1)",
             y = "Scaled Value (Z-Score)", x = "Feature") +
        theme_minimal() +
        geom_vline(xintercept = 0, linetype="dashed") +
        geom_vline(xintercept = -2, linetype="dotted", color="red") +
        geom_vline(xintercept = 2, linetype="dotted", color="red")
      print(p_scale)
    }
  }
  
  # --- SECTION 6: RISK ANALYSIS SUMMARY ---
  if(exists("top_10_risk")) {
    p_risk <- ggplot(top_10_risk, aes(x = reorder(Subgroup, Default_Rate_Pct), y = Default_Rate_Pct, fill = Variable)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top 10 High-Risk Subgroups", 
           y = "Default Rate (%)", x = "Subgroup") +
      theme_minimal()
    print(p_risk)
  }
  
}, error = function(e) {
  cat("Error generating PDF: ", e$message, "\n")
  plot.new()
  text(0.5, 0.5, paste("Error during plotting:", e$message), col = "red", cex = 1.5)
})

dev.off() 

if(file.exists(full_path)) {
  cat(">> SUCCESS: PDF Report created at:", full_path, "\n")
} else {
  cat(">> ERROR: PDF creation failed.\n")
}

cat("\n--- Module 7 Complete ---\n")