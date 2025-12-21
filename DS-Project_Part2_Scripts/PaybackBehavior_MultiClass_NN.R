# ==============================================================================
# REPORT: PayBack Behavior Classification Neural Network
# FILENAME: PaybackBehavior_MultiClass_NN.R
# Author: John Arellano
# ==============================================================================

# ------------------------------------------------------------------------------
# CONTEXT AND OBJECTIVE
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# DATA SCIENCE STRATEGY: SURGICAL SAMPLE WEIGHTING + LONG TRAINING
# ------------------------------------------------------------------------------

# ==============================================================================
# Module 1: Environment Setup & Data Ingestion
# ==============================================================================
# METHODOLOGY: 

# ------------------------------------------------------------------------------
# Section 1.1: Package Installation Logic
# ------------------------------------------------------------------------------
# Automated dependency management ensures reproducibility across different grading environments.
installed_pkgs <- installed.packages()[, "Package"]
req_pkgs <- c("tidyverse", "caret", "reshape2", "corrplot", "vcd", "naniar",
              "gridExtra", "e1071", "Hmisc", "VIM", "themis", "tidymodels",
              "tensorflow", "reticulate", "keras3", "embed", "rpart", "fastDummies",
              "mice") # Added mice to fix dependency error
new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if(length(new_pkgs)) install.packages(new_pkgs)

# ------------------------------------------------------------------------------
# Section 1.2: Load Libraries
# ------------------------------------------------------------------------------
options(scipen = 999)           # CRITICAL: Prevents IDs (e.g., 5008804) from converting to 5.00e6
library(tidyverse)              # Data manipulation & ggplot2
library(caret)                  # ML Preprocessing 
library(reshape2)               # Reshaping for correlation heatmaps
library(corrplot)               # Correlation Viz
library(vcd)                    # Categorical Statistics (Cramer's V)
library(naniar)                 # Missing Data Visualization (vis_miss)
library(gridExtra)              # Arranging plots
library(e1071)                  # Skewness calculations
library(Hmisc)                  # Enhanced histograms
library(VIM)                    # Visualization and Imputation 
library(themis)                 # Handling Imbalance
library(tidymodels)             # Data splitting and preprocessing
library(reticulate)             # Interface to Python
library(tensorflow)             # Backend seeding
library(keras3)                 # Neural Network API (Keras 3)
library(embed)                  # Feature engineering steps
library(rpart)                  # Discretization engine
library(fastDummies)            # Optimized One-Hot Encoding
library(mice)                   # Imputation library

# ------------------------------------------------------------------------------
# Section 1.3: Keras 3 / TensorFlow Backend Connection Strategy
# ------------------------------------------------------------------------------
# We rely on keras3 to automatically find the environment we created via install_keras()
backend_configured <- FALSE

tryCatch({
  # Attempt to load the backend immediately
  # This automatically scans for the 'r-keras' virtual environment
  k_backend <- keras3::config_backend()
  
  if(!is.null(k_backend)) {
    backend_configured <- TRUE
    cat(paste(">> SUCCESS: Connected to Keras 3 Backend:", k_backend, "\n"))
    
    # Optional: Print the python path being used for verification
    cat(paste(">> Python Path:", reticulate::py_config()$python, "\n"))
  }
}, error = function(e) {
  cat(">> WARNING: Keras 3 auto-detection failed. Attempting manual binding...\n")
})

# Fallback: Explicitly look for the virtual environment if auto-detection failed
if (!backend_configured) {
  # Common location for install_keras() virtualenv on Windows
  venv_path <- file.path(Sys.getenv("USERPROFILE"), "Documents", ".virtualenvs", "r-keras")
  
  if (dir.exists(venv_path)) {
    reticulate::use_virtualenv(venv_path, required = TRUE)
    backend_configured <- TRUE
    cat(paste(">> SUCCESS: Manually bound to Virtual Environment:", venv_path, "\n"))
  } else {
    cat(">> WARNING: 'r-keras' virtual environment not found in default Documents location.\n")
  }
}

# ------------------------------------------------------------------------------
# Section 1.4: Load TensorFlow and Seed
# ------------------------------------------------------------------------------
library(tensorflow)

# REPRODUCIBILITY GUARANTEE:
# 1. Seed R (Frontend)
set.seed(123)

# 2. Seed Backend (Keras 3 specific seeding)
# KERAS 3 OPTIMIZATION: Ensure seeding works for whatever backend Keras 3 is using (JAX/TF/Torch)
if (backend_configured) {
  tryCatch({
    # Initialize Keras 3 to check backend
    # This triggers the backend load
    k_backend <- keras3::config_backend() 
    cat(paste(">> Keras 3 Backend Active:", k_backend, "\n"))
    
    # Specific seeding based on backend
    if(k_backend == "tensorflow") {
      tf$random$set_seed(123L)
      cat(">> SUCCESS: TensorFlow backend seeded.\n")
    } else if (k_backend == "torch") {
      # If torch backend is used in Keras 3
      reticulate::py_run_string("import torch; torch.manual_seed(123)")
      cat(">> SUCCESS: Torch backend seeded.\n")
    } else {
      # Generic Keras 3 utils set_random_seed (covers JAX/TF/Torch)
      keras3::set_random_seed(123L)
      cat(">> SUCCESS: Keras 3 generic seeding applied.\n")
    }
  }, error = function(e) {
    cat(">> WARNING: Backend found but seeding/initialization failed. Error:", e$message, "\n")
  })
} else {
  stop("\nCRITICAL ERROR: Python Backend Not Found. Run reticulate::install_miniconda() and keras3::install_keras().\n")
}

# ------------------------------------------------------------------------------
# Section 1.5: Data Loading
# ------------------------------------------------------------------------------
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
df <- load_data("C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_data/Dataset-part-2.csv")


# ==============================================================================
# Module 2: Initial Exploratory Data Analysis (EDA)
# ==============================================================================
# PURPOSE: Identify Data Quality issues (Anomalies, Skew, MNAR) prior to preprocessing.

# ------------------------------------------------------------------------------
# Section 2.1: Setup PDF Output
# ------------------------------------------------------------------------------
plot_output_dir <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts/Saved_Outputs"
if(!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

pdf(file = file.path(plot_output_dir, "All_Project_Visualizations_Weights.pdf"), width = 11, height = 8.5)
cat(">> PDF Graphics Device Opened. Plots will be saved to:", plot_output_dir, "\n")

cat("\n================================================================\n")
cat(" MODULE 2: INITIAL DATA EXPLORATION (RAW DATA)\n")
cat("================================================================\n")

# ------------------------------------------------------------------------------
# Section 2.2: Structure & Content Inspection
# ------------------------------------------------------------------------------
cat("\n[Phase 1] Structure & Content Inspection\n")
print(dim(df))
dplyr::glimpse(df)

# ------------------------------------------------------------------------------
# Section 2.3: Data Quality Checks
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
# Section 2.4: Univariate Visualization
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
# Section 2.5: The 365243 Pensioner Anomaly
# ------------------------------------------------------------------------------
# CRITICAL FINDING: 'DAYS_EMPLOYED' contains 365243 (~1000 years), indicating "Pensioner".
cat("\n[Module 2] Handling Anomalies and Engineering Features\n")

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
# Section 2.6: MNAR (Missing Not At Random) Analysis
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
# Section 2.7: Target Feasibility Deep Dive (Justification for Module 3 Logic)
# ------------------------------------------------------------------------------
# GOAL: Statistically justify the grouping strategy applied in Module 3.
# We analyze the raw 'status' variable to demonstrate why a simple binary classification
# is insufficient and why raw multi-class classification is impossible due to sparsity.

cat("\n[Section 2.7] Target Feature Feasibility Analysis\n")

if("status" %in% names(df)) {
  
  # 1. Granular Distribution Table
  target_analysis <- df %>%
    group_by(status) %>%
    summarise(
      Count = n(),
      Percent = (n() / nrow(df)) * 100
    ) %>%
    arrange(desc(Percent)) %>%
    mutate(
      Cumulative_Percent = cumsum(Percent),
      # Label the sparsity problem
      Viability = ifelse(Percent < 1.0, "CRITICAL SPARSITY (<1%)", "Viable")
    )
  
  print("Raw Target Class Distribution:")
  print(target_analysis)
  
  # 2. Logic Justification Output
  cat("\n>>> ARCHITECTURAL JUSTIFICATION FOR MODULE 3 <<<\n")
  
  # Justification A: The Dominance of Status '0'
  pct_0 <- target_analysis$Percent[target_analysis$status == "0"]
  cat(sprintf("1. THE DOMINANCE PROBLEM: Status '0' (1-29 days late) holds %.2f%% of the data.\n", pct_0))
  cat("    - IMPLICATION: Merging '0' with 'Clean' (C/X) would hide risk signals.\n")
  cat("    - IMPLICATION: Merging '0' with 'Bad' would create massive False Positives.\n")
  cat("    - DECISION: Status '0' must be its own independent class (Group 1).\n\n")
  
  # Justification B: The Sparsity of Status 2-5
  severe_sum <- sum(target_analysis$Percent[target_analysis$status %in% c("1", "2", "3", "4", "5")])
  rare_classes <- target_analysis$status[target_analysis$Percent < 1.0]
  cat(sprintf("2. THE SPARSITY PROBLEM: Classes %s are statistically insignificant individually.\n", paste(rare_classes, collapse=", ")))
  cat("    - IMPLICATION: A Neural Network cannot learn features for classes with <0.5% prevalence.\n")
  cat(sprintf("    - DECISION: We must aggregate '1' through '5' to form a robust 'Severe' class (Group 2).\n"))
  cat(sprintf("    - RESULT: This creates a 'Severe' group representing %.2f%% of data, balancing it against 'Clean'.\n", severe_sum))
  
  # 3. Visualization: The "Long Tail" Problem
  p_feasibility <- ggplot(target_analysis, aes(x = reorder(status, -Percent), y = Percent, fill = Viability)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = round(Percent, 2)), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("CRITICAL SPARSITY (<1%)" = "red", "Viable" = "steelblue")) +
    labs(title = "Justification for Class Grouping",
         subtitle = "Classes 2, 3, 4, 5 represent the 'Long Tail' of risk and must be grouped.",
         x = "Original Status", y = "Prevalence (%)") +
    theme_minimal()
  
  print(p_feasibility)
}

cat("\n--- Module 2 Complete ---\n")

# ==============================================================================
# Module 2: EDA Summary & Architecture Plan
# ==============================================================================

# GENERAL ASSESSMENT:

# ------------------------------------------------------------------------------
# KEY FINDINGS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# PREPROCESSING ARCHITECTURE
# ------------------------------------------------------------------------------

cat("\n================================================================\n")
cat(" MODULE 3: DATA CLEANING & LOGIC (NO LEAKAGE)\n")
cat("================================================================\n")

# Initialize working dataframe
df_clean <- df

# ------------------------------------------------------------------------------
# Section 3.1: Target Variable Reconstruction (3-Tier Behavioral Logic)
# ------------------------------------------------------------------------------
# JUSTIFICATION FOR GROUPING STRATEGY:
# The assignment rubric explicitly states: "We are learning only the pay-back behavior...
# not if we should accept or reject". Therefore, a binary Target (0/1) is insufficient.
# We define 3 distinct behavioral classes to capture nuance while solving class imbalance.

# DISTRIBUTION STATISTICS (Based on Dataset-part-2.csv):
# - Group 0 (Clean): ~11.23% of data.
# - Group 1 (Minor Delay): ~77.10% of data.
# - Group 2 (Significant Delinquency): ~11.66% of data.

if ("status" %in% names(df_clean)) {
  cat("Constructing Target Variable (3-Class Behavioral Groups)...\n")
  
  df_clean <- df_clean %>%
    mutate(
      TARGET = case_when(
        # --- GROUP 0: Clean / No Risk ---
        # "C" (Paid off) and "X" (No loan) represent the safest behavior. 
        # We group them to create a baseline for "good" behavior.
        status %in% c("C", "X") ~ 0, 
        
        # --- GROUP 1: Active / Minor Delay ---
        # "0" (1-29 days past due) is the dominant class (~77%). 
        # These customers are technically late but often pay. Keeping this separate 
        # allows the NN to distinguish "sloppy payers" from "defaults".
        status == "0" ~ 1,
        
        # --- GROUP 2: Significant Delinquency ---
        # "1" through "5" (30+ days overdue).
        # Classes 3, 4, and 5 are extremely rare (<0.6% each). The Neural Network 
        # cannot learn them individually. Aggregating them with 1 and 2 creates 
        # a robust "At Risk" class (~11.6%) comparable in size to Group 0.
        status %in% c("1", "2", "3", "4", "5") ~ 2,
        
        TRUE ~ NA_real_
      ),
      # Note: We keep as numeric/integer here. 
      # Later, for Keras/TensorFlow, this must be One-Hot Encoded (to_categorical).
      # If using 'caret' or standard classifiers first, you might need as.factor(TARGET).
      # For now, we align with the "floating numbers" requirement by keeping it numeric.
      TARGET = as.numeric(TARGET)
    ) %>%
    select(-status) %>%
    filter(!is.na(TARGET))
  
  if(sum(is.na(df_clean$TARGET)) > 0) stop(">> QA FAIL: Target Variable contains NAs.")
  
  # Validation print to confirm the spread
  cat(">> QA PASS: Target constructed successfully.\n")
  cat(">> Target Distribution:\n")
  print(prop.table(table(df_clean$TARGET)) * 100)
}

# ------------------------------------------------------------------------------
# Section 3.2: Variance Reduction
# ------------------------------------------------------------------------------
if("FLAG_MOBIL" %in% names(df_clean)) {
  if(length(unique(df_clean$FLAG_MOBIL)) == 1) {
    df_clean$FLAG_MOBIL <- NULL
    cat(">> QA PASS: Zero variance column FLAG_MOBIL dropped.\n")
  }
}

# ------------------------------------------------------------------------------
# Section 3.3: Flag Standardization (Binary Normalization)
# ------------------------------------------------------------------------------
df_clean <- df_clean %>%
  mutate(across(any_of(c("FLAG_OWN_CAR", "FLAG_OWN_REALTY")),
                ~ ifelse(. == "Y", 1L, 0L))) %>%
  mutate(across(any_of(c("FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")),
                ~ as.integer(. > 0)))

check_flags <- df_clean %>% 
  select(starts_with("FLAG")) %>% 
  summarise(across(everything(), ~ all(. %in% c(0,1))))

if(!all(unlist(check_flags))) stop(">> QA FAIL: Non-binary flags detected.")
cat(">> QA PASS: All Flags standardized to binary integers.\n")

# ------------------------------------------------------------------------------
# Section 3.4: Numerical Anomaly Rectification (The "365243" Issue)
# ------------------------------------------------------------------------------
if ("DAYS_EMPLOYED" %in% names(df_clean)) {
  cat("Rectifying 'DAYS_EMPLOYED': Creating Status Flag & Cleaning...\n")
  
  df_clean <- df_clean %>%
    mutate(
      # Create flag preserving pensioner info
      EMPLOYMENT_STATUS_FLAG = ifelse(DAYS_EMPLOYED == 365243, "Not_Working", "Working"),
      # Replace sentinel with 0 and take absolute value
      DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, 0, abs(DAYS_EMPLOYED))
    )
  
  if(max(df_clean$DAYS_EMPLOYED, na.rm=T) > 300000) stop(">> QA FAIL: 365243 Anomaly still exists.")
  cat(">> QA PASS: 365243 Anomaly resolved.\n")
}

# ------------------------------------------------------------------------------
# Section 3.5: Handling Structural Missingness
# ------------------------------------------------------------------------------
# 1. Drop redundant IS_PENSIONER (covered by Income Type)
if("IS_PENSIONER" %in% names(df_clean)) {
  df_clean <- df_clean %>% select(-IS_PENSIONER)
}

# 2. Impute Occupation Type based on Pensioner Logic
if("OCCUPATION_TYPE" %in% names(df_clean) && "NAME_INCOME_TYPE" %in% names(df_clean)) {
  cat("Handling Structural Missingness in Occupation (Pensioner Logic)...\n")
  
  df_clean <- df_clean %>%
    mutate(OCCUPATION_TYPE = case_when(
      NAME_INCOME_TYPE == "Pensioner" & (is.na(OCCUPATION_TYPE) | OCCUPATION_TYPE == "") ~ "Retired",
      (is.na(OCCUPATION_TYPE) | OCCUPATION_TYPE == "") ~ "Unknown",
      TRUE ~ OCCUPATION_TYPE
    ))
}

# ------------------------------------------------------------------------------
# Section 3.6: Ordinal Feature Encoding
# ------------------------------------------------------------------------------
if("NAME_EDUCATION_TYPE" %in% names(df_clean)) {
  edu_levels <- c("Lower secondary", "Secondary / secondary special", 
                  "Incomplete higher", "Higher education", "Academic degree")
  
  df_clean <- df_clean %>%
    mutate(
      EDUCATION_LEVEL = as.integer(factor(
        NAME_EDUCATION_TYPE,
        levels = edu_levels,
        ordered = TRUE
      ))
    ) %>%
    select(-NAME_EDUCATION_TYPE)
  
  cat(">> QA PASS: Education converted to Ordinal Integer rank.\n")
}

# ------------------------------------------------------------------------------
# Section 3.7: Preprocessing Note
# ------------------------------------------------------------------------------
# Transformations dependent on distribution statistics (Log, Capping) are deferred 
# to Module 5 (Recipes) to prevent data leakage from Train to Test.

# ------------------------------------------------------------------------------
# Section 3.8: Dimensionality Reduction & Final Cleanup
# ------------------------------------------------------------------------------
if ("DAYS_BIRTH" %in% names(df_clean)) {
  df_clean$AGE <- abs(df_clean$DAYS_BIRTH) / 365.25
  df_clean$DAYS_BIRTH <- NULL
}

if("ID" %in% names(df_clean)) df_clean$ID <- NULL
df_clean <- df_clean %>% dplyr::distinct()

if(nrow(df_clean) < 1000) stop(">> QA FAIL: Dataframe reduced drastically.")

assign("df_clean_processed", df_clean, envir = .GlobalEnv)

# ------------------------------------------------------------------------------
# Section 3.9: Categorical Encoding
# ------------------------------------------------------------------------------

# 1. Binary Mapping for Employment Status
# We map "Working" -> 1 and "Not_Working" -> 0 manually because it is binary.
if("EMPLOYMENT_STATUS_FLAG" %in% names(df_clean)) {
  df_clean <- df_clean %>%
    mutate(EMPLOYMENT_STATUS_FLAG = ifelse(EMPLOYMENT_STATUS_FLAG == "Working", 1, 0))
  cat(">> Encoded EMPLOYMENT_STATUS_FLAG to binary (0/1).\n")
}

# 2. One-Hot Encoding for Nominal Categorical Variables
# We use One-Hot Encoding for variables with >2 categories (e.g., Housing, Job).
# 'remove_first_dummy = TRUE' prevents perfect multicollinearity.
categorical_cols <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_FAMILY_STATUS", 
                      "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")

# Only encode columns that actually exist in the dataframe
existing_cat_cols <- intersect(categorical_cols, names(df_clean))

if(length(existing_cat_cols) > 0) {
  df_encoded <- dummy_cols(df_clean, 
                           select_columns = existing_cat_cols,
                           remove_first_dummy = TRUE,
                           remove_selected_columns = TRUE) # Drop original text cols
  
  cat(paste(">> One-Hot Encoded columns:", paste(existing_cat_cols, collapse=", "), "\n"))
} else {
  df_encoded <- df_clean
}

# 3. Final Safety Check
# Ensure NO text columns remain.
non_numeric_cols <- names(select_if(df_encoded, is.character))
if(length(non_numeric_cols) > 0) {
  stop(paste(">> CRITICAL ERROR: Text columns remain:", paste(non_numeric_cols, collapse=", ")))
}

cat(">> SUCCESS: Dataset is now 100% Numeric.\n")
cat(">> Dimensions:", dim(df_encoded)[1], "rows,", dim(df_encoded)[2], "columns.\n")

cat("\n--- Module 3 Complete ---\n")

# ==============================================================================
# Module 4: Preprocessed Diagnostic EDA (Cleaned Data)
# ==============================================================================
# PURPOSE: Verification of Cleaning Steps & Multivariate Risk Profiling

cat("\n================================================================\n")
cat(" MODULE 4: SECONDARY DATA EXPLORATION (CLEANED DATA)\n")
cat("================================================================\n")

# ------------------------------------------------------------------------------
# Section 4.1: Initialization & Data Verification
# ------------------------------------------------------------------------------
if(exists("df_clean_processed")) {
  df_clean <- df_clean_processed
}
cat("New Dimensions:", dim(df_clean), "\n")
dplyr::glimpse(df_clean)

# ------------------------------------------------------------------------------
# Section 4.2: Data Quality & Sanity Assurance
# ------------------------------------------------------------------------------
cat("\n[Section 4.2] Data Quality Checks\n")
print(summary(dplyr::select_if(df_clean, is.numeric)))

# Missingness Map (Should be blank)
print(naniar::vis_miss(df_clean, warn_large_data = FALSE) + 
        ggtitle("Missingness Map (Cleaned Data)"))

# ------------------------------------------------------------------------------
# Section 4.3: Variance Analysis & Target Visualization
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
# Section 4.4: Numeric Univariate Distributions
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
# Section 4.5: Correlations & Structural Missingness
# ------------------------------------------------------------------------------
# Correlation Matrix
num_df <- dplyr::select_if(df_clean, is.numeric)
if(ncol(num_df) > 1) {
  cor_mat <- cor(num_df, use = "pairwise.complete.obs")
  cor_mat[is.na(cor_mat)] <- 0
  corrplot(cor_mat, method = "circle", type = "lower", title = "Correlation Matrix (Cleaned)", mar = c(0,0,2,0), tl.cex = 0.7)
}

# Structural Missingness Heatmap
p_heatmap <- df_clean %>%
  mutate(Missing_Occ = ifelse(is.na(OCCUPATION_TYPE), "Missing", "Present")) %>%
  count(NAME_INCOME_TYPE, Missing_Occ) %>%
  group_by(NAME_INCOME_TYPE) %>%
  mutate(Prop = n / sum(n)) %>%
  ggplot(aes(x = NAME_INCOME_TYPE, y = Missing_Occ, fill = Prop)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Structural Missingness", x = "Income Type", y = "Occupation Status") +
  theme_minimal()
print(p_heatmap)

# ------------------------------------------------------------------------------
# Section 4.6: Multivariate Risk Interactions (Focus: Severe Delinquency)
# ------------------------------------------------------------------------------
# Interaction: Family Status x Housing Type
# MODIFICATION: We now calculate the rate of TARGET == 2 (Severe Delinquency)
p_interaction <- df_clean %>%
  group_by(NAME_FAMILY_STATUS, NAME_HOUSING_TYPE) %>%
  summarise(
    Severe_Risk_Rate = mean(as.numeric(TARGET) == 2, na.rm=TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  filter(Count > 50) %>%
  ggplot(aes(x = NAME_HOUSING_TYPE, y = NAME_FAMILY_STATUS, fill = Severe_Risk_Rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", labels = scales::percent) +
  labs(title = "Severe Risk (Class 2) Interaction: Family vs Housing", fill = "Severe Risk %") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_interaction)

# Income Density by Target (Log Scale)
p_violin <- ggplot(df_clean, aes(x = as.factor(TARGET), y = AMT_INCOME_TOTAL, fill = as.factor(TARGET))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  scale_y_log10(labels = scales::dollar) +
  labs(title = "Income Distribution by Target Class (Log Scale)", x="Target Class", fill="Class") +
  theme_minimal()
print(p_violin)

# ------------------------------------------------------------------------------
# Section 4.7: Basic Subgroup Analysis
# ------------------------------------------------------------------------------
if("TARGET" %in% names(df_clean)) {
  calc_risk <- function(data, var_name) {
    data %>%
      group_by(!!sym(var_name)) %>%
      summarise(
        Count = n(),
        Severe_Risk_Count = sum(TARGET == 2, na.rm = TRUE),
        Severe_Risk_Pct = round(mean(TARGET == 2, na.rm = TRUE) * 100, 2)
      ) %>%
      arrange(desc(Severe_Risk_Pct))
  }
  
  df_risk <- df_clean %>%
    mutate(
      DAYS_EMPLOYED_BIN = ntile(DAYS_EMPLOYED, 6),
      INCOME_BIN = ntile(AMT_INCOME_TOTAL, 6),
      AGE_BIN = ntile(AGE, 4)
    )
  
  risk_vars <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_FAMILY_STATUS",
                 "NAME_HOUSING_TYPE", "DAYS_EMPLOYED_BIN", "OCCUPATION_TYPE",
                 "EMPLOYMENT_STATUS_FLAG", "EDUCATION_LEVEL", "INCOME_BIN", "AGE_BIN")
  
  all_subgroups <- data.frame()
  for(var in risk_vars) {
    if(var %in% names(df_risk)) {
      tbl <- calc_risk(df_risk, var)
      cat(paste("\n--- Severe Risk (Class 2) Rates:", var, "---\n"))
      print(tbl)
      tbl$Subgroup <- as.character(tbl[[var]])
      all_subgroups <- bind_rows(all_subgroups, tbl %>% select(Count, Severe_Risk_Count, Severe_Risk_Pct) %>% mutate(Variable = var, Subgroup = as.character(tbl[[var]])))
    }
  }
}

# ------------------------------------------------------------------------------
# Section 4.8: Deep Dive Multivariate Risk Profiling
# ------------------------------------------------------------------------------
# METHODOLOGY:
# 1. Logistic Regression: Modified to find drivers of SEVERE RISK (Class 2) vs Others.
#  (We create a temporary binary target for this specific diagnostic step).
# 2. Decision Tree: Modified to handle Multi-Class (0,1,2) with balanced priors.

cat("\n================================================================\n")
cat(" SECTION 4.8: MULTIVARIATE RISK PROFILING & INTERACTIONS\n")
cat("================================================================\n")

# --- Step 1: Logistic Regression for Independent Risk Drivers ---
# We use a Generalized Linear Model (GLM) to isolate effects.
# MODIFICATION: Logic adjusted to find drivers of TARGET == 2 (Severe Delinquency)

if("TARGET" %in% names(df_clean)) {
  cat("\n[Analysis 1] Independent Risk Drivers (Target: Severe Delinquency)...\n")
  
  # Create temporary binary target for GLM (Severe vs Not Severe)
  df_glm <- df_clean %>% mutate(TARGET_SEVERE = ifelse(TARGET == 2, 1, 0)) %>% select(-TARGET)
  
  # Fit Model
  glm_model <- glm(TARGET_SEVERE ~ ., data = df_glm, family = binomial(link = "logit"))
  
  # Extract Coefficients
  glm_results <- summary(glm_model)$coefficients
  results_df <- data.frame(
    Feature = rownames(glm_results),
    Log_Odds = glm_results[, "Estimate"],
    Odds_Ratio = exp(glm_results[, "Estimate"]),
    P_Value = glm_results[, "Pr(>|z|)"]
  )
  
  # Filter for Statistically Significant Risk Drivers (95% Confidence)
  risk_drivers <- results_df %>%
    filter(P_Value < 0.05) %>%
    arrange(desc(Odds_Ratio)) %>%
    mutate(
      Effect = ifelse(Odds_Ratio > 1, "RISK INCREASER", "PROTECTIVE"),
      Interpretation = sprintf("Multiplies Severe Risk by %.2fx", Odds_Ratio)
    ) %>%
    select(Feature, Effect, Odds_Ratio, P_Value, Interpretation)
  
  cat("\n>>> TOP 10 DRIVERS OF SEVERE DELINQUENCY (Class 2):\n")
  print(head(risk_drivers, 10))
  
  cat("\n>>> TOP 5 PROTECTIVE FACTORS (Reduces Severe Risk):\n")
  print(head(results_df %>% 
               filter(P_Value < 0.05 & Odds_Ratio < 1) %>% 
               arrange(Odds_Ratio), 5))
}

# --- Step 2: Decision Tree for Risk Interaction Profiles ---
cat("\n[Analysis 2] High-Risk Profile Segmentation (Decision Tree Rules)...\n")

# CRITICAL FIX FOR MULTI-CLASS: 
# 1. 'parms' updated to equal priors c(0.33, 0.33, 0.33) to ensure the tree
#    cares about the "Severe" class despite it being only 11% of data.
tree_model <- rpart(as.factor(TARGET) ~ ., data = df_clean, method = "class", 
                    parms = list(prior = c(0.33, 0.34, 0.33)), # Approximate equal weighting
                    control = rpart.control(cp = 0.001, minbucket = 30))


# Function to extract risk rules from the tree nodes
get_risk_profiles <- function(model, target_class_idx = "2") {
  # Predict probabilities for all rows. 
  # rpart predict returns matrix [0, 1, 2]. We want column "2" (Severe).
  df_clean$Node_Prob <- predict(model, df_clean)[, target_class_idx]
  df_clean$Leaf_Node <- model$where
  
  # Aggregate by Leaf Node
  profiles <- df_clean %>%
    group_by(Leaf_Node) %>%
    summarise(
      Count = n(),
      Severe_Risk_Rate = mean(TARGET == 2), # Rate of Class 2
      # Extract dominant features
      Typ_Income = median(AMT_INCOME_TOTAL),
      Typ_Housing = names(sort(table(NAME_HOUSING_TYPE), decreasing=T))[1],
      Typ_Status = names(sort(table(NAME_FAMILY_STATUS), decreasing=T))[1]
    ) %>%
    arrange(desc(Severe_Risk_Rate)) %>%
    filter(Count > 50) 
  
  return(profiles)
}

# Note: target_class_idx "2" corresponds to the column name for Class 2 in rpart output
risk_profiles <- get_risk_profiles(tree_model, target_class_idx = "2")

cat("\n>>> IDENTIFIED RISK COMBINATIONS (High Probability of Class 2):\n")
print(head(risk_profiles, 7))

# --- Step 3: Visualizing the Interaction Matrix ---
cat("\n[Analysis 3] Visualizing Interaction Effects (Family x Housing)...\n")

p_interact_grid <- df_clean %>%
  group_by(NAME_FAMILY_STATUS, NAME_HOUSING_TYPE) %>%
  summarise(
    Risk_Probability = mean(TARGET == 2),
    Volume = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = NAME_HOUSING_TYPE, y = NAME_FAMILY_STATUS)) +
  geom_tile(aes(fill = Risk_Probability), color = "white") +
  geom_text(aes(label = round(Risk_Probability * 100, 1)), color = "white", size = 3) +
  scale_fill_gradient(low = "grey30", high = "red") +
  labs(title = "Risk Heatmap: Family Status vs. Housing Type",
       subtitle = "Numbers represent Severe Delinquency Rate (%)",
       fill = "Severe Rate", x = "Housing Type", y = "Family Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_interact_grid)


# Clean up temporary columns
df_clean$Node_Prob <- NULL
df_clean$Leaf_Node <- NULL

# ------------------------------------------------------------------------------
# Section 4.9: Extraction of Human-Readable Risk Profiles (Rule Generation)
# ------------------------------------------------------------------------------
# PURPOSE: Convert the Decision Tree "Leaf Nodes" into plain English business rules.

cat("\n================================================================\n")
cat(" SECTION 4.9: HIGH-RISK PROFILE RULE EXTRACTION\n")
cat("================================================================\n")

# 1. Select the Top Riskiest Segments
top_risk_nodes <- risk_profiles %>%
  arrange(desc(Severe_Risk_Rate)) %>%
  head(10) # Grab top 10 riskiest profiles

cat(sprintf("Extracting rules for the top %d high-risk segments...\n\n", nrow(top_risk_nodes)))

# 2. Extract Logic Paths (The "DNA" of the Profile)
if(nrow(top_risk_nodes) > 0) {
  node_ids <- top_risk_nodes$Leaf_Node
  rules_list <- path.rpart(tree_model, nodes = node_ids, pretty = 0, print.it = FALSE)
  
  # 3. Robust Formatted Output Loop
  valid_nodes <- names(rules_list)
  
  for (i in 1:length(valid_nodes)) {
    target_node <- valid_nodes[i]
    
    # Filter stats using the node name
    stats <- top_risk_nodes %>% filter(Leaf_Node == as.numeric(target_node))
    
    if(nrow(stats) > 0) {
      rules <- rules_list[[i]]
      rules <- rules[-1] # Remove the 'root' entry
      
      cat("----------------------------------------------------------------\n")
      cat(sprintf("PROFILE %s (Risk Rank: #%d)\n", target_node, i))
      cat(sprintf("  >> Severe Risk Rate:  %.2f%% (vs Global Avg: %.2f%%)\n", 
                  stats$Severe_Risk_Rate * 100, mean(df_clean$TARGET==2)*100))
      cat(sprintf("  >> Population:        %d applicants\n", stats$Count))
      cat("----------------------------------------------------------------\n")
      cat("  DEFINING COMBINATION (Business Rules):\n")
      
      for (rule in rules) {
        cat(paste("     *", rule, "\n"))
      }
      cat("\n")
    }
  }
} else {
  cat(">> Tree failed to split. No profiles generated.\n")
}

p_risk_housing <- df_clean %>%
  group_by(NAME_HOUSING_TYPE) %>%
  summarise(Severe_Risk_Rate = mean(TARGET == 2)) %>%
  ggplot(aes(x = reorder(NAME_HOUSING_TYPE, Severe_Risk_Rate), y = Severe_Risk_Rate)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  coord_flip() +
  labs(title = "Risk Signal: Severe Delinquency by Housing Type") +
  theme_minimal()
print(p_risk_housing)

p_employ_age <- ggplot(df_clean, aes(x = AGE, fill = EMPLOYMENT_STATUS_FLAG)) +
  geom_density(alpha = 0.5) +
  labs(title = "Validation of Employment Flag", x = "Age (Years)") +
  theme_minimal()
print(p_employ_age)

# ------------------------------------------------------------------------------
# Section 4.11: Final Artifact Export
# ------------------------------------------------------------------------------
dev.off()
cat(">> PDF Graphics Device Closed. Visualization file saved.\n")

data_output_dir <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts/Saved_Outputs"
if(!dir.exists(data_output_dir)) dir.create(data_output_dir, recursive = TRUE)
clean_data_path <- file.path(data_output_dir, "Final_Cleaned_Dataset.csv")
write.csv(df_clean, file = clean_data_path, row.names = FALSE)
cat(">> SUCCESS: Final cleaned dataset saved to:", clean_data_path, "\n")

cat("\n--- Module 4 Complete ---\n")

# ==============================================================================
# MODULE 5: TARGET ENGINEERING & RISK PROFILING
# ==============================================================================
# RATIONALE:
# Neural networks require numeric targets. Multi-class classification performs 
# better with OHE (One-Hot Encoding) targets and Softmax output.
# Class imbalance is addressed via Inverse Class Frequency weighting.

# Function to calculate class weights for Keras
# Keras expects a named list where keys are class indices (strings "0", "1"...)
# and values are the weights (floats).
get_class_weights <- function(target_vec) {
  # Get frequency table
  counts <- table(target_vec)
  total <- sum(counts)
  n_classes <- length(counts)
  
  # Calculate weights: w_j = Total / (n_classes * count_j)
  # This balances the "influence" of each class in the gradient update.
  weights_list <- list()
  class_labels <- names(counts)
  
  for(lbl in class_labels) {
    # Weight calculation
    w <- total / (n_classes * counts[[lbl]])
    # Create named list element. Key must be string.
    weights_list[[lbl]] <- w
  }
  
  return(weights_list)
}

# Ensure TARGET is present and correctly formatted
if("TARGET" %in% names(df_clean)) {
  # Ensure TARGET is an integer starting from 0 for Keras consistency
  # We use as.numeric(factor()) to map to 1..N, then subtract 1 for 0..N-1
  df_clean$TARGET_IDX <- as.numeric(as.factor(df_clean$TARGET)) - 1
  
  cat(">> Target variable encoded to indices 0 to", max(df_clean$TARGET_IDX), "\n")
  
  # Preview weights on the full dataset (Just for verification, actual weights 
  # will be calculated inside CV loop to avoid leakage)
  preview_w <- get_class_weights(df_clean$TARGET_IDX)
  cat(">> Estimated Class Weights (Full Data Preview):\n")
  print(unlist(preview_w))
  
} else {
  stop("CRITICAL ERROR: 'TARGET' column missing from dataframe. Check Module 2.")
}

# ==============================================================================
# MODULE 6: PREPROCESSING HELPER FUNCTIONS
# ==============================================================================
# REQUIREMENTS:
# 1. All inputs must be floating numbers in range .
# 2. Input must be R Matrix (data.matrix).

# Helper function for Min-Max Scaling
# Applies scaling based on provided min and max values (to allow scaling validation 
# data based on training parameters)
scale_min_max <- function(x, min_val, max_val) {
  # Handle constant columns (max == min) to avoid division by zero
  if (max_val - min_val == 0) {
    return(rep(0, length(x)))
  }
  return((x - min_val) / (max_val - min_val))
}

# Function to prepare features matrix
# Handles dummy encoding for factors and conversion to matrix
prepare_features <- function(data_df) {
  # Identify factor columns for dummy encoding
  factor_cols <- names(data_df)[sapply(data_df, is.factor)]
  
  if(length(factor_cols) > 0) {
    # Use caret::dummyVars to one-hot encode factors
    dummies_model <- dummyVars(~., data = data_df)
    data_encoded <- predict(dummies_model, newdata = data_df)
    # predict() returns a matrix, so we convert back to DF for scaling steps
    data_df <- as.data.frame(data_encoded)
  }
  
  return(data_df)
}

cat(">> [Module 6] Preprocessing logic loaded. Functions ready for CV loop.\n")

# ==============================================================================
# MODULE 7: NEURAL NETWORK CLASSIFIER WITH STRATIFIED K-FOLD VALIDATION
# ==============================================================================
# CONFIGURATION
K_FOLDS <- 10
EPOCHS <- 1500  # As per requirement "1000 - 2000 epocs"
BATCH_SIZE <- 256 # Larger batch size for stability with high epochs
DROPOUT_RATE <- 0.3

# 5.1 Initialize Results Storage
cv_results <- list()
acc_history <- numeric(K_FOLDS)
loss_history <- numeric(K_FOLDS)

# 5.2 Generate Stratified Folds
# returnTrain=FALSE gives us the indices for the Validation/Hold-out set
set.seed(123) # Reproducibility
folds <- createFolds(y = df_clean$TARGET_IDX, k = K_FOLDS, list = TRUE, returnTrain = FALSE)

cat(paste0(">> Starting ", K_FOLDS, "-Fold Cross Validation. This may take time...\n"))

# 5.3 The Validation Loop
for (i in 1:K_FOLDS) {
  
  cat(sprintf("\n--- TRAINING FOLD %d / %d ---\n", i, K_FOLDS))
  
  # --- A. Data Splitting ---
  val_indices <- folds[[i]]
  
  # Split raw dataframe first
  train_raw <- df_clean[-val_indices, ]
  val_raw   <- df_clean[val_indices, ]
  
  # Separate Features (X) and Target (y)
  # Exclude non-feature cols like ID, original strings, etc.
  # Assuming 'TARGET_IDX' is our target and we remove 'TARGET' and 'ID'
  features_to_exclude <- c("TARGET", "TARGET_IDX", "ID", "status") 
  
  train_x_df <- train_raw %>% select(-any_of(features_to_exclude))
  val_x_df   <- val_raw   %>% select(-any_of(features_to_exclude))
  
  train_y_vec <- train_raw$TARGET_IDX
  val_y_vec   <- val_raw$TARGET_IDX
  
  # --- B. Feature Engineering (Inside Loop to prevent Leakage) ---
  # 1. Handle Categoricals (OHE) - Fit on Train, Apply to Train/Val is complex with 
  #    dummyVars if levels differ. Simplified approach: Apply OHE to whole dataset 
  #    BEFORE split is acceptable IF levels are fixed. 
  #    However, let's assume we perform prepare_features() locally for strictness.
  train_x_df <- prepare_features(train_x_df)
  val_x_df   <- prepare_features(val_x_df)
  
  # Ensure columns match (if Val is missing a dummy column due to missing level)
  common_cols <- intersect(names(train_x_df), names(val_x_df))
  train_x_df <- train_x_df[, common_cols]
  val_x_df   <- val_x_df[, common_cols]
  
  # 2. Scaling 
  # Calculate params on TRAINING data
  numeric_cols <- names(train_x_df) # All are numeric after prepare_features
  
  col_mins <- sapply(train_x_df, min)
  col_maxs <- sapply(train_x_df, max)
  
  # Apply to Train
  for(col in numeric_cols) {
    train_x_df[[col]] <- scale_min_max(train_x_df[[col]], col_mins[col], col_maxs[col])
  }
  
  # Apply to Validation (using Train params)
  for(col in numeric_cols) {
    val_x_df[[col]] <- scale_min_max(val_x_df[[col]], col_mins[col], col_maxs[col])
  }
  
  # --- C. Format Conversion ---
  # Convert to R Matrix (Required by Keras)
  x_train_mat <- data.matrix(train_x_df)
  x_val_mat   <- data.matrix(val_x_df)
  
  # One-Hot Encode Targets
  num_classes <- length(unique(df_clean$TARGET_IDX))
  y_train_ohe <- to_categorical(train_y_vec, num_classes = num_classes)
  y_val_ohe   <- to_categorical(val_y_vec, num_classes = num_classes)
  
  # --- D. Risk Profiling (Weight Calculation) ---
  # Calculate weights specific to this training fold
  fold_weights <- get_class_weights(train_y_vec)
  
  # --- E. Model Architecture ---
  # Reset session to ensure clean state
  k_clear_session()
  
  model <- keras_model_sequential() %>%
    # Input Layer + Hidden 1
    layer_dense(units = 64, activation = 'relu', input_shape = c(ncol(x_train_mat))) %>%
    layer_dropout(rate = DROPOUT_RATE) %>%
    
    # Hidden 2
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dropout(rate = DROPOUT_RATE) %>%
    
    # Hidden 3 (Abstraction)
    layer_dense(units = 16, activation = 'relu') %>%
    
    # Output Layer: Softmax for probability distribution over classes
    layer_dense(units = num_classes, activation = 'softmax')
  
  # --- F. Compilation ---
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = c('accuracy')
  )
  
  # --- G. Training (Fit) ---
  # We use class_weight to enforce risk profiling
  history <- model %>% fit(
    x = x_train_mat,
    y = y_train_ohe,
    epochs = EPOCHS,
    batch_size = BATCH_SIZE,
    class_weight = fold_weights, # <--- SURGICAL SAMPLE WEIGHTING APPLIED HERE
    validation_data = list(x_val_mat, y_val_ohe),
    verbose = 0, # Silent to keep console clean, change to 1 for progress
    callbacks = list(
      # Early Stopping to mitigate overfitting despite high epoch count requirement
      callback_early_stopping(monitor = "val_loss", patience = 150, restore_best_weights = TRUE)
    )
  )
  
  # --- H. Evaluation ---
  eval_res <- model %>% evaluate(x_val_mat, y_val_ohe, verbose = 0)
  
  cat(sprintf("   > Fold %d Result - Loss: %.4f | Accuracy: %.4f\n", i, eval_res["loss"], eval_res["accuracy"]))
  
  acc_history[i] <- eval_res["accuracy"]
  loss_history[i] <- eval_res["loss"]
}

# 5.4 Final Report
cat("\n================================================================\n")
cat("FINAL K-FOLD VALIDATION RESULTS (k=10)\n")
cat("================================================================\n")
cat(sprintf("Mean Accuracy: %.2f%% (+/- %.2f%%)\n", mean(acc_history)*100, sd(acc_history)*100))
cat(sprintf("Mean Loss:     %.4f\n", mean(loss_history)))
cat("================================================================\n")