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
req_pkgs <- c("tidyverse",    # Collection of packages for data science (ggplot2, dplyr, etc.)
              "caret",        # Classification And REgression Training (ML model training helper)
              "reshape2",     # Flexibly reshape data (melt/cast between wide and long formats)
              "corrplot",     # Visualizing correlation matrices
              "vcd",          # Visualizing Categorical Data (mosaic plots, etc.)
              "naniar",       # Tidy data structures and visualizations for missing values
              "gridExtra",    # Misc functions for "grid" graphics (arranging multiple plots)
              "e1071",        # Misc functions (skewness, kurtosis, SVM, Naive Bayes)
              "Hmisc",        # Harrell Miscellaneous (data analysis, high-level graphics, imputation)
              "VIM",          # Visualization and Imputation of Missing Values
              "themis",       # Recipes for dealing with unbalanced data (SMOTE, undersampling)
              "tidymodels",   # Meta-package for modeling and machine learning using tidy principles
              "tensorflow",   # Interface to TensorFlow numerical computation library
              "reticulate",   # R interface to Python (essential for Keras/TensorFlow backend)
              "embed",        # Extra recipe steps for embeddings (e.g., target encoding)
              "rpart",        # Recursive Partitioning and Regression Trees
              "fastDummies",  # Fast creation of dummy variables (One-Hot Encoding)
              "keras3")       # R interface to Keras (Deep Learning API)

new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if(length(new_pkgs)) install.packages(new_pkgs)

# Load necessary libraries for Data Cleaning and EDA
library(tidyverse)    # Master suite for data manipulation and visualization
library(ggplot2)      # System for declaratively creating graphics (plots)
library(Hmisc)        # Helper functions for data analysis and advanced graphics
library(fastDummies)  # Efficiently create dummy variables (rows to columns)
library(dplyr)        # Grammar of data manipulation (filter, select, mutate, etc.)
library(gridExtra)    # Arrange multiple ggplot objects on a single page
library(corrplot)     # Graphical display of a correlation matrix
library(naniar)       # Tools for visualizing and managing missing values
library(e1071)        # Statistical functions (specifically used for skewness/kurtosis here)

# Load necessary libraries for Modeling
library(keras3)       # High-level API for building and training neural networks

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
# SECTION 2: DATA INGESTION AND TYPING
# ------------------------------------------------------------------------------

cat("--- Loading and Examining Data ---\n")

# 2.1 Load Raw Data
# -----------------
# Load the dataset
path <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_data/Dataset-part-2.csv"
data <- read.csv(path)

# ==============================================================================
# Module 2: Initial Exploratory Data Analysis (EDA)
# ==============================================================================
# PURPOSE: Identify Data Quality issues (Anomalies, Skew, MNAR) prior to preprocessing.

# --- Integration Setup ---
# Loading libraries required for Module 2 that were not in the base script
if(!require(gridExtra)) install.packages("gridExtra"); library(gridExtra)
if(!require(corrplot)) install.packages("corrplot"); library(corrplot)
if(!require(naniar)) install.packages("naniar"); library(naniar)
if(!require(e1071)) install.packages("e1071"); library(e1071)
# Mapping 'data' to 'df' as expected by Module 2
df <- data 
# -------------------------

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
# SECTION 3: EDA AND LOGICAL CORRECTIONS
# ------------------------------------------------------------------------------

# 2.2 Define Data Types and Reload
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
data <- read.csv(path, colClasses = col_types, na.strings = c("NA", ""))

# 2.3 Initial Formatting
# ----------------------
# 3. Post-Load Logic (Conversions to Integer)
data$FLAG_OWN_CAR <- as.integer(data$FLAG_OWN_CAR == "Y")
data$FLAG_OWN_REALTY <- as.integer(data$FLAG_OWN_REALTY == "Y")

# Convert 0/1 integers to Logical Booleans
flags_numeric <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")
data[flags_numeric] <- lapply(data[flags_numeric], as.integer)

# Drop columns with no variance (FLAG_MOBIL)
data <- data %>% select(-FLAG_MOBIL)

cat("--- Correcting Age and Employment Data ---\n")

# 3.1 Age and Employment Logic
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

# 3.2 Family Size and Outliers
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

# 3.3 Income Outliers
# -------------------
# Income Outliers
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", 
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")

# Remove unlikely high incomes for specific jobs
data <- data %>%
  filter( !(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs) )

# Log transform Income
data$AMT_INCOME_TOTAL_LOG <- log1p(data$AMT_INCOME_TOTAL)

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

# ------------------------------------------------------------------------------
# SECTION 5: FINAL PRE-PROCESSING AND ENCODING
# ------------------------------------------------------------------------------

cat("--- Encoding Variables for Modeling ---\n")

# 5.1 Target Variable Encoding
# ----------------------------
# Drop all rows with target X
data <- data %>% filter(status != "X")

# TARGET ENCODING
data$status <- as.character(data$status)

# Map status levels to class IDs 0..6 (unordered)
# C=0, 0=1, 1=2, 2=3, 3=4, 4=5, 5=6
data$target_class <- recode(data$status,
                            "C" = 0,
                            "0" = 1,
                            "1" = 2,
                            "2" = 3,
                            "3" = 4,
                            "4" = 5,
                            "5" = 6
) |> as.numeric()

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

# ONE-HOT ENCODING (NOMINAL)
nominal_cols <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")
data[nominal_cols] <- lapply(data[nominal_cols], as.factor)
data <- dummy_cols(
  data,
  select_columns = nominal_cols,
  remove_selected_columns = TRUE,
  remove_first_dummy = FALSE
)

# 5.3 Cleanup and Save
# --------------------
# --------------------
# DROP UNUSED COLUMNS
# Removed '-Employment_Status' because it was never created in this script
data <- data %>% select(-ID, -DAYS_BIRTH, -DAYS_EMPLOYED, -AMT_INCOME_TOTAL, 
                        -ACTIVE_EMPLOYMENT_YEARS, -INCOME_PER_FAMILY_MEMBER, 
                        -INCOME_PER_AGE, -status)

# --- Feature Filtering ---
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

# SAVE INTERMEDIATE DATA
write.csv(
  data,
  file = "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_data/cleaned_dataset.csv",
  row.names = FALSE
)

cat("Data cleaning complete. Starting Cleaned Data EDA...\n")

# ==============================================================================
# Module 4: Cleaned Dataset EDA
# ==============================================================================
# PURPOSE: Verification of Cleaning Steps & Multivariate Risk Profiling

# --- Integration Setup ---
# Mapping 'data' to 'df_clean_processed' as expected by Module 4
df_clean_processed <- data
# Crucial: Module 4 visualization expects a "TARGET" column, but Section 5.3 
# renamed/removed "status". We map "target_class" to "TARGET" for visualization.
df_clean_processed$TARGET <- df_clean_processed$target_class
# -------------------------

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
# Section 4.7: Final Artifact Export
# ------------------------------------------------------------------------------
dev.off()
cat(">> PDF Graphics Device Closed. Visualization file saved.\n")

data_output_dir <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts/Saved_Outputs"
if(!dir.exists(data_output_dir)) dir.create(data_output_dir, recursive = TRUE)
clean_data_path <- file.path(data_output_dir, "Final_Cleaned_Dataset.csv")
write.csv(df_clean, file = clean_data_path, row.names = FALSE)
cat(">> SUCCESS: Final cleaned dataset saved to:", clean_data_path, "\n")

cat("\n--- Module 4 Complete ---\n")

# ------------------------------------------------------------------------------
# SECTION 6: MODEL CONFIGURATION AND ARCHITECTURE
# ------------------------------------------------------------------------------

# 6.1 Data Preparation for Keras
# ------------------------------
# Assign cleaned data to 'df'
df <- data

# Define Status Levels (Mapped 0 to 6)
status_levels <- c("C", "0", "1", "2", "3", "4", "5")
num_classes <- 7 

# Convert to R matrix
mat_data <- data.matrix(df)

# Scale inputs to [0, 1]
scale_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

X <- mat_data[, -ncol(mat_data)]
y_numeric <- mat_data[, ncol(mat_data)] # This contains levels 0 to 6

X_scaled <- apply(X, 2, scale_01)

# One-Hot Encoding for Target
y_onehot <- to_categorical(y_numeric, num_classes = num_classes)

# 6.2 Calculate Class Weights
# ---------------------------
# --- Calculate Class Weights ---
target_counts <- table(y_numeric)
class_weights <- list()

for (i in names(target_counts)) {
  val <- sqrt(max(target_counts) / target_counts[i])
  class_weights[[i]] <- as.numeric(val)
}

# 6.3 Evaluation Metrics Function
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

# 6.4 Model Architecture Definition
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
# SECTION 7: MODEL EXECUTION AND TRAINING
# ------------------------------------------------------------------------------

# 7.1 Cross-Validation Loop
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

# 7.2 Final Model Training
# ------------------------
# --- Final Training ---
cat("\nTraining final model for 5000 epochs...\n")

split_idx <- sample(1:nrow(X), 0.8 * nrow(X))
X_train_raw <- X[split_idx, ]
y_train <- y_onehot[split_idx, ]
X_val_raw <- X[-split_idx, ]
y_val <- y_onehot[-split_idx, ]

# --- Apply Transformations on Split Data (Final Model) ---

# 1. Skewness Correction
for(col_idx in 1:ncol(X_train_raw)) {
  skew_val <- skewness(X_train_raw[, col_idx], na.rm = TRUE)
  
  if(!is.na(skew_val) && (skew_val > 1 || skew_val < -1)) {
    if(min(X_train_raw[, col_idx], na.rm=TRUE) >= 0) {
      X_train_raw[, col_idx] <- log1p(X_train_raw[, col_idx])
      X_val_raw[, col_idx] <- log1p(X_val_raw[, col_idx])
    }
  }
}

# 2. Outlier Clipping
for(col_idx in 1:ncol(X_train_raw)) {
  quantiles <- quantile(X_train_raw[, col_idx], probs = c(0.01, 0.99), na.rm = TRUE)
  min_cap <- quantiles[1]
  max_cap <- quantiles[2]
  
  X_train_raw[, col_idx] <- pmax(X_train_raw[, col_idx], min_cap)
  X_train_raw[, col_idx] <- pmin(X_train_raw[, col_idx], max_cap)
  
  X_val_raw[, col_idx] <- pmax(X_val_raw[, col_idx], min_cap)
  X_val_raw[, col_idx] <- pmin(X_val_raw[, col_idx], max_cap)
}

# Apply Robust Scaling (Train-to-Val) logic to final model too
X_train_final <- X_train_raw
X_val_final   <- X_val_raw

for(col_idx in 1:ncol(X_train_raw)) {
  min_v <- min(X_train_raw[, col_idx], na.rm = TRUE)
  max_v <- max(X_train_raw[, col_idx], na.rm = TRUE)
  
  X_train_final[, col_idx] <- scale_col(X_train_raw[, col_idx], min_v, max_v)
  X_val_final[, col_idx]   <- scale_col(X_val_raw[, col_idx], min_v, max_v)
}

final_model <- create_model(ncol(X_train_final))

final_history <- fit(
  object = final_model,
  x = X_train_final, 
  y = y_train,
  epochs = 5000, 
  batch_size = 1024,
  validation_data = list(X_val_final, y_val),
  class_weight = class_weights,
  verbose = 1
)

# ------------------------------------------------------------------------------
# SECTION 8: EVALUATION AND REPORTING
# ------------------------------------------------------------------------------

# 8.1 Detailed Predictions and Metrics
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

# 8.2 Requirements Verification
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

# 8.3 Saving the Model
# --------------------
# --- Save Model ---
# Define the path with .keras extension to ensure optimized single-file saving
save_model_path <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts/final_model.keras"

save_model(final_model, save_model_path, overwrite = TRUE)
cat(sprintf("\nModel saved to: %s\n", save_model_path))