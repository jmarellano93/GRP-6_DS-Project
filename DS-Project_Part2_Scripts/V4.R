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

# Load necessary libraries for Data Cleaning and EDA
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(fastDummies)
library(dplyr)

# Load necessary libraries for Modeling
library(keras3)

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

# Display dimensions and structure
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")
str(data)

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
# 3. Post-Load Logic (Conversions)
data$FLAG_OWN_CAR <- data$FLAG_OWN_CAR == "Y"
data$FLAG_OWN_REALTY <- data$FLAG_OWN_REALTY == "Y"

# Convert 0/1 integers to Logical Booleans
flags_numeric <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL")
data[flags_numeric] <- lapply(data[flags_numeric], as.logical)

# Drop columns with no variance (FLAG_MOBIL)
data <- data %>% select(-FLAG_MOBIL)

# ------------------------------------------------------------------------------
# SECTION 3: EDA AND LOGICAL CORRECTIONS
# ------------------------------------------------------------------------------

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
# DROP UNUSED COLUMNS
# Removed '-Employment_Status' because it was never created in this script
data <- data %>% select(-ID, -DAYS_BIRTH, -DAYS_EMPLOYED, -AMT_INCOME_TOTAL, 
                        -ACTIVE_EMPLOYMENT_YEARS, -INCOME_PER_FAMILY_MEMBER, 
                        -INCOME_PER_AGE, -status)

# SAVE INTERMEDIATE DATA
write.csv(
  data,
  file = "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_data/cleaned_dataset.csv",
  row.names = FALSE
)

cat("Data cleaning complete. Starting Neural Network preparations...\n")

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
folds <- sample(rep(1:k, length.out = nrow(X_scaled)))
cv_accuracies <- numeric(k)

for (i in 1:k) {
  cat(sprintf("Fold %d/%d...", i, k))
  v_idx <- which(folds == i)
  t_idx <- which(folds != i)
  
  m_cv <- create_model(ncol(X_scaled))
  
  # FIX: Removed pipe (%>%) and explicitly named arguments 'x' and 'y'
  h_cv <- fit(
    object = m_cv,
    x = X_scaled[t_idx, ], 
    y = y_onehot[t_idx, ],
    epochs = 50, 
    batch_size = 1024,
    validation_data = list(X_scaled[v_idx, ], y_onehot[v_idx, ]),
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
final_model <- create_model(ncol(X_scaled))

split_idx <- sample(1:nrow(X_scaled), 0.8 * nrow(X_scaled))
X_train <- X_scaled[split_idx, ]
y_train <- y_onehot[split_idx, ]
X_val <- X_scaled[-split_idx, ]
y_val <- y_onehot[-split_idx, ]

final_history <- fit(
  object = final_model,
  x = X_train, 
  y = y_train,
  epochs = 5000, 
  batch_size = 1024,
  validation_data = list(X_val, y_val),
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
y_pred_val <- final_model %>% predict(X_val)
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