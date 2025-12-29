# ==============================================================================
# Setup and Libraries (Auto-Install Logic)
# ==============================================================================
# Function to check and install packages
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# List of required libraries
required_packages <- c("tidyverse", "fastDummies", "caret", "keras3")

# Apply the function to each package
invisible(lapply(required_packages, install_if_missing))



# ==============================================================================
# 0. Dynamic Setup (Works on any machine)
# ==============================================================================
# This block automatically sets the Working Directory to wherever this script is saved.
# It allows the script to run from Downloads, Desktop, or any project folder.

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  # Get the path of the currently open script in RStudio
  current_script_path <- rstudioapi::getActiveDocumentContext()$path
  
  # If the script is saved (not a new unsaved file), set WD to its folder
  if (current_script_path != "") {
    script_dir <- dirname(current_script_path)
    setwd(script_dir)
    message("Working Directory successfully set to: ", script_dir)
  } else {
    warning("Script is not saved yet. Please save the file before running.")
  }
} else {
  # Fallback for non-RStudio environments (e.g. basic R console)
  message("Not running via RStudio API. Assuming current WD is correct.")
}

# --- CONFIGURATION ---
FILE_PATH      <- "/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Dataset-part-2.csv" # Path to new data
SCALER_PATH    <- "final_champion_scaler.rds"            # Path to saved scaler
MODEL_PATH     <- "final_sgd_champion_model.keras"
THRESHOLD_CL3  <- 0.39                                   # Your Champion Threshold

# ==============================================================================
# 1. Load Data
# ==============================================================================
if (!file.exists(FILE_PATH)) stop("Error: secret_data.csv not found!")
df <- read.csv(FILE_PATH)

# ==============================================================================
# 2. Preprocessing Pipeline (Exact Match to EDA)
# ==============================================================================
# 1. Import and Type Definition
df <- df %>%
  mutate(
    ID                  = as.character(ID),
    CODE_GENDER         = as.factor(CODE_GENDER),
    FLAG_OWN_CAR        = as.character(FLAG_OWN_CAR),
    FLAG_OWN_REALTY     = as.character(FLAG_OWN_REALTY),
    CNT_CHILDREN        = as.integer(CNT_CHILDREN),
    AMT_INCOME_TOTAL    = as.numeric(AMT_INCOME_TOTAL),
    NAME_INCOME_TYPE    = as.character(NAME_INCOME_TYPE),
    NAME_EDUCATION_TYPE = as.factor(NAME_EDUCATION_TYPE),
    NAME_FAMILY_STATUS  = as.factor(NAME_FAMILY_STATUS),
    NAME_HOUSING_TYPE   = as.factor(NAME_HOUSING_TYPE),
    DAYS_BIRTH          = as.integer(DAYS_BIRTH),
    DAYS_EMPLOYED       = as.integer(DAYS_EMPLOYED),
    FLAG_MOBIL          = as.integer(FLAG_MOBIL),
    FLAG_WORK_PHONE     = as.integer(FLAG_WORK_PHONE),
    FLAG_PHONE          = as.integer(FLAG_PHONE),
    FLAG_EMAIL          = as.integer(FLAG_EMAIL),
    OCCUPATION_TYPE     = as.factor(OCCUPATION_TYPE),
    CNT_FAM_MEMBERS     = as.integer(CNT_FAM_MEMBERS),
    status              = as.factor(status)
  )

# Convert Flags
df$FLAG_OWN_CAR <- ifelse(df$FLAG_OWN_CAR == "Y", 1, 0)
df$FLAG_OWN_REALTY <- ifelse(df$FLAG_OWN_REALTY == "Y", 1, 0)
flags_list <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL", 
                "FLAG_OWN_CAR", "FLAG_OWN_REALTY")
df[flags_list] <- lapply(df[flags_list], as.integer)

# 2. Duplicate Removal
duplicates_mask <- duplicated(df[, !names(df) %in% "ID"])
df <- df[!duplicates_mask, ]

# 3. Univariate Cleaning
df <- df %>% select(-FLAG_MOBIL)

# 4. Age Transformation
df <- df %>%
  mutate(AGE = round(abs(DAYS_BIRTH) / 365.25)) %>%
  select(-DAYS_BIRTH)

# 5. Employment Cleaning
df <- df %>%
  filter(!(NAME_INCOME_TYPE == "Pensioner" & DAYS_EMPLOYED != 365243))

df <- df %>%
  mutate(
    OCCUPATION_TYPE = as.character(OCCUPATION_TYPE),
    OCCUPATION_TYPE = case_when(
      DAYS_EMPLOYED == 365243 ~ "Retired",
      is.na(OCCUPATION_TYPE)  ~ "Unknown",
      TRUE ~ OCCUPATION_TYPE
    ),
    OCCUPATION_TYPE = as.factor(OCCUPATION_TYPE)
  )

df <- df %>%
  mutate(
    ACTIVE_EMPLOYMENT_YEARS = ifelse(DAYS_EMPLOYED == 365243, 0, abs(DAYS_EMPLOYED) / 365.25),
    ACTIVE_EMPLOYMENT_YEARS_LOG = log1p(ACTIVE_EMPLOYMENT_YEARS)
  ) %>%
  select(-c(DAYS_EMPLOYED, ACTIVE_EMPLOYMENT_YEARS))

# 6. Family Features Logic
df <- df[df$CNT_FAM_MEMBERS <= 10, ]

anomaly_mask <- df$CNT_FAM_MEMBERS < df$CNT_CHILDREN
df <- df %>%
  mutate(CNT_FAM_MEMBERS = case_when(
    anomaly_mask & NAME_FAMILY_STATUS %in% c("Married", "Civil marriage") ~ CNT_CHILDREN + 2,
    anomaly_mask & NAME_FAMILY_STATUS %in% c("Separated", "Widow", "Single / not married") ~ CNT_CHILDREN + 1,
    TRUE ~ CNT_FAM_MEMBERS
  ))

# 7. Income Logic (Includes both Job Filter and Hard Cap)
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", 
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")

df <- df %>%
  filter(!(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs))

# Added Cap from RMD Section 3.6
df <- df %>%
  filter(AMT_INCOME_TOTAL <= 3000000)

# 8. Feature Engineering
df <- df %>%
  mutate(AMT_INCOME_TOTAL_LOG = log1p(AMT_INCOME_TOTAL))

df$INCOME_PER_FAMILY_MEMBER <- df$AMT_INCOME_TOTAL / df$CNT_FAM_MEMBERS
df <- df %>%
  mutate(INCOME_PER_FAMILY_MEMBER_LOG = log1p(INCOME_PER_FAMILY_MEMBER)) %>%
  select(-c(AMT_INCOME_TOTAL, INCOME_PER_FAMILY_MEMBER))

# 9. Final Preprocessing and Encoding
df$status <- as.character(df$status)
df$target_class <- recode(df$status,
                                    "X" = 0, "C" = 1, "0" = 2, "1" = 3,  
                                    "2" = 4, "3" = 5, "4" = 6, "5" = 7) |> as.numeric()
df <- df %>% select(-status)

df$EDUCATION_ENCODED <- case_when(
  df$NAME_EDUCATION_TYPE == "Lower secondary" ~ 0,
  df$NAME_EDUCATION_TYPE == "Secondary / secondary special" ~ 1,
  df$NAME_EDUCATION_TYPE == "Incomplete higher" ~ 2,
  df$NAME_EDUCATION_TYPE == "Higher education" ~ 3,
  df$NAME_EDUCATION_TYPE == "Academic degree" ~ 4,
  TRUE ~ NA_real_
)
df <- df %>% select(-NAME_EDUCATION_TYPE)

nominal_cols <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_FAMILY_STATUS", 
                  "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")

# Inject temporary Student row to ensure correct column structure
temp_row <- df[1, ]
temp_row$NAME_INCOME_TYPE <- "Student"
df <- bind_rows(df, temp_row)

df[nominal_cols] <- lapply(df[nominal_cols], as.factor)

df <- dummy_cols(
  df,
  select_columns = nominal_cols,
  remove_selected_columns = TRUE,
  remove_first_dummy = FALSE 
)

# Remove temporary Student row
df <- df[-nrow(df), ]

df <- df %>% select(-ID)

cat("Secret Data cleaning complete. Final dimensions:", dim(df), "\n")

# ==============================================================================
# 3. Prepare Data for Model (Scaling)
# ==============================================================================
target_col <- "target_class"

# Split Features (X) and Target (Y)
x_test_raw <- as.matrix(df %>% select(-all_of(target_col)))
# Force numeric double type
storage.mode(x_test_raw) <- "double"

# Identify continuous columns (Logic: columns that are NOT just 0 or 1)
continuous_cols <- colnames(x_test_raw)[apply(x_test_raw, 2, function(x) !all(x %in% c(0, 1)))]

# Load the Champion Scaler
if (!file.exists(SCALER_PATH)) stop("Error: Scaler file not found!")
scaler <- readRDS(SCALER_PATH)

# Apply Scaling
x_test <- x_test_raw
x_test[, continuous_cols] <- predict(scaler, x_test_raw[, continuous_cols, drop = FALSE])

# Prepare Target (One-Hot)
y_test_indices <- df[[target_col]]
y_test <- to_categorical(y_test_indices, num_classes = 8)

# ==============================================================================
# 4. Load Model & Predict
# ==============================================================================
if (!file.exists(MODEL_PATH)) stop("Error: Model file not found!")
final_model <- load_model(MODEL_PATH)

cat("\n--- Predicting on Secret Data ---\n")
probs <- predict(final_model, x_test, verbose = 0)

# ==============================================================================
# 5. Apply Threshold Logic (0.39 for Class 3)
# ==============================================================================
# Standard Argmax prediction
preds_base <- apply(probs, 1, which.max) - 1

# Apply Tuned Threshold
preds_tuned <- preds_base
# Note: R uses 1-based indexing, so Class 3 is Column 4
preds_tuned[probs[, 4] > THRESHOLD_CL3] <- 3

# Get True Labels
true_y <- apply(y_test, 1, which.max) - 1

# ==============================================================================
# 6. Final Evaluation Output
# ==============================================================================
cat("\n--- Final Evaluation on SECRET DATA SET ---\n")

# Create Confusion Matrix
# Ensure levels 0:7 are present even if missing in predictions
cm <- confusionMatrix(
  factor(preds_tuned, levels=0:7), 
  factor(true_y, levels=0:7),
  mode = "everything"
)

# Print Required Metrics
print(cm$table)

cat("\nTest Accuracy:", sprintf("%.4f", cm$overall["Accuracy"]))
cat("\n\nClass 3 Metrics (Risk):\n")
print(cm$byClass["Class: 3", c("Sensitivity", "Precision", "F1")])