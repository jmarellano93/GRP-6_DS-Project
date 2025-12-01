# ==============================================================================
# SCRIPT: Credit Risk Data Exploration & Preprocessing (Part 2)
# AUTHOR:
# REVIEWER: Senior Data Science Mentor
# PURPOSE: To audit, clean, and transform raw credit data into a Neural Network-ready format.
#          Includes robust EDA, justification for cleaning steps, and automated reporting.
# ==============================================================================

# ==============================================================================
# Module 1: Environment Setup
# ==============================================================================

# --- Package Installation Logic ---
installed_pkgs <- installed.packages()[, "Package"]
req_pkgs <- c("tidyverse", "caret", "reshape2", "corrplot", "vcd", "naniar", "gridExtra", "e1071")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed_pkgs)]
if(length(new_pkgs)) install.packages(new_pkgs)

# --- Load Libraries ---
library(tidyverse)  # Data manipulation & ggplot2
library(caret)      # ML Preprocessing & Dummy Variables
library(reshape2)   # Reshaping for correlation heatmaps
library(corrplot)   # Correlation Viz
library(vcd)        # Categorical Statistics (Cramer's V)
library(naniar)     # Missing Data Visualization
library(gridExtra)  # arranging plots
library(e1071)      # Skewness calculations

# Ensure reproducibility
set.seed(123)

# --- Data Loading Function ---
load_data <- function(path) {
  if(!file.exists(path)) stop(paste("File not found at:", path))
  
  # Strings as factors = FALSE initially to facilitate inspection
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  cat("--------------------------------------------------------\n")
  cat(" DATA LOADED SUCCESSFULLY \n")
  cat(" Dimensions:", dim(df)[1], "rows,", dim(df)[2], "columns\n")
  cat("--------------------------------------------------------\n")
  return(df)
}

# ==============================================================================
# Module 2: Initial Exploratory Data Analysis (EDA)
# ==============================================================================
# JUSTIFICATION: 
# Before any cleaning, we must understand the raw data structure to identify:
# 1. Wrong data types (categorical read as numeric).
# 2. Outliers (impossible ages, employment years).
# 3. Missingness patterns (random vs. structural).
# This informs the strategy in Module 3.

perform_initial_eda <- function(df) {
  cat("\n================================================================\n")
  cat(" MODULE 2: INITIAL DATA EXPLORATION (RAW DATA)\n")
  cat("================================================================\n")
  
  # --- Phase 1: Structure & Content Inspection ---
  cat("\n[Phase 1] Structure & Content Inspection\n")
  
  # 1. Dimension Check
  cat("\nDimensions:\n")
  print(dim(df))
  
  # 2. Structure Check
  cat("\nData Structure (glimpse):\n")
  dplyr::glimpse(df)
  
  # --- Phase 2: Data Quality "Sanity Checks" ---
  cat("\n[Phase 2] Data Quality & Sanity Checks\n")
  
  # 1. Statistical Summary
  cat("\nStatistical Summary of Numeric Columns:\n")
  print(summary(select_if(df, is.numeric)))
  
  # 2. Missing Values
  cat("\nMissing Values Count:\n")
  miss_counts <- colSums(is.na(df))
  print(miss_counts[miss_counts > 0])
  
  # 3. Duplicate Rows
  dup_count <- sum(duplicated(df))
  cat("\nDuplicate Rows Detected:", dup_count, "\n")
  
  # 4. Unique Value Counts for Categorical (Cardinality Check)
  cat("\nUnique Values in Categorical Variables:\n")
  cat_cols <- names(select_if(df, is.character))
  for(col in cat_cols) {
    cat(paste0(col, ": ", length(unique(df[[col]])), " unique levels\n"))
  }
  
  # --- Phase 3: Univariate Visualization ---
  cat("\n[Phase 3] Univariate Visualization\n")
  
  # 1. Target Variable Distribution
  if("status" %in% names(df)) {
    p_target <- ggplot(df, aes(x = as.factor(status))) +
      geom_bar(fill = "steelblue") +
      labs(title = "Target Distribution (Class Imbalance Check)", x = "Status Code", y = "Count") +
      theme_minimal()
    print(p_target)
  }
  
  # 2. Numeric Distributions (Histograms & Boxplots)
  num_cols <- names(select_if(df, is.numeric))
  num_cols <- num_cols # Exclude ID
  
  for(col in num_cols) {
    # Histogram
    p_hist <- ggplot(df, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "coral", color = "white") +
      labs(title = paste("Distribution:", col)) +
      theme_minimal()
    
    # Boxplot
    p_box <- ggplot(df, aes_string(y = col)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Outliers:", col)) +
      theme_minimal()
    
    grid.arrange(p_hist, p_box, ncol = 2)
  }
  
  # 3. Categorical Frequencies
  for(col in cat_cols) {
    p_cat <- ggplot(df, aes_string(x = col)) +
      geom_bar(fill = "darkseagreen") +
      coord_flip() + 
      labs(title = paste("Frequency:", col)) +
      theme_minimal()
    print(p_cat)
  }
  
  cat("\n--- Module 2 Complete ---\n")
}

# ==============================================================================
# Module 3: Advanced Feature Engineering & Cleaning
# ==============================================================================

process_credit_data <- function(df) {
  cat("\n================================================================\n")
  cat(" MODULE 3: DATA CLEANING & FEATURE ENGINEERING\n")
  cat("================================================================\n")
  
  df_clean <- df
  
  # 1. ID Removal
  if("ID" %in% names(df_clean)) {
    cat("Dropping 'ID' column...\n")
    df_clean$ID <- NULL
  }
  
  # 2. Standardize flag variables (Y/N and numeric) to 0/1 -----------------
  cat("Standardizing flag variables to numeric 0/1...\n")
  
  yn_flags <- intersect(c("FLAG_OWN_CAR", "FLAG_OWN_REALTY"), names(df_clean))
  for (f in yn_flags) {
    df_clean[[f]] <- ifelse(df_clean[[f]] == "Y", 1L, 0L)
  }
  
  numeric_flags <- intersect(c("FLAG_MOBIL", "FLAG_WORK_PHONE", 
                               "FLAG_PHONE", "FLAG_EMAIL"),
                             names(df_clean))
  for (f in numeric_flags) {
    df_clean[[f]] <- as.integer(df_clean[[f]] > 0)
  }
  
  # 3. DAYS_EMPLOYED → handle sentinel & convert to years ------------------
  cat("Handling 'DAYS_EMPLOYED' sentinel and creating EMPLOYMENT_YEARS...\n")
  
  if ("DAYS_EMPLOYED" %in% names(df_clean)) {
    
    # Retirement sentinel (365243)
    df_clean$IS_RETIRED <- ifelse(df_clean$DAYS_EMPLOYED == 365243, 1L, 0L)
    
    # Replace sentinel with NA temporarily
    df_clean$DAYS_EMPLOYED[df_clean$DAYS_EMPLOYED == 365243] <- NA
    
    # Convert negative days to positive years, pensioners → 0 years
    df_clean$EMPLOYMENT_YEARS <- ifelse(
      is.na(df_clean$DAYS_EMPLOYED),
      0,
      pmax(0, -df_clean$DAYS_EMPLOYED / 365.25)
    )
    
    # Remove original days column
    df_clean$DAYS_EMPLOYED <- NULL
  }
  
  # 4. Convert DAYS_BIRTH into AGE in years --------------------------------
  cat("Converting DAYS_BIRTH to AGE...\n")
  if ("DAYS_BIRTH" %in% names(df_clean)) {
    df_clean$AGE <- abs(df_clean$DAYS_BIRTH) / 365.25
    df_clean$DAYS_BIRTH <- NULL
  }
  
  # 5. Fix inconsistent family sizes --------------------------------------
  cat("Fixing inconsistent family size (CNT_FAM_MEMBERS < CNT_CHILDREN)...\n")
  
  if (all(c("CNT_CHILDREN", "CNT_FAM_MEMBERS") %in% names(df_clean))) {
    inconsistent <- df_clean$CNT_FAM_MEMBERS < df_clean$CNT_CHILDREN
    if (any(inconsistent, na.rm = TRUE)) {
      cat(" Correcting", sum(inconsistent, na.rm = TRUE), "rows.\n")
      df_clean$CNT_FAM_MEMBERS[inconsistent] <-
        df_clean$CNT_CHILDREN[inconsistent] + 1
    }
  }
  
  # 6. Log-transform income ------------------------------------------------
  cat("Log-transforming AMT_INCOME_TOTAL...\n")
  if("AMT_INCOME_TOTAL" %in% names(df_clean)) {
    df_clean$AMT_INCOME_TOTAL_LOG <- log1p(df_clean$AMT_INCOME_TOTAL)
    df_clean$AMT_INCOME_TOTAL <- NULL
  }
  
  # 7. Impute missing values ----------------------------------------------
  cat("Imputing missing values (Median for numeric, 'Unknown' for categoricals)...\n")
  
  num_vars <- names(dplyr::select_if(df_clean, is.numeric))
  for (v in num_vars) {
    if (any(is.na(df_clean[[v]]))) {
      df_clean[[v]][is.na(df_clean[[v]])] <- median(df_clean[[v]], na.rm = TRUE)
    }
  }
  
  cat_vars <- names(dplyr::select_if(df_clean, is.character))
  for (v in cat_vars) {
    missing_idx <- is.na(df_clean[[v]]) | df_clean[[v]] == "" | df_clean[[v]] == "NA"
    if (any(missing_idx)) {
      df_clean[[v]][missing_idx] <- "Unknown"
    }
  }
  
  
  # 8. Target Engineering (Buffer Zone Strategy) ---------------------------
  cat("Applying Buffer Zone Strategy for 'status' and creating TARGET...\n")
  
  if ("status" %in% names(df_clean)) {
    df_clean$status <- as.character(df_clean$status)
    
    # Remove ambiguous 30–59 DPD (status = "1")
    before <- nrow(df_clean)
    df_clean <- dplyr::filter(df_clean, status != "1")
    cat(" Removed", before - nrow(df_clean), "rows with status '1'.\n")
    
    df_clean$TARGET <- ifelse(df_clean$status %in% c("2","3","4","5"), 1L, 0L)
    df_clean$status <- NULL
  }
  
  df_clean$TARGET <- as.integer(df_clean$TARGET)
  
  # 9. Remove duplicate records -------------------------------------------
  cat("Removing duplicate rows...\n")
  before_dup <- nrow(df_clean)
  df_clean <- dplyr::distinct(df_clean)
  cat(" Removed", before_dup - nrow(df_clean), "duplicate rows.\n")
  
  
  # 10. Zero-variance filter ----------------------------------------------
  cat("Checking for zero-variance columns...\n")
  features_only <- dplyr::select(df_clean, -TARGET)
  nzv <- caret::nearZeroVar(features_only, saveMetrics = TRUE)
  
  if (any(nzv$zeroVar)) {
    drops <- rownames(nzv)[nzv$zeroVar]
    cat(" Dropping constant columns:", paste(drops, collapse = ", "), "\n")
    df_clean <- df_clean[, !names(df_clean) %in% drops, drop = FALSE]
  }
  
  cat("\n--- Module 3 Complete (Optimized) ---\n")
  return(df_clean)
}

# ==============================================================================
# Module 4: Preprocessed Diagnostic EDA
# ==============================================================================

perform_eda <- function(df_clean) {
  cat("\n================================================================\n")
  cat(" MODULE 4: SECONDARY DATA EXPLORATION (CLEANED DATA)\n")
  cat("================================================================\n")
  
  cat("New Dimensions:", dim(df_clean), "\n")
  
  # --- Phase 1: Structure & Content Inspection ---
  cat("\n[Phase 1] Structure & Content Inspection\n")
  
  # 1. View() is interactive; usually commented out in automated scripts
  # View(df_clean) 
  
  # 2. Head/Tail Check
  cat("First 5 rows:\n")
  print(head(df_clean, 5))
  
  # 3. Dimension Check
  cat("\nDimensions:\n")
  print(dim(df_clean))
  
  # 4. Structure Check
  cat("\nData Structure (glimpse):\n")
  dplyr::glimpse(df_clean)
  
  # --- Phase 2: Data Quality "Sanity Checks" ---
  cat("\n[Phase 2] Data Quality & Sanity Checks\n")
  
  # 1. Statistical Summary
  cat("\nStatistical Summary of Numeric Columns:\n")
  print(summary(select_if(df_clean, is.numeric)))
  
  # 2. Missing Values
  cat("\nMissing Values Count:\n")
  miss_counts <- colSums(is.na(df_clean))
  print(miss_counts[miss_counts > 0])
  # Visualizing Missingness
  print(naniar::vis_miss(df_clean, warn_large_data = FALSE) + 
          ggtitle("Missingness Map (Raw Data)"))
  
  # 3. Duplicate Rows
  dup_count <- sum(duplicated(df_clean))
  cat("\nDuplicate Rows Detected:", dup_count, "\n")
  # JUSTIFICATION: Duplicates in credit data usually indicate data entry errors 
  # or system glitches, as ID should be unique.
  
  # 4. Unique Value Counts for Categorical
  cat("\nUnique Values in Categorical Variables (Cardinality Check):\n")
  cat_cols <- names(select_if(df_clean, is.character))
  for(col in cat_cols) {
    cat(paste0(col, ": ", length(unique(df_clean[[col]])), " unique levels\n"))
  }
  
  # --- Phase 3: Univariate Visualization ---
  cat("\n[Phase 3] Univariate Visualization\n")
  
  # 1. Target Variable Distribution (The most important check)
  # CHANGE: Use 'TARGET' instead of 'status'
  p_target <- ggplot(df_clean, aes(x = as.factor(TARGET))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Target Distribution (Class Imbalance Check)", 
         x = "Target (0=Good, 1=Bad)", # Updated label
         y = "Count") +
    theme_minimal()
  print(p_target)
  
  # 2. Numeric Distributions (Histograms & Boxplots)
  num_cols <- names(select_if(df_clean, is.numeric))
  # Exclude ID from plotting
  num_cols <- num_cols[num_cols != "ID"]
  
  for(col in num_cols) {
    # Histogram
    p_hist <- ggplot(df_clean, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "coral", color = "white") +
      labs(title = paste("Distribution:", col)) +
      theme_minimal()
    
    # Boxplot
    p_box <- ggplot(df_clean, aes_string(y = col)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Outliers:", col)) +
      theme_minimal()
    
    grid.arrange(p_hist, p_box, ncol = 2)
  }
  
  # 3. Categorical Frequencies
  for(col in cat_cols) {
    p_cat <- ggplot(df_clean, aes_string(x = col)) +
      geom_bar(fill = "darkseagreen") +
      coord_flip() + # Flip for readability of labels
      labs(title = paste("Frequency:", col)) +
      theme_minimal()
    print(p_cat)
  }
  
  cat("\n--- Module 2 Complete ---\n")
  
  # Visualize New Features
  if("AGE" %in% names(df_clean)) {
    p_age <- ggplot(df_clean, aes(x=AGE)) + 
      geom_histogram(fill="purple", bins=30) + 
      labs(title="Distribution of Derived Feature: AGE") + theme_minimal()
    print(p_age)
  }
  
  # Correlation Analysis
  cat("Generating Correlation Matrix...\n")
  num_df <- select_if(df_clean, is.numeric)
  if(ncol(num_df) > 1) {
    cor_mat <- cor(num_df)
    corrplot(cor_mat, method="circle", type="lower", title="Correlation Matrix (Cleaned)", mar=c(0,0,2,0))
  }
  
  # Target Separation by Category
  cat_vars <- names(select_if(df_clean, is.character))
  for(var in cat_vars) {
    p_bivar <- ggplot(df_clean, aes_string(x=var, fill="factor(TARGET)")) +
      geom_bar(position="fill") +
      labs(title = paste("Target Proportion by", var), y="Proportion", fill="Target") +
      coord_flip() +
      theme_minimal()
    print(p_bivar)
  }
  
  cat("\n--- Module 4 Complete ---\n")
}


# ==============================================================================
# Module 5: Neural Network Data Preparation
# ==============================================================================

fit_nn_preprocessing <- function(df_clean, target_var = "TARGET") {
  cat("\n================================================================\n")
  cat(" FIT NN PREPROCESSING (Train-only encoding + scaling)\n")
  cat("================================================================\n")
  
  y <- df_clean[[target_var]]
  X <- df_clean %>% dplyr::select(-all_of(target_var))
  
  # Convert characters to factors for dummyVars
  char_cols <- names(dplyr::select_if(X, is.character))
  X[char_cols] <- lapply(X[char_cols], factor)
  
  # One-hot encoding model
  cat("Fitting dummyVars model...\n")
  dummies_model <- caret::dummyVars(" ~ .", data = X, fullRank = FALSE)
  X_encoded <- predict(dummies_model, newdata = X) %>% as.data.frame()
  
  # Min–Max scaler
  cat("Fitting Min–Max scaler...\n")
  scaler <- caret::preProcess(X_encoded, method = "range")
  X_scaled <- predict(scaler, X_encoded)
  
  cat("Encoded & scaled feature matrix has", ncol(X_scaled), "columns.\n")
  
  return(list(
    dummies_model = dummies_model,
    scaler = scaler,
    feature_names = colnames(X_scaled),
    target_var = target_var
  ))
}

apply_nn_preprocessing <- function(df_clean, preprocessing) {
  target_var <- preprocessing$target_var
  
  y <- df_clean[[target_var]]
  X <- df_clean %>% dplyr::select(-all_of(target_var))
  
  char_cols <- names(dplyr::select_if(X, is.character))
  X[char_cols] <- lapply(X[char_cols], factor)
  
  # One-hot encoding using training dummyVars
  X_encoded <- predict(preprocessing$dummies_model, newdata = X) %>% as.data.frame()
  
  # Add any missing dummy columns
  missing_cols <- setdiff(preprocessing$feature_names, colnames(X_encoded))
  if (length(missing_cols) > 0) {
    for (m in missing_cols) {
      X_encoded[[m]] <- 0
    }
  }
  
  # Align column order
  X_encoded <- X_encoded[, preprocessing$feature_names, drop = FALSE]
  
  # Scaling
  X_scaled <- predict(preprocessing$scaler, X_encoded)
  
  return(list(
    X = as.matrix(X_scaled),
    Y = as.numeric(y)
  ))
}

prepare_for_nn <- function(df_clean, target_var = "TARGET", train_ratio = 0.7) {
  cat("\n================================================================\n")
  cat(" MODULE 5: NN PREPARATION (Split + Preprocessing)\n")
  cat("================================================================\n")
  
  set.seed(123)
  idx_train <- caret::createDataPartition(df_clean[[target_var]], 
                                          p = train_ratio, list = FALSE)
  
  df_train <- df_clean[idx_train, ]
  df_test  <- df_clean[-idx_train, ]
  
  cat("Train rows:", nrow(df_train), "| Test rows:", nrow(df_test), "\n")
  
  preprocessing <- fit_nn_preprocessing(df_train, target_var)
  
  train_mats <- apply_nn_preprocessing(df_train, preprocessing)
  test_mats  <- apply_nn_preprocessing(df_test, preprocessing)
  
  cat("Train matrix:", dim(train_mats$X), "\n")
  cat("Test matrix :", dim(test_mats$X), "\n")
  
  return(list(
    X_train = train_mats$X,
    Y_train = train_mats$Y,
    X_test  = test_mats$X,
    Y_test  = test_mats$Y,
    preprocessing = preprocessing
  ))
}


# ==============================================================================
# Module 6: Execution & Reporting
# ==============================================================================

execute_pipeline <- function() {
  
  # --- 1. Robust Path Setup ---
  # We prioritize the specific path you requested.
  # If that path doesn't exist (e.g., you move to a new PC), it falls back to getwd().
  
  target_path <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts"
  
  if(dir.exists(target_path)) {
    base_dir <- target_path
  } else {
    base_dir <- getwd()
    cat("WARNING: Target directory not found. Defaulting to Working Directory:", base_dir, "\n")
  }
  
  plot_dir <- file.path(base_dir, "Auto_Generated_Plots")
  
  # Create directory if it doesn't exist
  if(!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  cat("Plots will be saved to:", plot_dir, "\n")
  
  # --- 2. Load Data ---
  data_filename <- "Dataset-part-2.csv"
  
  # Logic: Look in target path first, then project root, then subfolders
  possible_paths <- c(
    file.path(target_path, data_filename),
    file.path(getwd(), data_filename),
    file.path(getwd(), "DS-Project_data", data_filename)
  )
  
  data_path <- ""
  for(p in possible_paths) {
    if(file.exists(p)) {
      data_path <- p
      break
    }
  }
  
  if(data_path == "") stop("Dataset-part-2.csv not found in script folder or project root.")
  
  raw_data <- load_data(data_path)
  
  # --- 3. Define Auto-Save Function (With Safety Locks) ---
  save_plots_to_pdf <- function(raw_df, cleaned_df) {
    
    # Clear any previous stuck plotting devices
    while(!is.null(dev.list())) dev.off()
    
    ts <- format(Sys.time(), "%d.%m.%Y.(%H%M)")
    filename <- paste0("Autogenerated_Plots_", ts, ".pdf")
    full_path <- file.path(plot_dir, filename)
    
    cat("\nStarting PDF Generation to:", full_path, "\n")
    
    # Open PDF Device
    pdf(file = full_path, width = 10, height = 7)
    
    # SAFETY LOCK: This ensures the file closes/saves even if the plots inside fail
    on.exit(if(dev.cur() > 1) dev.off())
    
    tryCatch({
      # Plot Module 2
      cat("... Adding Raw Data EDA\n")
      perform_initial_eda(raw_df)
      
      # Plot Module 4
      cat("... Adding Cleaned Data EDA\n")
      perform_eda(cleaned_df)
      
    }, error = function(e) {
      cat("ERROR during plotting: ", e$message, "\n")
    })
    
    cat("PDF Generation Complete.\n")
  }
  
  # --- EXECUTION FLOW ---
  
  # Step A: Initial Analysis (Screen)
  perform_initial_eda(raw_data)
  
  # Step B: Cleaning
  clean_data <- process_credit_data(raw_data)
  
  # Step C: Post-Clean Analysis (Screen)
  perform_eda(clean_data)
  
  # Step D: Generate PDF Report (File)
  save_plots_to_pdf(raw_data, clean_data)
  
  # Step E: Final Prep
  nn_data <- prepare_for_nn(clean_data)
  
  return(nn_data)
}

# Run the pipeline
final_matrices <- execute_pipeline()