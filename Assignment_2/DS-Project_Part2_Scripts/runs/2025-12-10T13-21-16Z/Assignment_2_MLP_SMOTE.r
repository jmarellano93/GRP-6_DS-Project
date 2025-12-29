# ==============================================================================
# STEP 2 (VARIANT B): DATA BALANCING WITH SMOTE
# 
# CHANGE LOG:
# 1. Replaced simple 'upSample' with 'smotefamily::SMOTE'.
#    - Creates synthetic data points for minority classes.
#    - Reduces the risk of memorizing exact duplicates.
# 2. Ensures SMOTE is applied ONLY to the training fold (Validation is untouched).
# ==============================================================================

# Assignment 2 MLP (Multi-Layer Perceptron)
# Supports Variable Depth, Robust Scaling, and SMOTE

library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(smotefamily) # REQUIRED FOR SMOTE

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # Regularization
  flag_numeric("dropout1", 0),
  flag_numeric("dropout2", 0),
  flag_numeric("dropout3", 0.0),
  
  # Architecture
  flag_integer("units1", 256),
  flag_integer("units2", 128),
  flag_integer("units3", 64),
  
  # Optimization
  flag_numeric("learning_rate", 0.001),
  flag_integer("batch_size", 128),
  flag_integer("epochs", 100)
)

# 2. Load & Prepare Data -------------------------------------------------
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned.csv")
target_col <- "target_class" 

x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()
y_all <- to_categorical(data[[target_col]], num_classes = 8)

# 3. Split Data ----------------------------------------------------------
set.seed(42)

# Holdout Test Set (15%)
n_rows <- nrow(x_all)
test_idx <- sample(1:n_rows, size = floor(0.15 * n_rows))

x_test_holdout_raw <- x_all[test_idx, ]
y_test_holdout     <- y_all[test_idx, ]

x_cv_raw <- x_all[-test_idx, ] 
y_cv     <- y_all[-test_idx, ]

# 4. Cross-Validation Setup ----------------------------------------------
k <- 5
folds <- cut(seq(1, nrow(x_cv_raw)), breaks = k, labels = FALSE)

# Initialize Storage
results <- data.frame(
  Fold = integer(),
  Train_Loss = numeric(),
  Val_Loss = numeric(),
  Train_Acc = numeric(),
  Val_Acc = numeric(),
  Val_Macro_F1 = numeric(),
  Val_Bal_Acc = numeric()
)

cat("Starting", k, "-Fold Cross-Validation (With SMOTE)...\n")

# 5. Training Loop -------------------------------------------------------
for(i in 1:k){
  cat("\n--- Processing Fold #", i, "---\n")
  
  # a. Split
  val_indices <- which(folds == i, arr.ind = TRUE)
  x_fold_val_raw   <- x_cv_raw[val_indices, ]
  y_fold_val       <- y_cv[val_indices, ]
  x_fold_train_raw <- x_cv_raw[-val_indices, ]
  y_fold_train     <- y_cv[-val_indices, ]
  
  # b. Robust Scaling
  fold_scaler <- preProcess(x_fold_train_raw, method = c("zv", "center", "scale"))
  x_fold_train_scaled <- predict(fold_scaler, x_fold_train_raw)
  x_fold_val          <- predict(fold_scaler, x_fold_val_raw) 
  
  # --- NEW: STEP 2 - SMOTE BALANCING ----------------------------------
  # Goal: Create synthetic samples for minority classes in Training Set only.
  
  # 1. Prepare Data for SMOTE (Needs Dataframe with combined Target)
  # Convert One-Hot Y back to a single column (0-7)
  y_train_indices <- apply(y_fold_train, 1, which.max) - 1
  
  # Combine X and Y into a data frame
  train_df <- as.data.frame(x_fold_train_scaled)
  train_df$class <- as.factor(y_train_indices) # SMOTE needs target as factor
  
  # 2. Run SMOTE
  # SMOTE() automatically detects minority classes and upsamples them.
  # dup_size = 0 means "Auto detect optimal amount"
  cat("  Running SMOTE... (This may take a moment)\n")
  smote_output <- SMOTE(X = train_df[, -ncol(train_df)], 
                        target = train_df$class, 
                        dup_size = 0)
  
  # 3. Extract Balanced Data
  # smote_output$data contains the balanced dataset (Original + Synthetic)
  balanced_data <- smote_output$data
  
  # 4. Separate Back into X and Y for Keras
  # The target column is usually named "class" by SMOTE, appearing last
  target_col_idx <- ncol(balanced_data)
  
  x_fold_train <- as.matrix(balanced_data[, -target_col_idx])
  
  # Convert target back to integer, then One-Hot
  # Note: SMOTE might return target as char/factor. ensure conversion is safe.
  y_balanced_vec <- as.integer(as.character(balanced_data[, target_col_idx]))
  y_fold_train   <- to_categorical(y_balanced_vec, num_classes = 8)
  
  # Optional: Print new counts
  # cat("  Balanced Size:", nrow(x_fold_train), "\n")
  # --------------------------------------------------------------------
  
  # c. Define Model
  model <- keras_model_sequential(input_shape = c(ncol(x_fold_train))) %>%
    layer_dense(units = FLAGS$units1, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$units2, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout2)
  
  if (FLAGS$units3 > 0) {
    model <- model %>%
      layer_dense(units = FLAGS$units3, activation = 'relu') %>%
      layer_dropout(rate = FLAGS$dropout3)
  }
  
  model <- model %>% layer_dense(units = 8, activation = 'softmax')
  
  # d. Compile
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
    metrics = c('accuracy')
  )
  
  # e. Train
  history <- model %>% fit(
    x_fold_train, y_fold_train,
    batch_size = FLAGS$batch_size,
    epochs = FLAGS$epochs,
    verbose = 1,
    validation_data = list(x_fold_val, y_fold_val)
  )
  
  # f. Extract Metrics
  best_epoch_idx <- which.max(history$metrics$val_accuracy)
  best_val_acc   <- history$metrics$val_accuracy[best_epoch_idx]
  best_train_acc <- history$metrics$accuracy[best_epoch_idx]
  final_val_loss <- history$metrics$val_loss[best_epoch_idx]
  final_train_loss <- history$metrics$loss[best_epoch_idx]
  
  # Imbalance Metrics
  val_probs <- model %>% predict(x_fold_val, verbose = 0)
  val_preds <- apply(val_probs, 1, which.max) - 1
  val_true  <- apply(y_fold_val, 1, which.max) - 1
  
  cm <- confusionMatrix(
    factor(val_preds, levels = 0:7),
    factor(val_true, levels = 0:7),
    mode = "everything"
  )
  
  macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
  bal_acc  <- mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)
  
  results[i, ] <- c(i, final_train_loss, final_val_loss, best_train_acc, 
                    best_val_acc, macro_f1, bal_acc)
  
  cat(sprintf("Fold %d | Val Acc: %.4f | Macro F1: %.4f | Bal Acc: %.4f\n", 
              i, best_val_acc, macro_f1, bal_acc))
}

# 6. Final Report --------------------------------------------------------
cat("\n========================================\n")
cat("      ARCHITECTURE PERFORMANCE          \n")
cat("            (WITH SMOTE)                \n")
cat("========================================\n")

avg_train_acc <- mean(results$Train_Acc)
avg_val_acc   <- mean(results$Val_Acc)
avg_macro_f1  <- mean(results$Val_Macro_F1)
avg_bal_acc   <- mean(results$Val_Bal_Acc)

gap <- avg_train_acc - avg_val_acc

cat("Avg Train Accuracy:   ", round(avg_train_acc, 4), "\n")
cat("Avg Val Accuracy:     ", round(avg_val_acc, 4), "\n")
cat("Avg Macro F1 Score:   ", round(avg_macro_f1, 4), " (KEY METRIC)\n")
cat("Avg Balanced Acc:     ", round(avg_bal_acc, 4), "\n")
cat("Overfitting Gap:      ", round(gap, 4), "\n")
cat("----------------------------------------\n")

if(gap > 0.05) {
  cat("WARNING: High Overfitting. SMOTE often requires higher Dropout.\n")
} else if(avg_macro_f1 < 0.50) {
  cat("WARNING: SMOTE Failed. Check if classes are too small (<5 samples).\n")
} else {
  cat("STATUS: Good candidate architecture.\n")
}