# ==============================================================================
# UPDATES FOR IMBALANCED DATA:
# 1. Added Macro F1 and Balanced Accuracy to the results tracking.
#    - Why: Standard Accuracy is misleading for imbalanced data (it favors the majority class).
#    - Fix: We now calculate metrics that weigh all classes equally using caret::confusionMatrix.
# 2. Updated the Final Report to summarize these new, more honest metrics.
# ==============================================================================

# Assignment 2 MLP (Multi-Layer Perceptron)
# Supports Variable Depth (2 or 3 Layers) and Robust Scaling

library(keras3)
library(caret)
library(dplyr)
library(tfruns)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # Regularization
  flag_numeric("dropout1", 0),
  flag_numeric("dropout2", 0),
  flag_numeric("dropout3", 0.0), # New: For optional 3rd layer
  
  # Architecture
  flag_integer("units1", 256),
  flag_integer("units2", 128),
  flag_integer("units3", 64),     # New: 0 = "Off" (2 Layers), >0 = "On" (3 Layers)
  
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

# Initialize Storage (Added F1 and Balanced Acc)
results <- data.frame(
  Fold = integer(),
  Train_Loss = numeric(),
  Val_Loss = numeric(),
  Train_Acc = numeric(),
  Val_Acc = numeric(),
  Val_Macro_F1 = numeric(),   # NEW
  Val_Bal_Acc = numeric()     # NEW
)

cat("Starting", k, "-Fold Cross-Validation (Variable Depth)...\n")

# 5. Training Loop -------------------------------------------------------
for(i in 1:k){
  cat("\n--- Processing Fold #", i, "---\n")
  
  # a. Split
  val_indices <- which(folds == i, arr.ind = TRUE)
  x_fold_val_raw   <- x_cv_raw[val_indices, ]
  y_fold_val       <- y_cv[val_indices, ]
  x_fold_train_raw <- x_cv_raw[-val_indices, ]
  y_fold_train     <- y_cv[-val_indices, ]
  
  # b. Robust Scaling (Include 'zv' for safety)
  fold_scaler <- preProcess(x_fold_train_raw, method = c("zv", "center", "scale"))
  x_fold_train <- predict(fold_scaler, x_fold_train_raw)
  x_fold_val   <- predict(fold_scaler, x_fold_val_raw) 
  
  # c. Define Model (Variable Depth Logic)
  # Base: Layer 1 + Layer 2
  model <- keras_model_sequential() %>%
    layer_dense(
      units = FLAGS$units1,
      activation = "relu",
      input_shape = c(ncol(x_fold_train))
    ) %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$units2, activation = "relu") %>%
    layer_dropout(rate = FLAGS$dropout2)

  
  # Optional: Layer 3 (Only added if units3 > 0)
  if (FLAGS$units3 > 0) {
    model <- model %>%
      layer_dense(units = FLAGS$units3, activation = 'relu') %>%
      layer_dropout(rate = FLAGS$dropout3)
  }
  
  # Output Layer
  model <- model %>% 
    layer_dense(units = 8, activation = 'softmax')
  
  # d. Compile
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
    metrics = c('accuracy')
  )
  
  # e. Train (Clean Output)
  history <- model %>% fit(
    x_fold_train, y_fold_train,
    batch_size = FLAGS$batch_size,
    epochs = FLAGS$epochs,
    verbose = 1,
    validation_data = list(x_fold_val, y_fold_val)
  )
  
  # f. Extract Metrics
  # Standard Keras Metrics (from history)
  best_epoch_idx <- which.max(history$metrics$val_accuracy)
  best_val_acc   <- history$metrics$val_accuracy[best_epoch_idx]
  best_train_acc <- history$metrics$accuracy[best_epoch_idx]
  final_val_loss <- history$metrics$val_loss[best_epoch_idx]
  final_train_loss <- history$metrics$loss[best_epoch_idx]
  
  # --- NEW: Calculate Imbalance-Aware Metrics Manually ---
  # 1. Generate probabilities for the validation set
  val_probs <- model %>% predict(x_fold_val, verbose = 0)
  
  # 2. Convert to hard classes (0 to 7)
  val_preds <- apply(val_probs, 1, which.max) - 1
  val_true  <- apply(y_fold_val, 1, which.max) - 1
  
  # 3. Create Confusion Matrix (Ensure all 8 levels are present)
  cm <- confusionMatrix(
    factor(val_preds, levels = 0:7),
    factor(val_true, levels = 0:7),
    mode = "everything"
  )
  
  # 4. Extract Macro Averages
  # (cm$byClass returns a matrix of metrics per class. We take the mean.)
  macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
  bal_acc  <- mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)
  # -------------------------------------------------------
  
  # Store all metrics
  results[i, ] <- c(i, final_train_loss, final_val_loss, best_train_acc, 
                    best_val_acc, macro_f1, bal_acc)
  
  cat(sprintf("Fold %d | Val Acc: %.4f | Macro F1: %.4f | Bal Acc: %.4f\n", 
              i, best_val_acc, macro_f1, bal_acc))
}

# 6. Final Report --------------------------------------------------------
cat("\n========================================\n")
cat("      ARCHITECTURE PERFORMANCE          \n")
cat("========================================\n")

avg_train_acc <- mean(results$Train_Acc)
avg_val_acc   <- mean(results$Val_Acc)
avg_macro_f1  <- mean(results$Val_Macro_F1) # NEW
avg_bal_acc   <- mean(results$Val_Bal_Acc)  # NEW

gap <- avg_train_acc - avg_val_acc

cat("Avg Train Accuracy:   ", round(avg_train_acc, 4), "\n")
cat("Avg Val Accuracy:     ", round(avg_val_acc, 4), "\n")
cat("Avg Macro F1 Score:   ", round(avg_macro_f1, 4), " (KEY METRIC)\n")
cat("Avg Balanced Acc:     ", round(avg_bal_acc, 4), "\n")
cat("Overfitting Gap:      ", round(gap, 4), "\n")
cat("----------------------------------------\n")

if(gap > 0.05) {
  cat("WARNING: High Overfitting. Increase Dropout.\n")
} else if(avg_macro_f1 < 0.50) {
  cat("WARNING: Poor Minority Class Performance. Check Class Weights.\n")
} else {
  cat("STATUS: Good candidate architecture.\n")
}

# tfruns will create columns named 'metric_val_f1', 'metric_val_bal_acc', etc.
list(
  val_loss = avg_val_loss,
  val_accuracy = avg_val_acc,
  val_f1 = avg_val_f1,
  val_bal_acc = avg_val_bal
)
