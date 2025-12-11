# ==============================================================================
# STEP 2 (VARIANT D): FOCAL LOSS
# 
# CHANGE LOG:
# 1. Defined a Custom Loss Function 'focal_loss' (Gamma = 2.0).
#    - Down-weights "easy" examples so the model focuses on "hard" ones.
# 2. Removed Class Weights and Data Balancing.
# 3. Updated 'compile()' to use this custom loss instead of categorical_crossentropy.
# ==============================================================================

# Assignment 2 MLP (Multi-Layer Perceptron)
# Supports Variable Depth, Robust Scaling, and Focal Loss

library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(tensorflow) # Required for custom loss math

# --- 0. DEFINE CUSTOM FOCAL LOSS FUNCTION -------------------------------
# Gamma: Focusing parameter (2.0 is standard). 
# Higher gamma = more focus on hard, misclassified examples.
focal_loss <- function(gamma = 2.0, alpha = 0.25) {
  function(y_true, y_pred) {
    # Ensure types match
    y_true <- tf$cast(y_true, tf$float32)
    y_pred <- tf$cast(y_pred, tf$float32)
    
    # Clip predictions to prevent Log(0) errors
    epsilon <- k_epsilon()
    y_pred <- tf$clip_by_value(y_pred, epsilon, 1.0 - epsilon)
    
    # Calculate Standard Cross Entropy
    # (y_true * log(y_pred))
    cross_entropy <- -y_true * tf$math$log(y_pred)
    
    # Calculate Focal Scaling Factor: (1 - p)^gamma
    # If p is high (easy), weight approaches 0.
    # If p is low (hard), weight approaches 1.
    weight <- tf$math$pow(1.0 - y_pred, gamma)
    
    # Final Loss
    loss <- weight * cross_entropy
    
    # Sum over classes
    tf$reduce_sum(loss, axis = -1L)
  }
}
# ------------------------------------------------------------------------

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

cat("Starting", k, "-Fold Cross-Validation (With Focal Loss)...\n")

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
  x_fold_train <- predict(fold_scaler, x_fold_train_raw)
  x_fold_val   <- predict(fold_scaler, x_fold_val_raw) 
  
  # c. Define Model (Standard Baseline)
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
  
  # d. Compile (USING CUSTOM FOCAL LOSS)
  # Note: We pass the function object, not a string.
  model %>% compile(
    loss = focal_loss(gamma = 2.0, alpha = 0.25),
    optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
    metrics = c('accuracy')
  )
  
  # e. Train (No class_weight, no SMOTE)
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
cat("      (WITH FOCAL LOSS GAMMA=2.0)       \n")
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
  cat("WARNING: High Overfitting. Focal Loss may over-focus on outliers.\n")
} else if(avg_macro_f1 < 0.50) {
  cat("WARNING: Focal Loss Failed. The minority classes might be too noisy.\n")
} else {
  cat("STATUS: Good candidate architecture.\n")
}