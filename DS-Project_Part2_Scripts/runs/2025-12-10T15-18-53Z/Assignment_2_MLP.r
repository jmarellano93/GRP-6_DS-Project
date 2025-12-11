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
  flag_integer("units1", 128),
  flag_integer("units2", 256),
  flag_integer("units3", 0),     # New: 0 = "Off" (2 Layers), >0 = "On" (3 Layers)
  
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
  Val_Loss = numeric(), # Check loss function
  Train_Acc = numeric(),
  Val_Acc = numeric()
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
  model <- keras_model_sequential(input_shape = c(ncol(x_fold_train))) %>%
    layer_dense(units = FLAGS$units1, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$units2, activation = 'relu') %>%
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
    verbose = 0,
    validation_data = list(x_fold_val, y_fold_val),
    # Optional: Add Early Stopping if 100 epochs is too long
    #callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE))
  )
  
  # f. Extract BEST Metrics (Not just the last one)
  best_epoch_idx <- which.max(history$metrics$val_accuracy)
  best_val_acc   <- history$metrics$val_accuracy[best_epoch_idx]
  best_train_acc <- history$metrics$accuracy[best_epoch_idx]
  final_val_loss <- history$metrics$val_loss[best_epoch_idx]
  final_train_loss <- history$metrics$loss[best_epoch_idx]
  
  # Store
  results[i, ] <- c(i, final_train_loss, final_val_loss, best_train_acc, best_val_acc)
  
  cat(sprintf("Fold %d | Max Val Acc: %.4f (at Epoch %d)\n", 
              i, best_val_acc, best_epoch_idx))
}

# 6. Final Report --------------------------------------------------------
cat("\n========================================\n")
cat("      ARCHITECTURE PERFORMANCE          \n")
cat("========================================\n")

avg_train_acc <- mean(results$Train_Acc)
avg_val_acc   <- mean(results$Val_Acc)
gap <- avg_train_acc - avg_val_acc

cat("Avg Train Accuracy: ", round(avg_train_acc, 4), "\n")
cat("Avg Val Accuracy:   ", round(avg_val_acc, 4), "\n")
cat("Overfitting Gap:    ", round(gap, 4), "\n")
cat("----------------------------------------\n")

if(gap > 0.05) {
  cat("WARNING: High Overfitting. Increase Dropout.\n")
} else if(avg_train_acc < 0.70) {
  cat("WARNING: Underfitting. Increase Units/Layers.\n")
} else {
  cat("STATUS: Good candidate architecture.\n")
}
