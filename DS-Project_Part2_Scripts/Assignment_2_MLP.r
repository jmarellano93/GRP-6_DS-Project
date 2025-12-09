# Assignment 2 MLP (Multi-Layer Perceptron)

library(keras3)
library(caret)
library(dplyr)
library(tfruns) # Needed for flags()

# Hyperparameter flags ---------------------------------------------------
FLAGS <- flags(
  flag_numeric("dropout1", 0),
  flag_numeric("dropout2", 0),
  flag_integer("units1", 128),
  flag_integer("units2", 64),
  flag_numeric("learning_rate", 0.001),
  flag_integer("batch_size", 128),
  flag_integer("epochs", 100)
)

# 1. Load Pre-Processed Data ---------------------------------------------
# Replace with the actual name of your exported CSV from the EDA step
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned.csv")

# Separate Features (X) and Target (Y)
target_col <- "target_class" 

x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()
y_all <- to_categorical(data[[target_col]], num_classes = 8)

# 2. Split (No Scaling Yet!) ---------------------------------------------
set.seed(42)

# Create Holdout Test Set (15%)
n_rows <- nrow(x_all)
test_idx <- sample(1:n_rows, size = floor(0.15 * n_rows))

# Keep these RAW for now
x_test_holdout_raw <- x_all[test_idx, ]
y_test_holdout     <- y_all[test_idx, ]

x_cv_raw <- x_all[-test_idx, ] 
y_cv     <- y_all[-test_idx, ]

# 3. Cross-Validation Loop (5 Folds) -------------------------------------
k <- 5
folds <- cut(seq(1, nrow(x_cv_raw)), breaks = k, labels = FALSE)

# --- FIX: Initialize the results dataframe container ---
results <- data.frame(
  Fold = integer(),
  Train_Loss = numeric(),
  Val_Loss = numeric(),
  Train_Acc = numeric(),
  Val_Acc = numeric()
)
# -----------------------------------------------------

cat("Starting", k, "-Fold Cross-Validation (Strict Scaling)...\n")

for(i in 1:k){
  cat("\n--- Processing Fold #", i, "---\n")
  
  # a. Split Raw Data
  val_indices <- which(folds == i, arr.ind = TRUE)
  
  x_fold_val_raw   <- x_cv_raw[val_indices, ]
  y_fold_val       <- y_cv[val_indices, ]
  
  x_fold_train_raw <- x_cv_raw[-val_indices, ]
  y_fold_train     <- y_cv[-val_indices, ]
  
  # b. Scale INSIDE the loop
  # Fit scaler ONLY on the training portion of this fold
  fold_scaler <- preProcess(x_fold_train_raw, method = c("zv", "center", "scale"))
  # Add zero variance for columns with low variance, with k-fold it would be possible that a column has no variance in the training fold
  
  # Apply to Train AND Validation
  x_fold_train <- predict(fold_scaler, x_fold_train_raw)
  x_fold_val   <- predict(fold_scaler, x_fold_val_raw) 
  
  # c. Define Model
  model <- keras_model_sequential(input_shape = c(ncol(x_fold_train))) %>%
    layer_dense(units = FLAGS$units1, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$units2, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout2) %>%
    layer_dense(units = 8, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
    metrics = c('accuracy')
  )
  
  # d. Train & Capture History
  history <- model %>% fit(
    x_fold_train, y_fold_train,
    batch_size = FLAGS$batch_size,
    epochs = FLAGS$epochs,
    verbose = 1, 
    validation_data = list(x_fold_val, y_fold_val)
  )
  
  # e. Extract Metrics from the LAST epoch
  last_train_loss <- tail(history$metrics$loss, 1)
  last_val_loss   <- tail(history$metrics$val_loss, 1)
  last_train_acc  <- tail(history$metrics$accuracy, 1)
  last_val_acc    <- tail(history$metrics$val_accuracy, 1)
  
  # Store
  results[i, ] <- c(i, last_train_loss, last_val_loss, last_train_acc, last_val_acc)
  
  # Print progress
  cat(sprintf("Fold %d | Train Acc: %.4f vs Val Acc: %.4f | Gap: %.4f\n", 
              i, last_train_acc, last_val_acc, (last_train_acc - last_val_acc)))
  
  # Optional: Plot curves for the first fold only
  if(i == 1) { plot(history) }
}

# 4. Final Architecture Report -------------------------------------------
cat("\n========================================\n")
cat("      ARCHITECTURE PERFORMANCE          \n")
cat("========================================\n")

avg_train_acc <- mean(results$Train_Acc)
avg_val_acc   <- mean(results$Val_Acc)
gap <- avg_train_acc - avg_val_acc

cat("Avg Train Accuracy: ", round(avg_train_acc, 4), "\n")
cat("Avg Val Accuracy:   ", round(avg_val_acc, 4), "\n")
cat("----------------------------------------\n")
cat("Overfitting Gap:    ", round(gap, 4), "\n")

if(gap > 0.05) {
  cat("WARNING: High Overfitting. Consider increasing Dropout or reducing Units.\n")
} else if(avg_train_acc < 0.70) {
  cat("WARNING: Underfitting. Model might be too simple or needs more epochs.\n")
} else {
  cat("STATUS: Balanced fit. Good candidate architecture.\n")
}
