# ==============================================================================
# UPDATES FOR IMBALANCED DATA (NO K-FOLD VERSION):
# 1. Split: Stratified Train (70%) / Validation (15%) / Test (15%)
# 2. Metrics: Macro F1 and Balanced Accuracy on the Validation Set.
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
  flag_numeric("dropout3", 0.0), 
  
  # Architecture (Small Model for Så©GD Test)
  flag_integer("units1", 1024),
  flag_integer("units2", 512),
  flag_integer("units3", 0),     
  
  # Optimization
  flag_numeric("learning_rate", 0.001),
  flag_integer("batch_size", 512),
  flag_integer("epochs", 5000)
)

# 2. Load & Prepare Data -------------------------------------------------
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned_v2.csv")
target_col <- "target_class" 

# Prepare Input Matrix
x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()

# Prepare Targets (One-Hot Encoded)
y_all <- to_categorical(data[[target_col]], num_classes = 7)

# Prepare Raw Target Vector (Needed for Stratified Splitting logic)
y_raw_factor <- as.factor(data[[target_col]])

# 3. Stratified Split Data (Train / Val / Test) --------------------------
set.seed(42)

# A. Create Test Holdout (15% of Total) - STRATIFIED
# createDataPartition returns indices that preserve class distribution
test_idx <- createDataPartition(y_raw_factor, p = 0.15, list = FALSE)

x_test_raw <- x_all[test_idx, ]
y_test     <- y_all[test_idx, ]

# Remaining Data (85% of Total)
x_remain <- x_all[-test_idx, ]
y_remain <- y_all[-test_idx, ]
y_remain_factor <- y_raw_factor[-test_idx]

# B. Create Validation Set (15% of Total => ~17.65% of Remaining) - STRATIFIED
val_p <- 0.15 / 0.85 # Calculate exact proportion needed
val_idx <- createDataPartition(y_remain_factor, p = val_p, list = FALSE)

x_val_raw   <- x_remain[val_idx, ]
y_val       <- y_remain[val_idx, ]

# Train Set (Remaining ~70% of Total)
x_train_raw <- x_remain[-val_idx, ]
y_train     <- y_remain[-val_idx, ]

cat("Data Split Summary (Stratified):\n")
cat("Train samples: ", nrow(x_train_raw), "\n")
cat("Val samples:   ", nrow(x_val_raw), "\n")
cat("Test samples:  ", nrow(x_test_raw), "\n")

# 4. Preprocessing (Robust Scaling) --------------------------------------
# IMPORTANT: Fit scaler ONLY on Training data, apply to Val and Test
scaler <- preProcess(x_train_raw, method = c("zv", "center", "scale"))

x_train <- predict(scaler, x_train_raw)
x_val   <- predict(scaler, x_val_raw)
# x_test  <- predict(scaler, x_test_raw) # Ready if needed for final eval

# 5. Define Model --------------------------------------------------------
model <- keras_model_sequential() %>%
  layer_dense(
    units = FLAGS$units1,
    activation = "relu",
    #kernel_regularizer = regularizer_l2(0.01),
    input_shape = c(ncol(x_train))
  ) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  
  layer_dense(
    units = FLAGS$units2, 
    activation = "relu"
    #kernel_regularizer = regularizer_l2(0.01)
  ) %>%
  layer_dropout(rate = FLAGS$dropout2)

# Optional: Layer 3
if (FLAGS$units3 > 0) {
  model <- model %>%
    layer_dense(
      units = FLAGS$units3, 
      activation = 'relu',
      kernel_regularizer = regularizer_l2(0.01)
    ) %>%
    layer_dropout(rate = FLAGS$dropout3) 
}

# Output Layer
model <- model %>% 
  layer_dense(units = 7, activation = 'softmax')

# 6. Compile & Train -----------------------------------------------------
model %>% compile(
  loss = 'categorical_crossentropy',
  # SGD with Momentum (Classic "Grokking" Setup)
  optimizer = tensorflow::tf$keras$optimizers$AdamW(learning_rate= FLAGS$learning_rate, weight_decay = 0.1),
  metrics = c('accuracy')
)

lr_scheduler <- callback_reduce_lr_on_plateau(
  monitor = "loss",        # Watch Training Loss (to fix underfitting)
  factor = 0.5,            # Cut LR in half when stuck
  patience = 200,           # Wait 100 epochs before cutting
  min_lr = 1e-6,           # Don't go below this
  verbose = 1              # Print a message when LR changes
)

  history <- model %>% fit(
  x_train, y_train,
  epochs = 5000,           # Your 5000 epoch goal
  batch_size = 512,        # The recommended batch size
  validation_data = list(x_val, y_val)
  #callbacks = list(lr_scheduler)  
  )

# 7. Metrics Calculation -------------------------------------------------
# Extract Keras metrics (Best Epoch)
best_epoch_idx <- which.max(history$metrics$val_accuracy)
best_val_acc   <- history$metrics$val_accuracy[best_epoch_idx]
best_train_acc <- history$metrics$accuracy[best_epoch_idx]
final_val_loss <- history$metrics$val_loss[best_epoch_idx]

# --- Calculate Imbalance-Aware Metrics (Macro F1 & Balanced Acc) ---
# 1. Generate probabilities for the validation set
val_probs <- model %>% predict(x_val, verbose = 0)

# 2. Convert to hard classes (0 to 7)
val_preds <- apply(val_probs, 1, which.max) - 1
val_true  <- apply(y_val, 1, which.max) - 1

# 3. Create Confusion Matrix
cm <- confusionMatrix(
  factor(val_preds, levels = 0:6),
  factor(val_true, levels = 0:6),
  mode = "everything"
)

# 4. Extract Macro Averages
macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
bal_acc  <- mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)

# 8. Final Report --------------------------------------------------------
cat("\n========================================\n")
cat("       SINGLE RUN PERFORMANCE           \n")
cat("========================================\n")

gap <- best_train_acc - best_val_acc

cat("Train Accuracy:     ", round(best_train_acc, 4), "\n")
cat("Val Accuracy:       ", round(best_val_acc, 4), "\n")
cat("Val Macro F1 Score: ", round(macro_f1, 4), " (KEY METRIC)\n")
cat("Val Balanced Acc:   ", round(bal_acc, 4), "\n")
cat("Overfitting Gap:    ", round(gap, 4), "\n")
cat("----------------------------------------\n")

if(gap > 0.05) {
  cat("WARNING: High Overfitting. Increase Dropout.\n")
} else if(macro_f1 < 0.50) {
  cat("WARNING: Poor Minority Class Performance. Check Class Weights.\n")
} else {
  cat("STATUS: Good candidate architecture.\n")
}

# Return metrics to tfruns
list(
  val_loss = final_val_loss,
  val_accuracy = best_val_acc,
  val_f1 = macro_f1,
  val_bal_acc = bal_acc
)
