# Assignment 2 MLP: Class Weighting + Train/Val/Test Split
# Architecture: Batch Norm + ReLU + SGD + Variable Depth

library(keras3)
library(caret)
library(dplyr)
library(tfruns)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # Architecture
  flag_integer("units1", 512),
  flag_integer("units2", 256),
  flag_integer("units3", 0),       # 0 = Disabled
  
  # Regularization
  flag_numeric("dropout1", 0.5), 
  flag_numeric("dropout2", 0.3),
  flag_numeric("dropout3", 0.0),   
  
  # Optimization (SGD Settings)
  flag_numeric("learning_rate", 0.0005), 
  flag_numeric("momentum", 0.9),       
  flag_integer("batch_size", 32),
  flag_integer("epochs", 2500),    
  flag_integer("patience", 200)    
)

# 2. Load & Prepare Data -------------------------------------------------
# (Assuming 'data' is loaded in your environment)
target_col <- "target_class" 

# 3. Stratified Split (Train / Val / Test) -------------------------------
set.seed(42)
y_factor <- factor(data[[target_col]])

# Step A: Create Test Set (15%)
test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data <- data[test_idx, ]
remain_data <- data[-test_idx, ]

# Step B: Create Val Set from the remainder (15% of total ~ 17.6% of remain)
val_p <- 0.15 / 0.85
val_idx <- createDataPartition(factor(remain_data[[target_col]]), p = val_p, list = FALSE)

val_data   <- remain_data[val_idx, ]
train_data <- remain_data[-val_idx, ]

# 4. Preprocessing (MinMax Scaling) --------------------------------------
# Extract X matrices
x_train_raw <- as.matrix(mutate(train_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
x_val_raw   <- as.matrix(mutate(val_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))

# Prepare One-Hot Targets
y_train <- to_categorical(train_data[[target_col]], num_classes = 8)
y_val   <- to_categorical(val_data[[target_col]],   num_classes = 8)

# Scale Features (Fit on Train, Apply to Train & Val)
# "zv" removes zero variance, "range" scales to 0-1
scaler <- preProcess(x_train_raw, method = c("zv", "range"))

x_train <- predict(scaler, x_train_raw)
x_val   <- predict(scaler, x_val_raw)

# 5. Calculate Class Weights (On Train Set Only) -------------------------
# Convert One-Hot back to integer class labels (0-7) for counting
y_train_integers <- max.col(y_train) - 1

# Count frequencies
counts <- table(factor(y_train_integers, levels = 0:7))

# Calculate Weights: Total / (n_classes * count)
n_classes <- 8
total_samples <- sum(counts)
weights_vec <- total_samples / (n_classes * (counts + 1e-7)) # epsilon for safety

class_weights_list <- as.list(weights_vec)
names(class_weights_list) <- 0:7

cat("Class Weights applied:\n")
print(unlist(class_weights_list))

# 6. Define Model (BN + ReLU + Dropout) ----------------------------------
model <- keras_model_sequential() %>%
  # Layer 1
  layer_dense(units = FLAGS$units1, input_shape = c(ncol(x_train)), use_bias = FALSE) %>%
  layer_batch_normalization() %>%  
  layer_activation("relu") %>%
  layer_dropout(FLAGS$dropout1) %>%
  # Layer 2
  layer_dense(units = FLAGS$units2, use_bias = FALSE) %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_dropout(FLAGS$dropout2)

# Layer 3 (Conditional)
if (FLAGS$units3 > 0) {
  model <- model %>%
    layer_dense(units = FLAGS$units3, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(FLAGS$dropout3)
}

# Output Layer
model <- model %>%
  layer_dense(units = 8, activation = "softmax")

# 7. Compile (SGD with Momentum) -----------------------------------------
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(
    learning_rate = FLAGS$learning_rate,
    momentum = FLAGS$momentum,
    nesterov = TRUE
  ),
  metrics = list("accuracy")
)

# 8. Train (Early Stopping + Class Weights) ------------------------------
history <- model %>% fit(
  x_train, y_train,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_data = list(x_val, y_val),
  class_weight = class_weights_list, # <--- Apply Weights
  verbose = 1,
  callbacks = list(
    callback_early_stopping(
      monitor = "val_accuracy", 
      patience = FLAGS$patience, 
      restore_best_weights = TRUE,
      verbose = 1
    )
  )
)

# 9. Evaluation ----------------------------------------------------------
# Make predictions
val_probs <- model %>% predict(x_val, verbose = 0)
val_preds <- apply(val_probs, 1, which.max) - 1
val_true  <- apply(y_val, 1, which.max) - 1

# Generate Confusion Matrix
cm <- confusionMatrix(
  factor(val_preds, levels = 0:7), 
  factor(val_true, levels = 0:7), 
  mode = "everything"
)

print(cm)

# Extract key metrics for report
list(
  val_accuracy = cm$overall["Accuracy"],
  val_f1       = mean(cm$byClass[, "F1"], na.rm = TRUE),
  val_bal_acc  = mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE),
  epochs_run   = length(history$metrics$loss)
)
