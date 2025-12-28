# ==============================================================================
# STEP 2 (VARIANT C - SINGLE SPLIT): MODERATE CLASS WEIGHTS
# ==============================================================================

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
  
  # Architecture
  flag_integer("units1", 256),
  flag_integer("units2", 128),
  flag_integer("units3", 0),
  
  # Optimization
  flag_numeric("learning_rate", 0.001),
  flag_numeric("momentum", 0.9),
  flag_integer("batch_size", 32),
  flag_integer("epochs", 1000)
)

# 2. Load & Prepare Data -------------------------------------------------
# Ensure data is loaded

target_col <- "target_class" 

# Separate features and target
x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()
y_all <- to_categorical(data[[target_col]], num_classes = 8)

# 3. Create 70/15/15 Split -----------------------------------------------
set.seed(42)
n_rows <- nrow(x_all)

# Step A: Isolate Test Set (15%)
test_idx <- sample(1:n_rows, size = floor(0.15 * n_rows))
x_test_raw <- x_all[test_idx, ]
y_test     <- y_all[test_idx, ]

x_remain_raw <- x_all[-test_idx, ]
y_remain     <- y_all[-test_idx, ]

# Step B: Split Remaining (85%) into Train (70% total) and Val (15% total)
# Validation needs to be 15/85ths of the remaining data (~17.6%)
val_size_prop <- 0.15 / 0.85
val_idx <- createDataPartition(y_remain[,1], p = val_size_prop, list = FALSE) # Using caret for stratified split

x_val_raw   <- x_remain_raw[val_idx, ]
y_val       <- y_remain[val_idx, ]

x_train_raw <- x_remain_raw[-val_idx, ]
y_train     <- y_remain[-val_idx, ]

cat("Split Sizes:\n")
cat("Train:", nrow(x_train_raw), "| Val:", nrow(x_val_raw), "| Test:", nrow(x_test_raw), "\n")

# 4. Scaling (MinMax recommended for ReLU) -------------------------------
# Fit on TRAIN only
scaler <- preProcess(x_train_raw, method = c("range")) # MinMax Scaling

x_train <- predict(scaler, x_train_raw)
x_val   <- predict(scaler, x_val_raw)
x_test  <- predict(scaler, x_test_raw)

# 5. Calculate Moderate Class Weights ------------------------------------
# 1. Get counts from Training Data
y_train_indices <- apply(y_train, 1, which.max) - 1
total_count     <- length(y_train_indices)
class_counts    <- table(factor(y_train_indices, levels = 0:7))
n_classes       <- 8

# 2. Calculate Weights: sqrt(Total / (N * Count))
std_weights <- total_count / (n_classes * class_counts)
mod_weights <- sqrt(std_weights)

# 3. Convert to List
class_weights_list <- as.list(mod_weights)
names(class_weights_list) <- as.character(0:7)

cat("\nModerate Class Weights:\n")
print(round(mod_weights, 2))

# 6. Define Model --------------------------------------------------------
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

# Layer 3 (Optional)
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

# 7. Compile & Train -----------------------------------------------------
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(
    learning_rate = FLAGS$learning_rate,
    momentum = FLAGS$momentum,
    nesterov = TRUE
  ),
  metrics = list("accuracy")
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = FLAGS$batch_size,
  epochs = FLAGS$epochs,
  validation_data = list(x_val, y_val),
  class_weight = class_weights_list, # Applying the weights here
  callbacks = list(
    callback_early_stopping(patience = 15, restore_best_weights = TRUE)
  )
)

# 8. Final Evaluation (On Test Set) --------------------------------------
# Generate predictions
probs <- model %>% predict(x_test)
preds <- apply(probs, 1, which.max) - 1
truth <- apply(y_test, 1, which.max) - 1

cm <- confusionMatrix(
  factor(preds, levels = 0:7),
  factor(truth, levels = 0:7),
  mode = "everything"
)

# Print Final Report
cat("\n========================================\n")
cat("      FINAL TEST SET RESULTS            \n")
cat("========================================\n")
print(cm$overall['Accuracy'])
print(cm$byClass[, c("Sensitivity", "Precision", "F1")])

# Save Model for Reality Check
save_model(model, "best_model.keras")
saveRDS(scaler, "scaler_params.rds") # CRITICAL: Save this for Reality Check file
