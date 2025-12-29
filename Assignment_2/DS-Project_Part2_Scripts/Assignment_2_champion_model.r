library(keras3)
library(caret)
library(dplyr)
library(tensorflow)

# --- 1. CHAMPION CONFIGURATION (SGD + Momentum) -------------------------
HYPER <- list(
  units1 = 128,
  units2 = 64,
  learning_rate = 0.0001,
  weight_decay = 1e-04,
  momentum = 0.9,
  batch_size = 32,
  epochs = 5000,
  patience = 1000,
  threshold_class3 = 0.39
)
SEED_VAL <- 42

# --- 2. LOAD DATA -------------------------------------------------------
if (!exists("data")) {
  if (file.exists("project_data.RData")) {
    load("project_data.RData") 
  } else {
    stop("Data file not found. Please load 'project_data.RData'")
  }
}
target_col <- "target_class"
set.seed(SEED_VAL)

# --- 3. THE "SAFE" STRATIFIED SPLIT -------------------------------------
y_factor <- factor(data[[target_col]])

# Step A: Lock away 15% for FINAL TEST (The "Exam")
test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data  <- data[test_idx, ]
remain_data <- data[-test_idx, ] # 85% of original data

# Step B: Create a small 10% Internal Validation Set from the remainder
# This ensures STRATIFICATION so we don't kill the minority classes.
# 10% of Remainder is sufficient for callbacks to monitor loss.
val_idx <- createDataPartition(factor(remain_data[[target_col]]), p = 0.10, list = FALSE)

val_data   <- remain_data[val_idx, ]
train_data <- remain_data[-val_idx, ]

cat(sprintf("Final Optimization Split:\n- TRAIN (Active Learning): %d rows (Contains ~90%% of minority classes)\n- VAL   (Early Stop Monitor): %d rows (Stratified 10%%)\n- TEST  (Final Exam):         %d rows\n", 
            nrow(train_data), nrow(val_data), nrow(test_data)))

# --- 4. PREPROCESSING ---------------------------------------------------
# Create matrices
x_train_raw <- as.matrix(mutate(train_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
x_val_raw   <- as.matrix(mutate(val_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
x_test_raw  <- as.matrix(mutate(test_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))

storage.mode(x_train_raw) <- "double"
storage.mode(x_val_raw)   <- "double"
storage.mode(x_test_raw)  <- "double"

continuous_cols <- colnames(x_train_raw)[apply(x_train_raw, 2, function(x) !all(x %in% c(0, 1)))]

# FIT SCALER ON TRAIN
# We fit on the 'train_data' to prevent leakage.
scaler <- preProcess(x_train_raw[, continuous_cols, drop = FALSE], method = c("zv", "range"))

# Save Scaler
saveRDS(scaler, "final_champion_scaler.rds")

# Apply Transform
x_train <- x_train_raw
x_val   <- x_val_raw
x_test  <- x_test_raw

x_train[, continuous_cols] <- predict(scaler, x_train_raw[, continuous_cols, drop = FALSE])
x_val[, continuous_cols]   <- predict(scaler, x_val_raw[, continuous_cols, drop = FALSE])
x_test[, continuous_cols]  <- predict(scaler, x_test_raw[, continuous_cols, drop = FALSE])

# One-Hot Encoding
y_train <- to_categorical(train_data[[target_col]], num_classes = 8)
y_val   <- to_categorical(val_data[[target_col]], num_classes = 8)
y_test  <- to_categorical(test_data[[target_col]], num_classes = 8)

# --- 5. MODEL DEFINITION ------------------------------------------------
clear_session()
l2_reg <- regularizer_l2(l = HYPER$weight_decay)

model <- keras_model_sequential() %>%
  layer_dense(units = HYPER$units1, input_shape = c(ncol(x_train)), 
              use_bias = FALSE, kernel_regularizer = l2_reg) %>%
  layer_batch_normalization() %>%  
  layer_activation("relu") %>%
  layer_dense(units = HYPER$units2, use_bias = FALSE, kernel_regularizer = l2_reg) %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_dense(units = 8, activation = "softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(learning_rate = HYPER$learning_rate, 
                            momentum = HYPER$momentum, 
                            nesterov = TRUE),
  metrics = list("accuracy")
)

# --- 6. CALLBACKS & TRAIN -----------------------------------------------
callbacks_list <- list(
  # Monitors the EXPLICIT validation set we created
  callback_early_stopping(monitor = "val_loss", patience = HYPER$patience, 
                          restore_best_weights = TRUE, verbose = 1),
  
  callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, 
                                patience = 300, min_lr = 1e-6, verbose = 1)
)

cat("\nStarting Final Training (Stratified)...\n")
# We use 'validation_data' (Explicit) instead of 'validation_split' (Automatic/Unsafe)
history <- model %>% fit(
  x_train, y_train,
  epochs = HYPER$epochs,
  batch_size = HYPER$batch_size,
  validation_data = list(x_val, y_val), # Explicit Stratified Set
  verbose = 1,
  callbacks = callbacks_list
)

# --- 7. FINAL EVALUATION ------------------------------------------------
cat("\n--- Final Evaluation on HELD-OUT TEST SET ---\n")
probs <- predict(model, x_test, verbose = 0)
true_y <- apply(y_test, 1, which.max) - 1

preds_base <- apply(probs, 1, which.max) - 1

# Apply Champion Threshold
preds_tuned <- preds_base
preds_tuned[probs[, 4] > HYPER$threshold_class3] <- 3

cm <- confusionMatrix(factor(preds_tuned, levels=0:7), factor(true_y, levels=0:7))

print(cm$table)
cat("\nTest Accuracy:", cm$overall["Accuracy"])
cat("\nClass 3 Metrics (Risk):\n")
print(cm$byClass["Class: 3", c("Sensitivity", "Precision", "F1")])

save_model(model, "final_sgd_champion_model.keras")
