library(keras3)
library(caret)
library(dplyr)
library(tensorflow)
library(tfruns)

# --- 1. GLOBAL CONFIGURATION --------------------------------------------
FLAGS <- flags(
  flag_string("model_type", "SGD") # Default value
)

MODEL_TYPE <- FLAGS$model_type 

# Define Hyperparameters based on the flag
if (MODEL_TYPE == "ADAMW") {
  HYPER <- list(
    units1 = 128, units2 = 64, units3 = 0,
    dropout1 = 0.2, dropout2 = 0.0, dropout3 = 0.0,
    learning_rate = 0.0001, weight_decay = 0.01,
    batch_size = 32, epochs = 2500, patience = 300,
    threshold_class3 = 0.35
  )
} else if (MODEL_TYPE == "SGD") {
  HYPER <- list(
    units1 = 128, units2 = 64, units3 = 0,
    dropout1 = 0.0, dropout2 = 0.0, dropout3 = 0.0,
    learning_rate = 0.0001, weight_decay = 1e-04, momentum = 0.9,
    batch_size = 32, epochs = 5000, patience = 1000,
    threshold_class3 = 0.39
  )
}

K_FOLDS <- 5
SEED_VAL <- 42

# --- 2. LOAD DATA -------------------------------------------------------
# Ensure 'data' is loaded. If running via tfruns, explicit loading is safer.
if (!exists("data")) {
  if (file.exists("project_data.RData")) {
    load("project_data.RData") 
  } else {
    stop("Data not found. Please load project_data.RData")
  }
}
target_col <- "target_class"

set.seed(SEED_VAL)

# Lock away the final TEST set (15%) - This is NOT touched during CV
y_factor <- factor(data[[target_col]])
test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data <- data[test_idx, ]
cv_data   <- data[-test_idx, ]  # The remaining 85% used for Cross-Validation

# --- 3. PREPARE K-FOLDS -------------------------------------------------
# Create 5 stratified folds
folds <- createFolds(factor(cv_data[[target_col]]), k = K_FOLDS, list = TRUE)

# Initialize dataframe to store results
results_log <- data.frame(
  Fold = integer(),
  Val_Accuracy = double(),
  Val_Loss = double(),
  Class3_Recall_Base = double(),
  Class3_F1_Base = double(),
  Class3_Recall_Tuned = double(),
  Class3_F1_Tuned = double()
)

# --- 4. CROSS-VALIDATION LOOP -------------------------------------------
for (i in 1:K_FOLDS) {
  
  cat(sprintf("\n\n==================================================\n"))
  cat(sprintf("STARTING FOLD %d / %d (%s)\n", i, K_FOLDS, MODEL_TYPE))
  cat(sprintf("==================================================\n"))
  
  # A. Split Data for this Fold
  val_indices <- folds[[i]]
  fold_val_data   <- cv_data[val_indices, ]
  fold_train_data <- cv_data[-val_indices, ]
  
  # B. Preprocessing (Inside loop to prevent leakage)
  x_train_raw <- as.matrix(mutate(fold_train_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
  x_val_raw   <- as.matrix(mutate(fold_val_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
  
  storage.mode(x_train_raw) <- "double"
  storage.mode(x_val_raw)   <- "double"
  
  # Scale based on Training data of this fold
  continuous_cols <- colnames(x_train_raw)[apply(x_train_raw, 2, function(x) !all(x %in% c(0, 1)))]
  scaler <- preProcess(x_train_raw[, continuous_cols, drop = FALSE], method = c("zv", "range"))
  
  x_train <- x_train_raw
  x_val   <- x_val_raw
  x_train[, continuous_cols] <- predict(scaler, x_train_raw[, continuous_cols, drop = FALSE])
  x_val[, continuous_cols]   <- predict(scaler, x_val_raw[, continuous_cols, drop = FALSE])
  
  # One-Hot Encode Targets
  y_train_fold <- to_categorical(fold_train_data[[target_col]], num_classes = 8)
  y_val_fold   <- to_categorical(fold_val_data[[target_col]], num_classes = 8)
  
  # C. Define Model Structure
  # Note: Accessing HYPER$ instead of FLAGS$
  keras3::clear_session() 
  
  l2_reg <- regularizer_l2(l = HYPER$weight_decay)
  
  model <- keras_model_sequential() %>%
    # Layer 1
    layer_dense(units = HYPER$units1, input_shape = c(ncol(x_train)), use_bias = FALSE, kernel_regularizer = l2_reg) %>%
    layer_batch_normalization() %>%  
    layer_activation("relu") %>%
    layer_dropout(HYPER$dropout1) %>%
    # Layer 2
    layer_dense(units = HYPER$units2, use_bias = FALSE, kernel_regularizer = l2_reg) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(HYPER$dropout2)
  
  # Layer 3 (Conditional)
  if (HYPER$units3 > 0) {
    model <- model %>%
      layer_dense(units = HYPER$units3, use_bias = FALSE, kernel_regularizer = l2_reg) %>%
      layer_batch_normalization() %>%
      layer_activation("relu") %>%
      layer_dropout(HYPER$dropout3)
  }
  
  # Output Layer
  model <- model %>%
    layer_dense(units = 8, activation = "softmax")
  
  # D. Compile Model
  if (MODEL_TYPE == "ADAMW") {
    opt <- tf$keras$optimizers$AdamW(learning_rate = HYPER$learning_rate, weight_decay = HYPER$weight_decay)
  } else {
    opt <- optimizer_sgd(learning_rate = HYPER$learning_rate, momentum = HYPER$momentum, nesterov = TRUE)
  }
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = opt,
    metrics = list("accuracy")
  )
  
  # E. Train
  history <- model %>% fit(
    x_train, y_train_fold,
    epochs = HYPER$epochs,
    batch_size = HYPER$batch_size,
    validation_data = list(x_val, y_val_fold),
    verbose = 0, # Silence per-epoch logs
    callbacks = list(
      callback_early_stopping(monitor = "val_loss", patience = HYPER$patience, restore_best_weights = TRUE, verbose = 1)
    )
  )
  
  # F. Evaluate & Threshold Tuning
  scores <- model %>% evaluate(x_val, y_val_fold, verbose = 0)
  probs  <- predict(model, x_val, verbose = 0)
  true_y <- apply(y_val_fold, 1, which.max) - 1
  
  # --- Strategy 1: Standard Argmax (Base) ---
  preds_base <- apply(probs, 1, which.max) - 1
  cm_base <- confusionMatrix(factor(preds_base, levels=0:7), factor(true_y, levels=0:7))
  
  # --- Strategy 2: Threshold Tuned (Class 3) ---
  preds_tuned <- preds_base
  # Apply Override using HYPER threshold
  preds_tuned[probs[, 4] > HYPER$threshold_class3] <- 3
  cm_tuned <- confusionMatrix(factor(preds_tuned, levels=0:7), factor(true_y, levels=0:7))
  
  # G. Log Results
  recall_base <- cm_base$byClass["Class: 3", "Sensitivity"]
  f1_base     <- cm_base$byClass["Class: 3", "F1"]
  
  recall_tuned <- cm_tuned$byClass["Class: 3", "Sensitivity"]
  f1_tuned     <- cm_tuned$byClass["Class: 3", "F1"]
  
  cat(sprintf("   > Fold %d Results:\n", i))
  cat(sprintf("     - Acc: %.4f | Loss: %.4f\n", scores["accuracy"], scores["loss"]))
  cat(sprintf("     - Class 3 Recall: %.4f -> %.4f (Tuned)\n", recall_base, recall_tuned))
  
  results_log <- rbind(results_log, data.frame(
    Fold = i,
    Val_Accuracy = scores["accuracy"],
    Val_Loss = scores["loss"],
    Class3_Recall_Base = recall_base,
    Class3_F1_Base = f1_base,
    Class3_Recall_Tuned = recall_tuned,
    Class3_F1_Tuned = f1_tuned
  ))
}

# --- 5. FINAL SUMMARY REPORT --------------------------------------------
cat("\n\n##########################################################\n")
cat(sprintf("FINAL K-FOLD RESULTS (%s)\n", MODEL_TYPE))
cat("##########################################################\n")
print(results_log)

# Return metrics to tfruns so they appear in the dashboard
list(
  val_accuracy = mean(results_log$Val_Accuracy),
  val_loss = mean(results_log$Val_Loss),
  c3_recall_tuned = mean(results_log$Class3_Recall_Tuned),
  stability_sd = sd(results_log$Val_Accuracy)
)
