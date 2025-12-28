library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(smotefamily)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # Architecture
  flag_integer("units1", 512),
  flag_integer("units2", 256),
  flag_integer("units3", 0),       # 0 means disabled
  
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

x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()
y_all <- to_categorical(data[[target_col]], num_classes = 8)

# 3. Split Data (Holdout) ------------------------------------------------
set.seed(42)

n_rows <- nrow(x_all)
test_idx <- sample(1:n_rows, size = floor(0.15 * n_rows))

x_test_holdout_raw <- x_all[test_idx, ]
y_test_holdout     <- y_all[test_idx, ]

x_cv_raw <- x_all[-test_idx, ] 
y_cv     <- y_all[-test_idx, ]

# 4. Cross-Validation Setup ----------------------------------------------
k <- 5
folds <- cut(seq(1, nrow(x_cv_raw)), breaks = k, labels = FALSE)

results <- data.frame(
  Fold = integer(),
  Train_Loss = numeric(),
  Val_Loss = numeric(),
  Train_Acc = numeric(),
  Val_Acc = numeric(),
  Val_Macro_F1 = numeric(),
  Val_Bal_Acc = numeric()
)

cat("Starting", k, "-Fold Cross-Validation (With SMOTE + BN + SGD)...\n")

# 5. Training Loop -------------------------------------------------------
for(i in 1:k){
  cat("\n--- Processing Fold #", i, "---\n")
  
  # a. Split Train/Val
  val_indices <- which(folds == i, arr.ind = TRUE)
  x_fold_val_raw   <- x_cv_raw[val_indices, ]
  y_fold_val       <- y_cv[val_indices, ]
  x_fold_train_raw <- x_cv_raw[-val_indices, ]
  y_fold_train     <- y_cv[-val_indices, ]
  
  # b. MinMax Scaling (Range 0-1)
  #    Note: "zv" removes zero variance columns, "range" scales to 0-1
  fold_scaler <- preProcess(x_fold_train_raw, method = c("zv", "range"))
  
  x_fold_train_scaled <- predict(fold_scaler, x_fold_train_raw)
  x_fold_val          <- predict(fold_scaler, x_fold_val_raw) 
  
  # --- STEP 2: SMOTE BALANCING (Train Set Only) -----------------------
  cat("  Running SMOTE... (This may take a moment)\n")
  
  # 1. Prepare Data for SMOTE (Needs Index Target, not One-Hot)
  y_train_indices <- apply(y_fold_train, 1, which.max) - 1
  
  # Combine into dataframe
  train_df <- as.data.frame(x_fold_train_scaled)
  train_df$class <- as.factor(y_train_indices)
  
  # 2. Run SMOTE
  # dup_size = 0 means auto-detect amount needed to balance
  smote_output <- SMOTE(X = train_df[, -ncol(train_df)], 
                        target = train_df$class, 
                        dup_size = 0)
  
  # 3. Extract Balanced Data
  balanced_data <- smote_output$data
  target_col_idx <- ncol(balanced_data)
  
  x_fold_train <- as.matrix(balanced_data[, -target_col_idx])
  
  # 4. Convert Target back to One-Hot
  y_balanced_vec <- as.integer(as.character(balanced_data[, target_col_idx]))
  y_fold_train   <- to_categorical(y_balanced_vec, num_classes = 8)
  # --------------------------------------------------------------------
  
  # c. Define Model (Batch Norm + ReLU Architecture)
  model <- keras_model_sequential() %>%
    # Layer 1
    layer_dense(units = FLAGS$units1, input_shape = c(ncol(x_fold_train)), use_bias = FALSE) %>%
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
  
  # d. Compile (SGD with Momentum)
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_sgd(
      learning_rate = FLAGS$learning_rate,
      momentum = FLAGS$momentum,
      nesterov = TRUE
    ),
    metrics = list("accuracy")
  )
  
  # e. Train with Early Stopping
  history <- model %>% fit(
    x_fold_train, y_fold_train,
    epochs = FLAGS$epochs,
    batch_size = FLAGS$batch_size,
    validation_data = list(x_fold_val, y_fold_val),
    verbose = 0,
    callbacks = list(
      callback_early_stopping(
        monitor = "val_accuracy", 
        patience = FLAGS$patience, 
        restore_best_weights = TRUE,
        verbose = 0
      )
    )
  )
  
  # f. Extract Metrics
  # Because we used restore_best_weights, the model state is already at best epoch
  # But history logs all epochs. We find the max val_acc index.
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
cat("      (SMOTE + BN + SGD)                \n")
cat("========================================\n")

avg_train_acc <- mean(results$Train_Acc)
avg_val_acc   <- mean(results$Val_Acc)
avg_macro_f1  <- mean(results$Val_Macro_F1)
avg_bal_acc   <- mean(results$Val_Bal_Acc)

gap <- avg_train_acc - avg_val_acc

cat("Avg Train Accuracy:   ", round(avg_train_acc, 4), "\n")
cat("Avg Val Accuracy:     ", round(avg_val_acc, 4), "\n")
cat("Avg Macro F1 Score:   ", round(avg_macro_f1, 4), "\n")
cat("Avg Balanced Acc:     ", round(avg_bal_acc, 4), "\n")
cat("Overfitting Gap:      ", round(gap, 4), "\n")
cat("----------------------------------------\n")
