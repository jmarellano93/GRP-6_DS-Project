# ==============================================================================
# Assignment 2 MLP (Keras 3 - CPU Version)
# ==============================================================================


# Helper for "Default value if null"
`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============================================================================
# 1. Hyperparameters (Standard R List)
# ==============================================================================
FLAGS <- list(
  dropout1 = 0,
  dropout2 = 0,
  dropout3 = 0.0, 
  units1 = 128,
  units2 = 256,
  units3 = 0,    
  learning_rate = 0.001,
  batch_size = 128,
  epochs = 50 # Reduced slightly for CPU speed
)

# ==============================================================================
# 2. Load & Prepare Data
# ==============================================================================
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned.csv")
target_col <- "target_class"

x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()

# FIX: Keras 3 requires Integers (0-7)
raw_labels <- as.integer(data[[target_col]]) - 1

# Convert to One-Hot Encoding
y_all <- to_categorical(raw_labels, num_classes = 8)

# ==============================================================================
# 3. Split Data
# ==============================================================================
set.seed(42)
n_rows  <- nrow(x_all)
test_idx <- sample(1:n_rows, size = floor(0.15 * n_rows))

x_test_holdout_raw <- x_all[test_idx, ]
y_test_holdout     <- y_all[test_idx, ]
x_cv_raw <- x_all[-test_idx, ]
y_cv     <- y_all[-test_idx, ]

# ==============================================================================
# 4. Cross-Validation Loop
# ==============================================================================
k <- 5
folds <- cut(seq(1, nrow(x_cv_raw)), breaks = k, labels = FALSE)

results <- data.frame(Fold = integer(k), Val_Acc = numeric(k))

cat("Starting", k, "-Fold Cross-Validation (Keras 3 CPU)...\n")

for (i in seq_len(k)) {
  cat("\n--- Processing Fold #", i, "---\n")
  
  # a. Split
  val_indices      <- which(folds == i, arr.ind = TRUE)
  x_fold_val_raw   <- x_cv_raw[val_indices, , drop = FALSE]
  y_fold_val       <- y_cv[val_indices, , drop = FALSE]
  x_fold_train_raw <- x_cv_raw[-val_indices, , drop = FALSE]
  y_fold_train     <- y_cv[-val_indices, , drop = FALSE]
  
  # b. Scale
  fold_scaler <- preProcess(x_fold_train_raw, method = c("zv", "center", "scale"))
  x_fold_train <- predict(fold_scaler, x_fold_train_raw)
  x_fold_val   <- predict(fold_scaler, x_fold_val_raw)
  
  # c. Define Model (KERAS 3 SYNTAX)
  # Note: input_shape goes inside keras_model_sequential() for safety
  model <- keras_model_sequential(input_shape = c(ncol(x_fold_train))) %>%
    layer_dense(units = FLAGS$units1, activation = "relu") %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$units2, activation = "relu") %>%
    layer_dropout(rate = FLAGS$dropout2)
  
  if (FLAGS$units3 > 0) {
    model <- model %>%
      layer_dense(units = FLAGS$units3, activation = "relu") %>%
      layer_dropout(rate = FLAGS$dropout3)
  }
  
  model <- model %>% layer_dense(units = 8, activation = "softmax")
  
  # d. Compile
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
    metrics = c("accuracy")
  )
  
  # e. Train
  history <- model %>% fit(
    x = x_fold_train, y = y_fold_train,
    batch_size = FLAGS$batch_size,
    epochs = FLAGS$epochs,
    verbose = 0,
    validation_data = list(x_fold_val, y_fold_val)
  )
  
  # f. Metrics
  val_acc_vec <- history$metrics$val_accuracy %||% history$metrics$val_acc
  best_acc    <- max(val_acc_vec)
  
  results[i, ] <- c(i, best_acc)
  cat(sprintf("Fold %d | Max Val Acc: %.4f\n", i, best_acc))
  
  # Clear memory
  keras3::k_clear_session()
}

cat("\nDone! Average Accuracy:", mean(results$Val_Acc), "\n")
