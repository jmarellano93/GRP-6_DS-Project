# Assignment 2 MLP: SMOTE + Train/Val/Test Split
# Architecture: Batch Norm + ReLU + SGD + Variable Depth

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
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned.csv")
target_col <- "target_class" 

# 3. Stratified Split (Train / Val / Test) -------------------------------
set.seed(42)
y_factor <- factor(data[[target_col]])

# Step A: Create Test Set (15%)
test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data <- data[test_idx, ]
remain_data <- data[-test_idx, ]

# Step B: Create Val Set (15% of total ~ 17.6% of remainder)
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
scaler <- preProcess(x_train_raw, method = c("zv", "range"))

x_train_scaled <- predict(scaler, x_train_raw)
x_val_scaled   <- predict(scaler, x_val_raw)

# 5. SMOTE Balancing (Train Set Only) ------------------------------------
cat("Running SMOTE on Training Set... (This may take a moment)\n")

# A. Prepare Data for SMOTE (Needs integer target, not One-Hot)
y_train_indices <- apply(y_train, 1, which.max) - 1

# B. Combine X and Y into a data frame
train_df <- as.data.frame(x_train_scaled)
train_df$class <- as.factor(y_train_indices)

# C. Run SMOTE
# dup_size = 0 means auto-detect amount needed to balance
smote_output <- SMOTE(X = train_df[, -ncol(train_df)], 
                      target = train_df$class, 
                      dup_size = 0)

# D. Extract Balanced Data
balanced_data <- smote_output$data
target_col_idx <- ncol(balanced_data)

x_train_final <- as.matrix(balanced_data[, -target_col_idx])

# E. Convert Target back to One-Hot
y_balanced_vec <- as.integer(as.character(balanced_data[, target_col_idx]))
y_train_final  <- to_categorical(y_balanced_vec, num_classes = 8)

cat("Original Train Size:", nrow(x_train_scaled), "\n")
cat("SMOTE Train Size:   ", nrow(x_train_final), "\n")

# 6. Define Model (BN + ReLU + Dropout) ----------------------------------
model <- keras_model_sequential() %>%
  # Layer 1
  layer_dense(units = FLAGS$units1, input_shape = c(ncol(x_train_final)), use_bias = FALSE) %>%
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

# 8. Train (Early Stopping) ----------------------------------------------
# Note: We use x_train_final (SMOTE) for training, but x_val_scaled (Original) for validation
history <- model %>% fit(
  x_train_final, y_train_final,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_data = list(x_val_scaled, y_val),
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
# Make predictions on Validation Set
val_probs <- model %>% predict(x_val_scaled, verbose = 0)
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
 