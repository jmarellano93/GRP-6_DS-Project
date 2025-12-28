# Assignment 2 MLP: Undersampling + Train/Val/Test Split
# Architecture: Batch Norm + ReLU + SGD + Variable Depth

library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(tensorflow)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # Architecture
  flag_integer("units1", 512),
  flag_integer("units2", 256),
  flag_integer("units3", 0),       # 0 = Disabled
  
  # Regularization
  flag_numeric("dropout1", 0), 
  flag_numeric("dropout2", 0),
  flag_numeric("dropout3", 0.0),   
  
  # Optimization (SGD Settings)
  flag_numeric("learning_rate", 0.001), 
  flag_numeric("momentum", 0.9),       
  flag_integer("batch_size", 32),
  flag_integer("epochs", 1000),    
  flag_integer("patience", 200),
  
  # Undersampling Specific
  flag_integer("max_samples_per_class", 5000) # Cap for majority classes
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

# Step B: Create Val Set (15% of total ~ 17.6% of remainder)
val_p <- 0.15 / 0.85
val_idx <- createDataPartition(factor(remain_data[[target_col]]), p = val_p, list = FALSE)

val_data       <- remain_data[val_idx, ]
train_data_raw <- remain_data[-val_idx, ] # "Raw" because we will undersample it

# --- 3.5 UNDERSAMPLING THE MAJORITY -------------------------------------
# We ONLY undersample the training data. Validation and Test must remain realistic.
cat("Original Training Distribution:\n")
print(table(train_data_raw[[target_col]]))

# Strategy: Group by class, randomly sample down to 'max_samples_per_class'
train_data <- train_data_raw %>%
  group_by(!!sym(target_col)) %>%
  group_modify(~ {
    if (nrow(.x) > FLAGS$max_samples_per_class) {
      .x[sample(1:nrow(.x), FLAGS$max_samples_per_class), ]
    } else {
      .x
    }
  }) %>%
  ungroup()

cat("\nUndersampled Training Distribution:\n")
print(table(train_data[[target_col]]))

# 4. Preprocessing (MinMax Scaling) --------------------------------------
x_train_raw <- as.matrix(mutate(train_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
x_val_raw   <- as.matrix(mutate(val_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))

y_train <- to_categorical(train_data[[target_col]], num_classes = 8)
y_val   <- to_categorical(val_data[[target_col]],   num_classes = 8)

# Scale Features (Fit on Train, Apply to Train & Val)
scaler <- preProcess(x_train_raw, method = c("zv", "range"))

x_train <- predict(scaler, x_train_raw)
x_val   <- predict(scaler, x_val_raw)

# 5. Define Model (BN + ReLU + Dropout) ----------------------------------
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

# 6. Compile (SGD with Momentum) -----------------------------------------
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(
    learning_rate = FLAGS$learning_rate,
    momentum = FLAGS$momentum,
    nesterov = TRUE
  ),
  metrics = list("accuracy")
)

# 7. Train (Early Stopping) ----------------------------------------------
history <- model %>% fit(
  x_train, y_train,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_data = list(x_val, y_val),
  verbose = 0,
  callbacks = list(
    callback_early_stopping(
      monitor = "val_accuracy", 
      patience = FLAGS$patience, 
      restore_best_weights = TRUE,
      verbose = 1
    )
  )
)

# 8. Evaluation ----------------------------------------------------------
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
