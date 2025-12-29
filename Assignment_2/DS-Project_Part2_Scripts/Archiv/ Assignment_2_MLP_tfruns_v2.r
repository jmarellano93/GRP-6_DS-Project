# ==============================================================================
# SCRIPT: Assignment_2_MLP_no_Fold.r
# PURPOSE: Engineering Grid Search for Best Validation F1 Score
# ==============================================================================

library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(tensorflow)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # --- Architecture ---
  flag_integer("units1", 512),
  flag_integer("units2", 256),
  flag_integer("units3", 0),    # Set to 0 for a 2-Layer model
  
  # --- Regularization ---
  flag_numeric("dropout1", 0.0),
  flag_numeric("dropout2", 0.0),
  flag_numeric("dropout3", 0.0),
  flag_numeric("weight_decay", 0.01), # Added this flag (Critical)
  
  # --- Optimization ---
  flag_numeric("learning_rate", 0.001),
  flag_integer("batch_size", 256),
  flag_integer("epochs", 100)  # Reduced for Grid Search (Speed)
)

# 2. Load & Prepare Data -------------------------------------------------
# Ensure this path is correct on your machine
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned_v3.csv")
target_col <- "target_class" 

x_all <- data %>% select(-all_of(target_col)) %>% as.matrix()
y_all <- to_categorical(data[[target_col]], num_classes = 7)
y_raw_factor <- as.factor(data[[target_col]])

# 3. Stratified Split Data (70% Train / 15% Val / 15% Test) --------------
set.seed(42)

# A. Test Holdout (15%)
test_idx <- createDataPartition(y_raw_factor, p = 0.15, list = FALSE)
x_test_raw <- x_all[test_idx, ]
y_test     <- y_all[test_idx, ]

# Remaining (85%)
x_remain <- x_all[-test_idx, ]
y_remain <- y_all[-test_idx, ]
y_remain_factor <- y_raw_factor[-test_idx]

# B. Validation Set (15% of total)
val_p <- 0.15 / 0.85 
val_idx <- createDataPartition(y_remain_factor, p = val_p, list = FALSE)

x_val_raw   <- x_remain[val_idx, ]
y_val       <- y_remain[val_idx, ]

x_train_raw <- x_remain[-val_idx, ]
y_train     <- y_remain[-val_idx, ]

# 4. Preprocessing (Robust Scaling) --------------------------------------
scaler <- preProcess(x_train_raw, method = c("zv", "center", "scale"))
x_train <- predict(scaler, x_train_raw)
x_val   <- predict(scaler, x_val_raw)

# 5. Define Model --------------------------------------------------------
model <- keras_model_sequential() %>%
  # Layer 1
  layer_dense(
    units = FLAGS$units1,
    activation = "relu",
    input_shape = c(ncol(x_train))
  ) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  
  # Layer 2
  layer_dense(
    units = FLAGS$units2, 
    activation = "relu"
  ) %>%
  layer_dropout(rate = FLAGS$dropout2)

# Optional: Layer 3 (Controlled by FLAGS$units3)
if (FLAGS$units3 > 0) {
  model <- model %>%
    layer_dense(
      units = FLAGS$units3, 
      activation = 'relu'
    ) %>%
    layer_dropout(rate = FLAGS$dropout3) 
}

# Output Layer
model <- model %>% 
  layer_dense(units = 7, activation = 'softmax')

# 6. Compile -------------------------------------------------------------
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = tensorflow::tf$keras$optimizers$AdamW(
    learning_rate = FLAGS$learning_rate, 
    weight_decay = FLAGS$weight_decay  # Now controlled by Flag
  ),
  metrics = c('accuracy')
)

# 7. Train ---------------------------------------------------------------
# Note: No custom callback needed for Grid Search (tfruns handles logging)
history <- model %>% fit(
  x_train, y_train,
  epochs = FLAGS$epochs,          
  batch_size = FLAGS$batch_size,        
  validation_data = list(x_val, y_val),
  verbose = 2 # Less output for grid search
)

# 8. Evaluation Metrics --------------------------------------------------
# Get best epoch stats
best_epoch_idx <- which.max(history$metrics$val_accuracy)
final_val_loss <- history$metrics$val_loss[best_epoch_idx]
best_val_acc   <- history$metrics$val_accuracy[best_epoch_idx]

# Calculate Macro F1
val_probs <- model %>% predict(x_val, verbose = 0)
val_preds <- apply(val_probs, 1, which.max) - 1
val_true  <- apply(y_val, 1, which.max) - 1

cm <- confusionMatrix(
  factor(val_preds, levels = 0:6),
  factor(val_true, levels = 0:6),
  mode = "everything"
)

macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
bal_acc  <- mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)

# 9. Return to tfruns ----------------------------------------------------
list(
  val_loss = final_val_loss,
  val_accuracy = best_val_acc,
  val_f1 = macro_f1,        # This is what we want to maximize
  val_bal_acc = bal_acc
)