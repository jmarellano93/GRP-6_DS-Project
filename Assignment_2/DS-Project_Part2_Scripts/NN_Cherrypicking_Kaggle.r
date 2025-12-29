library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(tensorflow)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  flag_integer("units1", 512),
  flag_integer("units2", 256),
  flag_integer("units3", 0),       # 0 means disabled
  flag_numeric("dropout1", 0), 
  flag_numeric("dropout2", 0),
  flag_numeric("dropout3", 0.0),   
  flag_numeric("learning_rate", 0.001), 
  flag_numeric("momentum", 0.9),       
  flag_integer("batch_size", 32),
  flag_integer("epochs", 1000),    # Max epochs (Early stopping will cut this short)
  flag_integer("patience", 200)    # NEW: How many epochs to wait for improvement
)

# 2. Load Data -----------------------------------------------------------
# (Assuming 'data' is loaded in your environment)
target_col <- "target_class"

# 3. Stratified Split ----------------------------------------------------
set.seed(42)
y_factor <- factor(data[[target_col]])

test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data <- data[test_idx, ]
remain_data <- data[-test_idx, ]

val_p <- 0.15 / 0.85
val_idx <- createDataPartition(factor(remain_data[[target_col]]), p = val_p, list = FALSE)

val_data   <- remain_data[val_idx, ]
train_data <- remain_data[-val_idx, ]

minority_boost <- df_not_found %>% 
  filter(target_class != "0")

print(paste("Original df_not_found size:", nrow(df_not_found)))
print(paste("Rows selected for boosting (Non-Zero Status):", nrow(minority_boost)))

# 2. Merge with your existing training data
#    Ensure 'train_set' is your original training dataframe before scaling/encoding
train_data <- bind_rows(train_data, minority_boost)
set.seed(42)
train_data <- train_data[sample(nrow(train_data)), ]

print(paste("New Augmented Training Set Size:", nrow(train_data)))

# Check the new distribution to verify '0' dominance is reduced
table(train_data$target_class)


# 4. Preprocessing -------------------------------------------------------
x_train_raw <- as.matrix(mutate(train_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
x_val_raw   <- as.matrix(mutate(val_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))

storage.mode(x_train_raw) <- "double"
storage.mode(x_val_raw)   <- "double"

y_train <- to_categorical(train_data[[target_col]], num_classes = 8)
y_val   <- to_categorical(val_data[[target_col]],   num_classes = 8)

continuous_cols <- colnames(x_train_raw)[apply(x_train_raw, 2, function(x) !all(x %in% c(0, 1)))]

scaler <- preProcess(x_train_raw[, continuous_cols, drop = FALSE], method = c("zv", "range"))

x_train <- x_train_raw
x_val   <- x_val_raw

x_train[, continuous_cols] <- predict(scaler, x_train_raw[, continuous_cols, drop = FALSE])
x_val[, continuous_cols]   <- predict(scaler, x_val_raw[, continuous_cols, drop = FALSE])

# 5. Define Model --------------------------------------------------------
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

# 6. Compile -------------------------------------------------------------
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(
    learning_rate = FLAGS$learning_rate,
    momentum = FLAGS$momentum,
    nesterov = TRUE
  ),
  metrics = list("accuracy")
)

# 7. Train with Early Stopping -------------------------------------------
history <- model %>% fit(
  x_train, y_train,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_data = list(x_val, y_val),
  verbose = 0,
  callbacks = list(
    # Stop if 'val_accuracy' doesn't improve for 'patience' epochs
    callback_early_stopping(
      monitor = "val_accuracy", 
      patience = FLAGS$patience, 
      restore_best_weights = TRUE, # IMPORTANT: Reverts model to the best epoch
      verbose = 1
    )
  )
)

# 8. Evaluation ----------------------------------------------------------
# Because we used restore_best_weights=TRUE, this evaluation uses the BEST model found
val_probs <- model %>% predict(x_val, verbose = 0)
val_preds <- apply(val_probs, 1, which.max) - 1
val_true  <- apply(y_val, 1, which.max) - 1

cm <- confusionMatrix(factor(val_preds, levels = 0:7), factor(val_true, levels = 0:7), mode = "everything")

print(cm)

list(
  val_accuracy = cm$overall["Accuracy"],
  val_f1       = mean(cm$byClass[, "F1"], na.rm = TRUE),
  val_bal_acc  = mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE),
  epochs_trained = length(history$metrics$loss) # Log how many epochs it actually took
)
