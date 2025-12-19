library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(tensorflow)

# 1. Hyperparameter flags (Pure SGD setup) -------------------------------
FLAGS <- flags(
  flag_integer("units1", 128),
  flag_integer("units2", 64),
  flag_numeric("dropout1", 0.0), # No dropout for a "clean" run
  flag_numeric("dropout2", 0.0),
  flag_numeric("learning_rate", 0.01), # SGD usually needs a higher LR than Adam
  flag_numeric("momentum", 0.9),       # Momentum helps SGD escape local minima
  flag_integer("batch_size", 64),
  flag_integer("epochs", 5000)         # Full 5000 epoch run
)

# 2. Load Data -----------------------------------------------------------
data <- read.csv(
  "/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned_v2.csv",
  stringsAsFactors = FALSE
)

target_col <- "target_class"

# 3. Stratified Split Data ----------------------------------------------
set.seed(42)
y_factor <- factor(data[[target_col]])

test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data <- data[test_idx, ]
remain_data <- data[-test_idx, ]

val_p <- 0.15 / 0.85
val_idx <- createDataPartition(factor(remain_data[[target_col]]), p = val_p, list = FALSE)

val_data   <- remain_data[val_idx, ]
train_data <- remain_data[-val_idx, ]

# 4. Prepare Matrices & Preprocessing ------------------------------------
x_train_raw <- as.matrix(mutate(train_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))
x_val_raw   <- as.matrix(mutate(val_data %>% select(-all_of(target_col)), across(everything(), as.numeric)))

storage.mode(x_train_raw) <- "double"
storage.mode(x_val_raw)   <- "double"

y_train <- to_categorical(train_data[[target_col]], num_classes = 7)
y_val   <- to_categorical(val_data[[target_col]],   num_classes = 7)

continuous_cols <- colnames(x_train_raw)[apply(x_train_raw, 2, function(x) !all(x %in% c(0, 1)))]
scaler <- preProcess(x_train_raw[, continuous_cols, drop = FALSE], method = c("zv", "center", "scale"))

x_train <- x_train_raw
x_val   <- x_val_raw

x_train[, continuous_cols] <- predict(scaler, x_train_raw[, continuous_cols, drop = FALSE])
x_val[, continuous_cols]   <- predict(scaler, x_val_raw[, continuous_cols, drop = FALSE])

# 5. Define Model --------------------------------------------------------
model <- keras_model_sequential(input_shape = c(ncol(x_train))) %>%
  layer_dense(units = FLAGS$units1, activation = "relu") %>%
  layer_dense(units = FLAGS$units2, activation = "relu") %>%
  layer_dense(units = 7, activation = "softmax")

# 6. Compile & Train (Using Basic SGD) -----------------------------------
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(
    learning_rate = FLAGS$learning_rate,
    momentum = FLAGS$momentum
  ),
  metrics = list("accuracy")
)

# Fit without weights or early stopping
history <- model %>% fit(
  x_train, y_train,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_data = list(x_val, y_val),
  verbose = 2
)

# 7. Final Evaluation ----------------------------------------------------
val_probs <- model %>% predict(x_val, verbose = 0)
val_preds <- apply(val_probs, 1, which.max) - 1
val_true  <- apply(y_val, 1, which.max) - 1

cm <- confusionMatrix(factor(val_preds, levels = 0:6), factor(val_true, levels = 0:6), mode = "everything")

print(cm)

list(
  val_accuracy = cm$overall["Accuracy"],
  val_f1       = mean(cm$byClass[, "F1"], na.rm = TRUE),
  val_bal_acc  = mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)
)
