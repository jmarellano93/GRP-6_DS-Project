library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(tensorflow)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  flag_integer("units1", 128),
  flag_integer("units2", 64),
  flag_integer("units3", 0),
  flag_numeric("dropout1", 0.0),
  flag_numeric("dropout2", 0.0),
  flag_numeric("dropout3", 0.0),
  flag_numeric("weight_decay", 0.05),
  flag_numeric("learning_rate", 0.0001),
  flag_integer("batch_size", 64),
  flag_integer("epochs", 5000)
)

# 2. Load Data -----------------------------------------------------------
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned_v2.csv")
target_col <- "target_class"

# 3. Stratified Split Data (Original Distribution) -----------------------
set.seed(42)
y_factor <- as.factor(data[[target_col]])

# Create Test Set (15%)
test_idx <- createDataPartition(y_factor, p = 0.15, list = FALSE)
test_data <- data[test_idx, ]
remain_data <- data[-test_idx, ]

# Create Validation Set (15% of total)
val_p <- 0.15 / 0.85
val_idx <- createDataPartition(as.factor(remain_data[[target_col]]), p = val_p, list = FALSE)
val_data <- remain_data[val_idx, ]
train_data_raw <- remain_data[-val_idx, ]

# --- 3.5 UNDERSAMPLING THE MAJORITY ---
# We ONLY undersample the training data. Validation and Test must remain realistic.
cat("Original Training Distribution:\n")
print(table(train_data_raw[[target_col]]))

# Strategy: Keep all minority samples, but cap the majority (Class 1 and 0)
# Adjust these numbers to control how aggressive the undersampling is
max_samples_per_class <- 1000 

train_data <- train_data_raw %>%
  group_by(!!sym(target_col)) %>%
  group_modify(~ {
    if (nrow(.x) > max_samples_per_class) {
      .x[sample(1:nrow(.x), max_samples_per_class), ]
    } else {
      .x
    }
  }) %>%
  ungroup()

cat("\nUndersampled Training Distribution:\n")
print(table(train_data[[target_col]]))

# Convert to matrices for Keras
x_train_raw <- as.matrix(train_data %>% select(-all_of(target_col)))
y_train     <- to_categorical(train_data[[target_col]], num_classes = 7)

x_val_raw   <- as.matrix(val_data %>% select(-all_of(target_col)))
y_val       <- to_categorical(val_data[[target_col]], num_classes = 7)

x_test_raw  <- as.matrix(test_data %>% select(-all_of(target_col)))
y_test      <- to_categorical(test_data[[target_col]], num_classes = 7)

# 4. Preprocessing (Robust Scaling) --------------------------------------
continuous_cols <- colnames(x_train_raw)[apply(x_train_raw, 2, function(x) !all(x %in% c(0, 1)))]

scaler <- preProcess(x_train_raw[, continuous_cols], method = c("zv", "center", "scale"))

x_train <- x_train_raw
x_val   <- x_val_raw
x_test  <- x_test_raw

x_train[, continuous_cols] <- predict(scaler, x_train_raw[, continuous_cols])
x_val[, continuous_cols]   <- predict(scaler, x_val_raw[, continuous_cols])
x_test[, continuous_cols]  <- predict(scaler, x_test_raw[, continuous_cols])

# 5. Define Model --------------------------------------------------------
model <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$units1, activation = "relu", input_shape = c(ncol(x_train))) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$units2, activation = "relu") %>%
  layer_dropout(rate = FLAGS$dropout2)

if (FLAGS$units3 > 0) {
  model <- model %>%
    layer_dense(units = FLAGS$units3, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout3) 
}

model <- model %>% layer_dense(units = 7, activation = 'softmax')

# 6. Compile & Train (NO CLASS WEIGHTS) ----------------------------------
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = tensorflow::tf$keras$optimizers$AdamW(
    learning_rate = FLAGS$learning_rate, 
    weight_decay = FLAGS$weight_decay
  ),
  metrics = c('accuracy')
)

# --- DIAGNOSTIC CALLBACK ---
callback_weightnorm_f1 <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 100 == 0) {
      val_probs <- model %>% predict(x_val, verbose = 0)
      val_preds <- apply(val_probs, 1, which.max) - 1
      val_true  <- apply(y_val, 1, which.max) - 1
      
      cm <- confusionMatrix(factor(val_preds, levels = 0:6), factor(val_true, levels = 0:6), mode = "everything")
      cat(sprintf("\nEPOCH %d | Macro F1: %.4f | Bal Acc: %.4f\n", epoch + 1, mean(cm$byClass[, "F1"], na.rm = TRUE), mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)))
    }
  }
)

history <- model %>% fit(
  x_train, y_train,
  epochs = FLAGS$epochs,          
  batch_size = FLAGS$batch_size,        
  validation_data = list(x_val, y_val),
  callbacks = list(callback_weightnorm_f1) # Class Weights removed here
)

# 7. Final Report --------------------------------------------------------
val_probs <- model %>% predict(x_val, verbose = 0)
val_preds <- apply(val_probs, 1, which.max) - 1
val_true  <- apply(y_val, 1, which.max) - 1
cm <- confusionMatrix(factor(val_preds, levels = 0:6), factor(val_true, levels = 0:6), mode = "everything")

list(
  val_f1 = mean(cm$byClass[, "F1"], na.rm = TRUE),
  val_bal_acc = mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)
)