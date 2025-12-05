library(data.table)
library(xgboost)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)

# ============================================================
# 1. LOAD DATA
# ============================================================
df <- fread("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/cleaned_data.csv")

# ----- Set target variable -----
target <- "int_rate"   # <-- CHANGE if needed

# Keep only numeric (xgboost requires matrix input)
df <- df %>% mutate(across(where(is.logical), as.numeric))
df_num <- df %>% select(where(is.numeric))

# ============================================================
# 2. TRAIN / VALIDATION / TEST SPLIT 70 / 15 / 15
# ============================================================
set.seed(123)

n <- nrow(df_num)
idx <- sample(1:n)

train_idx <- idx[1:floor(0.70*n)]
val_idx   <- idx[(floor(0.70*n)+1):floor(0.85*n)]
test_idx  <- idx[(floor(0.85*n)+1):n]

train <- df_num[train_idx, ]
val   <- df_num[val_idx, ]
test  <- df_num[test_idx, ]

# Create matrices for XGBoost
train_x <- as.matrix(train %>% select(-all_of(target)))
train_y <- train[[target]]

val_x <- as.matrix(val %>% select(-all_of(target)))
val_y <- val[[target]]

test_x <- as.matrix(test %>% select(-all_of(target)))
test_y <- test[[target]]

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dval   <- xgb.DMatrix(data = val_x,   label = val_y)

# ============================================================
# 3. HYPERPARAMETER GRID
# ============================================================
grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(4, 6, 8),
  subsample = c(0.7, 1),
  colsample_bytree = c(0.7, 1)
)

results <- list()

# ============================================================
# 4. TRAIN MODELS + COMPUTE RMSE AND ACCURACY
# ============================================================
for (i in 1:nrow(grid)) {
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    watchlist = list(train = dtrain, val = dval),
    verbose = 0,
    early_stopping_rounds = 25
  )
  
  # TRAIN predictions
  pred_train <- predict(model, train_x)
  rmse_train <- RMSE(pred_train, train_y)
  
  # VALIDATION predictions
  pred_val <- predict(model, val_x)
  rmse_val <- RMSE(pred_val, val_y)
  
  # "Accuracy" as % of predictions within ±1% absolute error of true value
  tol <- 0.01 * mean(val_y)
  acc_val <- mean(abs(pred_val - val_y) < tol)
  
  results[[i]] <- data.frame(
    model_id = i,
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i],
    rmse_train = rmse_train,
    rmse_val = rmse_val,
    accuracy_val = acc_val
  )
}

results_df <- do.call(rbind, results)

# ============================================================
# 5. WINNING MODEL
# ============================================================
best_model_row <- results_df[which.min(results_df$rmse_val), ]
best_model_row





# ============================================================
# 6. PLOT TRAIN & VALIDATION RMSE
# ============================================================
library(ggplot2)
library(reshape2)

hyperparams <- c("eta", "max_depth", "subsample", "colsample_bytree")

for (hp in hyperparams) {
  
  df_plot <- melt(
    results_df[, c(hp, "rmse_train", "rmse_val")],
    id.vars = hp,
    variable.name = "type",
    value.name = "rmse"
  )
  
  print(
    ggplot(df_plot, aes_string(x = hp, y = "rmse", color = "type", group = "type")) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = paste("RMSE vs", hp),
        x = hp,
        y = "RMSE",
        color = "Metric"
      )
  )
}



# ============================================================
# 7. TRAIN FINAL MODEL USING BEST HYPERPARAMETERS
# ============================================================
best_params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = best_model_row$eta,
  max_depth = best_model_row$max_depth,
  subsample = best_model_row$subsample,
  colsample_bytree = best_model_row$colsample_bytree
)

final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, val = dval),
  verbose = 0,
  early_stopping_rounds = 25
)

# TEST SET PERFORMANCE
pred_test <- predict(final_model, test_x)
test_rmse <- RMSE(pred_test, test_y)

cat("\nFinal Test RMSE:", test_rmse, "\n")