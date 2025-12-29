# ==============================================================================
# Assignment 2 MLP with Imbalance Handling
# - Adds SMOTE oversampling on training folds only (UBL::smoteClassif)
# - Adds per-fold class weights in Keras fit()
# - Uses a slightly smaller, more regularized MLP
# - Tracks Macro F1 and Balanced Accuracy as key metrics
# ==============================================================================

library(keras3)
library(caret)
library(dplyr)
library(tfruns)
library(UBL)        # for smoteClassif
library(magrittr)

# 1. Hyperparameter flags ------------------------------------------------
FLAGS <- flags(
  # Regularization
  flag_numeric("dropout1", 0), # recommend 0.3
  flag_numeric("dropout2", 0),
  flag_numeric("dropout3", 0), # For optional 3rd layer
  
  # Architecture (slightly smaller than before)
  flag_integer("units1", 128),
  flag_integer("units2", 64),
  flag_integer("units3", 32),     # 0 = off, >0 = on
  
  # Optimization
  flag_numeric("learning_rate", 0.001),
  flag_integer("batch_size", 128),
  flag_integer("epochs", 100)
)

# 2. Load & Prepare Data -------------------------------------------------
data <- read.csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/Assignment_2_cleaned.csv")
target_col <- "target_class"   # integer 0..7

x_all <- data %>% select(-all_of(target_col)) %>% as.data.frame()
y_all_vec <- data[[target_col]]   # integer vector 0..7
y_all_cat <- to_categorical(y_all_vec, num_classes = 8)

# 3. Split Data ----------------------------------------------------------
set.seed(42)

n_rows <- nrow(x_all)
test_idx <- sample(1:n_rows, size = floor(0.15 * n_rows))

x_test_holdout_raw <- x_all[test_idx, , drop = FALSE]
y_test_holdout     <- y_all_cat[test_idx, ]

x_cv_raw <- x_all[-test_idx, , drop = FALSE]
y_cv_vec <- y_all_vec[-test_idx]        # keep as vector for SMOTE & weights
y_cv     <- y_all_cat[-test_idx, ]

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

cat("Starting", k, "-Fold CV with SMOTE + class weights...\n")

# 5. Training Loop -------------------------------------------------------
for (i in 1:k) {
  cat("\n--- Processing Fold #", i, "---\n")
  
  # a. Split indices
  val_indices <- which(folds == i, arr.ind = TRUE)
  train_indices <- setdiff(seq_len(nrow(x_cv_raw)), val_indices)
  
  x_fold_train_raw <- x_cv_raw[train_indices, , drop = FALSE]
  y_fold_train_vec <- y_cv_vec[train_indices]
  
  x_fold_val_raw <- x_cv_raw[val_indices, , drop = FALSE]
  y_fold_val     <- y_cv[val_indices, , drop = FALSE]
  y_fold_val_vec <- y_cv_vec[val_indices]
  
  # b. SMOTE on training fold only (multiclass)
  #    target must be a factor for UBL::smoteClassif
  train_df_for_smote <- x_fold_train_raw
  train_df_for_smote$target <- factor(y_fold_train_vec, levels = 0:7)
  
  # Adjust C.perc as needed (here: over-sample all minority classes to ~50% of majority)
  smote_res <- smoteClassif(
    target ~ .,
    data = train_df_for_smote,
    C.perc = "balance",   # let UBL balance automatically; can specify list for more control
    k = 5
  )
  
  x_fold_train_smote <- smote_res %>% select(-target)
  y_fold_train_smote_vec <- as.integer(as.character(smote_res$target))  # back to 0..7
  
  # c. Robust Scaling (fit on SMOTE'd training only)
  fold_scaler <- preProcess(x_fold_train_smote, method = c("zv", "center", "scale"))
  x_fold_train <- predict(fold_scaler, x_fold_train_smote) %>% as.matrix()
  x_fold_val   <- predict(fold_scaler, x_fold_val_raw) %>% as.matrix()
  
  # d. One-hot encode training labels after SMOTE
  y_fold_train <- to_categorical(y_fold_train_smote_vec, num_classes = 8)
  
  # e. Compute class weights from SMOTE'd training labels
  class_counts <- table(y_fold_train_smote_vec)
  total <- sum(class_counts)
  n_classes <- length(class_counts)
  class_weights_vec <- total / (n_classes * as.numeric(class_counts))
  names(class_weights_vec) <- names(class_counts)
  
  # Keras expects a *named list* with character keys "0", "1", ...
  class_weight_list <- as.list(class_weights_vec)
  
  # f. Define Model (Variable Depth)
  model <- keras_model_sequential() %>%
    layer_dense(
      units = FLAGS$units1,
      activation = "relu",
      input_shape = c(ncol(x_fold_train))
    ) %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = FLAGS$units2, activation = "relu") %>%
    layer_dropout(rate = FLAGS$dropout2)
  
  if (FLAGS$units3 > 0) {
    model <- model %>%
      layer_dense(units = FLAGS$units3, activation = "relu") %>%
      layer_dropout(rate = FLAGS$dropout3)
  }
  
  model <- model %>%
    layer_dense(units = 8, activation = "softmax")
  
  # g. Compile (still categorical_crossentropy, now with class weights)
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = FLAGS$learning_rate),
    metrics = c("accuracy")
  )
  
  # h. Train with class_weight
  history <- model %>% fit(
    x_fold_train, y_fold_train,
    batch_size = FLAGS$batch_size,
    epochs = FLAGS$epochs,
    verbose = 1,
    validation_data = list(x_fold_val, y_fold_val),
    class_weight = class_weight_list
  )
  
  # i. Best epoch by validation *loss* (safer under imbalance)
  best_epoch_idx <- which.min(history$metrics$val_loss)
  best_val_acc   <- history$metrics$val_accuracy[best_epoch_idx]
  best_train_acc <- history$metrics$accuracy[best_epoch_idx]
  final_val_loss <- history$metrics$val_loss[best_epoch_idx]
  final_train_loss <- history$metrics$loss[best_epoch_idx]
  
  # j. Imbalance-aware metrics
  val_probs <- model %>% predict(x_fold_val, verbose = 0)
  val_preds <- apply(val_probs, 1, which.max) - 1
  val_true  <- y_fold_val_vec
  
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
cat("   ARCHITECTURE + IMBALANCE HANDLING   \n")
cat("========================================\n")

avg_train_acc <- mean(results$Train_Acc)
avg_val_acc   <- mean(results$Val_Acc)
avg_macro_f1  <- mean(results$Val_Macro_F1)
avg_bal_acc   <- mean(results$Val_Bal_Acc)
gap           <- avg_train_acc - avg_val_acc

cat("Avg Train Accuracy:   ", round(avg_train_acc, 4), "\n")
cat("Avg Val Accuracy:     ", round(avg_val_acc, 4), "\n")
cat("Avg Macro F1 (KEY):   ", round(avg_macro_f1, 4), "\n")
cat("Avg Balanced Acc:     ", round(avg_bal_acc, 4), "\n")
cat("Overfitting Gap:      ", round(gap, 4), "\n")
cat("----------------------------------------\n")

if (avg_macro_f1 < 0.5) {
  cat("STATUS: Macro F1 still low. Consider tuning SMOTE (C.perc) and class weights.\n")
} else if (gap > 0.05) {
  cat("STATUS: Good F1 but overfitting. Increase dropout or reduce units.\n")
} else {
  cat("STATUS: Good candidate configuration for imbalanced pay-back behavior.\n")
}



# ==============================================================================
# IMBALANCED MULTI-CLASS CREDIT BEHAVIOR – DESIGN RATIONALE & REFERENCES
#
# Goal:
#   Predict multi-class pay-back behavior (status 0–5, C, X) on an imbalanced
#   credit dataset. We explicitly do NOT learn the accept/reject decision here;
#   that is a separate downstream process. The focus is on modeling behavior,
#   not making lending decisions.
#
# Why SMOTE oversampling on training folds only?
#   - Credit-risk and credit-rating studies consistently report that minority
#     classes (defaults, severe delinquency) are underrepresented and that
#     naive training of neural networks leads to poor recall/F1 for these
#     classes, even when overall accuracy appears high.
#   - To address this, many works apply SMOTE or SMOTE variants (e.g.
#     SMOTE-ENN, k-means-SMOTE) to the TRAINING data only, increasing the
#     number of rare-class examples while keeping the validation/test
#     distributions untouched. This improves minority-class learning without
#     contaminating evaluation.
#   - Our code mirrors this: for each CV fold, SMOTE is applied only to the
#     training fold, then scaling and model fitting are done on this
#     oversampled data, while the validation fold stays original.
#
#   Representative references:
#     - Deep / credit-risk papers using SMOTE + neural nets to improve
#       prediction on underrepresented default or rating classes in financial
#       data:
#       https://www.sciencedirect.com/science/article/pii/S2666827025000751
#       https://www.sciencedirect.com/science/article/abs/pii/S1568494621010176
#       https://arxiv.org/pdf/2408.03497.pdf
#       https://www.nature.com/articles/s41598-025-09173-x
#       https://www.sciencedirect.com/science/article/pii/S2667345224000087
#       https://onlinelibrary.wiley.com/doi/10.1002/for.70042
#     - R implementations of SMOTE for multiclass data (UBL::smoteClassif),
#       which follow the same principle of resampling training sets only:
#       https://www.rdocumentation.org/packages/UBL/versions/0.0.9
#       https://rdrr.io/cran/UBL/man/smoteClassif.html
#       https://search.r-project.org/CRAN/refmans/UBL/html/00Index.html
#
# Why add class weights in the neural network loss?
#   - Even with SMOTE, residual imbalance or differing misclassification costs
#     means that errors on severe delinquency classes are more critical than
#     errors on common “on-time / slight delay” classes.
#   - Cost-sensitive learning via class weighting is widely recommended for
#     imbalanced credit scoring and credit-risk models, including MLPs,
#     because it directly modifies the loss so the model pays more attention
#     to minority outcomes.
#   - For multiclass neural networks, examples show how to compute
#     inverse-frequency weights per class and pass them as class_weight to
#     the training routine, improving macro-F1 and minority recall.
#
#   Representative references:
#     - Studies on cost-sensitive / class-weighted deep learning for credit
#       risk and multi-class imbalanced problems:
#       https://www.sciencedirect.com/science/article/pii/S2666827025000751
#       https://www.sciencedirect.com/science/article/pii/S0377221723005088
#       https://dl.acm.org/doi/fullHtml/10.1145/3675888.3676052
#       https://pmc.ncbi.nlm.nih.gov/articles/PMC11065699/
#       https://www.sciencedirect.com/science/article/pii/S0925231225030334
#       https://onlinelibrary.wiley.com/doi/10.1002/jcaf.70020
#     - R/Keras examples demonstrating class_weight for multiclass imbalanced
#       classification:
#       https://www.geeksforgeeks.org/deep-learning/how-to-set-class-weight-in-keras-for-different-classification-using-r/
#       https://www.geeksforgeeks.org/deep-learning/how-to-set-classweight-in-keras-package-using-r/
#       https://machinelearningmastery.com/multi-class-imbalanced-classification/
#       https://github.com/keras-team/keras/issues/116
#
# Why a smaller, more regularized MLP?
#   - Many credit-rating / credit-risk MLP architectures in the literature
#     use 2–4 hidden layers with tens of neurons (e.g. 50–30–30–50 or
#     128–64–32) plus regularization (dropout, early stopping). Large,
#     overparameterized networks can overfit the majority pattern and still
#     perform poorly on minority classes.
#   - Reducing width (e.g. 128–64–32 instead of 256–128–64) and adding
#     meaningful dropout (0.3–0.5) encourages the model to learn more robust
#     decision boundaries across all classes, which empirical studies report
#     as beneficial for credit default / rating tasks under imbalance.
#
#   Representative references:
#     - MLP and deep credit-risk models reporting shallow-to-medium networks
#       with regularization as an effective choice for tabular financial
#       data:
#       https://www.sciencedirect.com/science/article/pii/S2666827025000751
#       https://www.ijsat.org/papers/2025/2/4759.pdf
#       https://ideas.repec.org/r/mse/cesdoc/18003.html
#       https://www.academia.edu/19833616/Investigation_of_Multilayer_Perceptron_and_Class_Imbalance_Problems_for_Credit_Rating
#       https://www.scitepress.org/Papers/2024/128181/128181.pdf
#       https://www.sciencedirect.com/science/article/pii/S2667345224000087
#
# Why Macro F1 and Balanced Accuracy as key metrics?
#   - Standard accuracy is dominated by the majority class in imbalanced
#     datasets and is explicitly criticized as misleading for credit scoring
#     and credit-risk assessment.
#   - Macro F1 (unweighted average over classes) and Balanced Accuracy
#     (average recall per class) are recommended to fairly evaluate
#     performance across all rating / behavior categories, especially the
#     rare and most critical ones.
#   - Many recent credit-risk and imbalanced learning papers report F1,
#     macro-F1, recall and balanced accuracy (or per-class metrics) as main
#     evaluation criteria, often alongside ROC AUC.
#
#   Representative references:
#     - Interpretable and deep ML for imbalanced credit data that emphasize
#       F1, macro-F1, and balanced accuracy for multi-class outcomes:
#       https://www.sciencedirect.com/science/article/pii/S2666827025000751
#       https://www.ijsat.org/papers/2025/2/4759.pdf
#       https://www.sciencedirect.com/science/article/pii/S0377221723005088
#       https://ideas.repec.org/r/mse/cesdoc/18003.html
#       https://www.academia.edu/19833616/Investigation_of_Multilayer_Perceptron_and_Class_Imbalance_Problems_for_Credit_Rating
#       https://machinelearningmastery.com/multi-class-imbalanced-classification/
#       https://dl.acm.org/doi/10.1145/3568199.3568204
#
# Connection to assignment statement:
#   - The assignment explicitly states that the task is to learn the
#     multi-class "pay-back behavior" (status 0–5, C, X) and NOT to decide
#     whether to accept or reject a customer.
#   - All changes above (SMOTE, class weighting, smaller MLP, macro-F1 /
#     balanced accuracy focus) are aimed purely at improving how well the
#     model recovers this multi-class BEHAVIOR distribution under imbalance.
#     They do not alter or pre-empt any acceptance/rejection policy, which
#     remains a separate downstream step by design.
#
#   Assignment dataset / task references:
#     - Original Kaggle credit-approval / behavior dataset description:
#       https://www.kaggle.com/datasets/rikdifos/credit-card-approval-prediction
#     - A more general credit-card user behavior / default dataset
#       illustrating multi-class behavior labels:
#       https://www.kaggle.com/datasets/aadarshvani/credit-card-dataset-comprehensive
#     - Recent paper on enhanced credit card approval / default prediction
#       with advanced ML methods (including imbalance-aware approaches):
#       https://journals.sagepub.com/doi/10.1177/24056456251356175
# ==============================================================================```

