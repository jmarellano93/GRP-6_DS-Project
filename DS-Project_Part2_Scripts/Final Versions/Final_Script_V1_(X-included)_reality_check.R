# -------------------------------------------------------------------------
# 1. Setup and Libraries
# -------------------------------------------------------------------------
if (!require("keras")) install.packages("keras")
if (!require("tensorflow")) install.packages("tensorflow")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret") 
if (!require("pROC")) install.packages("pROC")         # Added for AUC/ROC
if (!require("MLmetrics")) install.packages("MLmetrics") # Added for LogLoss
if (!require("vip")) install.packages("vip")             # Added for Importance Audit
if (!require("gridExtra")) install.packages("gridExtra") # Added for Side-by-Side Plots (Section 9)
if (!require("here")) install.packages("here")           # Added for path management

library(keras)
library(tidyverse)
library(caret)
library(pROC)
library(MLmetrics)
library(vip)
library(gridExtra)
library(here)

# -------------------------------------------------------------------------
# 2. Define File Paths
# -------------------------------------------------------------------------
# Base Output Directory for saving PDFs and results
output_dir <- "C:/Users/John Arellano/RstudioProjects/GRP-6_DS-Project/DS-Project_Part2_Scripts/Saved_Outputs"

# Model Path
model_path <- file.path(output_dir, "final_model.keras")

# Processed Data Paths (Baked/Cleaned Data)
train_data_path <- file.path(output_dir, "final_processed_train_data.csv")
test_data_path  <- file.path(output_dir, "final_processed_test_data.csv")

# -------------------------------------------------------------------------
# 3. Load Model and Data
# -------------------------------------------------------------------------
message("Loading model from: ", model_path)
model <- load_model(model_path)

message("Loading processed test data from: ", test_data_path)
test_data <- read.csv(test_data_path)

# CRITICAL: We must load training data for Section 9.6 (Feature Audit)
message("Loading processed training data from: ", train_data_path)
train_baked <- read.csv(train_data_path) 

# -------------------------------------------------------------------------
# 4. Prepare Data for Evaluation (Matrix Conversion)
# -------------------------------------------------------------------------
target_col <- "target_class" 

# --- PREPARE TEST SET (x_test, y_test) ---
if (target_col %in% colnames(test_data)) {
  y_test <- test_data[[target_col]] # The Eval block expects 'y_test'
  
  # Remove Target AND 'ID' column 
  x_test_df <- test_data %>% select(-all_of(target_col), -any_of("ID"))
} else {
  stop("Target column not found in Test Data. Evaluation cannot run without ground truth.")
}

# Convert to Matrix (Keras requirement)
x_test <- data.matrix(x_test_df)

# --- SAFETY CHECK ---
input_shape <- model$input_shape[[2]] 
current_shape <- ncol(x_test)
if (!is.null(input_shape) && input_shape != current_shape) {
  warning(paste("SHAPE MISMATCH: Model expects", input_shape, 
                "features, but dataset has", current_shape))
}

# ------------------------------------------------------------------------------
# SECTION 5: COMPLETE EVALUATION & VISUALIZATION SUITE
# ------------------------------------------------------------------------------

# SCRIPT ARCHITECTURE SUMMARY: SECTION 9
#
# 5.1: GLOBAL DEFINITIONS & PREDICTION SETUP
# Reason: The model outputs raw probabilities for 8 arbitrary classes (0-7). 
#         To make this useful for business, we must map these numbers back to 
#         human-readable statuses (e.g., "Status_3_90-119d") and define exactly 
#         what constitutes "Bad Debt".
# Function: 
#         1. Creates a 'Class Map' acting as the central dictionary for translation.
#         2. Aggregates Risk: Sums the probabilities of the three worst classes 
#            (indices 5, 6, 7) to create a single "Probability of Default" (PD) score.
#         3. Generates the final predictions on the Test set.
#
# 5.2: REPORTING SETUP
# Reason: Professional evaluation requires a consolidated document, not just 
#         console output.
# Function: Initializes a PDF graphics device to capture all subsequent charts 
#         (Confusion Matrix, ROC, Lift, History) into a single "model_evaluation_report.pdf".
#
# 5.3: FUNDAMENTAL METRICS (CONFUSION MATRIX & CLASS PERFORMANCE)
# Reason: Checks the general health of the classifier and identifies specific 
#         weaknesses. Global accuracy hides per-class failures (e.g., ignoring 
#         minority classes).
# Function: 
#         - Heatmap: Visualizes the Confusion Matrix (Darker diagonal = Better).
#         - Class-Specific Bar Chart: Explicitly plots F1, Precision, and Recall 
#           for *each* status code side-by-side. This instantly reveals if the 
#           model is sacrificing specific classes (like Status 1 or X) to boost 
#           overall accuracy.
#
# 5.4: PROBABILITY, CALIBRATION & MULTI-CLASS ROC
# Reason: Accuracy tells us *what* the model predicted, but Calibration tells us 
#         how *confident* we should be. ROC curves show the trade-off between 
#         sensitivity and false alarms for every single class.
# Function: 
#         - Log Loss: Penalizes confident wrong answers.
#         - One-vs-Rest ROC Curves: Plots 8 distinct curves (one per status) on 
#           a single chart. This allows us to compare how well the model separates 
#           "Status 0" vs "Everyone Else", "Status X" vs "Everyone Else", etc.
#         - Reliability Diagram: Plots "Predicted Probability" vs "Actual Default Rate". 
#           (Ideally, a diagonal line: if the model says 20% risk, 20% of those people should default).
#
# 5.5: CREDIT RISK SPECIFICS (GINI & KS)
# Reason: Banks care less about "Accuracy" and more about "Discrimination" 
#         (ranking bad customers higher than good ones).
# Function: 
#         - Binary Transformation: Temporarily treats the problem as "Good vs Bad" 
#           (ignoring the granular delays) to calculate standard banking metrics.
#         - Gini Coefficient: The industry standard for scorecard performance.
#         - KS Statistic: Measures the maximum separation between Good and Bad distributions.
#         - Lift/Gain Charts: Visualizes how much better the model is compared to random guessing.
#
# 5.6: AUDIT & HISTORY (INTERPRETABILITY)
# Reason: We need to verify the model didn't "memorize" the training data or rely 
#         on cheating features.
# Function: 
#         - Training History: Plots Loss/Accuracy over epochs to check for overfitting 
#           (divergence between Train and Val lines).
#         - Permutation Importance (VIP): Shuffles each feature column one by one 
#           to measure how much the model relies on it. If shuffling "Income" 
#           doesn't hurt accuracy, the model isn't using Income.
#
# 5.7: EXPORT & SAVE
# Reason: Traceability.
# Function: 
#         - Saves the full table of predictions (Classes + Risk Scores) to CSV.
#         - Saves a "Metrics Summary" CSV containing the final Gini, Accuracy, 
#           and KS scores for easy comparison with future model versions.

# ==============================================================================
# 5.1: GLOBAL DEFINITIONS & PREDICTION SETUP
# ==============================================================================
cat("\n=== INITIALIZING FINAL EVALUATION ===\n")

# 1. Define Class Map (Central Source of Truth)
class_map <- c("0"="Status_C_Paid", 
               "1"="Status_0_1-29d", 
               "2"="Status_1_30-59d", 
               "3"="Status_2_60-89d",
               "4"="Status_3_90-119d", # START RISK
               "5"="Status_4_120-149d", 
               "6"="Status_5_Over150d", # END RISK
               "7"="Status_X_NoLoan")

# 2. Define Risk Indices (Status 3, 4, 5 correspond to indices 5, 6, 7 in the matrix cols)
# Note: Keras predict output columns are 1-based in R (1 to 8).
# Status 3 (Class 4) is col 5. Status 4 (Class 5) is col 6. Status 5 (Class 6) is col 7.
risk_indices <- 5:7 

# 3. Generate Predictions
pred_probs   <- model %>% predict(x_test)
colnames(pred_probs) <- levels(factor(names(class_map), levels = 0:7, labels = class_map))

# Get predicted class (0-based index to match y_test)
pred_classes <- apply(pred_probs, 1, which.max) - 1

actual_factor <- factor(y_test, levels = 0:7, labels = class_map)
pred_factor   <- factor(pred_classes, levels = 0:7, labels = class_map)

# 4. Calculate Risk Score (Sum of Risk Indices)
risk_probs   <- rowSums(pred_probs[, risk_indices])
binary_truth <- ifelse(y_test >= 4 & y_test <= 6, 1, 0) # 1 = Bad, 0 = Good/X

# ==============================================================================
# 5.2: REPORTING SETUP
# ==============================================================================
# UPDATED: Using 'output_dir' from Section 2 to ensure path consistency
eval_pdf_path <- file.path(output_dir, "reality-check_model_evaluation_report.pdf")

pdf(eval_pdf_path, width = 11, height = 8.5)
cat(sprintf(">> Report initiated: %s\n", eval_pdf_path))

# ==============================================================================
# 5.3: FUNDAMENTAL METRICS (Confusion Matrix & Accuracy)
# ==============================================================================
cat("\n>>> FUNDAMENTAL METRICS <<<\n")

cm <- confusionMatrix(pred_factor, actual_factor, mode = "everything")
mcc_score <- cm$overall['Kappa']

cat(sprintf("Overall Accuracy:    %.2f%%\n", cm$overall['Accuracy'] * 100))
cat(sprintf("Kappa (MCC Proxy):   %.4f\n", mcc_score))

# VISUALIZATION 1: Confusion Matrix Heatmap
cm_melt <- as.data.frame(cm$table)
p_cm <- ggplot(cm_melt, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
  labs(title = "Confusion Matrix Heatmap", 
       subtitle = "Darker squares = Higher density (Diagonal is Good)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_cm)

# VISUALIZATION 1b: Class-Specific Performance (matches PDF Page 2)
# Extracts per-class metrics to show which statuses are hardest to predict
cm_by_class <- as.data.frame(cm$byClass) %>%
  tibble::rownames_to_column("Class_Name") %>%
  select(Class_Name, Sensitivity, Specificity, Precision, F1) %>%
  tidyr::pivot_longer(cols = -Class_Name, names_to = "Metric", values_to = "Score")

p_class_perf <- ggplot(cm_by_class, aes(x = Class_Name, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip() +
  labs(title = "Class-Specific Performance Metrics",
       subtitle = "Comparison of F1, Precision, and Recall across all statuses") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
print(p_class_perf)

# ==============================================================================
# 5.4: PROBABILITY & CALIBRATION METRICS
# ==============================================================================
cat("\n>>> PROBABILITY METRICS <<<\n")

# A. Log Loss
log_loss_val <- MLmetrics::MultiLogLoss(y_true = model.matrix(~ actual_factor - 1), 
                                        y_pred = pred_probs)
cat(sprintf("Categorical Log Loss: %.4f (Lower is better)\n", log_loss_val))

# B. Multi-Class AUC
roc_multi <- pROC::multiclass.roc(actual_factor, pred_probs)
cat(sprintf("Multi-Class AUC-ROC:  %.4f\n", pROC::auc(roc_multi)))

# VISUALIZATION 2b: One-vs-Rest ROC Curves (matches PDF Page 4)
# Loops through every class to generate 8 distinct ROC curves
roc_data_list <- list()
for(cls_idx in 0:7) {
  # Create binary target for current class vs all others
  curr_binary_truth <- as.numeric(y_test == cls_idx)
  # Get probability for current class (Index is cls_idx + 1)
  curr_prob <- pred_probs[, cls_idx + 1]
  
  # Calculate ROC
  r <- roc(curr_binary_truth, curr_prob, quiet = TRUE)
  
  # Store coordinates
  roc_data_list[[cls_idx + 1]] <- data.frame(
    FPR = 1 - r$specificities,
    TPR = r$sensitivities,
    Class = class_map[as.character(cls_idx)]
  )
}
roc_plot_data <- bind_rows(roc_data_list)

p_multi_roc <- ggplot(roc_plot_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves (One-vs-Rest)", 
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  coord_fixed()
print(p_multi_roc)

# C. Calibration Plot
cal_data <- data.frame(prob = risk_probs, truth = binary_truth) %>%
  mutate(bin = ntile(prob, 10)) %>%
  group_by(bin) %>%
  summarise(mean_prob = mean(prob), actual_rate = mean(truth))

p_cal <- ggplot(cal_data, aes(x = mean_prob, y = actual_rate)) +
  geom_point(size = 3) + geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Calibration Plot (Reliability Diagram)",
       x = "Predicted Risk Probability", y = "Actual Default Rate") +
  theme_minimal()
print(p_cal)

# ==============================================================================
# 5.5: CREDIT RISK SPECIFICS (Binary Metrics)
# ==============================================================================
cat("\n>>> CREDIT RISK METRICS (Binary: 90+ DPD vs Rest) <<<\n")

# A. Binary AUC & Gini
roc_risk <- roc(binary_truth, risk_probs, quiet=TRUE)
auc_risk <- as.numeric(auc(roc_risk))
gini_coeff <- 2 * auc_risk - 1 # Calculated directly (No redundant function call)

cat(sprintf("Binary AUC (Risk):       %.4f\n", auc_risk))
cat(sprintf("Gini Coefficient:        %.4f\n", gini_coeff))

# B. KS Statistic
ks_stat <- max(roc_risk$sensitivities + roc_risk$specificities - 1)
cat(sprintf("KS Statistic:            %.4f\n", ks_stat))

# VISUALIZATION 3: Lift & Gain (Combined Grid)
lift_obj <- caret::lift(factor(binary_truth, labels=c("Good","Bad")) ~ risk_probs)
p_gain <- ggplot(lift_obj, plot = "gain") + labs(title = "Cumulative Gain") + theme_minimal()
p_lift <- ggplot(lift_obj, plot = "lift") + labs(title = "Lift Chart") + theme_minimal()
grid.arrange(p_gain, p_lift, ncol = 2, top = "Credit Risk Discrimination Power")

# ==============================================================================
# 5.6: AUDIT & HISTORY
# ==============================================================================
# A. Training History
if(exists("history")) {
  hist_df <- as.data.frame(history) %>% filter(!is.na(value))
  p_hist <- ggplot(hist_df, aes(x = epoch, y = value, color = data)) +
    geom_line() + facet_wrap(~metric, scales = "free_y") +
    labs(title = "Training History (Overfitting Check)") + theme_minimal()
  print(p_hist)
}

# B. VIP Audit (Using Pre-Defined Logic)
cat(">> Running Permutation Importance Audit...\n")
pred_wrapper_optimized <- function(object, newdata) {
  probs <- predict(object, as.matrix(newdata))
  # REUSE: Using the global 'risk_indices' defined in 9.1
  return(rowSums(probs[, 5:7])) 
}

# Ensure no ID or Target columns in audit data
if("ID" %in% names(train_baked)) {
  audit_data <- train_baked %>% sample_n(min(2000, nrow(train_baked))) %>% select(-target_class, -ID)
} else {
  audit_data <- train_baked %>% sample_n(min(2000, nrow(train_baked))) %>% select(-target_class)
}

vip_obj <- vip(
  object = model, method = "permute", train = audit_data,
  target = as.numeric(train_baked$target_class[1:nrow(audit_data)]), 
  metric = "rmse", pred_wrapper = pred_wrapper_optimized, nsim = 5
)
print(vip_obj + ggtitle("Feature Importance Audit"))

# ==============================================================================
# 5.7: EXPORT & SAVE
# ==============================================================================
dev.off()
cat(sprintf(">> Report Saved: %s\n", eval_pdf_path))

# 1. Save Predictions
results_df <- data.frame(
  actual_class = y_test,
  predicted_class = pred_classes,
  risk_score_90plus = risk_probs,
  predicted_label = as.character(pred_factor)
)

# UPDATED: Using 'output_dir'
results_path <- file.path(output_dir, "reality-check_comprehensive_test_predictions.csv")
write.csv(results_df, results_path, row.names = FALSE)

# 2. Save Metrics Summary
metrics_summary <- data.frame(
  Timestamp = Sys.time(),
  Accuracy = as.numeric(cm$overall['Accuracy']),
  Kappa = mcc_score,
  Log_Loss = log_loss_val,
  AUC_Risk = auc_risk,
  Gini = gini_coeff,
  KS = ks_stat
)

# UPDATED: Using 'output_dir'
metrics_path <- file.path(output_dir, "reality-check_final_model_metrics.csv")
write.csv(metrics_summary, metrics_path, row.names = FALSE)

cat("=== EVALUATION COMPLETE ===\n")