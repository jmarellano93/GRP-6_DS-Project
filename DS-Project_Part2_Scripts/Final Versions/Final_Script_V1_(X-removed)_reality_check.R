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

library(keras)
library(tidyverse)
library(caret)
library(pROC)
library(MLmetrics)
library(vip)

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

# -------------------------------------------------------------------------
# SECTION 5: COMPLETE EVALUATION & VISUALIZATION SUITE
# -------------------------------------------------------------------------

# ==============================================================================
# 5.1: Prediction Setup
# ==============================================================================
message("\n=== INITIALIZING FINAL EVALUATION ===")

# Get Probability Matrix
pred_probs <- model %>% predict(x_test)

# Get Predicted Classes (Indices 0-6)
# Note: Keras returns 0-based indices if classes were 0-6. 
# If your model output is 1-based, adjust here. Assuming 0-6 based on previous context.
if (ncol(pred_probs) > 1) {
  pred_classes <- max.col(pred_probs) - 1 
} else {
  pred_classes <- ifelse(pred_probs > 0.5, 1, 0)
}

# Define Class Map (Strictly 0-6 per cleaning)
class_map <- c("0"="Status_C_Paid", 
               "1"="Status_0_1-29d", 
               "2"="Status_1_30-59d", 
               "3"="Status_2_60-89d",
               "4"="Status_3_90-119d", 
               "5"="Status_4_120-149d", 
               "6"="Status_5_Over150d")

# Create Factors
# Ensure levels match the columns of your model output
actual_factor <- factor(y_test, levels = 0:6, labels = class_map)
pred_factor   <- factor(pred_classes, levels = 0:6, labels = class_map)

# ==============================================================================
# 5.2: Reporting Setup (PDF)
# ==============================================================================
eval_pdf_path <- file.path(output_dir, "realitycheck_model_evaluation_report.pdf")
pdf(eval_pdf_path, width = 11, height = 8.5)
message(sprintf(">> Report initiated: %s", eval_pdf_path))

# ==============================================================================
# 5.3: Fundamental & Class-Specific Metrics
# ==============================================================================
message("\n>>> FUNDAMENTAL METRICS <<<")

# A. Confusion Matrix & Basic Stats
cm <- confusionMatrix(pred_factor, actual_factor, mode = "everything")

# B. Use Kappa as the MCC proxy
cat(sprintf("Overall Accuracy:    %.2f%%\n", cm$overall['Accuracy'] * 100))
cat(sprintf("Kappa Statistic:     %.4f\n", cm$overall['Kappa']))

# C. Class-Specific Metrics Table
class_stats <- cm$byClass %>%
  as.data.frame() %>%
  select(Sensitivity, Specificity, Precision, Recall, F1) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

# D. Macro Metrics (Balanced)
macro_f1 <- mean(class_stats$F1, na.rm = TRUE)
macro_recall <- mean(class_stats$Recall, na.rm = TRUE)
cat(sprintf("Balanced Acc (Macro Recall): %.4f\n", macro_recall))
cat(sprintf("Macro F1 Score:              %.4f\n", macro_f1))

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

# VISUALIZATION 2: Class-Specific Performance Bar Chart
class_stats %>%
  rownames_to_column("Class") %>%
  pivot_longer(cols = c(Sensitivity, Precision, F1), names_to = "Metric", values_to = "Score") %>%
  ggplot(aes(x = Class, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Class-Specific Performance Metrics", 
       subtitle = "Check Recall (Sensitivity) for Status 4, 5, 6") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") -> p_perf
print(p_perf)

# ==============================================================================
# 5.4: Probability & Calibration Metrics
# ==============================================================================
message("\n>>> PROBABILITY METRICS <<<")

# A. Log Loss (Categorical Crossentropy)
# Convert actuals to one-hot matrix for calculation
y_true_mat <- model.matrix(~ actual_factor - 1)
# Ensure dimensions match (sometimes model.matrix drops unused levels)
if(ncol(y_true_mat) == ncol(pred_probs)) {
  log_loss_val <- MLmetrics::MultiLogLoss(y_true = y_true_mat, y_pred = pred_probs)
  cat(sprintf("Categorical Log Loss: %.4f (Lower is better)\n", log_loss_val))
  
  # C. Brier Score
  brier_score <- mean(rowSums((y_true_mat - pred_probs)^2))
  cat(sprintf("Brier Score:          %.4f (Measures Calibration)\n", brier_score))
} else {
  message("Skipping LogLoss/Brier: Dimension mismatch in one-hot encoding.")
}

# B. Multi-Class AUC-ROC
# Name the columns to match factor levels for pROC
colnames(pred_probs) <- levels(actual_factor)

roc_multi <- pROC::multiclass.roc(actual_factor, pred_probs)
cat(sprintf("Multi-Class AUC-ROC:  %.4f\n", pROC::auc(roc_multi)))


# VISUALIZATION 3: Prediction Confidence Histogram
max_probs <- data.frame(Max_Prob = apply(pred_probs, 1, max),
                        Correct = (pred_classes == y_test))

p_conf <- ggplot(max_probs, aes(x = Max_Prob, fill = Correct)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Model Confidence Histogram",
       subtitle = "Peaks near 1.0 indicate high confidence.",
       x = "Max Predicted Probability", y = "Count") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal()
print(p_conf)

# VISUALIZATION 4: ROC Curves (One-vs-Rest)
roc_data <- data.frame()
for(i in 0:6) {
  # Create binary target for class i
  bin_y <- ifelse(y_test == i, 1, 0)
  
  # Calculate ROC for class i vs All Others
  # Ensure we select the column corresponding to class i
  # Since pred_probs has column names now, we can use indices or names
  r <- roc(bin_y, pred_probs[, i+1], quiet=TRUE)
  
  roc_data <- rbind(roc_data, data.frame(
    FPR = 1 - r$specificities,
    TPR = r$sensitivities,
    Class = class_map[as.character(i)],
    row.names = NULL 
  ))
}

p_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(linewidth = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves (One-vs-Rest)",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()
print(p_roc)

# ==============================================================================
# 5.5: Credit Risk Specific Metrics (Serious Delinquency)
# ==============================================================================
message("\n>>> CREDIT RISK METRICS (Binary: 90+ DPD vs Rest) <<<")

# Define "Risk" as the Sum of Probabilities of Classes 4, 5, and 6
# Columns 5, 6, 7 correspond to indices 4, 5, 6 (Status 3, 4, 5)
risk_probs <- rowSums(pred_probs[, 5:7]) 
binary_truth <- ifelse(y_test >= 4, 1, 0) # 1 = Bad, 0 = Good

# A. Binary AUC for Serious Delinquency
roc_risk <- roc(binary_truth, risk_probs, quiet=TRUE)
auc_risk <- auc(roc_risk)
cat(sprintf("Binary AUC (Serious Risk): %.4f\n", auc_risk))

# B. Gini Coefficient
gini_coeff <- 2 * auc_risk - 1
cat(sprintf("Gini Coefficient:          %.4f (Target > 0.40)\n", gini_coeff))

# C. KS Statistic
ks_stat <- max(roc_risk$sensitivities + roc_risk$specificities - 1)
cat(sprintf("KS Statistic:              %.4f\n", ks_stat))

# VISUALIZATION 5: Cumulative Gain Chart
lift_obj <- caret::lift(factor(binary_truth, labels=c("Good","Bad")) ~ risk_probs)
p_gain <- ggplot(lift_obj, plot = "gain") +
  labs(title = "Cumulative Gain Chart (Serious Delinquency)",
       subtitle = "Steeper curve = Better prioritization of risk") +
  theme_minimal()
print(p_gain)

# VISUALIZATION 6: Lift Chart
p_lift <- ggplot(lift_obj, plot = "lift") +
  labs(title = "Lift Chart",
       subtitle = "How much better than random guessing at top deciles?") +
  theme_minimal()
print(p_lift)

# VISUALIZATION 7: Calibration Plot
cal_data <- data.frame(prob = risk_probs, truth = binary_truth) %>%
  mutate(bin = ntile(prob, 10)) %>%
  group_by(bin) %>%
  summarise(mean_prob = mean(prob),
            actual_rate = mean(truth))

p_cal <- ggplot(cal_data, aes(x = mean_prob, y = actual_rate)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Calibration Plot (Reliability Diagram)",
       subtitle = "Points should lie on the red dashed line",
       x = "Predicted Risk Probability", y = "Actual Default Rate") +
  theme_minimal()
print(p_cal)


# ==============================================================================
# 5.6: AUDIT STEP: PERMUTATION IMPORTANCE
# ==============================================================================
message(">> Running Permutation Importance Audit (Wrapper Method)...")

# Define Wrapper for VIP
pred_wrapper <- function(object, newdata) {
  x_mat <- data.matrix(newdata) # Ensure matrix format
  probs <- predict(object, x_mat)
  # Sum probabilities of Status 3, 4, 5 (Indices 5, 6, 7)
  risk_score <- rowSums(probs[, 5:7]) 
  return(risk_score)
}

# Prepare Audit Data (Must remove Target and ID from train_baked)
# Ensure we don't error if ID isn't there
if("ID" %in% names(train_baked)) {
  audit_train <- train_baked %>% select(-ID, -target_class)
} else {
  audit_train <- train_baked %>% select(-target_class)
}

# Run VIP on a subset (2000 rows) for speed
set.seed(1)
sample_indices <- sample(1:nrow(audit_train), min(2000, nrow(audit_train)))
audit_subset <- audit_train[sample_indices, ]
audit_target <- train_baked$target_class[sample_indices]

vip_obj <- vip(
  object = model,
  method = "permute",           
  train = audit_subset,
  target = as.numeric(audit_target), 
  metric = "rmse",              
  pred_wrapper = pred_wrapper, 
  nsim = 5                      
)

# VISUALIZATION 8: VIP Plot
print(vip_obj + ggtitle("Feature Importance Audit (Leakage Check)"))

# ==============================================================================
# 5.7: Final Export
# ==============================================================================
dev.off()
message(sprintf(">> Evaluation Report Saved: %s", eval_pdf_path))

# Save Test Predictions with Risk Scores
results_df <- data.frame(
  actual_class = y_test,
  predicted_class = pred_classes,
  max_prob_confidence = apply(pred_probs, 1, max),
  risk_score_90plus = risk_probs,
  actual_label = as.character(actual_factor),
  predicted_label = as.character(pred_factor)
)

results_path <- file.path(output_dir, "realitycheck_test_predictions.csv")
write.csv(results_df, results_path, row.names = FALSE)
message(">> Predictions and Risk Scores exported to CSV.")
message("=== EVALUATION COMPLETE ===")