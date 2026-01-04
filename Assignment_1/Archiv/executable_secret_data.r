# ==============================================================================
# SECTION 1: SETUP & LIBRARIES
# ==============================================================================
required_packages <- c("tidyverse", "fastDummies", "lubridate", "caret", "tidymodels", "yardstick")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(missing_packages) > 0) install.packages(missing_packages)
lapply(required_packages, library, character.only = TRUE)

# Define the function
evaluate_model_on_csv <- function(csv_path, model_path) {
  
  message("Loading data from: ", csv_path)
  
  # ==============================================================================
  # SECTION 2: DATA LOADING
  # ==============================================================================
  original <- read.csv(csv_path, row.names = NULL, sep = ";", header = TRUE)
  
  columns_to_drop <- c("collection_recovery_fee", "installment", "funded_amnt", 
                       "funded_amnt_inv", "issue_d", "last_pymnt_amnt", 
                       "last_pymnt_d", "loan_status", "next_pymnt_d", "out_prncp", 
                       "out_prncp_inv", "pymnt_plan", "recoveries", "total_pymnt", 
                       "total_pymnt_inv", "total_rec_int", "total_rec_late_fee", 
                       "initial_list_status", "total_rec_prncp", "url")
  
  data <- original %>% select(-any_of(columns_to_drop))
  
  # ==============================================================================
  # SECTION 3: TYPE CASTING
  # ==============================================================================
  data <- data %>%
    mutate(
      across(c(acc_now_delinq, collections_12_mths_ex_med, delinq_2yrs, id, member_id,
               inq_fi, inq_last_12m, inq_last_6mths, loan_amnt, mths_since_last_delinq,
               mths_since_last_major_derog, mths_since_last_record, mths_since_rcnt_il,
               open_acc, open_acc_6m, open_il_12m, open_il_24m, open_il_6m, open_rv_12m,
               open_rv_24m, pub_rec, revol_bal, total_acc, total_cu_tl), as.integer),
      across(c(all_util, annual_inc, annual_inc_joint, dti, dti_joint, il_util,
               int_rate, max_bal_bc, revol_util, tot_coll_amt, tot_cur_bal,
               total_bal_il, total_rev_hi_lim), as.numeric),
      across(c(addr_state, application_type, desc, emp_length, emp_title, 
               home_ownership, policy_code, purpose, term, title, zip_code), as.factor)
    )
  
  # ==============================================================================
  # SECTION 4: CLEANING & FEATURE ENGINEERING
  # ==============================================================================
  placeholders <- c(999, 9999.00, 9999999)
  data <- data %>%
    mutate(across(where(is.numeric), ~ ifelse(. %in% placeholders, NA, .)))
  
  data$term <- trimws(as.character(data$term))
  data$term <- ifelse(grepl("^36", data$term), 36, 60)
  
  data$emp_length <- as.character(data$emp_length)
  data$emp_length[data$emp_length == "n/a"] <- NA
  data$emp_length <- factor(data$emp_length, 
                            levels = c("< 1 year", "1 year", "2 years", "3 years", 
                                       "4 years", "5 years", "6 years", "7 years", 
                                       "8 years", "9 years", "10+ years"), 
                            ordered = TRUE)
  
  data <- data %>% filter(!home_ownership %in% c("ANY", "NONE", "OTHER"))
  data$home_ownership <- droplevels(data$home_ownership)
  data$home_ownership <- factor(data$home_ownership, levels = c("RENT", "MORTGAGE", "OWN"), ordered = TRUE)
  
  data <- data %>% filter(purpose != "educational")
  data$purpose <- droplevels(data$purpose)
  
  data$verification_status <- factor(data$verification_status, 
                                     levels = c("Not Verified", "Source Verified", "Verified"), 
                                     ordered = TRUE)
  
  data <- data %>% select(-any_of(c("addr_state", "earliest_cr_line", "last_credit_pull_d", "title")))
  
  # ==============================================================================
  # SECTION 5: DROPPING HIGH MISSINGNESS & ROWS
  # ==============================================================================
  columns_to_drop_na <- c("open_acc_6m", "open_il_6m", "open_il_12m", "open_il_24m", 
                          "mths_since_rcnt_il", "total_bal_il", "il_util", "open_rv_12m", 
                          "open_rv_24m", "max_bal_bc", "all_util", "inq_fi", 
                          "total_cu_tl", "inq_last_12m")
  
  cleaned_data <- data %>% select(-any_of(columns_to_drop_na))
  
  cleaned_data <- cleaned_data %>%
    filter(!is.na(delinq_2yrs), !is.na(inq_last_6mths), !is.na(open_acc), 
           !is.na(pub_rec), !is.na(total_acc), !is.na(acc_now_delinq))
  
  cleaned_data <- cleaned_data %>%
    filter(!(is.na(tot_coll_amt) & is.na(tot_cur_bal) & is.na(total_rev_hi_lim)))
  
  cleaned_data <- cleaned_data %>%
    select(-any_of(c("annual_inc_joint", "dti_joint", "verification_status_joint"))) %>%
    filter(application_type != "JOINT") %>%
    select(-application_type)
  
  # ==============================================================================
  # SECTION 6: TRANSFORMATIONS & NEW FEATURES
  # ==============================================================================
  cleaned_data <- cleaned_data %>%
    mutate(log_loan_amnt = log(loan_amnt + 1)) %>%
    mutate(
      dti = ifelse(annual_inc > 500000, NA, dti),
      annual_inc = ifelse(annual_inc > 500000, annual_inc / 100, annual_inc),
      log_annual_inc = log(annual_inc + 1),
      dti = ifelse(dti > 100 | dti == 0, NA, dti)
    )
  
  cleaned_data <- cleaned_data %>%
    mutate(
      mths_since_last_delinq = ifelse(delinq_2yrs > 0 & mths_since_last_delinq > 24, NA, mths_since_last_delinq),
      delinq_2yrs = ifelse(!is.na(mths_since_last_delinq) & mths_since_last_delinq == 0 & delinq_2yrs == 0, 1, delinq_2yrs),
      mths_since_last_delinq = ifelse(acc_now_delinq > 0 & mths_since_last_delinq > 0, 0, mths_since_last_delinq)
    )
  
  # *** FIX 1: Levels adjusted to match training order (0 to 3+) ***
  cleaned_data <- cleaned_data %>%
    mutate(delinq_2yrs_cat = case_when(
      delinq_2yrs == 0 ~ "0",
      delinq_2yrs == 1 ~ "1",
      delinq_2yrs == 2 ~ "2",
      delinq_2yrs >= 3 ~ "3+",
      TRUE ~ NA_character_
    )) %>%
    mutate(delinq_2yrs_cat = factor(delinq_2yrs_cat, levels = c("0", "1", "2", "3+"), ordered = TRUE))
  
  cleaned_data <- cleaned_data %>%
    mutate(delinq_recency = case_when(
      is.na(mths_since_last_delinq) ~ "none",
      mths_since_last_delinq == 0 ~ "0",
      mths_since_last_delinq <= 6 ~ "1-6",
      mths_since_last_delinq <= 12 ~ "7-12",
      mths_since_last_delinq <= 24 ~ "13-24",
      mths_since_last_delinq > 24 ~ "24+",
      TRUE ~ NA_character_
    )) %>%
    mutate(delinq_recency = factor(delinq_recency, levels = c("0", "1-6", "7-12", "13-24", "24+", "none"), ordered = TRUE))
  
  cleaned_data <- cleaned_data %>%
    mutate(acc_now_delinq_bin = factor(ifelse(acc_now_delinq > 0, "1", "0"), levels = c("0", "1"))) %>%
    select(-acc_now_delinq)
  
  cleaned_data <- cleaned_data %>%
    mutate(pub_rec = ifelse(mths_since_last_record == 0 & pub_rec == 0, 1, pub_rec)) %>%
    filter(!(is.na(pub_rec) & !is.na(mths_since_last_record))) %>%
    filter(!(is.na(open_acc) & is.na(total_acc)))
  
  cleaned_data <- cleaned_data %>%
    mutate(revol_bal = ifelse(revol_bal > total_rev_hi_lim, NA, revol_bal))
  
  # *** FIX 2: Levels adjusted to match training order (0 to 49-84) ***
  cleaned_data <- cleaned_data %>%
    mutate(mths_since_last_major_derog = ifelse(pub_rec == 0, NA, mths_since_last_major_derog)) %>%
    mutate(major_derog_recency = case_when(
      is.na(mths_since_last_major_derog) ~ "none",
      mths_since_last_major_derog == 0 ~ "0",
      mths_since_last_major_derog <= 6 ~ "1-6",
      mths_since_last_major_derog <= 12 ~ "7-12",
      mths_since_last_major_derog <= 24 ~ "13-24",
      mths_since_last_major_derog <= 48 ~ "25-48",
      mths_since_last_major_derog <= 84 ~ "49-84",
      TRUE ~ ">84"
    )) %>%
    mutate(major_derog_recency = factor(major_derog_recency, 
                                        levels = c("0", "1-6", "7-12", "13-24", "25-48", "49-84", "none"), ordered = TRUE))
  
  cleaned_data <- cleaned_data %>%
    select(-any_of(c("id", "member_id", "desc", "zip_code", "emp_title", "policy_code", 
                     "revol_util", "mths_since_last_delinq", "mths_since_last_major_derog", 
                     "delinq_2yrs", "mths_since_last_record", "pub_rec", "loan_amnt", 
                     "annual_inc", "tot_coll_amt", "collections_12_mths_ex_med")))
  
  message("Data cleaning complete. Final dimensions: ", paste(dim(cleaned_data), collapse = " x "))
  
  # ==============================================================================
  # SECTION 8: PREDICTION
  # ==============================================================================
  message("Loading model and generating predictions...")
  final_xgb_model <- readRDS(model_path)
  
  # Use 'cleaned_data' for prediction
  pred <- predict(final_xgb_model, new_data = cleaned_data)
  
  # Combine predictions with actuals
  results <- bind_cols(cleaned_data %>% select(int_rate), pred)
  
  # ==============================================================================
  # SECTION 9: EVALUATION
  # ==============================================================================
  
  final_metrics <- metrics(results, truth = int_rate, estimate = .pred)
  mse_value <- mean((results$int_rate - results$.pred)^2, na.rm = TRUE)
  
  return(list(
    standard_metrics = final_metrics,
    mse = mse_value,
    predictions_preview = head(results)
  ))
}

# ==============================================================================
# EXECUTION
# ==============================================================================

my_csv_path <- "/Users/Jujou/Documents/Repos/GRP-6_DS-Project/Assignment_1/LCdata.csv"
my_model_path <- "final_xgb_model.rds"

output <- evaluate_model_on_csv(my_csv_path, my_model_path)

print(output$standard_metrics)
print(paste("MSE:", output$mse))