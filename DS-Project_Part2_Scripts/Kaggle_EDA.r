# ==============================================================================
# Setup and Libraries
# ==============================================================================
library(tidyverse)
library(fastDummies)

# Define path to your dataset
#path <- "/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Intermediate/dataset_minority_classes.csv"

# ==============================================================================
# 1. Import and Type Definition
# ==============================================================================
df_not_found <- df_not_found %>%
  # 1. Rename STATUS to status
  rename(status = STATUS) %>%
  
  # 2. Change dtypes according to list
  mutate(
    ID                  = as.character(ID),
    CODE_GENDER         = as.factor(CODE_GENDER),
    FLAG_OWN_CAR        = as.character(FLAG_OWN_CAR),
    FLAG_OWN_REALTY     = as.character(FLAG_OWN_REALTY),
    CNT_CHILDREN        = as.integer(CNT_CHILDREN),
    AMT_INCOME_TOTAL    = as.numeric(AMT_INCOME_TOTAL),
    NAME_INCOME_TYPE    = as.character(NAME_INCOME_TYPE),
    NAME_EDUCATION_TYPE = as.factor(NAME_EDUCATION_TYPE),
    NAME_FAMILY_STATUS  = as.factor(NAME_FAMILY_STATUS),
    NAME_HOUSING_TYPE   = as.factor(NAME_HOUSING_TYPE),
    DAYS_BIRTH          = as.integer(DAYS_BIRTH),
    DAYS_EMPLOYED       = as.integer(DAYS_EMPLOYED),
    FLAG_MOBIL          = as.integer(FLAG_MOBIL),
    FLAG_WORK_PHONE     = as.integer(FLAG_WORK_PHONE),
    FLAG_PHONE          = as.integer(FLAG_PHONE),
    FLAG_EMAIL          = as.integer(FLAG_EMAIL),
    OCCUPATION_TYPE     = as.factor(OCCUPATION_TYPE),
    CNT_FAM_MEMBERS     = as.integer(CNT_FAM_MEMBERS),
    status              = as.factor(status)
  )

# Check the result
str(df_not_found)

# Convert Flags
df_not_found$FLAG_OWN_CAR <- ifelse(df_not_found$FLAG_OWN_CAR == "Y", 1, 0)
df_not_found$FLAG_OWN_REALTY <- ifelse(df_not_found$FLAG_OWN_REALTY == "Y", 1, 0)
flags_list <- c("FLAG_MOBIL", "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL", 
                "FLAG_OWN_CAR", "FLAG_OWN_REALTY")
df_not_found[flags_list] <- lapply(df_not_found[flags_list], as.integer)

# ==============================================================================
# 2. Duplicate Removal
# ==============================================================================
duplicates_mask <- duplicated(df_not_found[, !names(df_not_found) %in% "ID"])
df_not_found <- df_not_found[!duplicates_mask, ]

# ==============================================================================
# 3. Univariate Cleaning
# ==============================================================================
df_not_found <- df_not_found %>% select(-FLAG_MOBIL)

# ==============================================================================
# 4. Age Transformation
# ==============================================================================
df_not_found <- df_not_found %>%
  mutate(AGE = round(abs(DAYS_BIRTH) / 365.25)) %>%
  select(-DAYS_BIRTH)

# ==============================================================================
# 5. Employment Cleaning
# ==============================================================================
df_not_found <- df_not_found %>%
  filter(!(NAME_INCOME_TYPE == "Pensioner" & DAYS_EMPLOYED != 365243))

df_not_found <- df_not_found %>%
  mutate(
    OCCUPATION_TYPE = as.character(OCCUPATION_TYPE),
    OCCUPATION_TYPE = case_when(
      DAYS_EMPLOYED == 365243 ~ "Retired",
      is.na(OCCUPATION_TYPE)  ~ "Unknown",
      TRUE ~ OCCUPATION_TYPE
    ),
    OCCUPATION_TYPE = as.factor(OCCUPATION_TYPE)
  )

df_not_found <- df_not_found %>%
  mutate(
    ACTIVE_EMPLOYMENT_YEARS = ifelse(DAYS_EMPLOYED == 365243, 0, abs(DAYS_EMPLOYED) / 365.25),
    ACTIVE_EMPLOYMENT_YEARS_LOG = log1p(ACTIVE_EMPLOYMENT_YEARS)
  ) %>%
  select(-c(DAYS_EMPLOYED, ACTIVE_EMPLOYMENT_YEARS))

# ==============================================================================
# 6. Family Features Logic
# ==============================================================================
df_not_found <- df_not_found[df_not_found$CNT_FAM_MEMBERS <= 10, ]

anomaly_mask <- df_not_found$CNT_FAM_MEMBERS < df_not_found$CNT_CHILDREN
df_not_found <- df_not_found %>%
  mutate(CNT_FAM_MEMBERS = case_when(
    anomaly_mask & NAME_FAMILY_STATUS %in% c("Married", "Civil marriage") ~ CNT_CHILDREN + 2,
    anomaly_mask & NAME_FAMILY_STATUS %in% c("Separated", "Widow", "Single / not married") ~ CNT_CHILDREN + 1,
    TRUE ~ CNT_FAM_MEMBERS
  ))

# ==============================================================================
# 7. Income Logic
# ==============================================================================
impossible_jobs <- c("Laborers", "Cleaning staff", "Secretaries", 
                     "Low-skill Laborers", "Drivers", "Waiters/barmen staff")

df_not_found <- df_not_found %>%
  filter(!(AMT_INCOME_TOTAL > 1000000 & OCCUPATION_TYPE %in% impossible_jobs))

# ==============================================================================
# 8. Feature Engineering
# ==============================================================================

# 8.1 Income Logs
df_not_found <- df_not_found %>%
  mutate(AMT_INCOME_TOTAL_LOG = log1p(AMT_INCOME_TOTAL))

df_not_found$INCOME_PER_FAMILY_MEMBER <- df_not_found$AMT_INCOME_TOTAL / df_not_found$CNT_FAM_MEMBERS
df_not_found <- df_not_found %>%
  mutate(INCOME_PER_FAMILY_MEMBER_LOG = log1p(INCOME_PER_FAMILY_MEMBER)) %>%
  select(-c(AMT_INCOME_TOTAL, INCOME_PER_FAMILY_MEMBER))



# ==============================================================================
# 9. Final Preprocessing and Encoding
# ==============================================================================

df_not_found$status <- as.character(df_not_found$status)
df_not_found$target_class <- recode(df_not_found$status,
                                   "X" = 0, "C" = 1, "0" = 2, "1" = 3,  
                                   "2" = 4, "3" = 5, "4" = 6, "5" = 7) |> as.numeric()
df_not_found <- df_not_found %>% select(-status)

df_not_found$EDUCATION_ENCODED <- case_when(
  df_not_found$NAME_EDUCATION_TYPE == "Lower secondary" ~ 0,
  df_not_found$NAME_EDUCATION_TYPE == "Secondary / secondary special" ~ 1,
  df_not_found$NAME_EDUCATION_TYPE == "Incomplete higher" ~ 2,
  df_not_found$NAME_EDUCATION_TYPE == "Higher education" ~ 3,
  df_not_found$NAME_EDUCATION_TYPE == "Academic degree" ~ 4,
  TRUE ~ NA_real_
)
df_not_found <- df_not_found %>% select(-NAME_EDUCATION_TYPE)

nominal_cols <- c("CODE_GENDER", "NAME_INCOME_TYPE", "NAME_FAMILY_STATUS", 
                  "NAME_HOUSING_TYPE", "OCCUPATION_TYPE")

# Inject temporary Student row
temp_row <- df_not_found[1, ]
temp_row$NAME_INCOME_TYPE <- "Student"
df_not_found <- bind_rows(df_not_found, temp_row)

df_not_found[nominal_cols] <- lapply(df_not_found[nominal_cols], as.factor)

df_not_found <- dummy_cols(
  df_not_found,
  select_columns = nominal_cols,
  remove_selected_columns = TRUE,
  remove_first_dummy = FALSE 
)

# Remove temporary Student row
df_not_found <- df_not_found[-nrow(df_not_found), ]

df_not_found <- df_not_found %>%
  select(-ID)
# ==============================================================================
# Final Output
# ==============================================================================
cat("df_not_found cleaning complete. Final dimensions:", dim(df_not_found), "\n")
str(df_not_found)

