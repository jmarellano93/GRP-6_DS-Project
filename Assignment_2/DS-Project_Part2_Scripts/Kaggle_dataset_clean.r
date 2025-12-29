library(dplyr)
library(readr)

# -----------------------------------------------------------------------------
# 1. Load Data
# -----------------------------------------------------------------------------
# Adjust paths as necessary
credit_record <- read_csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Raw/credit_record.csv")
application_record <- read_csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Raw/application_record.csv")
# Assuming dataset_part_2 is loaded:
 dataset_part_2 <- read_csv("/Users/Jujou/Documents/Repos/GRP-6_DS-Project/DS-Project_data/Dataset-part-2.csv")

# -----------------------------------------------------------------------------
# 2. Process credit_record (Create Target)
# -----------------------------------------------------------------------------
status_levels <- c("X", "C", "0", "1", "2", "3", "4", "5")

credit_worst <- credit_record %>%
  mutate(STATUS = factor(STATUS, levels = status_levels, ordered = TRUE)) %>%
  group_by(ID) %>%
  summarise(STATUS = max(STATUS)) %>%
  ungroup() %>%
  mutate(STATUS = as.character(STATUS))

# Create the master Kaggle dataset (Original ID + Content + Target)
kaggle_df <- application_record %>%
  inner_join(credit_worst, by = "ID")

# -----------------------------------------------------------------------------
# 3. Prepare for Content-Based Matching
# -----------------------------------------------------------------------------

# Clean dataset_part_2 to match kaggle_df structure
# We REMOVE the professor's ID here, as it is useless for matching
part2_clean <- dataset_part_2 %>%
  rename(STATUS = status) %>%        # Ensure Target column name matches
  select(-ID) %>%                    # Remove the scrambled ID
  mutate(across(where(is.numeric), as.double)) # Standardize types

# Identify the columns we will use for matching (All columns in part 2)
# This assumes part2 is a strict subset of columns found in kaggle_df
matching_cols <- colnames(part2_clean)

# Ensure kaggle_df has matching types for the join
kaggle_df_clean <- kaggle_df %>%
  mutate(across(where(is.numeric), as.double))

# -----------------------------------------------------------------------------
# 4. Perform Anti-Join (Preserving Original ID)
# -----------------------------------------------------------------------------

# anti_join(x, y, by = ...) returns rows from x that are not in y.
# By specifying 'by = matching_cols', we match on content but keep the ID column from 'x'.

df_not_found <- kaggle_df_clean %>%
  anti_join(part2_clean, by = matching_cols)

# -----------------------------------------------------------------------------
# 5. Validation
# -----------------------------------------------------------------------------

print(paste("Total rows in original Kaggle set:", nrow(kaggle_df_clean)))
print(paste("Rows found in University Dataset:", nrow(kaggle_df_clean) - nrow(df_not_found)))
print(paste("New 'Rest of World' Dataset size:", nrow(df_not_found)))

# Verify IDs are still present
head(df_not_found$ID)