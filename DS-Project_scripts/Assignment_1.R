library(tidyverse)
library(corrplot)
library(fastDummies)
library(dplyr)
library(ggplot2)

# Read the dataset
original <- read.csv("DS-Project_data/LCdata.csv", 
                 row.names = NULL, 
                 sep = ";",
                 header = TRUE)

columns_to_drop <- c("collection_recovery_fee",
                     "installment",
                     "funded_amnt",
                     "funded_amnt_inv",
                     "issue_d",
                     "last_pymnt_amnt",
                     "last_pymnt_d",
                     "loan_status",
                     "next_pymnt_d",
                     "out_prncp",
                     "out_prncp_inv",
                     "pymnt_plan",
                     "recoveries",
                     "total_pymnt",
                     "total_pymnt_inv",
                     "total_rec_int",
                     "total_rec_late_fee",
                     "total_rec_prncp")
data <- original[, -match(columns_to_drop,  names(original))]

dim(data) # The shape is 798641:54

# Display the structure of the dataset
str(data)
view(data)

# Display the first few rows of the dataset
head(data)

# Display summary statistics of the dataset
summary(data)

# Which columns has NA
na_counts <- colSums(is.na(data))
na_counts[na_counts > 0]


