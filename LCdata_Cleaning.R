# Load required packages
library(dplyr)
library(readr)

# Set the working directory where LCdata.csv is located
setwd("/Users/francescamorici/Desktop/GRP-6_DS-Project/DS-Project_data")

# Check if the file exists
if(!"LCdata.csv" %in% list.files()){
  stop("Error: LCdata.csv not found in the working directory")
}

# Import the dataset
# Using read_delim from readr with ';' as separator
LCdata <- read_delim("LCdata.csv", delim = ";")

# Inspect column names and first rows
names(LCdata)
head(LCdata)

# Clean missing values (NA) in key columns
LCdata <- LCdata %>%
  mutate(
    annual_inc = ifelse(is.na(annual_inc), median(annual_inc, na.rm = TRUE), annual_inc),
    delinq_2yrs = ifelse(is.na(delinq_2yrs), 0, delinq_2yrs),
    inq_last_6mths = ifelse(is.na(inq_last_6mths), 0, inq_last_6mths),
    mths_since_last_delinq = ifelse(is.na(mths_since_last_delinq), median(mths_since_last_delinq, na.rm = TRUE), mths_since_last_delinq),
    mths_since_last_record = ifelse(is.na(mths_since_last_record), median(mths_since_last_record, na.rm = TRUE), mths_since_last_record),
    open_acc = ifelse(is.na(open_acc), median(open_acc, na.rm = TRUE), open_acc),
    pub_rec = ifelse(is.na(pub_rec), median(pub_rec, na.rm = TRUE), pub_rec)
  )

# Verify that there are no remaining NAs
colSums(is.na(LCdata[, c("annual_inc","delinq_2yrs","inq_last_6mths",
                         "mths_since_last_delinq","mths_since_last_record",
                         "open_acc","pub_rec")]))

