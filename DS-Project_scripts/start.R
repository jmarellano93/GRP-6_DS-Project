library(tidyverse)
library(corrplot)
library(fastDummies)
library(dplyr)
library(ggplot2)

# Read the dataset
data <- read.csv("DS-Project_data/LCdata.csv", 
                 row.names = NULL, 
                 sep = ";",
                 header = TRUE)

# Display the structure of the dataset
str(data)

# Display the first few rows of the dataset
head(data)

# Display summary statistics of the dataset
summary(data)

