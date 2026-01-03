Data Science Project (GRP-6)
FHNW Data Science Class
==============================================================================

This repository contains the coursework and final project deliverables for 
Group 6. The project focuses on the end-to-end development of a neural 
network to predict customer repayment behavior using the Kaggle credit dataset.

------------------------------------------------------------------------------
REPOSITORY STRUCTURE
------------------------------------------------------------------------------
The assignments are organized into separate folders:

* Assignment_1/: Contains deliverables for the first assignment.
* Assignment_2/: Contains the final project, including data, scripts, and 
  documentation for the Neural Network classification task.

------------------------------------------------------------------------------
ASSIGNMENT 1: PROJECT OVERVIEW
------------------------------------------------------------------------------

------------------------------------------------------------------------------
ASSIGNMENT 2: PROJECT OVERVIEW
------------------------------------------------------------------------------

1. Documentation & Deliverables
   * Final Report: "Assignment_2/Assignment_2_Documentation.pdf"
     This is the primary deliverable containing the full report, methodology, 
     and code.
   
   * Main Script: "Assignment_2/main.rmd"
     Note: The main executable script is provided as an R Markdown file (.rmd) 
     rather than a standard .R file. This file matches the content of the 
     PDF deliverable exactly.

2. Data & Models
   * Preprocessed Data: "Assignment_2/data_eda.csv"
     This is the final version of the historic data used for training.
   
   * Champion Model: "final_sgd_champion_model.keras"
   * Scaler: "final_champion_scaler.rds"

3. How to Run the Model
   * Script: "Executable_secret_data.r"
     Use this script to check the model. It loads the pre-trained model and 
     scaler to generate predictions on new/secret data.

------------------------------------------------------------------------------
IMPORTANT NOTES
------------------------------------------------------------------------------
General: VERSION CONTROL
Due to the iterative nature of this project, we utilized Git version control 
rather than saving every intermediate script (e.g., initial EDA, discarded 
architectures) as separate files in the final folder. Previous iterations 
and experimental scripts can be retrieved via the commit history.

GITHUB REPOSITORY
https://github.com/jmarellano93/GRP-6_DS-Project

Assignment 1:
SEED 123
Please note that Seed 42 has been used throughout this entire project. 
While Seed 1 was requested in the assignment description, this discrepancy 
was identified too late in the development cycle. Re-training all models and 
output interpretations would have incurred prohibitive computational costs.

Assignment 2:
SEED 42
Please note that Seed 42 has been used throughout this entire project. 
While Seed 1 was requested in the assignment description, this discrepancy 
was identified too late in the development cycle. Re-training all models 
would have incurred prohibitive computational costs. Besides, 42 is the 
answer to everything.
