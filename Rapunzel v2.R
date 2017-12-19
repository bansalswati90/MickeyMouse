# Problem Statement
# ------------------------------------------------
  # HR analytics firm to understand what factors they should focus on, in order to curb attrition. 
  # In other words, they want to know what changes they should make to their workplace, in order to get most of their employees to stay. 
  # Also, they want to know which of these variables is most important and needs to be addressed right away.

# Goal of the case study
# ------------------------------------------------
  # You are required to model the probability of attrition using a logistic regression. The results thus obtained will be used by the management to understand what changes they should make to their workplace, in order to get most of their employees to stay.
  # Results Expected

  # 1. Write all your code in one well-commented R file; briefly, mention the insights and observations from the analysis 
  # 2. Present the overall approach of the analysis in a presentation 
  #  - Mention the problem statement and the analysis approach briefly 
  #  - Explain the results in business terms
  #  - Include visualisations and summarise the most important results in the presentation

# Load libraries
#---------------

library(ggplot2)
library(lubridate)
library(stringr)
library(MASS)
#library(caret)
library(car)
library(lmtest)
library(dplyr)


# Rubrics
# ------------------------------------------------

# 1. Data understanding, preparation and EDA (40%)
  # All data quality checks are performed, and all data quality issues are addressed in the right way 
  # (missing value imputation, 
  # removing duplicate data and 
  # other kinds of data redundancies, etc.). 
  # Explanations for data quality issues are clearly mentioned in comments or in the presentation.

  # Load All Data

  manager_survey_data <- read.csv("manager_survey_data.csv")

  general_data <- read.csv("general_data.csv")
  
  employee_survey_data <- read.csv("employee_survey_data.csv")
  
  out_time <- read.csv("out_time.csv")
  
  in_time <- read.csv("in_time.csv")

  # Dataset 1: manager_survey_data.csv
  
    # Step 1.1 - Check for unique values
    nrow(unique(manager_survey_data))    #4410
    
    # Step 1.2 - Check for NA values  - No NA values
    sum (sapply(manager_survey_data, is.na))
    
    # Step 1.3 - Checking blank values
    sapply(manager_survey_data, function(x) length(which(x==""))) #NO blank value

  # Dataset 2:  out_time.csv and in_time.csv
    # Step 2.1 - Check for Missing Metadata - 1st Column missing name e.g. 'Employee ID'
    
    colnames(in_time)[1] <- "EmployeeID"
    colnames(out_time)[1] <- "EmployeeID"
    
    as.POSIXct(in_time$X2015.01.02, format = "%Y-%m-%d %H:%M:%S")

    formatDateCol <- function (x) { as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S") }
    
    # Step 2.2 - Check for Unique Values
    nrow(unique(in_time)) #4410
    nrow(unique(out_time)) #4410
    
    # Step 2.3 - Remove Columns with Zero-Varience ot Complete NA Values
    # Step 2.4 - Check for NA Values for various Date Columns
    sum (sapply(in_time, is.na))
    sum (sapply(out_time, is.na))
    
    # Step 2.5 - Calculate Workings Hours Per day per Employee
    # Step 2.6 - Checking blank values
    sapply(in_time, function(x) length(which(x=="")))
    sapply(out_time , function(x) length(which(x=="")))
    
        # Step 2.5.1 - Convert all values to date format in both data frames
        # Step 2.5.2 - Convert both data frames to matrixes 1 and 2
        # Step 2.5.3 - Do Subtraction operation on both matrixes, resulttant matrix 3 contains office hours per day per employee

    # Step 2.6 - Calculate Mean Working Hours Per day per Employee
    
        # 2.6.1 - Use the 3rd matrix and calculate mean office hours time per employee 
        # 2.6.2 - Resultant data frame has EmployeeID and Mean Office hours time 
    
    # Step 2.7 - Merge with Main Dataset
      main_merge <- merge(manager_survey_data , in_time , by="EmployeeID")
      main_merge <- merge(main_merge , out_time , by="EmployeeID")

  # Dataset 3: general_data.csv
    # Step 3.1 - Check for unique values

    nrow(unique(general_data)) #4410
    
    # Step 3.2 - Columns to remove as Zero-Varience (i.e same values)
    # EmployeeCount, Over18, StandardHours, 

    # Step 3.3 - Checking NA Values & Data Imputation
    sum (sapply(general_data, is.na))
    # NumCompaniesWorked, TotalWorkingYears
    
    # Step 3.4 - Checking blank values
    sapply(general_data, function(x) length(which(x=="")))

    # Step 3.5 - Merge with Main Dataset
    main_merge <- merge(main_merge , general_data , by="EmployeeID")
    
  # Dataset 4: employee_survey_data.csv
    # Step 4.1 - Check for unique values
    nrow(unique(employee_survey_data)) #4410
    
    # Step 4.2 - Check for NA values - EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance
    sum (sapply(employee_survey_data, is.na))
    
    # Step 4.3 - Checking blank values
    sapply(employee_survey_data, function(x) length(which(x=="")))
    
    # Step 4.4 - Cleaning Data
    # Step 4.4 - Merge with Main Dataset
    main_merge <- merge(main_merge , employee_survey_data , by = "EmployeeID")



  # Dummy variables are created properly wherever applicable.
  # New metrics are derived if applicable and are used for analysis and modelling.
  # The data is converted to a clean format suitable for analysis in R.

# 2. Model building and evaluation (40%)
  # Model parameters are tuned using correct principles and the approach is explained clearly. Both technical and business aspects are considered while building the model.
  # Correct variable selection techniques are used. A reasonable number of different models are attempted and the best one is chosen based on key performance metrics.
  # Model evaluation is done using the correct principles and appropriate evaluation metrics are chosen.
  # The results are at par with the best possible model on the dataset.
  # The model is interpreted and explained correctly. The commented code includes a brief explanation of the important variables and the model in simple terms.

# 4. Conciseness and readability of the code (5%)

  # The code is concise and syntactically correct. Wherever appropriate, built-in functions and standard libraries are used instead of writing long code (if-else statements, for loops, etc.).
  # Custom functions are used to perform repetitive tasks.
  # The code is readable with appropriately named variables and detailed comments are written wherever necessary.