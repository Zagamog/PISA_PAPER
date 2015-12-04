#### SAppx_FryerLev1.R (Math, Table A2)

#### Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 ####

# Prepared by Suhas D. Parandekar and Elisabeth K. Sedmik (The World Bank Group)
# Accompanying code to research paper
# Date of this version: 12/02/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam.

#######################################################################################################
# Outline:
# S0_Prelims    Generating data sets (merging, cleaning) 
# S1_Figures      SECTION 1: Introduction (Descriptive statistics, plots, etc)
# S2_Endowments   SECTION 2: Endowments tables
# S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
# S4_OaxacaBlin   SECTION 4: Regressions following Oaxaca-Blinder approach
# SAppx_FryerLev  SECTION 5: Regressions following Fryer & Levitt (2004) on the rotated questionnaire
######################################################################################################

# We perform a dummy variable regression analysis following Fryer & Levitt (2004) for the rotated student
# questionnaire variables for MATH (Table A2). The PISA dataset contains 3 rotated student questionnaire, each one
# having been adminstered to 2/3 of the students tested. Thus, in order to keep the sample size meaningfully
# large, we do not add all the rotated student questionnaire variables to the non-rotated variables (see S3_FryerLevitt)
# at once but in turn. We create three additional columns per subject.

# Loading R packages to process PISA data:

# Admin packages
library(foreign)# To import and export data to and from R (eg. txt files)
library(xlsx)# To generate MS-Excel output
library(xtable)# To generate Latex output (in which the research paper is written)
library(epicalc)# For producing descriptives of data
library(tables) # Computes and displays complex tables of summary statistics
library(stargazer)# For latex regression and summary statistics tables

# Modeling packages
library(intsvy)# For PISA (and TIMSS, PIRLS, etc) analysis with Plausible Values (PV) and Balanced Repeated Replication (BRR)
library(TDMR)# For tuned data mining in R - eg. detect column of constants in dataframe
library(gmodels)# For model fitting, contains various R programming tools (eg. PROC FREQ like tables)
library(dplyr)# For varioys data manipulation
library(psych)# For rescaling variables to given mean and sd
library(sm)# for locally smoothed regressions and density estimation
library(lme4)# To run mixed-effects models using Eigen and S4
library(data.table) # For enhanced dataframe faster processing
library(dplyr) # For faster data crunching

##### 1. Table A2 - Columns 1 to 4

##########################
# Column (1) Vietnam dummy - please see S3_FryerLevitt1.R line 2185
##########################

###############################################
# Column (2) Vietnam dummy + Students + Parents 
###############################################






