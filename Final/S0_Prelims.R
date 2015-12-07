#### S0_Prelims.R

#### Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 ####

# Prepared by Suhas D. Parandekar and Elisabeth K. Sedmik (The World Bank Group)
# Accompanying code to research paper
# Date of this version: 12/02/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam.

##################################################################################
# Outline:
### S0_Prelims    Generating data sets (merging, cleaning) 
# S1_Figures      SECTION 1: Introduction (Descriptive statistics, plots, etc)
# S2_Endowments   SECTION 2: Endowments tables
# S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
# S4_OaxacaBlin   SECTION 4: Regressions following Oaxaca-Blinder approach
# SAppx_FryerLev  SECTION 5: Regressions following Fryer & Levitt (2004) on the rotated questionnaire
##################################################################################

# We load the raw data STUDENT and SCHOOL 2012 PISA downloaded from the OECD website
# And generate a master data file called DEVCON8a.rda that contains data from Vietnam
# 7 developing countries (with GDP below $10.000 per annum in PPP) that took part in PISA 2012.

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

# Please be aware that many packages (eg. tables, intsvy) require additional packages to run. When trying to load
# the package, R will tell you which ones are missing. Overall you may need to download around 40 packages.

# We generate new, merged data sets based on the student data set and school data set for the 8 countries
# that have a Per Capita GDP below $10,000 in PPP for 2010, based on OECD-PISA Table Table IV.3.2 
# OECD-PISA Report: "PISA 2012 Results: What Makes a School Successful?" (Volume IV, Chapter 3, Table IV.3.2)

# Original data downloaded from: http://pisa2012.acer.edu.au/downloads.php (txt files, spss or sas)

setwd("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012")
setwd("yourpathgoeshere")

student.rda <- read.dta("stu.dta")
school.rda <- read.dta("sch.dta")

# Please refer to the questionnaires, codebooks and the technical manual (pages 396-398) about the specific
# variables included in the data files and their abbreviations.  
# For an overview of included variables see: http://www.oecd.org/pisa/pisaproducts/pisa2012technicalreport.htm
# and for abbreviations see the student and school codebooks: http://pisa2012.acer.edu.au/downloads.php

# We will filter for the 8 developing countries per country by school id and merge into one file ("DEVCON8"):

# Albania 

ALB_T <- filter(student.rda, cnt == "ALB") # filter by "cnt" and create a new student file just for 'Albania'
ALB_S <- filter(school.rda, cnt == "ALB") # filter by "cnt" and create a new school file just for 'Albania'
ALB_P <- merge(ALB_T, ALB_S, by = "schoolid") # merge both files into one file just for 'Albania'
ALB_P$cnt <- ALB_P$cnt.x # we duplicate "cnt.x" as "cnt" (it is only called "cnt.x" since we merged ALB_S and ALB_T, not that in ALB_S and ALB_T it was called "cnt")
ALB_P$cnt.x <- NULL # we delete the column "cnt.x"
ALB_P$subnatio <- ALB_P$subnatio.x # we duplicate "subnatio.x" as "subnatio" (to have the same nomenclature as in the original data!)
ALB_P$subnatio.x <- NULL # we delete the column "subnatio.x"
ALB_P$stratum <- ALB_P$stratum.x # same as above
ALB_P$stratum.x <- NULL # same as above
ALB_P$oecd <- ALB_P$oecd.x # same as above
ALB_P$oecd.x <- NULL # same as above
ALB_P$nc <- ALB_P$nc.x  # same as above
ALB_P$nc.x <- NULL # same as above

# We want to have a unique identifier number per student per school per country. Note, that for all countries, numbering of school and student id's
# starts at 1; so we introduce "NEWID" to create for a cumulative list spanning all 8 countries:

ALB_P$COUNTRY <- 1 # We numerate each country alphabetically, starting with Albania = 1, Colombia = 2, etc.
ALB_P$NEWID <- (ALB_P$COUNTRY*10000000)+((as.numeric(ALB_P$schoolid))*10000)+(as.numeric(ALB_P$stidstd))

# Useful tip: these are very large data files. to double check if the new columns have been created, do not load 
# the new file ALB_P into the editor window, as it will only show up until a certain column or take very long to load. 
# Instead, type in "ALB_P" into the Console to see all columns (and the first 10 rows per column). 

# Now we repeat the same process for the remaining 7 countries:

# Colombia

COL_T <- filter(student.rda, cnt == "COL")
COL_S <- filter(school.rda, cnt == "COL")
COL_P <- merge(COL_T, COL_S, by = "schoolid")
COL_P$cnt <- COL_P$cnt.x
COL_P$cnt.x <- NULL
COL_P$subnatio <- COL_P$subnatio.x
COL_P$subnatio.x <- NULL
COL_P$stratum <- COL_P$stratum.x
COL_P$stratum.x <- NULL
COL_P$oecd <- COL_P$oecd.x
COL_P$oecd.x <- NULL
COL_P$nc <- COL_P$nc.x
COL_P$nc.x <- NULL
COL_P$COUNTRY <-2
COL_P$NEWID <- (COL_P$COUNTRY*10000000)+((as.numeric(COL_P$schoolid))*10000)+(as.numeric(COL_P$stidstd)) 

# The command 'as.numeric' converts individual columns to numeric variables 
# Useful tip: to test for the class of variables in the column 'schoolid', type in 'class(COL_P$schoolid)'

# Indonesia

IDN_T <- filter(student.rda, cnt == "IDN")
IDN_S <- filter(school.rda, cnt == "IDN")
IDN_P <- merge(IDN_T, IDN_S, by = "schoolid")
IDN_P$cnt <- IDN_P$cnt.x
IDN_P$cnt.x <- NULL
IDN_P$subnatio <- IDN_P$subnatio.x
IDN_P$subnatio.x <- NULL
IDN_P$stratum <- IDN_P$stratum.x
IDN_P$stratum.x <- NULL
IDN_P$oecd <- IDN_P$oecd.x
IDN_P$oecd.x <- NULL
IDN_P$nc <- IDN_P$nc.x
IDN_P$nc.x <- NULL
IDN_P$COUNTRY <-3
IDN_P$NEWID <- (IDN_P$COUNTRY*10000000)+((as.numeric(IDN_P$schoolid))*10000)+(as.numeric(IDN_P$stidstd))

# Jordan

JOR_T <- filter(student.rda, cnt == "JOR")
JOR_S <- filter(school.rda, cnt == "JOR")
JOR_P <- merge(JOR_T, JOR_S, by = "schoolid")
JOR_P$cnt <- JOR_P$cnt.x
JOR_P$cnt.x <- NULL
JOR_P$subnatio <- JOR_P$subnatio.x
JOR_P$subnatio.x <- NULL
JOR_P$stratum <- JOR_P$stratum.x
JOR_P$stratum.x <- NULL
JOR_P$oecd <- JOR_P$oecd.x
JOR_P$oecd.x <- NULL
JOR_P$nc <- JOR_P$nc.x
JOR_P$nc.x <- NULL
JOR_P$COUNTRY <-4
JOR_P$NEWID <- (JOR_P$COUNTRY*10000000)+((as.numeric(JOR_P$schoolid))*10000)+(as.numeric(JOR_P$stidstd))

# Peru

PER_T <- filter(student.rda, cnt == "PER")
PER_S <- filter(school.rda, cnt == "PER")
PER_P <- merge(PER_T, PER_S, by = "schoolid")
PER_P$cnt <- PER_P$cnt.x
PER_P$cnt.x <- NULL
PER_P$subnatio <- PER_P$subnatio.x
PER_P$subnatio.x <- NULL
PER_P$stratum <- PER_P$stratum.x
PER_P$stratum.x <- NULL
PER_P$oecd <- PER_P$oecd.x
PER_P$oecd.x <- NULL
PER_P$nc <- PER_P$nc.x
PER_P$nc.x <- NULL
PER_P$COUNTRY <-5
PER_P$NEWID <- (PER_P$COUNTRY*10000000)+((as.numeric(PER_P$schoolid))*10000)+(as.numeric(PER_P$stidstd))

# Thailand

THA_T <- filter(student.rda, cnt == "THA")
THA_S <- filter(school.rda, cnt == "THA")
THA_P <- merge(THA_T, THA_S, by = "schoolid")
THA_P$cnt <- THA_P$cnt.x
THA_P$cnt.x <- NULL
THA_P$subnatio <- THA_P$subnatio.x
THA_P$subnatio.x <- NULL
THA_P$stratum <- THA_P$stratum.x
THA_P$stratum.x <- NULL
THA_P$oecd <- THA_P$oecd.x
THA_P$oecd.x <- NULL
THA_P$nc <- THA_P$nc.x
THA_P$nc.x <- NULL
THA_P$COUNTRY <-6
THA_P$NEWID <- (THA_P$COUNTRY*10000000)+((as.numeric(THA_P$schoolid))*10000)+(as.numeric(THA_P$stidstd))

# Tunisia

TUN_T <- filter(student.rda, cnt == "TUN")
TUN_S <- filter(school.rda, cnt == "TUN")
TUN_P <- merge(TUN_T, TUN_S, by = "schoolid")
TUN_P$cnt <- TUN_P$cnt.x
TUN_P$cnt.x <- NULL
TUN_P$subnatio <- TUN_P$subnatio.x
TUN_P$subnatio.x <- NULL
TUN_P$stratum <- TUN_P$stratum.x
TUN_P$stratum.x <- NULL
TUN_P$oecd <- TUN_P$oecd.x
TUN_P$oecd.x <- NULL
TUN_P$nc <- TUN_P$nc.x
TUN_P$nc.x <- NULL
TUN_P$COUNTRY <-7
TUN_P$NEWID <- (TUN_P$COUNTRY*10000000)+((as.numeric(TUN_P$schoolid))*10000)+(as.numeric(TUN_P$stidstd))

# Vietnam

VNM_T <- filter(student.rda, cnt == "VNM")
VNM_S <- filter(school.rda, cnt == "VNM")
VNM_P <- merge(VNM_T, VNM_S, by = "schoolid")
VNM_P$cnt <- VNM_P$cnt.x
VNM_P$cnt.x <- NULL
VNM_P$subnatio <- VNM_P$subnatio.x
VNM_P$subnatio.x <- NULL
VNM_P$stratum <- VNM_P$stratum.x
VNM_P$stratum.x <- NULL
VNM_P$oecd <- VNM_P$oecd.x
VNM_P$oecd.x <- NULL
VNM_P$nc <- VNM_P$nc.x
VNM_P$nc.x <- NULL
VNM_P$COUNTRY <-8
VNM_P$NEWID <- (VNM_P$COUNTRY*10000000)+((as.numeric(VNM_P$schoolid))*10000)+(as.numeric(VNM_P$stidstd))

DEVCON8 <- rbind(ALB_P,COL_P,IDN_P,JOR_P,PER_P,THA_P,TUN_P,VNM_P) # combine all country specific files into the "DEVCON8" file, thanks to "dyplr" package

# Finally, we add a Vietnam dummy variable to "DEVCON8", which we will need to produce descriptive statistics 
# and for teh regression analyses

DEVCON8$VIETNAM[DEVCON8$COUNTRY==8] <- 1 # dummy takes value = 1, if the country is Vietnam
DEVCON8$VIETNAM[DEVCON8$COUNTRY!=8] <- 0 # dummy takes value = 0, if the country is not Vietnam

# The 'intsvy' package (the most important one for our PISA analysis) requires variable names to be in upper case; 
# you might have noticed how we put all newly created variables already in upper case; no we convert all 

names(DEVCON8) <- toupper(names(DEVCON8)) 

# Let's save the new file

save(DEVCON8, file ="DEVCON8.rda") 

#### End of S0_Prelims.R
