#### S3_FryerLevitt2.R (Reading Regression output)

#### Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 ####

# Prepared by Suhas D. Parandekar and Elisabeth K. Sedmik (The World Bank Group)
# Accompanying code to research paper
# Date of this version: 12/02/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam.

##################################################################################
# Outline:
# S0_Prelims      Generating data sets (merging, cleaning) 
# S1_Figures      SECTION 1: Introduction (Descriptive statistics, plots, etc)
# S2_Endowments   SECTION 2: Endowments tables
#### S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
# S4_OaxacaBlin   SECTION 4: Regressions following Oaxaca-Blinder approach
##################################################################################

# We create the regression output for Reading used for Section 3: Regression Approach I: Fryer-Levitt
# for table 11. In the 1. Data preparation, we individually add variables to see if they decrease the 
# Vietnam dummy. All 'gap decreasing' variables will be retained in 2. to create the regression output (Table 11)

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

load("DEVCON8.rda")

DEVCON8a <- DEVCON8

##### 1. DATA PREPARATION FOR TABLE 11 - WHICH VARIABLES DECREASE THE GAP/VIETNAM DUMMY?

###########################################################
# 1.A DATA PREPARATION - STUDENT & PARENTS-RELATED VARIABLES
###########################################################

T0 <- DEVCON8[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 # 48483 data points

# How many non-missing values for all non-rotated student variables (excl SC25)?
T1b <- DEVCON8[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","MISCED", "HISEI",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 35345
N0-N1 # 13138 NAs

DEVCON8l <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8l$FEMALE[DEVCON8l$ST04Q01==1] <- 1
DEVCON8l$FEMALE[DEVCON8l$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8l$PRESCHOOL[DEVCON8l$ST05Q01==1] <- 0
DEVCON8l$PRESCHOOL[DEVCON8l$ST05Q01==2] <- 1
DEVCON8l$PRESCHOOL[DEVCON8l$ST05Q01==3] <- 1

#ST08Q01
#_______________________________________________________________________________________________________________
# leave as is

#ST09Q01
#_______________________________________________________________________________________________________________
# leave as is 

# ST115Q01 
#______________________________________________________________________________________________________________
# leave as is

#ST28Q01
#______________________________________________________________________________________________________________
# We want to work with actual range of number of books (please see the student coding file p. 95):
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==1]  <- 5
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==2]  <- 15
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==3]  <- 60
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==4]  <- 150
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==5]  <- 350
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8l$PARPRESSURE[DEVCON8l$SC24Q01==1] <- 1
DEVCON8l$PARPRESSURE[DEVCON8l$SC24Q01==2] <- 0
DEVCON8l$PARPRESSURE[DEVCON8l$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
DEVCON8l$SC25Q01[is.na(DEVCON8l$SC25Q01)]  <- 0
DEVCON8l$SC25Q02[is.na(DEVCON8l$SC25Q02)]  <- 0
DEVCON8l$SC25Q03[is.na(DEVCON8l$SC25Q03)]  <- 0
DEVCON8l$SC25Q04[is.na(DEVCON8l$SC25Q04)]  <- 0
DEVCON8l$SC25Q05[is.na(DEVCON8l$SC25Q05)]  <- 0
DEVCON8l$SC25Q06[is.na(DEVCON8l$SC25Q06)]  <- 0
DEVCON8l$SC25Q07[is.na(DEVCON8l$SC25Q07)]  <- 0
DEVCON8l$SC25Q08[is.na(DEVCON8l$SC25Q08)]  <- 0
DEVCON8l$SC25Q09[is.na(DEVCON8l$SC25Q09)]  <- 0
DEVCON8l$SC25Q10[is.na(DEVCON8l$SC25Q10)]  <- 0
DEVCON8l$SC25Q11[is.na(DEVCON8l$SC25Q11)]  <- 0
DEVCON8l$SC25Q12[is.na(DEVCON8l$SC25Q12)]  <- 0

#TIGERMOM
DEVCON8l$TIGERMOM  <- DEVCON8l$SC25Q01+DEVCON8l$SC25Q03
DEVCON8l$TIGERMOM[DEVCON8l$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8l$VOLUMOM <- DEVCON8l$SC25Q05+DEVCON8l$SC25Q06+DEVCON8l$SC25Q07+DEVCON8l$SC25Q09+DEVCON8l$SC25Q12
DEVCON8l$VOLUMOM[DEVCON8l$VOLUMOM>100] <- 100 

#TEACHMOM
DEVCON8l$TEACHMOM <- DEVCON8l$SC25Q08

#FUNDMOM
DEVCON8l$FUNDMOM <-  DEVCON8l$SC25Q11

#COUNCILMOM
DEVCON8l$COUNCILMOM <- DEVCON8l$SC25Q10

#DUTYMOM
DEVCON8l$DUTYMOM <- DEVCON8l$SC25Q02

save(DEVCON8l, file="DEVCON8l.rda") 

R177 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R177
# Estimate Std. Error t value
# (Intercept)   411.08       2.58  159.18
# VIETNAM        99.18       4.98   19.92
# R-squared      20.36       1.98   10.29

# Just to re-iterate, on the whole data set (48483) we got:

R177a <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8a,export=FALSE)
R177a
#Estimate Std. Error t value
#(Intercept)   403.06       2.46  163.78
#VIETNAM       105.16       5.03   20.89
#R-squared      19.61       1.81   10.85

R178 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R178 # FEMALE decreases the gap
#VIETNAM:  98.81 

R179 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R179 # FEMALE decreases, PRESCHOOL decreases
#VIETNAM: 89.64

R180 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R180 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases
#VIETNAM: 84.42

R181 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R181 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases
#VIETNAM:  82.06 

R182 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R182 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 82.12

R183 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R183 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases
#VIETNAM: 81.54

R184 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R184 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases
#VIETNAM: 89.24

R185 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R185 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases
#VIETNAM: 90.00

# We only take HISEI and MISCED (not FISCED, HISCED) as they will be strongly correlated anyways

R186 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R186 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases
#VIETNAM: 90.52

R187 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R187 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases
#VIETNAM: 90.64

R188 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R188 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 90.74

R189 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R189 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 90.65

R190 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R190 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 89.70

R191 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R191 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
#VIETNAM: 90.35

R192 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R192 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases,
# VOLUMOM decreases
#VIETNAM: 90.17

R193 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R193 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM decreases, TEACHMOM increases
#VIETNAM: 91.05

R194 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R194 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM decreases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM:  87.76

R195 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R195 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM decreases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 84.67

# Please note: we only at a later stage (after having done all Fryer-Levitt analysis) decided 
# to include the variable DUTYMOM into our analysis, hence it is not included in sequence here.
# We tested and found that it decreases the VIETNAM dummy and thus included it in the final regressions
# please see lines (1682), but be aware that it is not included in the sequencing up to the final regressions

R195b <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM","DUTYMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R195b 
# VIETNAM: 84.38

R196 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM",
                        "PCGIRLS"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R196 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM decreases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases, PCGIRLS decreases
#VIETNAM: 84.00

# Now testing all 11 variables that decreased the gap

R197 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "VOLUMOM","FUNDMOM", "COUNCILMOM","PCGIRLS"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R197
#Estimate Std. Error t value
#(Intercept)   379.72       6.96   54.53
#VIETNAM        70.64       4.51   15.65
#FEMALE         21.20       1.56   13.58
#PRESCHOOL      39.15       4.13    9.48
#REPEAT        -55.92       3.31  -16.92
#ST08Q01        -8.47       1.28   -6.62
#ST115Q01       -8.15       1.69   -4.82
#BOOK_N          0.07       0.01    5.91
#PARPRESSURE    10.47       4.69    2.23
#VOLUMOM        -0.04       0.06   -0.73
#FUNDMOM         0.18       0.06    3.18
#COUNCILMOM     -0.14       0.05   -2.63
#PCGIRLS        41.37       9.94    4.16
#R-squared      37.70       1.70   22.15

# Now testing all 8 variables that increased the gap

R198 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R198
#Estimate Std. Error t value
#(Intercept)   452.55       4.67   96.84
#VIETNAM       105.05       4.28   24.55
#ST09Q01       -25.32       2.12  -11.96
#HISEI           0.46       0.05    8.53
#MISCED          1.37       0.59    2.32
#WEALTH         10.01       1.27    7.86
#CULTPOS        -3.78       0.95   -3.97
#HEDRES         11.75       0.88   13.36
#TIGERMOM       -0.01       0.05   -0.30
#TEACHMOM       -0.03       0.06   -0.55
#R-squared      33.05       1.74   18.98

#################################################################
# 1.B DATA PREPARATION - TEACHER-RELATED VARIABLES (PART 1) ADDED
#################################################################

# 11 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, VOLUMOM, FUNDMOM, COUNCILMOM     

# Teacher related variables (part 1): 
# STRATIO, PROPCERT, PROPQUAL, TCSHORT
# TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08

T1b <- DEVCON8a[, c("VIETNAM","STRATIO","PROPCERT","PROPQUAL","TCSHORT","TCFOCST", 
                    "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04","SC39Q08","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 #28960
N0-N1 #19523 NA's
DEVCON8m <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8m$FEMALE[DEVCON8m$ST04Q01==1] <- 1
DEVCON8m$FEMALE[DEVCON8m$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8m$PRESCHOOL[DEVCON8m$ST05Q01==1] <- 0
DEVCON8m$PRESCHOOL[DEVCON8m$ST05Q01==2] <- 1
DEVCON8m$PRESCHOOL[DEVCON8m$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8m$BOOK_N[DEVCON8m$ST28Q01==1]  <- 5
DEVCON8m$BOOK_N[DEVCON8m$ST28Q01==2]  <- 15
DEVCON8m$BOOK_N[DEVCON8m$ST28Q01==3]  <- 60
DEVCON8m$BOOK_N[DEVCON8m$ST28Q01==4]  <- 150
DEVCON8m$BOOK_N[DEVCON8m$ST28Q01==5]  <- 350
DEVCON8m$BOOK_N[DEVCON8m$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8m$PARPRESSURE[DEVCON8m$SC24Q01==1] <- 1
DEVCON8m$PARPRESSURE[DEVCON8m$SC24Q01==2] <- 0
DEVCON8m$PARPRESSURE[DEVCON8m$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8m$SC25Q05[is.na(DEVCON8m$SC25Q05)]  <- 0
DEVCON8m$SC25Q06[is.na(DEVCON8m$SC25Q06)]  <- 0
DEVCON8m$SC25Q07[is.na(DEVCON8m$SC25Q07)]  <- 0
DEVCON8m$SC25Q09[is.na(DEVCON8m$SC25Q09)]  <- 0
DEVCON8m$SC25Q10[is.na(DEVCON8m$SC25Q10)]  <- 0
DEVCON8m$SC25Q11[is.na(DEVCON8m$SC25Q11)]  <- 0
DEVCON8m$SC25Q12[is.na(DEVCON8m$SC25Q12)]  <- 0
DEVCON8m$FUNDMOM <-  DEVCON8m$SC25Q11
DEVCON8m$COUNCILMOM <- DEVCON8m$SC25Q10
DEVCON8m$VOLUMOM <- DEVCON8m$SC25Q05+DEVCON8m$SC25Q06+DEVCON8m$SC25Q07+DEVCON8m$SC25Q09+DEVCON8m$SC25Q12
DEVCON8m$VOLUMOM[DEVCON8m$VOLUMOM>100] <- 100 

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8m$TCM_STUASS[DEVCON8m$SC30Q01==1] <- 1
DEVCON8m$TCM_STUASS[DEVCON8m$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8m$TCM_PEER[DEVCON8m$SC30Q02==1] <- 1
DEVCON8m$TCM_PEER[DEVCON8m$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8m$TCM_OBSER[DEVCON8m$SC30Q03==1] <- 1
DEVCON8m$TCM_OBSER[DEVCON8m$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8m$TCM_INSPE[DEVCON8m$SC30Q04==1] <- 1
DEVCON8m$TCM_INSPE[DEVCON8m$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8m$TCH_MENT[DEVCON8m$SC39Q08==1] <- 1
DEVCON8m$TCH_MENT[DEVCON8m$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8m <- merge(DEVCON8m,SC31OUT.rda,by="NEWID")
DEVCON8m$TCH_INCENTV <- rescale(DEVCON8m$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

save(DEVCON8m, file="DEVCON8m.rda") 

R199 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R199
#Estimate Std. Error t value
#(Intercept)   406.82       2.89  140.91
#VIETNAM       103.93       5.40   19.23
#R-squared      22.75       2.06   11.07

R200 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R200
#Estimate Std. Error t value
#(Intercept)   369.23       8.40   43.93
#VIETNAM        74.73       4.52   16.55
#FEMALE         22.77       1.68   13.59
#PRESCHOOL      37.31       4.48    8.32
#REPEAT        -53.49       3.87  -13.81
#ST08Q01        -7.38       1.50   -4.91
#ST115Q01      -10.46       1.79   -5.85
#BOOK_N          0.06       0.01    5.65
#PARPRESSURE    11.99       4.15    2.89
#PCGIRLS        61.05      14.07    4.34
#VOLUMOM        -0.05       0.06   -0.81
#FUNDMOM         0.17       0.05    3.13
#COUNCILMOM     -0.18       0.06   -3.32
#R-squared      40.53       1.95   20.79

R201 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R201 # STRATIO increases gap
#VIETNAM: 75.00

R202 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R202 # STRATIO increases, PROPCERT decreases
#VIETNAM: 74.39 

R203 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R203 # STRATIO increases, PROPCERT decreases, PROPQUAL increases
#VIETNAM:  74.50

R204 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT","PROPQUAL",
                        "TCSHORT"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R204 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases
#VIETNAM: 73.56

R205 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT","PROPQUAL",
                        "TCSHORT","TCFOCST"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R205 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases, TCFOCST increases
#VIETNAM: 73.74

R206 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R206 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases
#VIETNAM: 71.93

R207 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R207 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER increases
#VIETNAM: 72.26

R208 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R208 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER increases, TCM_OBSER increases
#VIETNAM: 73.35

R209 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R209 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER increases, TCM_OBSER increases, TCM_INSPE increases
#VIETNAM: 74.58

R210 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                        "TCH_INCENTV"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R210 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER increases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV increases
#VIETNAM: 74.71

R211 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                        "TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R211 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER increases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV increases, TCH_MENT increases
#VIETNAM: 75.10

# Now testing all 3 variables that decreased the gap

R212 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R212
#Estimate Std. Error t value
#(Intercept)   349.93      12.47   28.05
#VIETNAM        71.91       4.55   15.81
#FEMALE         22.93       1.62   14.12
#PRESCHOOL      35.39       4.10    8.62
#REPEAT        -51.69       3.91  -13.22
#ST08Q01        -7.43       1.44   -5.16
#ST115Q01      -11.00       1.76   -6.25
#BOOK_N          0.06       0.01    5.42
#PARPRESSURE    10.79       4.09    2.64
#PCGIRLS        61.55      13.67    4.50
#VOLUMOM        -0.05       0.06   -0.92
#FUNDMOM         0.17       0.05    3.32
#COUNCILMOM     -0.19       0.06   -3.43
#PROPCERT        9.37       5.14    1.82
#TCSHORT        -3.74       1.91   -1.96
#TCM_STUASS     18.93       9.54    1.99
#R-squared      41.27       1.91   21.58

# Now testing all 8 variables that increased the gap

R213 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","STRATIO","PROPQUAL","TCFOCST","TCM_PEER","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8m,export=FALSE)
R213
#Estimate Std. Error t value
#(Intercept)   362.03      19.16   18.90
#VIETNAM        77.91       5.52   14.12
#FEMALE         22.73       1.66   13.68
#PRESCHOOL      35.52       4.59    7.74
#REPEAT        -53.07       3.85  -13.77
#ST08Q01        -7.31       1.50   -4.87
#ST115Q01      -10.18       1.76   -5.79
#BOOK_N          0.06       0.01    5.87
#PARPRESSURE    11.00       4.17    2.64
#PCGIRLS        58.69      14.10    4.16
#VOLUMOM        -0.05       0.06   -0.72
#FUNDMOM         0.16       0.05    2.98
#COUNCILMOM     -0.18       0.06   -3.21
#STRATIO         0.21       0.25    0.85
#PROPQUAL        7.77      14.34    0.54
#TCFOCST         0.83       2.25    0.37
#TCM_PEER       13.88       7.63    1.82
#TCM_OBSER      -5.19       5.98   -0.87
#TCM_INSPE      -2.81       4.92   -0.57
#TCH_INCENTV    -0.73       2.50   -0.29
#TCH_MENT       -8.23       7.84   -1.05
#R-squared      40.90       2.08   19.64

#################################################################
# 1.C DATA PREPARATION - TEACHER-RELATED VARIABLES (PART 2) ADDED
#################################################################

# 11 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, VOLUMOM, FUNDMOM, COUNCILMOM       

# 3 gap decreasing teacher-related variables (part 1): 
# PROPCERT, TCSHORT, TCM_STUASS

# Teacher related variables (part 2)
# SC18Q01-Q08
# SC39Q07 (Seeking student feedback, SC)

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q03","SC18Q04","SC18Q05",
                    "SC18Q06","SC18Q07","SC18Q08","SC39Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 #31796
N0-N1 #16687 NA's
DEVCON8n <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8n$FEMALE[DEVCON8n$ST04Q01==1] <- 1
DEVCON8n$FEMALE[DEVCON8n$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8n$PRESCHOOL[DEVCON8n$ST05Q01==1] <- 0
DEVCON8n$PRESCHOOL[DEVCON8n$ST05Q01==2] <- 1
DEVCON8n$PRESCHOOL[DEVCON8n$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8n$BOOK_N[DEVCON8n$ST28Q01==1]  <- 5
DEVCON8n$BOOK_N[DEVCON8n$ST28Q01==2]  <- 15
DEVCON8n$BOOK_N[DEVCON8n$ST28Q01==3]  <- 60
DEVCON8n$BOOK_N[DEVCON8n$ST28Q01==4]  <- 150
DEVCON8n$BOOK_N[DEVCON8n$ST28Q01==5]  <- 350
DEVCON8n$BOOK_N[DEVCON8n$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8n$PARPRESSURE[DEVCON8n$SC24Q01==1] <- 1
DEVCON8n$PARPRESSURE[DEVCON8n$SC24Q01==2] <- 0
DEVCON8n$PARPRESSURE[DEVCON8n$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8n$SC25Q05[is.na(DEVCON8n$SC25Q05)]  <- 0
DEVCON8n$SC25Q06[is.na(DEVCON8n$SC25Q06)]  <- 0
DEVCON8n$SC25Q07[is.na(DEVCON8n$SC25Q07)]  <- 0
DEVCON8n$SC25Q09[is.na(DEVCON8n$SC25Q09)]  <- 0
DEVCON8n$SC25Q10[is.na(DEVCON8n$SC25Q10)]  <- 0
DEVCON8n$SC25Q11[is.na(DEVCON8n$SC25Q11)]  <- 0
DEVCON8n$SC25Q12[is.na(DEVCON8n$SC25Q12)]  <- 0
DEVCON8n$FUNDMOM <-  DEVCON8n$SC25Q11
DEVCON8n$COUNCILMOM <- DEVCON8n$SC25Q10
DEVCON8n$VOLUMOM <- DEVCON8n$SC25Q05+DEVCON8n$SC25Q06+DEVCON8n$SC25Q07+DEVCON8n$SC25Q09+DEVCON8n$SC25Q12
DEVCON8n$VOLUMOM[DEVCON8n$VOLUMOM>100] <- 100 

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8n$TCM_STUASS[DEVCON8n$SC30Q01==1] <- 1
DEVCON8n$TCM_STUASS[DEVCON8n$SC30Q01==2] <- 0

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8n$ASS_PROG[DEVCON8n$SC18Q01==1] <- 1
DEVCON8n$ASS_PROG[DEVCON8n$SC18Q01==2] <- 0

DEVCON8n$ASS_PROM[DEVCON8n$SC18Q02==1] <- 1
DEVCON8n$ASS_PROM[DEVCON8n$SC18Q02==2] <- 0

DEVCON8n$ASS_INSTR[DEVCON8n$SC18Q03==1] <- 1
DEVCON8n$ASS_INSTR[DEVCON8n$SC18Q03==2] <- 0

DEVCON8n$ASS_NAT[DEVCON8n$SC18Q04==1] <- 1
DEVCON8n$ASS_NAT[DEVCON8n$SC18Q04==2] <- 0

DEVCON8n$ASS_SCH[DEVCON8n$SC18Q05==1] <- 1
DEVCON8n$ASS_SCH[DEVCON8n$SC18Q05==2] <- 0

DEVCON8n$ASS_TCH[DEVCON8n$SC18Q06==1] <- 1
DEVCON8n$ASS_TCH[DEVCON8n$SC18Q06==2] <- 0

DEVCON8n$ASS_CUR[DEVCON8n$SC18Q07==1] <- 1
DEVCON8n$ASS_CUR[DEVCON8n$SC18Q07==2] <- 0

DEVCON8n$ASS_OTH[DEVCON8n$SC18Q08==1] <- 1
DEVCON8n$ASS_OTH[DEVCON8n$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8n$STU_FEEDB[DEVCON8n$SC39Q07==1] <- 1
DEVCON8n$STU_FEEDB[DEVCON8n$SC39Q07==2] <- 0

save(DEVCON8n, file="DEVCON8n.rda") 

R214 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R214
#Estimate Std. Error t value
#(Intercept)   360.65      10.55   34.18
#VIETNAM        73.08       4.44   16.44
#FEMALE         23.29       1.64   14.20
#PRESCHOOL      35.52       3.83    9.28
#REPEAT        -53.64       3.56  -15.08
#ST08Q01        -8.51       1.45   -5.89
#ST115Q01      -10.12       1.78   -5.67
#BOOK_N          0.06       0.01    5.51
#PARPRESSURE    11.98       4.07    2.94
#PCGIRLS        44.88      10.26    4.37
#VOLUMOM        -0.05       0.06   -0.90
#FUNDMOM         0.15       0.05    2.83
#COUNCILMOM     -0.17       0.05   -3.09
#PROPCERT        7.88       4.81    1.64
#TCSHORT        -4.28       1.83   -2.34
#TCM_STUASS     17.27       8.09    2.13
#R-squared      40.68       1.81   22.44

R215 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R215 # ASS_PROG decreases
#VIETNAM: 72.95

R216 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R216 # ASS_PROG decreases, ASS_PROM decreases
# VIETNAM: 72.60

R217 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R217 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases
# VIETNAM: 72.64

R218 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R218 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases
# VIETNAM: 72.28

R219 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R219 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH increases
# VIETNAM: 72.30

R220 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R220 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH increases,
# ASS_TCH increases
# VIETNAM: 72.95

R221 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R221 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH increases,
# ASS_TCH increases, ASS_CUR decreases
# VIETNAM: 72.91

R222 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R222 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH increases,
# ASS_TCH increases, ASS_CUR decreases, ASS_OTH increases
# VIETNAM: 73.05

R223 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR","ASS_OTH",
                        "STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R223 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH increases,
# ASS_TCH increases, ASS_CUR decreases, ASS_OTH increases, STU_FEEDB decreases
# VIETNAM: 72.76

# Now testing all 5 variables that decreased the gap

R224 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R224
#Estimate Std. Error t value
#(Intercept)   343.45      17.49   19.64
#VIETNAM        71.78       4.46   16.08
#FEMALE         23.16       1.64   14.11
#PRESCHOOL      35.15       3.93    8.95
#REPEAT        -53.37       3.60  -14.83
#ST08Q01        -8.26       1.44   -5.74
#ST115Q01      -10.08       1.80   -5.60
#BOOK_N          0.06       0.01    5.60
#PARPRESSURE    10.92       4.37    2.50
#PCGIRLS        45.44      10.48    4.33
#VOLUMOM        -0.06       0.05   -1.10
#FUNDMOM         0.15       0.05    2.79
#COUNCILMOM     -0.17       0.05   -3.15
#PROPCERT        8.17       4.89    1.67
#TCSHORT        -3.98       1.79   -2.22
#TCM_STUASS     15.79       7.97    1.98
#ASS_PROG        8.68      17.06    0.51
#ASS_PROM        6.55       5.64    1.16
#ASS_NAT         3.95       5.52    0.72
#ASS_CUR        -2.88       7.76   -0.37
#STU_FEEDB       5.60       4.52    1.24
#R-squared      40.84       1.85   22.10

# Now testing all 4 variables that increased the gap

R225 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_INSTR","ASS_SCH" ,"ASS_TCH","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8n,export=FALSE)
R225
#Estimate Std. Error t value
#(Intercept)   361.06      11.58   31.17
#VIETNAM        73.82       4.60   16.03
#FEMALE         23.28       1.63   14.26
#PRESCHOOL      35.36       3.81    9.29
#REPEAT        -53.85       3.53  -15.26
#ST08Q01        -8.50       1.48   -5.73
#ST115Q01      -10.13       1.76   -5.76
#BOOK_N          0.06       0.01    5.54
#PARPRESSURE    11.94       4.18    2.86
#PCGIRLS        43.42      10.83    4.01
#VOLUMOM        -0.05       0.06   -0.94
#FUNDMOM         0.15       0.05    2.71
#COUNCILMOM     -0.17       0.05   -3.07
#PROPCERT        7.93       4.96    1.60
#TCSHORT        -4.28       1.82   -2.36
#TCM_STUASS     17.64       8.31    2.12
#ASS_INSTR       3.21       4.37    0.73
#ASS_SCH         3.49       8.79    0.40
#ASS_TCH        -4.70       5.20   -0.90
#ASS_OTH        -1.60       5.61   -0.29
#R-squared      40.72       1.81   22.48

#######################################################
# 1.D DATA PREPARATION - SCHOOLS-RELATED VARIABLES ADDED
#######################################################

# 11 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, VOLUMOM, FUNDMOM, COUNCILMOM       

# 3 gap decreasing teacher-related variables: 
# PROPCERT, TCSHORT, TCM_STUASS

# 5 gap decreasing teacher-related variables (part 2):
# ASS_PROG, ASS_PROM, ASS_NAT, ASS_CUR, STU_FEEDB

# 4. Schools
# SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SC03Q01, 
# CLSIZE (Class Size based on SC05, SC), SCHSIZE (based on SC07, SC)
# RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),
# SC16Q01-Q11
# SCMATEDU (Quality of educ. resources, SC), SCMATBUI (Quality of Physical Infrastructure, SC),
# LEADCOM (Framing Schools goal and curriculum, SC), LEADINST (Instructional Leadership, SC), 
# LEADPD (Promoting Development, SC), LEADTCH (Teacher Participation in Leadership, SC),
# SC19Q01 & SC19Q02 (if Student Achievement data is made available, SC), SCHAUTON (School autonomy, SC), 
# TCHPARTI (Teacher participation, SC), SC39Q03 (recording of student/teacher/test data, SC)
# SCHSEL (School Selectivity of students, SC)
# STUDCLIM (Student aspects of school climate, SC), TEACCLIM (teacher aspects of school climate, SC), 
# TCMORALE (Teacher Morale, SC)

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07","SC39Q07",
                    "SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SC16Q01","SC16Q02",
                    "SC16Q03","SC16Q04","SC16Q05","SC16Q06","SC16Q07","SC16Q08","SC16Q09","SC16Q10","SC16Q11",
                    "SCMATEDU","SCMATBUI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC19Q01",
                    "SC19Q02","SCHAUTON","TCHPARTI","SC39Q03","SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" )]
N1 <- NROW(na.omit(T1b)) 
N1 #22680
N0-N1 #25803 NA's
DEVCON8o <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8o$FEMALE[DEVCON8o$ST04Q01==1] <- 1
DEVCON8o$FEMALE[DEVCON8o$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8o$PRESCHOOL[DEVCON8o$ST05Q01==1] <- 0
DEVCON8o$PRESCHOOL[DEVCON8o$ST05Q01==2] <- 1
DEVCON8o$PRESCHOOL[DEVCON8o$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8o$BOOK_N[DEVCON8o$ST28Q01==1]  <- 5
DEVCON8o$BOOK_N[DEVCON8o$ST28Q01==2]  <- 15
DEVCON8o$BOOK_N[DEVCON8o$ST28Q01==3]  <- 60
DEVCON8o$BOOK_N[DEVCON8o$ST28Q01==4]  <- 150
DEVCON8o$BOOK_N[DEVCON8o$ST28Q01==5]  <- 350
DEVCON8o$BOOK_N[DEVCON8o$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8o$PARPRESSURE[DEVCON8o$SC24Q01==1] <- 1
DEVCON8o$PARPRESSURE[DEVCON8o$SC24Q01==2] <- 0
DEVCON8o$PARPRESSURE[DEVCON8o$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8o$SC25Q05[is.na(DEVCON8o$SC25Q05)]  <- 0
DEVCON8o$SC25Q06[is.na(DEVCON8o$SC25Q06)]  <- 0
DEVCON8o$SC25Q07[is.na(DEVCON8o$SC25Q07)]  <- 0
DEVCON8o$SC25Q09[is.na(DEVCON8o$SC25Q09)]  <- 0
DEVCON8o$SC25Q10[is.na(DEVCON8o$SC25Q10)]  <- 0
DEVCON8o$SC25Q11[is.na(DEVCON8o$SC25Q11)]  <- 0
DEVCON8o$SC25Q12[is.na(DEVCON8o$SC25Q12)]  <- 0
DEVCON8o$FUNDMOM <-  DEVCON8o$SC25Q11
DEVCON8o$COUNCILMOM <- DEVCON8o$SC25Q10
DEVCON8o$VOLUMOM <- DEVCON8o$SC25Q05+DEVCON8o$SC25Q06+DEVCON8o$SC25Q07+DEVCON8o$SC25Q09+DEVCON8o$SC25Q12
DEVCON8o$VOLUMOM[DEVCON8o$VOLUMOM>100] <- 100 

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8o$TCM_STUASS[DEVCON8o$SC30Q01==1] <- 1
DEVCON8o$TCM_STUASS[DEVCON8o$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8o$ASS_PROG[DEVCON8o$SC18Q01==1] <- 1
DEVCON8o$ASS_PROG[DEVCON8o$SC18Q01==2] <- 0

DEVCON8o$ASS_PROM[DEVCON8o$SC18Q02==1] <- 1
DEVCON8o$ASS_PROM[DEVCON8o$SC18Q02==2] <- 0

DEVCON8o$ASS_NAT[DEVCON8o$SC18Q04==1] <- 1
DEVCON8o$ASS_NAT[DEVCON8o$SC18Q04==2] <- 0

DEVCON8o$ASS_CUR[DEVCON8o$SC18Q07==1] <- 1
DEVCON8o$ASS_CUR[DEVCON8o$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8o$STU_FEEDB[DEVCON8o$SC39Q07==1] <- 1
DEVCON8o$STU_FEEDB[DEVCON8o$SC39Q07==2] <- 0

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8o$PRIVATESCL[DEVCON8o$SC01Q01==2] <- 1
DEVCON8o$PRIVATESCL[DEVCON8o$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8o$DUM_VILLAGE <- ifelse(DEVCON8o$SC03Q01==1,1,0)
DEVCON8o$DUM_SMLTOWN <- ifelse(DEVCON8o$SC03Q01==2,1,0)
DEVCON8o$DUM_TOWN    <- ifelse(DEVCON8o$SC03Q01==3,1,0)
DEVCON8o$DUM_CITY    <- ifelse(DEVCON8o$SC03Q01==4,1,0)
DEVCON8o$DUM_LRGCITY <- ifelse(DEVCON8o$SC03Q01==5,1,0)

DEVCON8o$TOWN <- DEVCON8o$DUM_SMLTOWN+DEVCON8o$DUM_TOWN
DEVCON8o$TOWN[DEVCON8o$TOWN>1] <- 1
DEVCON8o$CITY <- DEVCON8o$DUM_CITY+DEVCON8o$DUM_LRGCITY
DEVCON8o$CITY[DEVCON8o$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI leave as is

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8o$EXC1_BAND[DEVCON8o$SC16Q01==1] <- 1
DEVCON8o$EXC1_BAND[DEVCON8o$SC16Q01==2] <- 0

DEVCON8o$EXC2_PLAY[DEVCON8o$SC16Q02==1] <- 1
DEVCON8o$EXC2_PLAY[DEVCON8o$SC16Q02==2] <- 0

DEVCON8o$EXC3_NEWS[DEVCON8o$SC16Q03==1] <- 1
DEVCON8o$EXC3_NEWS[DEVCON8o$SC16Q03==2] <- 0

DEVCON8o$EXC4_VOLU[DEVCON8o$SC16Q04==1] <- 1
DEVCON8o$EXC4_VOLU[DEVCON8o$SC16Q04==2] <- 0

DEVCON8o$EXC5_MCLUB[DEVCON8o$SC16Q05==1] <- 1
DEVCON8o$EXC5_MCLUB[DEVCON8o$SC16Q05==2] <- 0

DEVCON8o$EXC6_MATHCOMP[DEVCON8o$SC16Q06==1] <- 1
DEVCON8o$EXC6_MATHCOMP[DEVCON8o$SC16Q06==2] <- 0

DEVCON8o$EXC7_CHESS[DEVCON8o$SC16Q07==1] <- 1
DEVCON8o$EXC7_CHESS[DEVCON8o$SC16Q07==2] <- 0

DEVCON8o$EXC8_ICTCB[DEVCON8o$SC16Q08==1] <- 1
DEVCON8o$EXC8_ICTCB[DEVCON8o$SC16Q08==2] <- 0

DEVCON8o$EXC9_ARTCB[DEVCON8o$SC16Q09==1] <- 1
DEVCON8o$EXC9_ARTCB[DEVCON8o$SC16Q09==2] <- 0

DEVCON8o$EXC10_SPORT[DEVCON8o$SC16Q10==1] <- 1
DEVCON8o$EXC10_SPORT[DEVCON8o$SC16Q10==2] <- 0

DEVCON8o$EXC11_UNICORN[DEVCON8o$SC16Q11==1] <- 1
DEVCON8o$EXC11_UNICORN[DEVCON8o$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8o$SCORE_PUBLIC[DEVCON8o$SC19Q01==1] <- 1
DEVCON8o$SCORE_PUBLIC[DEVCON8o$SC19Q01==2] <- 0

DEVCON8o$SCORE_AUTHRITS[DEVCON8o$SC19Q02==1] <- 1
DEVCON8o$SCORE_AUTHRITS[DEVCON8o$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8o$QUAL_RECORD[DEVCON8o$SC39Q03==1] <- 1
DEVCON8o$QUAL_RECORD[DEVCON8o$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

save(DEVCON8o, file="DEVCON8o.rda") 

R226 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R226
#Estimate Std. Error t value
#(Intercept)   411.67       3.01  136.69
#VIETNAM        99.71       5.94   16.79
#R-squared      20.85       2.20    9.47

R227 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R227
# VIETNAM: 70.26

R228 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R228 # PRIVATESCL increases the gap
#VIETNAM: 70.54

R229 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R229 # PRIVATESCL increases, SC02Q02 increases 
#VIETNAM: 72.29

R230 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02","DUM_VILLAGE"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R230 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases
#VIETNAM: 76.78 

R230a <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                         "PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN"),
                     weight="W_FSTUWT",
                     data=DEVCON8o,export=FALSE)
R230a # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases
#VIETNAM: 75.79

R231 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R231 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases
#VIETNAM: 70.62

R232 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R232 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, 
# SCHSIZE increases
#VIETNAM: 74.76 

R233 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R233 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases
#VIETNAM: 75.11

R234 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R234 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases
#VIETNAM: 74.35

R235 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R235 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases
#VIETNAM: 72.71

R236 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R236 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases
#VIETNAM: 72.56

# For the school activities, we first group them together depending on endowment, so we look at the means
mean1A <- t(sapply(DEVCON8o[c("EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN")], function(x) 
  unlist(t.test(x~DEVCON8o$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#                     estimate.mean in group 0      estimate.mean in group 1       
#EXC1_BAND                    0.5481531                0.1847625  
#EXC2_PLAY                    0.6286655                0.9062307    X 
#EXC3_NEWS                    0.5936310                0.5573720  
#EXC4_VOLU                    0.8514250                0.8454658  
#EXC5_MCLUB                   0.4975306                0.2501542 
#EXC6_MATHCOMP                0.6024797                0.8047502    X
#EXC7_CHESS                   0.3670645                0.1964837 
#EXC8_ICTCB                   0.5761910                0.1958667  
#EXC9_ARTCB                   0.7239942                0.4438618 
#EXC10_SPORT                  0.9549851                0.9984577    X
#EXC11_UNICORN                0.7692149                0.9466379    X

# So let's regress with the one where Vietnam does more (ie has a higher mean)

R237 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R237 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease
#VIETNAM: 69.22 

# go from here to R238

# just to double check the ones where Vietnam has less of (lower means)
R237a <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                         "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC1_BAND",
                         "EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB"),
                     weight="W_FSTUWT",
                     data=DEVCON8o,export=FALSE)
R237a # PRIVATESCL increases, SC02Q02 increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC1_BAND + EXC3_NEWS
# + EXC4_VOLU + EXC5_MCLUB + EXC7_CHESS + EXC8_ICTCB + EXC9_ARTCB) increases
#VIETNAM: 88.31

R238 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R238 # PRIVATESCL increases, SC02Q02 increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases
#VIETNAM: 68.67 

R239 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC",
                        "SCORE_AUTHRITS"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R239 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases
#VIETNAM: 69.45

R240 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R240 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases
#VIETNAM: 72.14

R241 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON","TCHPARTI"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R241 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases
#VIETNAM: 78.79

R242 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R242 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases
#VIETNAM: 79.85

R243 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R243 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases
#VIETNAM: 79.69

R244 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R244 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases
#VIETNAM: 79.91

R245 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R245 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases
#VIETNAM: 81.05

R246 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R246 # PRIVATESCL increases, SC02Q02 increases, DUm_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases
#VIETNAM: 80.96

R247 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R247 # PRIVATESCL increases, SC02Q02 increases, DUM_VILALGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases
#VIETNAM: 80.42  

R248 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R248 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases
#VIETNAM: 81.64

R249 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R249 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease,SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM decreases
#VIETNAM: 81.19

R250 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM",
                        "TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R250 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM decreases, TCMORALE increases
#VIETNAM: 82.66

# Now testing all 11 (or more accurately 14 with 4 combined) variables that decrease the gap

# TOWN, CLSIZE, COMPWEB, SCMATEDU, SCMATBUI, (EXC2_PLAY + EXC6_MATHCOMP + EXC10_SPORT + EXC11_UNICORN),
# SCORE_PUBLIC, LEADINST, QUAL_RECORD, SCHSEL, TEACCLIM

R251 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R251
# VIETNAM: 57.11

# Now testing for all 13 variables that increased the gap

# PRIVATESCL, SC02Q02, DUM_VILLAGE, SCHSIZE, RATCMP15, SCORE_AUTHRITS, SCHAUTON, TCHPARTI, LEADCOM, 
# LEADPD, LEADTCH, STUDCLIM, TCMORALE

R252 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","SCHSIZE","RATCMP15","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADPD","LEADTCH","STUDCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R252
# VIETNAM: 90.43

##### 2. TABLE 11 - THE ESTIMATED IMPACT OF 'VIETNAM' on READING PISA TEST SCORES

##########################
# Column (1) Vietnam dummy
##########################

READ0 <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
READ0

######################################
# Column (2) Vietnam dummy + Students
######################################

# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"

T1b <- DEVCON8[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #
N0-N1 # NA's
DEVCON8q <- DEVCON8[complete.cases(T1b),]
#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1

R5 <- pisa.reg.pv(pvlabel="READ", 
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R5
write.csv(R5, "Read_stu.csv")

###############################################
# Column (3) Vietnam dummy + Students + Parents 
###############################################

# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"

T1b <- DEVCON8[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #17506
N0-N1 #30977 NA's
DEVCON8q <- DEVCON8[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==1]  <- 5
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==2]  <- 15
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==3]  <- 60
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==4]  <- 150
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==5]  <- 350
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==1] <- 1
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==2] <- 0
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8q$SC25Q05[is.na(DEVCON8q$SC25Q05)]  <- 0
DEVCON8q$SC25Q06[is.na(DEVCON8q$SC25Q06)]  <- 0
DEVCON8q$SC25Q07[is.na(DEVCON8q$SC25Q07)]  <- 0
DEVCON8q$SC25Q09[is.na(DEVCON8q$SC25Q09)]  <- 0
DEVCON8q$SC25Q10[is.na(DEVCON8q$SC25Q10)]  <- 0
DEVCON8q$SC25Q11[is.na(DEVCON8q$SC25Q11)]  <- 0
DEVCON8q$SC25Q12[is.na(DEVCON8q$SC25Q12)]  <- 0
DEVCON8q$FUNDMOM <-  DEVCON8q$SC25Q11
DEVCON8q$COUNCILMOM <- DEVCON8q$SC25Q10
DEVCON8q$VOLUMOM <- DEVCON8q$SC25Q05+DEVCON8q$SC25Q06+DEVCON8q$SC25Q07+DEVCON8q$SC25Q09+DEVCON8q$SC25Q12
DEVCON8q$VOLUMOM[DEVCON8q$VOLUMOM>100] <- 100
DEVCON8q$DUTYMOM <-  DEVCON8q$SC25Q02

R6 <- pisa.reg.pv(pvlabel="READ",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R6
write.csv(R6, "Read_stu_par.csv")

##########################################################
# Column (4) Vietnam dummy + Students + Parents + Teachers
##########################################################

# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"
T1b <- DEVCON8[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07","SC39Q07")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8q <- DEVCON8[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==1]  <- 5
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==2]  <- 15
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==3]  <- 60
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==4]  <- 150
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==5]  <- 350
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==6]  <- 500

#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==1] <- 1
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==2] <- 0
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8q$SC25Q05[is.na(DEVCON8q$SC25Q05)]  <- 0
DEVCON8q$SC25Q06[is.na(DEVCON8q$SC25Q06)]  <- 0
DEVCON8q$SC25Q07[is.na(DEVCON8q$SC25Q07)]  <- 0
DEVCON8q$SC25Q09[is.na(DEVCON8q$SC25Q09)]  <- 0
DEVCON8q$SC25Q10[is.na(DEVCON8q$SC25Q10)]  <- 0
DEVCON8q$SC25Q11[is.na(DEVCON8q$SC25Q11)]  <- 0
DEVCON8q$SC25Q12[is.na(DEVCON8q$SC25Q12)]  <- 0
DEVCON8q$FUNDMOM <-  DEVCON8q$SC25Q11
DEVCON8q$COUNCILMOM <- DEVCON8q$SC25Q10
DEVCON8q$VOLUMOM <- DEVCON8q$SC25Q05+DEVCON8q$SC25Q06+DEVCON8q$SC25Q07+DEVCON8q$SC25Q09+DEVCON8q$SC25Q12
DEVCON8q$VOLUMOM[DEVCON8q$VOLUMOM>100] <- 100
DEVCON8q$DUTYMOM <-  DEVCON8q$SC25Q02

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==1] <- 1
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==1] <- 1
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==2] <- 0
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==1] <- 1
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==2] <- 0
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==1] <- 1
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==2] <- 0
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==1] <- 1
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==1] <- 1
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==2] <- 0

R7 <- pisa.reg.pv(pvlabel="READ",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL","REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                      "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R7
write.csv(R7, "Read_stu_par_tea.csv")

####################################################################
# Column (5) Vietnam dummy + Students + Parents + Teachers + Schools 
####################################################################

# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"
# "PCGIRLS","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCORE_PUBLIC","LEADINST","QUAL_RECORD","SCHSEL","TEACCLIM"

T1b <- DEVCON8[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07",
                    "SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM")]
N1 <- NROW(na.omit(T1b))
N1 #17506
N0-N1 #30977 NA's

DEVCON8q <- DEVCON8[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==1]  <- 5
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==2]  <- 15
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==3]  <- 60
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==4]  <- 150
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==5]  <- 350
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==6]  <- 500

#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==1] <- 1
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==2] <- 0
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8q$SC25Q05[is.na(DEVCON8q$SC25Q05)]  <- 0
DEVCON8q$SC25Q06[is.na(DEVCON8q$SC25Q06)]  <- 0
DEVCON8q$SC25Q07[is.na(DEVCON8q$SC25Q07)]  <- 0
DEVCON8q$SC25Q09[is.na(DEVCON8q$SC25Q09)]  <- 0
DEVCON8q$SC25Q10[is.na(DEVCON8q$SC25Q10)]  <- 0
DEVCON8q$SC25Q11[is.na(DEVCON8q$SC25Q11)]  <- 0
DEVCON8q$SC25Q12[is.na(DEVCON8q$SC25Q12)]  <- 0
DEVCON8q$FUNDMOM <-  DEVCON8q$SC25Q11
DEVCON8q$COUNCILMOM <- DEVCON8q$SC25Q10
DEVCON8q$VOLUMOM <- DEVCON8q$SC25Q05+DEVCON8q$SC25Q06+DEVCON8q$SC25Q07+DEVCON8q$SC25Q09+DEVCON8q$SC25Q12
DEVCON8q$VOLUMOM[DEVCON8q$VOLUMOM>100] <- 100
DEVCON8q$DUTYMOM <-  DEVCON8q$SC25Q02

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==1] <- 1
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==1] <- 1
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==2] <- 0
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==1] <- 1
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==2] <- 0
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==1] <- 1
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==2] <- 0
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==1] <- 1
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==1] <- 1
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables
DEVCON8q$DUM_SMLTOWN <- ifelse(DEVCON8q$SC03Q01==2,1,0)
DEVCON8q$DUM_TOWN    <- ifelse(DEVCON8q$SC03Q01==3,1,0)
DEVCON8q$TOWN <- DEVCON8q$DUM_SMLTOWN+DEVCON8q$DUM_TOWN
DEVCON8q$TOWN[DEVCON8q$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8q$EXC2_PLAY[DEVCON8q$SC16Q02==1] <- 1
DEVCON8q$EXC2_PLAY[DEVCON8q$SC16Q02==2] <- 0
DEVCON8q$EXC6_MATHCOMP[DEVCON8q$SC16Q06==1] <- 1
DEVCON8q$EXC6_MATHCOMP[DEVCON8q$SC16Q06==2] <- 0
DEVCON8q$EXC10_SPORT[DEVCON8q$SC16Q10==1] <- 1
DEVCON8q$EXC10_SPORT[DEVCON8q$SC16Q10==2] <- 0
DEVCON8q$EXC11_UNICORN[DEVCON8q$SC16Q11==1] <- 1
DEVCON8q$EXC11_UNICORN[DEVCON8q$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8q$SCORE_PUBLIC[DEVCON8q$SC19Q01==1] <- 1
DEVCON8q$SCORE_PUBLIC[DEVCON8q$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8q$QUAL_RECORD[DEVCON8q$SC39Q03==1] <- 1
DEVCON8q$QUAL_RECORD[DEVCON8q$SC39Q03==2] <- 0

R8 <- pisa.reg.pv(pvlabel="READ",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                      "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                      "PCGIRLS","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                      "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                      "QUAL_RECORD","SCHSEL","TEACCLIM"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R8
write.csv(R8, "Read_stu_par_tea_school.csv")

#### End of S3_FryerLevitt2.R
