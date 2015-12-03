#### S3_FryerLevitt3.R (Science Regression output)

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

# We create the regression output for Science used for Section 3: Regression Approach I: Fryer-Levitt
# for table 12. In the 1. Data preparation, we individually add variables to see if they decrease the 
# Vietnam dummy. All 'gap decreasing' variables will be retained in 2. to create the regression output (Table 12)

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
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","MISCED", "HISEI",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 35345
N0-N1 # 13138 NAs
DEVCON8t <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8t$FEMALE[DEVCON8t$ST04Q01==1] <- 1
DEVCON8t$FEMALE[DEVCON8t$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8t$PRESCHOOL[DEVCON8t$ST05Q01==1] <- 0
DEVCON8t$PRESCHOOL[DEVCON8t$ST05Q01==2] <- 1
DEVCON8t$PRESCHOOL[DEVCON8t$ST05Q01==3] <- 1

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
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==1]  <- 5
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==2]  <- 15
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==3]  <- 60
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==4]  <- 150
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==5]  <- 350
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8t$PARPRESSURE[DEVCON8t$SC24Q01==1] <- 1
DEVCON8t$PARPRESSURE[DEVCON8t$SC24Q01==2] <- 0
DEVCON8t$PARPRESSURE[DEVCON8t$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
DEVCON8t$SC25Q01[is.na(DEVCON8t$SC25Q01)]  <- 0
DEVCON8t$SC25Q02[is.na(DEVCON8t$SC25Q02)]  <- 0
DEVCON8t$SC25Q03[is.na(DEVCON8t$SC25Q03)]  <- 0
DEVCON8t$SC25Q04[is.na(DEVCON8t$SC25Q04)]  <- 0
DEVCON8t$SC25Q05[is.na(DEVCON8t$SC25Q05)]  <- 0
DEVCON8t$SC25Q06[is.na(DEVCON8t$SC25Q06)]  <- 0
DEVCON8t$SC25Q07[is.na(DEVCON8t$SC25Q07)]  <- 0
DEVCON8t$SC25Q08[is.na(DEVCON8t$SC25Q08)]  <- 0
DEVCON8t$SC25Q09[is.na(DEVCON8t$SC25Q09)]  <- 0
DEVCON8t$SC25Q10[is.na(DEVCON8t$SC25Q10)]  <- 0
DEVCON8t$SC25Q11[is.na(DEVCON8t$SC25Q11)]  <- 0
DEVCON8t$SC25Q12[is.na(DEVCON8t$SC25Q12)]  <- 0

#TIGERMOM
DEVCON8t$TIGERMOM  <- DEVCON8t$SC25Q01+DEVCON8t$SC25Q03
DEVCON8t$TIGERMOM[DEVCON8t$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8t$VOLUMOM <- DEVCON8t$SC25Q05+DEVCON8t$SC25Q06+DEVCON8t$SC25Q07+DEVCON8t$SC25Q09+DEVCON8t$SC25Q12
DEVCON8t$VOLUMOM[DEVCON8t$VOLUMOM>100] <- 100 

#TEACHMOM
DEVCON8t$TEACHMOM <- DEVCON8t$SC25Q08

#FUNDMOM
DEVCON8t$FUNDMOM <-  DEVCON8t$SC25Q11

#COUNCILMOM
DEVCON8t$COUNCILMOM <- DEVCON8t$SC25Q10

save(DEVCON8t, file="DEVCON8t.rda") 

R293 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R293
# Estimate Std. Error t value
# (Intercept)   400.65       2.39  167.49
# VIETNAM       129.63       4.87   26.60
# R-squared      32.03       2.09   15.34

R294 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R294 # FEMALE decreases
#VIETNAM:  129.60

R295 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R295 # FEMALE decreases, PRESCHOOL decreases
#VIETNAM:  120.24

R296 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R296 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases
#VIETNAM: 115.42

R297 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R297 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases
#VIETNAM: 112.99 

R298 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R298 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 113.06

R299 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R299 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases
#VIETNAM: 112.84

R300 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R300 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases
#VIETNAM: 120.02 

R301 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R301 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases
#VIETNAM: 120.85

# We only take HISEI and MISCED (not FISCED, HISCED) as they will be strongly correlated anyways

R302 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R302 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases
#VIETNAM: 121.36

R303 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R303 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases
#VIETNAM: 121.48 

R304 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R304 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 121.59

R305 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R305 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 121.48

R306 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R306 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 120.82

R307 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R307 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
#VIETNAM: 121.18 

R308 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R308 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases,
# VOLUMOM increases
#VIETNAM: 121.35

R309 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R309 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases
#VIETNAM: 123.64

R310 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R310 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM: 119.15

R311 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R311 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 114.35

R312 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM",
                        "PCGIRLS"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R312 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases, PCGIRLS decreases
#VIETNAM: 113.73

# Now testing all 10 variables that decreased the gap

R313 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "FUNDMOM", "COUNCILMOM","PCGIRLS"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R313
#Estimate Std. Error t value
#(Intercept)   382.77       6.65   57.52
#VIETNAM        99.68       4.42   22.54
#FEMALE         -6.82       1.42   -4.79
#PRESCHOOL      39.54       3.85   10.26
#REPEAT        -51.19       3.03  -16.91
#ST08Q01        -9.19       1.14   -8.07
#ST115Q01       -5.66       1.75   -3.23
#BOOK_N          0.08       0.01    6.34
#PARPRESSURE     5.81       4.16    1.39
#FUNDMOM         0.24       0.05    4.53
#COUNCILMOM     -0.19       0.05   -3.80
#PCGIRLS        37.82       9.56    3.96
#R-squared      44.76       1.71   26.23

# Now testing all 9 variables that increased the gap

R314 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "VOLUMOM", "TIGERMOM","TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R314
#Estimate Std. Error t value
#(Intercept)   434.68       4.39   99.12
#VIETNAM       137.99       3.98   34.67
#ST09Q01       -21.38       1.86  -11.50
#HISEI           0.41       0.05    8.50
#MISCED          2.18       0.52    4.16
#WEALTH          9.59       1.11    8.63
#CULTPOS        -4.01       0.91   -4.39
#HEDRES         11.66       0.89   13.13
#VOLUMOM         0.10       0.06    1.64
#TIGERMOM       -0.03       0.04   -0.74
#TEACHMOM       -0.13       0.06   -2.05
#R-squared      43.06       1.76   24.49

#################################################################
# 1.B DATA PREPARATION - TEACHER-RELATED VARIABLES (PART 1) ADDED
#################################################################

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM     

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
DEVCON8u <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8u$FEMALE[DEVCON8u$ST04Q01==1] <- 1
DEVCON8u$FEMALE[DEVCON8u$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8u$PRESCHOOL[DEVCON8u$ST05Q01==1] <- 0
DEVCON8u$PRESCHOOL[DEVCON8u$ST05Q01==2] <- 1
DEVCON8u$PRESCHOOL[DEVCON8u$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==1]  <- 5
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==2]  <- 15
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==3]  <- 60
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==4]  <- 150
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==5]  <- 350
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8u$PARPRESSURE[DEVCON8u$SC24Q01==1] <- 1
DEVCON8u$PARPRESSURE[DEVCON8u$SC24Q01==2] <- 0
DEVCON8u$PARPRESSURE[DEVCON8u$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8u$SC25Q10[is.na(DEVCON8u$SC25Q10)]  <- 0
DEVCON8u$SC25Q11[is.na(DEVCON8u$SC25Q11)]  <- 0
DEVCON8u$FUNDMOM <-  DEVCON8u$SC25Q11
DEVCON8u$COUNCILMOM <- DEVCON8u$SC25Q10

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8u$TCM_STUASS[DEVCON8u$SC30Q01==1] <- 1
DEVCON8u$TCM_STUASS[DEVCON8u$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8u$TCM_PEER[DEVCON8u$SC30Q02==1] <- 1
DEVCON8u$TCM_PEER[DEVCON8u$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8u$TCM_OBSER[DEVCON8u$SC30Q03==1] <- 1
DEVCON8u$TCM_OBSER[DEVCON8u$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8u$TCM_INSPE[DEVCON8u$SC30Q04==1] <- 1
DEVCON8u$TCM_INSPE[DEVCON8u$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8u$TCH_MENT[DEVCON8u$SC39Q08==1] <- 1
DEVCON8u$TCH_MENT[DEVCON8u$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8u <- merge(DEVCON8u,SC31OUT.rda,by="NEWID")
DEVCON8u$TCH_INCENTV <- rescale(DEVCON8u$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

save(DEVCON8u, file="DEVCON8u.rda") 

R315 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R315
#Estimate Std. Error t value
#(Intercept)   373.19       7.90   47.26
#VIETNAM       101.48       4.61   22.02
#FEMALE         -5.80       1.54   -3.77
#PRESCHOOL      38.82       4.22    9.20
#REPEAT        -51.13       3.60  -14.19
#ST08Q01        -8.09       1.32   -6.15
#ST115Q01       -7.51       1.78   -4.21
#BOOK_N          0.06       0.01    5.69
#PARPRESSURE     7.31       3.86    1.89
#PCGIRLS        56.34      12.84    4.39
#FUNDMOM         0.23       0.05    4.34
#COUNCILMOM     -0.23       0.05   -4.61
#R-squared      46.53       1.83   25.41

R316 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R316 # STRATIO increases gap
#VIETNAM: 101.64

R317 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R317 # STRATIO increases, PROPCERT decreases
#VIETNAM: 100.90 

R318 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R318 # STRATIO increases, PROPCERT decreases, PROPQUAL increases
#VIETNAM:  101.10 

R319 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT","PROPQUAL",
                        "TCSHORT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R319 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases
#VIETNAM: 100.96

R320 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT","PROPQUAL",
                        "TCSHORT","TCFOCST"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R320 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases, TCFOCST increases
#VIETNAM: 101.29

R321 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R321 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases
#VIETNAM: 99.69

R322 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R322 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases
#VIETNAM: 99.62

R323 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R323 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases
#VIETNAM: 101.16

R324 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R324 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases
#VIETNAM: 102.07

R325 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                        "TCH_INCENTV"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R325 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV increases
#VIETNAM: 102.16 

R326 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                        "TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R326 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV increases, TCH_MENT increases
#VIETNAM: 102.57

# Now testing all 4 variables that decreased the gap

R327 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R327
#Estimate Std. Error t value
#(Intercept)   352.69      12.70   27.77
#VIETNAM        99.36       4.74   20.96
#FEMALE         -5.52       1.49   -3.71
#PRESCHOOL      37.17       3.97    9.35
#REPEAT        -49.30       3.56  -13.86
#ST08Q01        -8.19       1.31   -6.24
#ST115Q01       -8.00       1.78   -4.50
#BOOK_N          0.06       0.01    5.59
#PARPRESSURE     6.32       3.96    1.60
#PCGIRLS        56.34      12.75    4.42
#FUNDMOM         0.21       0.05    4.11
#COUNCILMOM     -0.22       0.05   -4.39
#PROPCERT       11.94       5.09    2.34
#TCSHORT        -0.23       1.89   -0.12
#TCM_STUASS     17.36       8.21    2.11
#TCM_PEER       -0.98       6.07   -0.16
#R-squared      46.99       1.83   25.67

# Now testing all 7 variables that increased the gap

R328 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPQUAL","TCFOCST","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R328
#Estimate Std. Error t value
#(Intercept)   375.15      17.32   21.66
#VIETNAM       103.99       5.13   20.26
#FEMALE         -5.91       1.53   -3.86
#PRESCHOOL      36.76       4.31    8.52
#REPEAT        -51.52       3.92  -13.15
#ST08Q01        -8.22       1.32   -6.22
#ST115Q01       -7.15       1.80   -3.97
#BOOK_N          0.07       0.01    5.82
#PARPRESSURE     8.00       3.96    2.02
#PCGIRLS        53.36      13.15    4.06
#FUNDMOM         0.22       0.05    4.14
#COUNCILMOM     -0.22       0.05   -4.05
#STRATIO         0.08       0.22    0.36
#PROPQUAL       13.83      12.37    1.12
#TCFOCST         1.10       2.30    0.48
#TCM_OBSER      -7.54       5.27   -1.43
#TCM_INSPE      -2.61       4.19   -0.62
#TCH_INCENTV     0.31       2.44    0.13
#TCH_MENT       -6.09       6.32   -0.96
#R-squared      46.78       1.90   24.60

#################################################################
# 1.C DATA PREPARATION - TEACHER-RELATED VARIABLES (PART 2) ADDED
#################################################################

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM       

# 4 gap decreasing teacher-related variables (part 1): 
# PROPCERT, TCSHORT, TCM_STUASS, TCM_PEER

# Teacher related variables (part 2)
# SC18Q01-Q08
# SC39Q07 (Seeking student feedback, SC)

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q03","SC18Q04","SC18Q05",
                    "SC18Q06","SC18Q07","SC18Q08","SC39Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 #31723
N0-N1 #16760 NA's
DEVCON8v <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8v$FEMALE[DEVCON8v$ST04Q01==1] <- 1
DEVCON8v$FEMALE[DEVCON8v$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8v$PRESCHOOL[DEVCON8v$ST05Q01==1] <- 0
DEVCON8v$PRESCHOOL[DEVCON8v$ST05Q01==2] <- 1
DEVCON8v$PRESCHOOL[DEVCON8v$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==1]  <- 5
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==2]  <- 15
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==3]  <- 60
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==4]  <- 150
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==5]  <- 350
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8v$PARPRESSURE[DEVCON8v$SC24Q01==1] <- 1
DEVCON8v$PARPRESSURE[DEVCON8v$SC24Q01==2] <- 0
DEVCON8v$PARPRESSURE[DEVCON8v$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8v$SC25Q10[is.na(DEVCON8v$SC25Q10)]  <- 0
DEVCON8v$SC25Q11[is.na(DEVCON8v$SC25Q11)]  <- 0
DEVCON8v$FUNDMOM <-  DEVCON8v$SC25Q11
DEVCON8v$COUNCILMOM <- DEVCON8v$SC25Q10

#SC30Q01/SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8v$TCM_STUASS[DEVCON8v$SC30Q01==1] <- 1
DEVCON8v$TCM_STUASS[DEVCON8v$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8v$TCM_PEER[DEVCON8v$SC30Q02==1] <- 1
DEVCON8v$TCM_PEER[DEVCON8v$SC30Q02==2] <- 0

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8v$ASS_PROG[DEVCON8v$SC18Q01==1] <- 1
DEVCON8v$ASS_PROG[DEVCON8v$SC18Q01==2] <- 0

DEVCON8v$ASS_PROM[DEVCON8v$SC18Q02==1] <- 1
DEVCON8v$ASS_PROM[DEVCON8v$SC18Q02==2] <- 0

DEVCON8v$ASS_INSTR[DEVCON8v$SC18Q03==1] <- 1
DEVCON8v$ASS_INSTR[DEVCON8v$SC18Q03==2] <- 0

DEVCON8v$ASS_NAT[DEVCON8v$SC18Q04==1] <- 1
DEVCON8v$ASS_NAT[DEVCON8v$SC18Q04==2] <- 0

DEVCON8v$ASS_SCH[DEVCON8v$SC18Q05==1] <- 1
DEVCON8v$ASS_SCH[DEVCON8v$SC18Q05==2] <- 0

DEVCON8v$ASS_TCH[DEVCON8v$SC18Q06==1] <- 1
DEVCON8v$ASS_TCH[DEVCON8v$SC18Q06==2] <- 0

DEVCON8v$ASS_CUR[DEVCON8v$SC18Q07==1] <- 1
DEVCON8v$ASS_CUR[DEVCON8v$SC18Q07==2] <- 0

DEVCON8v$ASS_OTH[DEVCON8v$SC18Q08==1] <- 1
DEVCON8v$ASS_OTH[DEVCON8v$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8v$STU_FEEDB[DEVCON8v$SC39Q07==1] <- 1
DEVCON8v$STU_FEEDB[DEVCON8v$SC39Q07==2] <- 0

save(DEVCON8v, file="DEVCON8v.rda") 

R329 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R329
#Estimate Std. Error t value
#(Intercept)   362.92      11.03   32.89
#VIETNAM       101.04       4.62   21.89
#FEMALE         -4.91       1.51   -3.24
#PRESCHOOL      36.24       3.61   10.04
#REPEAT        -49.56       3.28  -15.09
#ST08Q01        -9.17       1.34   -6.87
#ST115Q01       -7.16       1.82   -3.93
#BOOK_N          0.06       0.01    6.09
#PARPRESSURE     7.20       3.91    1.84
#PCGIRLS        40.18       9.72    4.13
#FUNDMOM         0.21       0.05    4.13
#COUNCILMOM     -0.22       0.05   -4.50
#PROPCERT       10.26       4.79    2.14
#TCSHORT        -1.06       1.77   -0.60
#TCM_STUASS     17.37       6.97    2.49
#TCM_PEER       -1.85       5.80   -0.32
#R-squared      46.74       1.84   25.45

R330 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R330 # ASS_PROG decreases (same coefficient, we never had that case before, let's put as decrease then, which is what we had
# for MATH and READ as well)
#VIETNAM: 101.04

R331 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R331 # ASS_PROG decreases, ASS_PROM decreases
# VIETNAM: 100.80

R332 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R332 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases
# VIETNAM:  100.81

R333 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R333 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases
# VIETNAM: 100.21

R334 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R334 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases
# VIETNAM:  100.18

R335 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R335 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases
# VIETNAM: 101.48

R336 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R336 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR decreases
# VIETNAM: 101.25

R337 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R337 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR decreases, ASS_OTH increases
# VIETNAM: 101.41

R338 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR","ASS_OTH",
                        "STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R338 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR decreases, ASS_OTH increases, STU_FEEDB decreases
# VIETNAM: 101.09

# Now testing all 6 variables that decreased the gap:

R339 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R339
#Estimate Std. Error t value
#(Intercept)   357.85      16.83   21.27
#VIETNAM        99.58       4.61   21.61
#FEMALE         -5.08       1.53   -3.32
#PRESCHOOL      36.09       3.70    9.75
#REPEAT        -49.48       3.29  -15.03
#ST08Q01        -8.93       1.30   -6.86
#ST115Q01       -7.17       1.80   -3.97
#BOOK_N          0.06       0.01    6.12
#PARPRESSURE     6.32       4.05    1.56
#PCGIRLS        39.73       9.92    4.00
#TCM_PEER       -2.62       5.97   -0.44
#FUNDMOM         0.20       0.05    4.12
#COUNCILMOM     -0.22       0.05   -4.33
#PROPCERT       10.58       4.92    2.15
#TCSHORT        -0.79       1.73   -0.46
#TCM_STUASS     15.90       7.11    2.24
#ASS_PROG       -1.57      17.17   -0.09
#ASS_PROM        4.77       6.13    0.78
#ASS_NAT         5.92       4.68    1.27
#ASS_SCH         2.58       6.24    0.41
#ASS_CUR        -6.65       7.56   -0.88
#STU_FEEDB       5.18       4.32    1.20
#R-squared      46.89       1.85   25.38

# Now testing all 3 variables that increased the gap

R340 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_INSTR","ASS_TCH","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R340
#Estimate Std. Error t value
#(Intercept)   368.80      10.62   34.73
#VIETNAM       102.24       4.85   21.06
#FEMALE         -4.94       1.51   -3.27
#PRESCHOOL      35.83       3.49   10.25
#REPEAT        -49.99       3.24  -15.45
#ST08Q01        -9.17       1.35   -6.80
#ST115Q01       -7.07       1.79   -3.94
#BOOK_N          0.06       0.01    6.19
#PARPRESSURE     7.45       3.98    1.87
#PCGIRLS        38.92      10.22    3.81
#TCM_PEER       -0.99       6.06   -0.16
#FUNDMOM         0.20       0.05    3.99
#COUNCILMOM     -0.21       0.05   -4.47
#PROPCERT       10.45       4.89    2.14
#TCSHORT        -1.08       1.76   -0.61
#TCM_STUASS     18.04       6.97    2.59
#ASS_INSTR       1.84       4.37    0.42
#ASS_TCH        -9.35       5.31   -1.76
#ASS_OTH         0.41       4.42    0.09
#R-squared      46.81       1.85   25.34

#######################################################
# 1.D DATA PREPARATION - SCHOOLS-RELATED VARIABLES ADDED
#######################################################

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM       

# 4 gap decreasing teacher-related variables (part 1): 
# PROPCERT, TCSHORT, TCM_STUASS, TCM_PEER

# 6 gap decreasing teacher-related variables (part 2):
# ASS_PROG, ASS_PROM, ASS_NAT, ASS_SCH, ASS_CUR, STU_FEEDB

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
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SC16Q01","SC16Q02",
                    "SC16Q03","SC16Q04","SC16Q05","SC16Q06","SC16Q07","SC16Q08","SC16Q09","SC16Q10","SC16Q11",
                    "SCMATEDU","SCMATBUI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC19Q01",
                    "SC19Q02","SCHAUTON","TCHPARTI","SC39Q03","SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" )]
N1 <- NROW(na.omit(T1b)) 
N1 #22631
N0-N1 #25852 NA's
DEVCON8w <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8w$FEMALE[DEVCON8w$ST04Q01==1] <- 1
DEVCON8w$FEMALE[DEVCON8w$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8w$PRESCHOOL[DEVCON8w$ST05Q01==1] <- 0
DEVCON8w$PRESCHOOL[DEVCON8w$ST05Q01==2] <- 1
DEVCON8w$PRESCHOOL[DEVCON8w$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==1]  <- 5
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==2]  <- 15
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==3]  <- 60
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==4]  <- 150
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==5]  <- 350
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8w$PARPRESSURE[DEVCON8w$SC24Q01==1] <- 1
DEVCON8w$PARPRESSURE[DEVCON8w$SC24Q01==2] <- 0
DEVCON8w$PARPRESSURE[DEVCON8w$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8w$SC25Q10[is.na(DEVCON8w$SC25Q10)]  <- 0
DEVCON8w$SC25Q11[is.na(DEVCON8w$SC25Q11)]  <- 0
DEVCON8w$FUNDMOM <-  DEVCON8w$SC25Q11
DEVCON8w$COUNCILMOM <- DEVCON8w$SC25Q10

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8w$TCM_STUASS[DEVCON8w$SC30Q01==1] <- 1
DEVCON8w$TCM_STUASS[DEVCON8w$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8w$TCM_PEER[DEVCON8w$SC30Q02==1] <- 1
DEVCON8w$TCM_PEER[DEVCON8w$SC30Q02==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8w$ASS_PROG[DEVCON8w$SC18Q01==1] <- 1
DEVCON8w$ASS_PROG[DEVCON8w$SC18Q01==2] <- 0

DEVCON8w$ASS_PROM[DEVCON8w$SC18Q02==1] <- 1
DEVCON8w$ASS_PROM[DEVCON8w$SC18Q02==2] <- 0

DEVCON8w$ASS_NAT[DEVCON8w$SC18Q04==1] <- 1
DEVCON8w$ASS_NAT[DEVCON8w$SC18Q04==2] <- 0

DEVCON8w$ASS_SCH[DEVCON8w$SC18Q05==1] <- 1
DEVCON8w$ASS_SCH[DEVCON8w$SC18Q05==2] <- 0

DEVCON8w$ASS_CUR[DEVCON8w$SC18Q07==1] <- 1
DEVCON8w$ASS_CUR[DEVCON8w$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8w$STU_FEEDB[DEVCON8w$SC39Q07==1] <- 1
DEVCON8w$STU_FEEDB[DEVCON8w$SC39Q07==2] <- 0

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8w$PRIVATESCL[DEVCON8w$SC01Q01==2] <- 1
DEVCON8w$PRIVATESCL[DEVCON8w$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8w$DUM_VILLAGE <- ifelse(DEVCON8w$SC03Q01==1,1,0)
DEVCON8w$DUM_SMLTOWN <- ifelse(DEVCON8w$SC03Q01==2,1,0)
DEVCON8w$DUM_TOWN    <- ifelse(DEVCON8w$SC03Q01==3,1,0)
DEVCON8w$DUM_CITY    <- ifelse(DEVCON8w$SC03Q01==4,1,0)
DEVCON8w$DUM_LRGCITY <- ifelse(DEVCON8w$SC03Q01==5,1,0)

DEVCON8w$TOWN <- DEVCON8w$DUM_SMLTOWN+DEVCON8w$DUM_TOWN
DEVCON8w$TOWN[DEVCON8w$TOWN>1] <- 1
DEVCON8w$CITY <- DEVCON8w$DUM_CITY+DEVCON8w$DUM_LRGCITY
DEVCON8w$CITY[DEVCON8w$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI leave as is

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8w$EXC1_BAND[DEVCON8w$SC16Q01==1] <- 1
DEVCON8w$EXC1_BAND[DEVCON8w$SC16Q01==2] <- 0

DEVCON8w$EXC2_PLAY[DEVCON8w$SC16Q02==1] <- 1
DEVCON8w$EXC2_PLAY[DEVCON8w$SC16Q02==2] <- 0

DEVCON8w$EXC3_NEWS[DEVCON8w$SC16Q03==1] <- 1
DEVCON8w$EXC3_NEWS[DEVCON8w$SC16Q03==2] <- 0

DEVCON8w$EXC4_VOLU[DEVCON8w$SC16Q04==1] <- 1
DEVCON8w$EXC4_VOLU[DEVCON8w$SC16Q04==2] <- 0

DEVCON8w$EXC5_MCLUB[DEVCON8w$SC16Q05==1] <- 1
DEVCON8w$EXC5_MCLUB[DEVCON8w$SC16Q05==2] <- 0

DEVCON8w$EXC6_MATHCOMP[DEVCON8w$SC16Q06==1] <- 1
DEVCON8w$EXC6_MATHCOMP[DEVCON8w$SC16Q06==2] <- 0

DEVCON8w$EXC7_CHESS[DEVCON8w$SC16Q07==1] <- 1
DEVCON8w$EXC7_CHESS[DEVCON8w$SC16Q07==2] <- 0

DEVCON8w$EXC8_ICTCB[DEVCON8w$SC16Q08==1] <- 1
DEVCON8w$EXC8_ICTCB[DEVCON8w$SC16Q08==2] <- 0

DEVCON8w$EXC9_ARTCB[DEVCON8w$SC16Q09==1] <- 1
DEVCON8w$EXC9_ARTCB[DEVCON8w$SC16Q09==2] <- 0

DEVCON8w$EXC10_SPORT[DEVCON8w$SC16Q10==1] <- 1
DEVCON8w$EXC10_SPORT[DEVCON8w$SC16Q10==2] <- 0

DEVCON8w$EXC11_UNICORN[DEVCON8w$SC16Q11==1] <- 1
DEVCON8w$EXC11_UNICORN[DEVCON8w$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8w$SCORE_PUBLIC[DEVCON8w$SC19Q01==1] <- 1
DEVCON8w$SCORE_PUBLIC[DEVCON8w$SC19Q01==2] <- 0

DEVCON8w$SCORE_AUTHRITS[DEVCON8w$SC19Q02==1] <- 1
DEVCON8w$SCORE_AUTHRITS[DEVCON8w$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8w$QUAL_RECORD[DEVCON8w$SC39Q03==1] <- 1
DEVCON8w$QUAL_RECORD[DEVCON8w$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

save(DEVCON8w, file="DEVCON8w.rda")

# First, remember, we have a smaller data set (22631 data points) compared to when we first regressed the Vietnam PISA READ score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R341 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R341
# Estimate Std. Error t value
# (Intercept)   403.23       2.98  135.24
# VIETNAM       128.54       5.70   22.56
# R-squared      31.50       2.24   14.09

R342 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R342
# VIETNAM: 97.61

# So let's get started on the school-related variables

R343 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R343 # PRIVATESCL decreases the gap (does this make sense?)
#VIETNAM: 97.08 

R344 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R344 # PRIVATESCL decreases, SC02Q02 increases 
#VIETNAM: 98.25

R345 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02","DUM_VILLAGE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R345 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases
#VIETNAM: 101.60

R345a <- pisa.reg.pv(pvlabel="SCIE",  
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                         "PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN"),
                     weight="W_FSTUWT",
                     data=DEVCON8w,export=FALSE)
R345a # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases
#VIETNAM: 100.41

R346 <- pisa.reg.pv(pvlabel="SCIE",  
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R346 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases
#VIETNAM: 95.81

R347 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R347 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, 
# SCHSIZE increases
#VIETNAM: 99.82

R348 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R348 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases
#VIETNAM: 100.91 

R349 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R349 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases
#VIETNAM: 100.00

R350 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R350 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases
#VIETNAM: 98.49 

R351 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R351 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increases
#VIETNAM: 98.67

# For the school activities, we first group them together depending on endowment, so we look at the means
mean1A <- t(sapply(DEVCON8w[c("EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN")], function(x) 
  unlist(t.test(x~DEVCON8w$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#                       estimate.mean in group 0      estimate.mean in group 1      
#EXC1_BAND                    0.5495384                0.1847625  
#EXC2_PLAY                    0.6302543                0.9062307    x
#EXC3_NEWS                    0.5951313                0.5573720 
#EXC4_VOLU                    0.8510496                0.8454658  
#EXC5_MCLUB                   0.4987880                0.2501542 
#EXC6_MATHCOMP                0.6040023                0.8047502    x
#EXC7_CHESS                   0.3669091                0.1964837 
#EXC8_ICTCB                   0.5765640                0.1958667  
#EXC9_ARTCB                   0.7247408                0.4438618 
#EXC10_SPORT                  0.9548713                0.9984577    x
#EXC11_UNICORN                0.7700758                0.9466379    x

# So let's regress with the one where Vietnam does more (ie has a higher mean)

R352 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R352 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease
#VIETNAM: 96.46

R353 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R353 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases
#VIETNAM: 94.78 

R354 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC",
                        "SCORE_AUTHRITS"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R354 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases
#VIETNAM: 95.44

R355 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R355 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases
#VIETNAM: 101.52

R356 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON","TCHPARTI"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R356 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases
#VIETNAM: 108.21

R357 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R357 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases
#VIETNAM: 108.89

R358 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R358 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases
#VIETNAM: 108.84

R359 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R359 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases
#VIETNAM: 109.07 

R360 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R360 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases
#VIETNAM: 111.49 

R361 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R361 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases
#VIETNAM: 111.40

R362 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R362 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases
#VIETNAM:  111.08

R363 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R363 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases
#VIETNAM: 112.03

R364 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R364 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease,SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM decreases
#VIETNAM: 111.67

R365 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM",
                        "TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R365 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM decreases, TCMORALE increases
#VIETNAM: 112.67

# Now testing all 11 (or more accurately 14 with 4 combined) variables that decrease the gap

# PRIVATESCL, TOWN, CLSIZE, COMPWEB, SCMATEDU, (EXC2_PLAY + EXC6_MATHCOMP + EXC10_SPORT + EXC11_UNICORN),
# SCORE_PUBLIC, LEADINST, QUAL_RECORD, SCHSEL, TEACCLIM

R366 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R366
# VIETNAM: 84.36

# Now testing for all 13 variables that increased the gap

# PRIVATESCL, SC02Q02, DUM_VILLAGE, SCHSIZE, RATCMP15, SCORE_AUTHRITS, SCHAUTON, TCHPARTI, LEADCOM, 
# LEADPD, LEADTCH, STUDCLIM, TCMORALE

R367 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","SC02Q02",
                        "DUM_VILLAGE","SCHSIZE","RATCMP15","SCMATBUI","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADPD","LEADTCH","STUDCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R367
# VIETNAM: 117.84

##### 2. TABLE 12 - THE ESTIMATED IMPACT OF 'VIETNAM' on SCIENCE PISA TEST SCORES

##########################
# Column (1) Vietnam dummy
##########################

SCIE0 <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
SCIE0

######################################
# Column (2) Vietnam dummy + Students
######################################

# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==1] <- 1
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==1] <- 0
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==2] <- 1
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==3] <- 1

R9 <- pisa.reg.pv(pvlabel="SCIE",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8y,export=FALSE)
R9

write.csv(R9, "Science_stu.csv")

###############################################
# Column (3) Vietnam dummy + Students + Parents 
###############################################

# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==1] <- 1
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==1] <- 0
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==2] <- 1
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==1]  <- 5
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==2]  <- 15
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==3]  <- 60
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==4]  <- 150
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==5]  <- 350
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==1] <- 1
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==2] <- 0
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8y$SC25Q10[is.na(DEVCON8y$SC25Q10)]  <- 0
DEVCON8y$SC25Q11[is.na(DEVCON8y$SC25Q11)]  <- 0
DEVCON8y$FUNDMOM <-  DEVCON8y$SC25Q11
DEVCON8y$COUNCILMOM <- DEVCON8y$SC25Q10
DEVCON8y$DUTYMOM <- DEVCON8y$SC25Q02

R10 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "TCM_PEER","FUNDMOM","COUNCILMOM","DUTYMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R10

write.csv(R10, "Science_stu_par.csv")

##########################################################
# Column (4) Vietnam dummy + Students + Parents + Teachers
##########################################################

# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01",
                    "PROPCERT","TCSHORT","SC39Q07","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07",
                    "SC30Q01","SC30Q02")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's

DEVCON8y <- DEVCON8a[complete.cases(T1b),]

ST04Q01
#___________________________________________________________________________________________________________
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==1] <- 1
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==1] <- 0
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==2] <- 1
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==1]  <- 5
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==2]  <- 15
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==3]  <- 60
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==4]  <- 150
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==5]  <- 350
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==1] <- 1
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==2] <- 0
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8y$SC25Q10[is.na(DEVCON8y$SC25Q10)]  <- 0
DEVCON8y$SC25Q11[is.na(DEVCON8y$SC25Q11)]  <- 0
DEVCON8y$FUNDMOM <-  DEVCON8y$SC25Q11
DEVCON8y$COUNCILMOM <- DEVCON8y$SC25Q10
DEVCON8y$DUTYMOM <- DEVCON8y$SC25Q02
#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==1] <- 1
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==2] <- 0
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==1] <- 1
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8y$ASS_PROG[DEVCON8y$SC18Q01==1] <- 1
DEVCON8y$ASS_PROG[DEVCON8y$SC18Q01==2] <- 0
DEVCON8y$ASS_PROM[DEVCON8y$SC18Q02==1] <- 1
DEVCON8y$ASS_PROM[DEVCON8y$SC18Q02==2] <- 0
DEVCON8y$ASS_NAT[DEVCON8y$SC18Q04==1] <- 1
DEVCON8y$ASS_NAT[DEVCON8y$SC18Q04==2] <- 0
DEVCON8y$ASS_SCH[DEVCON8y$SC18Q05==1] <- 1
DEVCON8y$ASS_SCH[DEVCON8y$SC18Q05==2] <- 0
DEVCON8y$ASS_CUR[DEVCON8y$SC18Q07==1] <- 1
DEVCON8y$ASS_CUR[DEVCON8y$SC18Q07==2] <- 0
#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8y$STU_FEEDB[DEVCON8y$SC39Q07==1] <- 1
DEVCON8y$STU_FEEDB[DEVCON8y$SC39Q07==2] <- 0

R11 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                       "TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R11

write.csv(R11, "Science_stu_par_tea.csv")

####################################################################
# Column (5) Vietnam dummy + Students + Parents + Teachers + Schools 
####################################################################

# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
# "PCGIRLS","PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCORE_PUBLIC","LEADINST","QUAL_RECORD","SCHSEL","TEACCLIM"

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's

DEVCON8y <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==1] <- 1
DEVCON8y$FEMALE[DEVCON8y$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==1] <- 0
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==2] <- 1
DEVCON8y$PRESCHOOL[DEVCON8y$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==1]  <- 5
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==2]  <- 15
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==3]  <- 60
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==4]  <- 150
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==5]  <- 350
DEVCON8y$BOOK_N[DEVCON8y$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==1] <- 1
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==2] <- 0
DEVCON8y$PARPRESSURE[DEVCON8y$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8y$SC25Q10[is.na(DEVCON8y$SC25Q10)]  <- 0
DEVCON8y$SC25Q11[is.na(DEVCON8y$SC25Q11)]  <- 0
DEVCON8y$FUNDMOM <-  DEVCON8y$SC25Q11
DEVCON8y$COUNCILMOM <- DEVCON8y$SC25Q10
DEVCON8y$DUTYMOM <- DEVCON8y$SC25Q02
#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==1] <- 1
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==2] <- 0
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==1] <- 1
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==2] <- 0
#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8y$ASS_PROG[DEVCON8y$SC18Q01==1] <- 1
DEVCON8y$ASS_PROG[DEVCON8y$SC18Q01==2] <- 0
DEVCON8y$ASS_PROM[DEVCON8y$SC18Q02==1] <- 1
DEVCON8y$ASS_PROM[DEVCON8y$SC18Q02==2] <- 0
DEVCON8y$ASS_NAT[DEVCON8y$SC18Q04==1] <- 1
DEVCON8y$ASS_NAT[DEVCON8y$SC18Q04==2] <- 0
DEVCON8y$ASS_SCH[DEVCON8y$SC18Q05==1] <- 1
DEVCON8y$ASS_SCH[DEVCON8y$SC18Q05==2] <- 0
DEVCON8y$ASS_CUR[DEVCON8y$SC18Q07==1] <- 1
DEVCON8y$ASS_CUR[DEVCON8y$SC18Q07==2] <- 0
#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8y$STU_FEEDB[DEVCON8y$SC39Q07==1] <- 1
DEVCON8y$STU_FEEDB[DEVCON8y$SC39Q07==2] <- 0
#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8y$PRIVATESCL[DEVCON8y$SC01Q01==2] <- 1
DEVCON8y$PRIVATESCL[DEVCON8y$SC01Q01==1] <- 0
#SC03Q01/City size
#_________________________________________________________________________________________________________
# First we have to generate a series of dummy variables
DEVCON8y$DUM_SMLTOWN <- ifelse(DEVCON8y$SC03Q01==2,1,0)
DEVCON8y$DUM_TOWN    <- ifelse(DEVCON8y$SC03Q01==3,1,0)
DEVCON8y$TOWN <- DEVCON8y$DUM_SMLTOWN+DEVCON8y$DUM_TOWN
DEVCON8y$TOWN[DEVCON8y$TOWN>1] <- 1
#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8y$EXC2_PLAY[DEVCON8y$SC16Q02==1] <- 1
DEVCON8y$EXC2_PLAY[DEVCON8y$SC16Q02==2] <- 0
DEVCON8y$EXC6_MATHCOMP[DEVCON8y$SC16Q06==1] <- 1
DEVCON8y$EXC6_MATHCOMP[DEVCON8y$SC16Q06==2] <- 0
DEVCON8y$EXC10_SPORT[DEVCON8y$SC16Q10==1] <- 1
DEVCON8y$EXC10_SPORT[DEVCON8y$SC16Q10==2] <- 0
DEVCON8y$EXC11_UNICORN[DEVCON8y$SC16Q11==1] <- 1
DEVCON8y$EXC11_UNICORN[DEVCON8y$SC16Q11==2] <- 0
#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8y$SCORE_PUBLIC[DEVCON8y$SC19Q01==1] <- 1
DEVCON8y$SCORE_PUBLIC[DEVCON8y$SC19Q01==2] <- 0
#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8y$QUAL_RECORD[DEVCON8y$SC39Q03==1] <- 1
DEVCON8y$QUAL_RECORD[DEVCON8y$SC39Q03==2] <- 0

R12 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                       "TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                       "PCGIRLS","PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                       "QUAL_RECORD","SCHSEL","TEACCLIM"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R12

write.csv(R12, "Science_stu_par_tea_school.csv")

#### End of S3_FryerLevitt3.R


