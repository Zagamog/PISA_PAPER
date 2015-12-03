#### S4_OaxacaBlin1.R 

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
# S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
### S4_OaxacaBlin SECTION 4: Regressions following Oaxaca-Blinder approach
##################################################################################

# We create the Oaxaca-Blinder output used for Tables 13 - 15. This file contains code for all country
# comparisons, except Shanghai, which is coded in S4_OaxacaBlin2.R

# We are using the 'oaxaca' package by M. Hlavac (2015). Unfortunately, the 'oaxaca' package does not take into 
# account different Plausible Values for test scores, so as with the Kernel Plots, we took the PV1. This
# practice is commonly adopted by other researchers. We did contemplate and ran our own code, based on the
# 'intsvy' package for PISA to create aggreg. test scores and subsequently our own code for the Oaxaca approach, 
# but eventually decided to go with the much quicker 'oaxaca' packages, since outputs where not very different. 

# Loading R packages to process PISA data:

library(foreign) # to import and export data from R
library(epicalc) # for use in descriptives
library(stargazer) # For latex summary tables
library(sm) # for locally smoothed regressions
library(lme4) # To run mixed models
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(Hmisc)# What can I say about this Man Friday/factotum
library(TDMR)# Need this for tuning data mining in R - eg. detect column of constants in dataframe

library(intsvy) # For PISA analysis with PVs and BRRs
library(xlsx)# To generate MS-Excel output
library(psych) # for the rescaling of TCH_INCENTIV variable

library(ggplot2) # For graphs 
library(reshape2)
library(gmodels) # For PROC FREQ like tables

library(dplyr)
library(data.table)
library(oaxaca)
library(gcookbook)

# Please note the two additional packages 'oaxaca' and 'gcookbook'

# For a detailed explanation of our Oaxaca-Blinder approach, please see Section 4: Regression Approach II: Oaxaca-Blinder Decomposition
# from our paper.

load("DEVCON8.rda")
DEVCON8a <- DEVCON8

# 1. DATA PREPARATION FOR THE OAXACA-BLINDER DECOMPOSITION

# We tried to include a very large set of variables for the Oaxaca-Blinder decomposition. We split it in two parts:
# 1.ED WEALTH and 2.WEALTH. Ed Wealth contains variables, where a mean comparison shows higher mean endowments for Vietnam
# than the DEV7 countries and Wealth contains variables, where a mean comparison shows higher mean endowmetns for the 
# DEV7 countries than Vietnam. Furthermore, our aim was to create a sort of 'balanced' list of factors in each subcategory 
# (Students, Parents, Teachers, Schools), so that the ED WEALTH - Student list contains roughly the same amount of variables
# as the WEALTH - Student list.

### The Wealth variables

# EXAPPLM
# EXPUREM

# LHRS, MHRS
DEVCON8a$MHRS <- (DEVCON8a$MMINS)/60
DEVCON8a$LHRS <- (DEVCON8a$LMINS)/60

# HISEI, MISCED, WEALTH, CULTPOS, HEDRES

# BOOK_N
#ST28Q01
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==1]  <- 5
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==2]  <- 15
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==3]  <- 60
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==4]  <- 150
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==5]  <- 350
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==6]  <- 500

#TXT_BOOK
DEVCON8a$TXT_BOOK[DEVCON8a$SC40Q02==1] <- 1
DEVCON8a$TXT_BOOK[DEVCON8a$SC40Q02==2] <- 0

# CLSIZE
# TCFOCST
# TCMORALE
# TCHPARTI

# TOWN, CITY
DEVCON8a$DUM_SMLTOWN <- ifelse(DEVCON8a$SC03Q01==2,1,0)
DEVCON8a$DUM_TOWN    <- ifelse(DEVCON8a$SC03Q01==3,1,0)
DEVCON8a$DUM_CITY    <- ifelse(DEVCON8a$SC03Q01==4,1,0)
DEVCON8a$DUM_LRGCITY <- ifelse(DEVCON8a$SC03Q01==5,1,0)

DEVCON8a$TOWN <- DEVCON8a$DUM_SMLTOWN+DEVCON8a$DUM_TOWN
DEVCON8a$TOWN[DEVCON8a$TOWN>1] <- 1
DEVCON8a$CITY <- DEVCON8a$DUM_CITY+DEVCON8a$DUM_LRGCITY
DEVCON8a$CITY[DEVCON8a$CITY>1] <- 1

# PRIVATESCL
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==2] <- 1
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==1] <- 0

#SC02Q02
DEVCON8a$STU_FEES <- DEVCON8a$SC02Q02

# EXC1_BAND
DEVCON8a$EXC1_BAND[DEVCON8a$SC16Q01==1] <- 1
DEVCON8a$EXC1_BAND[DEVCON8a$SC16Q01==2] <- 0

# RATCMP15
# SCHAUTON
# TCHPARTI

### The ED Wealth variables

# PRESCHOOL 
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==1] <- 0
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==2] <- 1
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==3] <- 1

# ST08Q01

# NOREPEAT
DEVCON8a$NOREPEAT <- factor(-(DEVCON8a$REPEAT-1))

# SHRS
DEVCON8a$SHRS <- (DEVCON8a$SMINS)/60

# OUTMATH 
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==1] <- 0
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==2] <- 1
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==3] <- 3
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==4] <- 5
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==5] <- 7

# PARPRESSURE
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==1] <- 1
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==2] <- 0
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==3] <- 0

#TIGERMOM
DEVCON8a$TIGERMOM  <- DEVCON8a$SC25Q01+DEVCON8a$SC25Q03
DEVCON8a$TIGERMOM[DEVCON8a$TIGERMOM>100] <- 100 

#TEACHMOM
DEVCON8a$TEACHMOM <- DEVCON8a$SC25Q08

# PROPCERT

# SC35Q02

# TCH_INCENTV
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8a <- merge(DEVCON8a,SC31OUT.rda,by="NEWID")
DEVCON8a$TCH_INCENTV <- rescale(DEVCON8a$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# TCM_INSPE
DEVCON8a$TCM_INSPE[DEVCON8a$SC30Q04==1] <- 1
DEVCON8a$TCM_INSPE[DEVCON8a$SC30Q04==2] <- 0

# TCM_OBSER
DEVCON8a$TCM_OBSER[DEVCON8a$SC30Q03==1] <- 1
DEVCON8a$TCM_OBSER[DEVCON8a$SC30Q03==2] <- 0

# COMP_USE
DEVCON8a$COMP_USE[DEVCON8a$SC40Q01==1] <- 1
DEVCON8a$COMP_USE[DEVCON8a$SC40Q01==2] <- 0

# STU_FEEDB
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==1] <- 1
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==2] <- 0

# EXC6_MATHCOMP
DEVCON8a$EXC6_MATHCOMP[DEVCON8a$SC16Q06==1] <- 1
DEVCON8a$EXC6_MATHCOMP[DEVCON8a$SC16Q06==2] <- 0

# SCMATBUI

# VILLAGE 
DEVCON8a$DUM_VILLAGE <- ifelse(DEVCON8a$SC03Q01==1,1,0)

# SCL_EXTR_CL
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==1] <- 1
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==2] <- 0

#SCORE_PUBLIC
DEVCON8a$SCORE_PUBLIC[DEVCON8a$SC19Q01==1] <- 1
DEVCON8a$SCORE_PUBLIC[DEVCON8a$SC19Q01==2] <- 0

### Creating the subsets

PISA_VN <- subset(DEVCON8a,CNT==c("VNM")) 
PISA_AL <- subset(DEVCON8a,CNT==c("ALB")) 
PISA_CO <- subset(DEVCON8a,CNT==c("COL")) 
PISA_ID <- subset(DEVCON8a,CNT==c("IDN")) 
PISA_JO <- subset(DEVCON8a,CNT==c("JOR")) 
PISA_PE <- subset(DEVCON8a,CNT==c("PER")) 
PISA_TH <- subset(DEVCON8a,CNT==c("THA")) 
PISA_TU <- subset(DEVCON8a,CNT==c("TUN")) 
PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) 

# 2. OAXACA-BLINDER DECOMPOSITION FOR ALL COUNTRIES (EXCL SHANGHAI)

### Table 13: Mean values (please see S2_Endowments for output)

### Table 13: ALBANIA 

PISA_VNAL <- rbind(PISA_AL,PISA_VN)
PISA_VNAL$OTHER <- factor(-(PISA_VNAL$VIETNAM-1))

# For ALBANIA and all other countries, we will test if a certain variable has too many missing cases, which will 
# make the sample too small and thus not suited to infer on results. 

# Test for NA's: ALBANIA
T1b <- PISA_AL[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_AL1 <- PISA_AL[complete.cases(T1b),]
summary(T1b)

# For Albania, we need to drop: 
# SHRS (rotated part 2): 2111 NA's
# OUTMATH (rotated part 2): 1918 NA's
# EXAPPLM (rotates part 2): 1854 NA's
# EXPUREM (rotated part 2): 1879 NA's
# LHRS (rotated part 2): 2067 NA's
# MHRS (rotated part 2): 2056 NA's
# HISEI: 4562 NA's

# Of course, the rotated part has quite a few NA's (its only 2/3 sample size), so we are not dropping them, due to
# their potential explanatory value for the decomposition. We decided to only drop HISEI (it is from the non-rotated part).
# Arguably, our reasoning is arbitrary, of course, but we follow the rule that if a variable has more than 1000 NA's and 
# comes from the non-rotated part, we drop it. If a variable has more than 2500 NA's and comes from the rotated part, we drop it.
# Again, we are faced with a trade off between sample size and number of explanatory variables.

# Test for NA's: Vietnam
T1b <- PISA_VN[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_VN1 <- PISA_VN[complete.cases(T1b),]
summary(T1b)

# All non-rotated and rotated variables have sufficient observations, (following our rule).
# Overall, we only drop HISEI. 

T1b <- PISA_VNAL[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNAL1 <- PISA_VNAL[complete.cases(T1b),]

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+PROPCERT+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNAL1, R=30,reg.fun=Marek) 

results1

# We are looking at a two-fold decomposition. Weight = 1 (as per our equations, please see research paper pages 28,29)

### Table 13: COLOMBIA

PISA_VNCO <- rbind(PISA_CO,PISA_VN)
PISA_VNCO$OTHER <- factor(-(PISA_VNCO$VIETNAM-1))

# Test for NA's: COLOMBIA
T1b <- PISA_CO[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_CO1 <- PISA_CO[complete.cases(T1b),]
summary(T1b)

# PROPCERT (rotated part) has 2239 NA's

# We already tested for NA's: Vietnam above, so we will drop PROPCERT only.

T1b <- PISA_VNCO[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNCO1 <- PISA_VNCO[complete.cases(T1b),]

results2 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNCO1, R=30,reg.fun=Marek) 

results2

### Table 14: INDONESIA 

PISA_VNID <- rbind(PISA_ID,PISA_VN)
PISA_VNID$OTHER <- factor(-(PISA_VNID$VIETNAM-1))

# Test for NA's: Indonesia
T1b <- PISA_ID[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_ID1 <- PISA_ID[complete.cases(T1b),]
summary(T1b)

# No need to rop any variables following our rule 

T1b <- PISA_VNID[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNID1 <- PISA_VNID[complete.cases(T1b),]

results3 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+PROPCERT+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNID1, R=30,reg.fun=Marek) 

results3

### Table 14: JORDAN 

PISA_VNJO <- rbind(PISA_JO,PISA_VN)
PISA_VNJO$OTHER <- factor(-(PISA_VNJO$VIETNAM-1))

# Test for NA's: Jordan
T1b <- PISA_JO[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_JO1 <- PISA_JO[complete.cases(T1b),]
summary(T1b)

# PROPCERT (only slightly: 1159), HISEI (1628), STU_FEES (1845) fail our test, so will be dropped

T1b <- PISA_VNJO[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNJO1 <- PISA_VNJO[complete.cases(T1b),]

results4 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNJO1, R=30,reg.fun=Marek) 

results4

### Table 14: PERU 

PISA_VNPE <- rbind(PISA_PE,PISA_VN)
PISA_VNPE$OTHER <- factor(-(PISA_VNPE$VIETNAM-1))

# Test for NA's: Peru
T1b <- PISA_PE[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_PE1 <- PISA_PE[complete.cases(T1b),]
summary(T1b)

# We drop: PROPCERT, STU_FEES (1845)

T1b <- PISA_VNPE[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNPE1 <- PISA_VNPE[complete.cases(T1b),]

results5 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNPE1, R=30,reg.fun=Marek) 

results5

### Table 15: THAILAND 

PISA_VNTH <- rbind(PISA_TH,PISA_VN)
PISA_VNTH$OTHER <- factor(-(PISA_VNTH$VIETNAM-1))

# Test for NA's: Thailand
T1b <- PISA_TH[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_TH1 <- PISA_TH[complete.cases(T1b),]
summary(T1b)

# We do not drop any variables

T1b <- PISA_VNTH[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNTH1 <- PISA_VNTH[complete.cases(T1b),]

results6 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+PROPCERT+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNTH1, R=30,reg.fun=Marek) 

results6

### Table 15: TUNISIA 

PISA_VNTU <- rbind(PISA_TU,PISA_VN)
PISA_VNTU$OTHER <- factor(-(PISA_VNTU$VIETNAM-1))

# Test for NA's: Tunisia
T1b <- PISA_TU[, c("PRESCHOOL","ST08Q01","NOREPEAT1","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_TU1 <- PISA_TU[complete.cases(T1b),]
summary(T1b)

# We do not drop any variables

T1b <- PISA_VNTU[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNTU1 <- PISA_VNTU[complete.cases(T1b),]

results7 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+PROPCERT+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNTU1, R=30,reg.fun=Marek) 

results7

### TABLE 15: SHANGHAI - please see R file S4_OaxacaBlin2.R

#### End of S4_OaxacaBlin1.R


