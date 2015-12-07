#### S4_OaxacaBlin2.R 

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
# SAppx_FryerLev  SECTION 5: Regressions following Fryer & Levitt (2004) on the rotated questionnaire
##################################################################################

# We create the Oaxaca-Blinder output used for Tables 15 - Shanghai. This file contains the code for a new masterfile (DEVCON9)
# instead of DEVCON8, which is a prerequisite for an Oaxaca-Blinder decomposition of Vietnam and Shanghai. In addition,
# this file contains the Oaxaca-Blinder decomposition with Shanghai. 

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

# 1. DATA PREPARATION FOR THE OAXACA-BLINDER DECOMPOSITION

# As mentioned above, we first need to create a new DEVCON9 file including Shanghai. It follows the lines of S0_Prelims
# to create DEVCON8. 

student.rda <- read.dta("stu.dta")
school.rda <- read.dta("sch.dta")

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

ALB_P$COUNTRY <- 1 # We numerate each country alphabetically, starting with Albania = 1, Colombia = 2, etc.
ALB_P$NEWID <- (ALB_P$COUNTRY*10000000)+((as.numeric(ALB_P$schoolid))*10000)+(as.numeric(ALB_P$stidstd))

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

# Shanghai

QCN_T <- filter(student.rda, cnt == "QCN") # filter by "cnt" and create a new student file just for 'Albania'
QCN_S <- filter(school.rda, cnt == "QCN") # filter by "cnt" and create a new school file just for 'Albania'
QCN_P <- merge(QCN_T, QCN_S, by = "schoolid") # merge both files into one file just for 'QCNania'
QCN_P$cnt <- QCN_P$cnt.x # we duplicate "cnt.x" as "cnt" (it is only called "cnt.x" since we merged QCN_S and QCN_T, not that in QCN_S and QCN_T it was called "cnt")
QCN_P$cnt.x <- NULL # we delete the column "cnt.x"
QCN_P$subnatio <- QCN_P$subnatio.x # we duplicate "subnatio.x" as "subnatio" (to have the same nomenclature as in the original data!)
QCN_P$subnatio.x <- NULL # we delete the column "subnatio.x"
QCN_P$stratum <- QCN_P$stratum.x # same as above
QCN_P$stratum.x <- NULL # same as above
QCN_P$oecd <- QCN_P$oecd.x # same as above
QCN_P$oecd.x <- NULL # same as above
QCN_P$nc <- QCN_P$nc.x  # same as above
QCN_P$nc.x <- NULL # same as above
QCN_P$COUNTRY <-9
QCN_P$NEWID <- (QCN_P$COUNTRY*10000000)+((as.numeric(QCN_P$schoolid))*10000)+(as.numeric(QCN_P$stidstd))

DEVCON8 <- rbind(ALB_P,COL_P,IDN_P,JOR_P,PER_P,THA_P,TUN_P,VNM_P,QCN_P) # combine all country specific files into the "DEVCON8" file, thanks to "dyplr" package

DEVCON8$VIETNAM[DEVCON8$COUNTRY==8] <- 1 # dummy takes value = 1, if the country is Vietnam
DEVCON8$VIETNAM[DEVCON8$COUNTRY!=8] <- 0 # dummy takes value = 0, if the country is not Vietnam

names(DEVCON8) <- toupper(names(DEVCON8)) 

DEVCON9a <- DEVCON8

### The Wealth variables

# EXAPPLM
# EXPUREM

# LHRS, MHRS
DEVCON9a$MHRS <- (DEVCON9a$MMINS)/60
DEVCON9a$LHRS <- (DEVCON9a$LMINS)/60

# HISEI, MISCED, WEALTH, CULTPOS, HEDRES

# BOOK_N
#ST28Q01
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==1]  <- 5
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==2]  <- 15
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==3]  <- 60
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==4]  <- 150
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==5]  <- 350
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==6]  <- 500

#TXT_BOOK
DEVCON9a$TXT_BOOK[DEVCON9a$SC40Q02==1] <- 1
DEVCON9a$TXT_BOOK[DEVCON9a$SC40Q02==2] <- 0

# CLSIZE
# TCFOCST
# TCMORALE
# TCHPARTI

# TOWN, CITY
DEVCON9a$DUM_SMLTOWN <- ifelse(DEVCON9a$SC03Q01==2,1,0)
DEVCON9a$DUM_TOWN    <- ifelse(DEVCON9a$SC03Q01==3,1,0)
DEVCON9a$DUM_CITY    <- ifelse(DEVCON9a$SC03Q01==4,1,0)
DEVCON9a$DUM_LRGCITY <- ifelse(DEVCON9a$SC03Q01==5,1,0)

DEVCON9a$TOWN <- DEVCON9a$DUM_SMLTOWN+DEVCON9a$DUM_TOWN
DEVCON9a$TOWN[DEVCON9a$TOWN>1] <- 1
DEVCON9a$CITY <- DEVCON9a$DUM_CITY+DEVCON9a$DUM_LRGCITY
DEVCON9a$CITY[DEVCON9a$CITY>1] <- 1

# PRIVATESCL
DEVCON9a$PRIVATESCL[DEVCON9a$SC01Q01==2] <- 1
DEVCON9a$PRIVATESCL[DEVCON9a$SC01Q01==1] <- 0

#SC02Q02
DEVCON9a$STU_FEES <- DEVCON9a$SC02Q02

# EXC1_BAND
DEVCON9a$EXC1_BAND[DEVCON9a$SC16Q01==1] <- 1
DEVCON9a$EXC1_BAND[DEVCON9a$SC16Q01==2] <- 0

# RATCMP15
# SCHAUTON
# TCHPARTI

### The ED Wealth variables

# PRESCHOOL 
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==1] <- 0
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==2] <- 1
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==3] <- 1

# ST08Q01

# NOREPEAT
DEVCON9a$NOREPEAT <- factor(-(DEVCON9a$REPEAT-1))

# SHRS
DEVCON9a$SHRS <- (DEVCON9a$SMINS)/60

# OUTMATH 
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==1] <- 0
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==2] <- 1
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==3] <- 3
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==4] <- 5
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==5] <- 7

# PARPRESSURE
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==1] <- 1
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==2] <- 0
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==3] <- 0

#TIGERMOM
DEVCON9a$TIGERMOM  <- DEVCON9a$SC25Q01+DEVCON9a$SC25Q03
DEVCON9a$TIGERMOM[DEVCON9a$TIGERMOM>100] <- 100 

#TEACHMOM
DEVCON9a$TEACHMOM <- DEVCON9a$SC25Q08

# PROPCERT

# SC35Q02

# TCH_INCENTV
# Important: please note that we created a new OECD style rasch index that measures incentives including Shanghai
S4_SC31DAT9 <- DEVCON9a[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(S4_SC31DAT9, file="S4_SC31DAT9.csv")
# We then generated Winsteps output using Winsteps control+data file SC31a.txt, that we are now reading back into R:
SC31OUTN.rda <- read.csv("S4_SC31DAT9OUT.csv")
DEVCON9a <- merge(DEVCON9a,SC31OUTN.rda,by="NEWID")
DEVCON9a$TCH_INCENTV <- rescale(DEVCON9a$WMLE, mean = 0, sd = 1,df=FALSE)

# TCM_INSPE
DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==1] <- 1
DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==2] <- 0

# TCM_OBSER
DEVCON9a$TCM_OBSER[DEVCON9a$SC30Q03==1] <- 1
DEVCON9a$TCM_OBSER[DEVCON9a$SC30Q03==2] <- 0

# COMP_USE
DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==1] <- 1
DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==2] <- 0

# STU_FEEDB
DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==1] <- 1
DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==2] <- 0

# EXC6_MATHCOMP
DEVCON9a$EXC6_MATHCOMP[DEVCON9a$SC16Q06==1] <- 1
DEVCON9a$EXC6_MATHCOMP[DEVCON9a$SC16Q06==2] <- 0

# SCMATBUI

# VILLAGE 
DEVCON9a$DUM_VILLAGE <- ifelse(DEVCON9a$SC03Q01==1,1,0)

# SCL_EXTR_CL
DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==1] <- 1
DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==2] <- 0

#SCORE_PUBLIC
DEVCON9a$SCORE_PUBLIC[DEVCON9a$SC19Q01==1] <- 1
DEVCON9a$SCORE_PUBLIC[DEVCON9a$SC19Q01==2] <- 0

### Creating the subsets

PISA_VN <- subset(DEVCON9a,CNT==c("VNM")) 
PISA_SH <- subset(DEVCON9a,CNT==c("QCN")) 

# 2. OAXACA-BLINDER DECOMPOSITION FOR SHANGHAI 

### Table 15: SHANGHAI

PISA_VNSH <- rbind(PISA_SH,PISA_VN)

# Very importantly, we do not create PISA_VNSH$OTHER variable, as we did before. For the Oaxaca-Blinder decomposition
# with Shanghai, we take Vietnam as the base country not OTHER (in this case SHANGHAI)

# Test for NA's: SHANGHAI
T1b <- PISA_SH[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_SH1 <- PISA_SH[complete.cases(T1b),]
summary(T1b)

# No need to drop variables

T1b <- PISA_VNSH[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNSH1 <- PISA_VNSH[complete.cases(T1b),]

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)

results8 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+PROPCERT+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| VIETNAM,
                   data=PISA_VNSH1, R=30,reg.fun=Marek) 

results8

#### End of S4_OaxacaBlin2.R

