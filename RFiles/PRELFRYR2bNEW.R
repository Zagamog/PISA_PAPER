# PISA2012_FL_part4
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 08/03/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# PLEASE NOTE THAT THIS IS THE FILE FOR THE MATH REGRESSIONS
# For the Reading and Science regressions please see PISA_2012_FL_part7 onwards
##################################################################################

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) (in part 1)
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES (in part 1)
# 3. PISA SCORES (in part 1)
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH (Math: part 2 - part 6)
##################################################################################

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

# Please be aware that many packages (eg. tables, intsvy) require additional packages to run. When trying to load
# the package, R will tell you which ones are missing. Overall you may need to download around 40 packages.

load("DEVCON8a.RDA")

# How big is our initital sample size? 
T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                         "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                         "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                         "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                         "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL")]
N1 <- NROW(na.omit(T1b)) 
N1 #25612
N0-N1 #22871 NA's

DEVCON8h <- DEVCON8a[complete.cases(T1b),]

# Student related variables

#ST04Q01
DEVCON8h$FEMALE[DEVCON8h$ST04Q01==1] <- 1
DEVCON8h$FEMALE[DEVCON8h$ST04Q01==2] <- 0

#ST05Q01

DEVCON8h$PRESCHOOL[DEVCON8h$ST05Q01==1] <- 0
DEVCON8h$PRESCHOOL[DEVCON8h$ST05Q01==2] <- 1
DEVCON8h$PRESCHOOL[DEVCON8h$ST05Q01==3] <- 1

#ST28Q01
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==1]  <- 5
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==2]  <- 15
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==3]  <- 60
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==4]  <- 150
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==5]  <- 350
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==6]  <- 500

#SC24Q01 
DEVCON8h$PARPRESSURE[DEVCON8h$SC24Q01==1] <- 1
DEVCON8h$PARPRESSURE[DEVCON8h$SC24Q01==2] <- 0
DEVCON8h$PARPRESSURE[DEVCON8h$SC24Q01==3] <- 0

#SC25Q01
DEVCON8h$SC25Q01[is.na(DEVCON8h$SC25Q01)]  <- 0
DEVCON8h$SC25Q02[is.na(DEVCON8h$SC25Q02)]  <- 0
DEVCON8h$SC25Q03[is.na(DEVCON8h$SC25Q03)]  <- 0
DEVCON8h$SC25Q04[is.na(DEVCON8h$SC25Q04)]  <- 0
DEVCON8h$SC25Q05[is.na(DEVCON8h$SC25Q05)]  <- 0
DEVCON8h$SC25Q06[is.na(DEVCON8h$SC25Q06)]  <- 0
DEVCON8h$SC25Q07[is.na(DEVCON8h$SC25Q07)]  <- 0
DEVCON8h$SC25Q08[is.na(DEVCON8h$SC25Q08)]  <- 0
DEVCON8h$SC25Q09[is.na(DEVCON8h$SC25Q09)]  <- 0
DEVCON8h$SC25Q10[is.na(DEVCON8h$SC25Q10)]  <- 0
DEVCON8h$SC25Q11[is.na(DEVCON8h$SC25Q11)]  <- 0
DEVCON8h$SC25Q12[is.na(DEVCON8h$SC25Q12)]  <- 0

# SC25Q01 is quite rich in information, so we create sub-variables
#TIGERMOM
DEVCON8h$TIGERMOM  <- DEVCON8h$SC25Q01+DEVCON8h$SC25Q03
DEVCON8h$TIGERMOM[DEVCON8h$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8h$VOLUMOM <- DEVCON8h$SC25Q05+DEVCON8h$SC25Q06+DEVCON8h$SC25Q07+DEVCON8h$SC25Q09+DEVCON8h$SC25Q12
DEVCON8h$VOLUMOM[DEVCON8h$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8h$TEACHMOM <- DEVCON8h$SC25Q08

#FUNDMOM
DEVCON8h$FUNDMOM <-  DEVCON8h$SC25Q11

#COUNCILMOM
DEVCON8h$COUNCILMOM <- DEVCON8h$SC25Q10

# Teacher related variables (part 1)

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8h$TCM_STUASS[DEVCON8h$SC30Q01==1] <- 1
DEVCON8h$TCM_STUASS[DEVCON8h$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8h$TCM_PEER[DEVCON8h$SC30Q02==1] <- 1
DEVCON8h$TCM_PEER[DEVCON8h$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8h$TCM_OBSER[DEVCON8h$SC30Q03==1] <- 1
DEVCON8h$TCM_OBSER[DEVCON8h$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8h$TCM_INSPE[DEVCON8h$SC30Q04==1] <- 1
DEVCON8h$TCM_INSPE[DEVCON8h$SC30Q04==2] <- 0

#SC39Q08
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8h$TCH_MENT[DEVCON8h$SC39Q08==1] <- 1
DEVCON8h$TCH_MENT[DEVCON8h$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8h <- merge(DEVCON8h,SC31OUT.rda,by="NEWID")
DEVCON8h$TCH_INCENTV <- rescale(DEVCON8h$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Teacher related variables (Part2)

# SC18Q01-Q08
DEVCON8h$ASS_PROG[DEVCON8h$SC18Q01==1] <- 1
DEVCON8h$ASS_PROG[DEVCON8h$SC18Q01==2] <- 0

DEVCON8h$ASS_PROM[DEVCON8h$SC18Q02==1] <- 1
DEVCON8h$ASS_PROM[DEVCON8h$SC18Q02==2] <- 0

DEVCON8h$ASS_INSTR[DEVCON8h$SC18Q03==1] <- 1
DEVCON8h$ASS_INSTR[DEVCON8h$SC18Q03==2] <- 0

DEVCON8h$ASS_NAT[DEVCON8h$SC18Q04==1] <- 1
DEVCON8h$ASS_NAT[DEVCON8h$SC18Q04==2] <- 0

DEVCON8h$ASS_SCH[DEVCON8h$SC18Q05==1] <- 1
DEVCON8h$ASS_SCH[DEVCON8h$SC18Q05==2] <- 0

DEVCON8h$ASS_TCH[DEVCON8h$SC18Q06==1] <- 1
DEVCON8h$ASS_TCH[DEVCON8h$SC18Q06==2] <- 0

DEVCON8h$ASS_CUR[DEVCON8h$SC18Q07==1] <- 1
DEVCON8h$ASS_CUR[DEVCON8h$SC18Q07==2] <- 0

DEVCON8h$ASS_OTH[DEVCON8h$SC18Q08==1] <- 1
DEVCON8h$ASS_OTH[DEVCON8h$SC18Q08==2] <- 0

#SC39Q07
DEVCON8h$STU_FEEDB[DEVCON8h$SC39Q07==1] <- 1
DEVCON8h$STU_FEEDB[DEVCON8h$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
DEVCON8h$COMP_USE[DEVCON8h$SC40Q01==1] <- 1
DEVCON8h$COMP_USE[DEVCON8h$SC40Q01==2] <- 0

DEVCON8h$TXT_BOOK[DEVCON8h$SC40Q02==1] <- 1
DEVCON8h$TXT_BOOK[DEVCON8h$SC40Q02==2] <- 0

DEVCON8h$STD_CUR[DEVCON8h$SC40Q03==1] <- 1
DEVCON8h$STD_CUR[DEVCON8h$SC40Q03==2] <- 0

# Now for the schools-related variables

#SC01Q01
DEVCON8h$PRIVATESCL[DEVCON8h$SC01Q01==2] <- 1
DEVCON8h$PRIVATESCL[DEVCON8h$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
DEVCON8h$DUM_VILLAGE <- ifelse(DEVCON8h$SC03Q01==1,1,0)
DEVCON8h$DUM_SMLTOWN <- ifelse(DEVCON8h$SC03Q01==2,1,0)
DEVCON8h$DUM_TOWN    <- ifelse(DEVCON8h$SC03Q01==3,1,0)
DEVCON8h$DUM_CITY    <- ifelse(DEVCON8h$SC03Q01==4,1,0)
DEVCON8h$DUM_LRGCITY <- ifelse(DEVCON8h$SC03Q01==5,1,0)

DEVCON8h$TOWN <- DEVCON8h$DUM_SMLTOWN+DEVCON8h$DUM_TOWN
DEVCON8h$TOWN[DEVCON8h$TOWN>1] <- 1
DEVCON8h$CITY <- DEVCON8h$DUM_CITY+DEVCON8h$DUM_LRGCITY
DEVCON8h$CITY[DEVCON8h$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

#SC16Q01-Q11
DEVCON8h$EXC1_BAND[DEVCON8h$SC16Q01==1] <- 1
DEVCON8h$EXC1_BAND[DEVCON8h$SC16Q01==2] <- 0

DEVCON8h$EXC2_PLAY[DEVCON8h$SC16Q02==1] <- 1
DEVCON8h$EXC2_PLAY[DEVCON8h$SC16Q02==2] <- 0

DEVCON8h$EXC3_NEWS[DEVCON8h$SC16Q03==1] <- 1
DEVCON8h$EXC3_NEWS[DEVCON8h$SC16Q03==2] <- 0

DEVCON8h$EXC4_VOLU[DEVCON8h$SC16Q04==1] <- 1
DEVCON8h$EXC4_VOLU[DEVCON8h$SC16Q04==2] <- 0

DEVCON8h$EXC5_MCLUB[DEVCON8h$SC16Q05==1] <- 1
DEVCON8h$EXC5_MCLUB[DEVCON8h$SC16Q05==2] <- 0

DEVCON8h$EXC6_MATHCOMP[DEVCON8h$SC16Q06==1] <- 1
DEVCON8h$EXC6_MATHCOMP[DEVCON8h$SC16Q06==2] <- 0

DEVCON8h$EXC7_CHESS[DEVCON8h$SC16Q07==1] <- 1
DEVCON8h$EXC7_CHESS[DEVCON8h$SC16Q07==2] <- 0

DEVCON8h$EXC8_ICTCB[DEVCON8h$SC16Q08==1] <- 1
DEVCON8h$EXC8_ICTCB[DEVCON8h$SC16Q08==2] <- 0

DEVCON8h$EXC9_ARTCB[DEVCON8h$SC16Q09==1] <- 1
DEVCON8h$EXC9_ARTCB[DEVCON8h$SC16Q09==2] <- 0

DEVCON8h$EXC10_SPORT[DEVCON8h$SC16Q10==1] <- 1
DEVCON8h$EXC10_SPORT[DEVCON8h$SC16Q10==2] <- 0

DEVCON8h$EXC11_UNICORN[DEVCON8h$SC16Q11==1] <- 1
DEVCON8h$EXC11_UNICORN[DEVCON8h$SC16Q11==2] <- 0

#SC20Q01
DEVCON8h$SCL_EXTR_CL[DEVCON8h$SC20Q01==1] <- 1
DEVCON8h$SCL_EXTR_CL[DEVCON8h$SC20Q01==2] <- 0

#SC19Q01-Q02
DEVCON8h$SCORE_PUBLIC[DEVCON8h$SC19Q01==1] <- 1
DEVCON8h$SCORE_PUBLIC[DEVCON8h$SC19Q01==2] <- 0

DEVCON8h$SCORE_AUTHRITS[DEVCON8h$SC19Q02==1] <- 1
DEVCON8h$SCORE_AUTHRITS[DEVCON8h$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" we leave as is

#SC39Q03
DEVCON8h$QUAL_RECORD[DEVCON8h$SC39Q03==1] <- 1
DEVCON8h$QUAL_RECORD[DEVCON8h$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" we leave as is

###############################################
# Column (2) Vietnam dummy + Students + Parents 
###############################################

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                  weight="W_FSTUWT",
                  data=DEVCON8h,export=FALSE)
R1

##########################################################
# Column (3) Vietnam dummy + Students + Parents + Teachers
##########################################################

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                      "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK"),
                  weight="W_FSTUWT",
                  data=DEVCON8h,export=FALSE)
R2

####################################################################
# Column (3) Vietnam dummy + Students + Parents + Teachers + Schools 
####################################################################

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                      "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                      "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                      "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                  weight="W_FSTUWT",
                  data=DEVCON8h,export=FALSE)
R3



