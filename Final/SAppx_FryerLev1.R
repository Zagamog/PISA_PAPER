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
##### SAppx_FryerLev  SECTION 5: Regressions following Fryer & Levitt (2004) on the rotated questionnaire
######################################################################################################

# We perform a dummy variable regression analysis following Fryer & Levitt (2004) for the rotated student
# questionnaire variables for MATH (Table A2). The PISA dataset contains 3 rotated student questionnaire, each one
# having been adminstered to 2/3 of the students tested. Thus, in order to keep the sample size meaningfully
# large, we do not add all the rotated student questionnaire variables to the non-rotated variables (see S3_FryerLevitt)
# at once but in turn. We create three additional columns per subject.
# We add to all gap-decreasing non-rotated variables the rotated part 1 variables in turn to determine which are gap decreasing
# Then we create a sample just on gap-decreasing variables (from non-rtoated and rotated part 1) and report those in Table A2

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

T0 <- DEVCON8[, c("VIETNAM")] 
N0<- NROW(na.omit(T0)) 
N0

########################################################################################################
# TABLE A2 COLUMN (6) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 1 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 1 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 1 variables

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "MATWKETH","PERSEV","OPENPS","INTMAT","INSTMOT","SUBNORM","MATHEFF","FAILMAT",
                    "MATINTFC","MATBEH")]
N1 <- NROW(na.omit(T1b)) 
N1 #15660
N0-N1 #32823 NA's
DEVCON8i <- DEVCON8a[complete.cases(T1b),]

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10

# Please note: We only decided to include DUTYMOM at a later stage, so it is not part of the one-by-one trial
# exercise but included in the final regression (column (1) in Table A2)

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==1] <- 1
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==1] <- 1
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==1] <- 1
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==2] <- 0

DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==1] <- 1
DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==2] <- 0

DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==1] <- 1
DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==1] <- 1
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==1] <- 1
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==2] <- 0

DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==1] <- 1
DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$DUM_SMLTOWN <- ifelse(DEVCON8i$SC03Q01==2,1,0)
DEVCON8i$DUM_TOWN    <- ifelse(DEVCON8i$SC03Q01==3,1,0)

DEVCON8i$TOWN <- DEVCON8i$DUM_SMLTOWN+DEVCON8i$DUM_TOWN
# DEVCON8i$TOWN[DEVCON8i$TOWN>1] <- 1 do not need to do this since mutually exclusive

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==1] <- 1
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==2] <- 0

DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==1] <- 1
DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==2] <- 0

DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==1] <- 1
DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==2] <- 0

DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==1] <- 1
DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==1] <- 1
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==1] <- 1
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==1] <- 1
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==2] <- 0

# Let's try our regression with all gap decreasing variables before we add the rotated parts

R104 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R104

# So with all the gap decreasing non rotated variables, the Vietnam dummy goes down to 73.29

R105 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R105 # MATWKETH decreases
#VIETNAM: 73.02

R106 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R106 # MATWKETH decreases, INSTMOT decreases
#VIETNAM: 71.83

R107 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R107 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases
#VIETNAM: 71.23

R108 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R108 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!)
#VIETNAM: 64.06

R109 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R109 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
#VIETNAM: 61.67

R110 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R110 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases
#VIETNAM: 63.19

R111 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R111 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases
#VIETNAM: 61.83

R112 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R112 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases, MATBEH increases
#VIETNAM: 62.14

R113 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R113 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases, MATBEH increases, PERSEV decreases
#VIETNAM: 61.54

R114 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH","PERSEV","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R114 # MATWKETH decreases, INSTMOT decreases, INTMAT decreases, SUBNORM decreases (drastically!), MATHEF decreases
# FAILMAT increases, MATINTFC decreases, MATBEH increases, PERSEV decreases, OPENPS increases
#VIETNAM: 64.38

# Let's check all the gap-decreasing variables together:

R115 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","MATINTFC","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R115

### 2. CREATING COLUMN (6) NON-ROTATED VARIABLES + ROTATED PART 1 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "MATWKETH","INSTMOT","INTMAT","SUBNORM","MATHEFF","MATINTFC","PERSEV")]
N1 <- NROW(na.omit(T1b)) 
N1 # 15775
N0-N1
DEVCON8i <- DEVCON8a[complete.cases(T1b),]

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10
DEVCON8i$DUTYMOM <- DEVCON8i$SC25Q02

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==1] <- 1
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==1] <- 1
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==1] <- 1
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==2] <- 0

DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==1] <- 1
DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==2] <- 0

DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==1] <- 1
DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==1] <- 1
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==1] <- 1
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==2] <- 0

DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==1] <- 1
DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$DUM_SMLTOWN <- ifelse(DEVCON8i$SC03Q01==2,1,0)
DEVCON8i$DUM_TOWN    <- ifelse(DEVCON8i$SC03Q01==3,1,0)

DEVCON8i$TOWN <- DEVCON8i$DUM_SMLTOWN+DEVCON8i$DUM_TOWN
# DEVCON8i$TOWN[DEVCON8i$TOWN>1] <- 1 do not need to do this since mutually exclusive

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==1] <- 1
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==2] <- 0

DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==1] <- 1
DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==2] <- 0

DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==1] <- 1
DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==2] <- 0

DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==1] <- 1
DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==1] <- 1
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==1] <- 1
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==1] <- 1
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==2] <- 0

R115b <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "FUNDMOM", "COUNCILMOM","DUTYMOM", "PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","PCGIRLS","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INSTMOT","INTMAT",
                        "SUBNORM","MATHEFF","MATINTFC","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R115b

write.csv(R115b, "Math_rot1.csv")

########################################################################################################
# TABLE A2 COLUMN (7) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 2 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 2 variables

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST55Q02","EXAPPLM","EXPUREM","FAMCONC","LMINS","MMINS","SMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 11944
N0-N1 #36539 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

#ST55Q02
#________________________________________________________________________________________________________
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==1] <- 0
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==2] <- 1
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==3] <- 3
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==4] <- 5
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==5] <- 7

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8j$SHRS <- (DEVCON8j$SMINS)/60
DEVCON8j$MHRS <- (DEVCON8j$MMINS)/60
DEVCON8j$LHRS <- (DEVCON8j$LMINS)/60

# Let's test just the non rotated

R135 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R135 # Vietnam 72.62

R136 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R136 # OUTMATH decreases
# Vietnam 69.59 

R140 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXAPPLM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R140 # OUTMATH decreases, EXAPPLM increases
# Vietnam 70.95 

R141 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXAPPLM","EXPUREM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R141 # OUTMATH decreases, EXAPPLM increases, EXPUREM decreases
# Vietnam 66.83

R142 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXAPPLM","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R142 # OUTMATH decreases, EXAPPLM increases, EXPUREM decreases, FAMCONC decreases drastically (-32%)
# Vietnam 45.38

# Testing all gap decreasing variables: 

R143 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                         "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                         "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                         "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                         "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXPUREM","FAMCONC"),
                     weight="W_FSTUWT",
                     data=DEVCON8j,export=FALSE)
R143

### 2. CREATING COLUMN (7) NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST55Q02","EXPUREM","FAMCONC")]
N1 <- NROW(na.omit(T1b)) 
N1 # 14855
N0-N1 # 33628 NA's
DEVCON8j <- DEVCON8a[complete.cases(T1b),]

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==1] <- 0
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==2] <- 1
DEVCON8j$PRESCHOOL[DEVCON8j$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==1]  <- 5
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==2]  <- 15
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==3]  <- 60
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==4]  <- 150
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==5]  <- 350
DEVCON8j$BOOK_N[DEVCON8j$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==1] <- 1
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==2] <- 0
DEVCON8j$PARPRESSURE[DEVCON8j$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8j$SC25Q10[is.na(DEVCON8j$SC25Q10)]  <- 0
DEVCON8j$SC25Q11[is.na(DEVCON8j$SC25Q11)]  <- 0
DEVCON8j$FUNDMOM <- DEVCON8j$SC25Q11
DEVCON8j$COUNCILMOM <- DEVCON8j$SC25Q10
DEVCON8j$DUTYMOM <- DEVCON8j$SC25Q02

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==1] <- 1
DEVCON8j$TCM_STUASS[DEVCON8j$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==1] <- 1
DEVCON8j$TCM_PEER[DEVCON8j$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==1] <- 1
DEVCON8j$ASS_PROG[DEVCON8j$SC18Q01==2] <- 0

DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==1] <- 1
DEVCON8j$ASS_PROM[DEVCON8j$SC18Q02==2] <- 0

DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==1] <- 1
DEVCON8j$ASS_SCH[DEVCON8j$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==1] <- 1
DEVCON8j$STU_FEEDB[DEVCON8j$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==1] <- 1
DEVCON8j$COMP_USE[DEVCON8j$SC40Q01==2] <- 0

DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==1] <- 1
DEVCON8j$TXT_BOOK[DEVCON8j$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8j$DUM_SMLTOWN <- ifelse(DEVCON8j$SC03Q01==2,1,0)
DEVCON8j$DUM_TOWN    <- ifelse(DEVCON8j$SC03Q01==3,1,0)

DEVCON8j$TOWN <- DEVCON8j$DUM_SMLTOWN+DEVCON8j$DUM_TOWN
DEVCON8j$TOWN[DEVCON8j$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==1] <- 1
DEVCON8j$EXC2_PLAY[DEVCON8j$SC16Q02==2] <- 0

DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==1] <- 1
DEVCON8j$EXC6_MATHCOMP[DEVCON8j$SC16Q06==2] <- 0

DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==1] <- 1
DEVCON8j$EXC10_SPORT[DEVCON8j$SC16Q10==2] <- 0

DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==1] <- 1
DEVCON8j$EXC11_UNICORN[DEVCON8j$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==1] <- 1
DEVCON8j$SCL_EXTR_CL[DEVCON8j$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==1] <- 1
DEVCON8j$SCORE_PUBLIC[DEVCON8j$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==1] <- 1
DEVCON8j$QUAL_RECORD[DEVCON8j$SC39Q03==2] <- 0

#ST55Q02
#________________________________________________________________________________________________________
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==1] <- 0
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==2] <- 1
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==3] <- 3
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==4] <- 5
DEVCON8j$OUTMATH[DEVCON8j$ST55Q02==5] <- 7


R144 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "FUNDMOM", "COUNCILMOM","DUTYMOM", "PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","PCGIRLS","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R144

write.csv(R144, "Math_rot2.csv")

########################################################################################################
# TABLE A2 COLUMN (8) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 3 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 3 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 3 variables

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST91Q03","SCMAT","ANXMAT","BELONG","ATSCHL","ATTLNACT","ST91Q02","MTSUP",
                    "STUDREL","ST91Q04","TCHBEHTD","TCHBEHSO","TCHBEHFA","COGACT","CLSMAN","DISCLIMA")]
N1 <- NROW(na.omit(T1b)) 
N1 #15422
N0-N1 #33061 NA's
DEVCON8k <- DEVCON8a[complete.cases(T1b),]

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8k$PRESCHOOL[DEVCON8k$ST05Q01==1] <- 0
DEVCON8k$PRESCHOOL[DEVCON8k$ST05Q01==2] <- 1
DEVCON8k$PRESCHOOL[DEVCON8k$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==1]  <- 5
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==2]  <- 15
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==3]  <- 60
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==4]  <- 150
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==5]  <- 350
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8k$PARPRESSURE[DEVCON8k$SC24Q01==1] <- 1
DEVCON8k$PARPRESSURE[DEVCON8k$SC24Q01==2] <- 0
DEVCON8k$PARPRESSURE[DEVCON8k$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8k$SC25Q10[is.na(DEVCON8k$SC25Q10)]  <- 0
DEVCON8k$SC25Q11[is.na(DEVCON8k$SC25Q11)]  <- 0
DEVCON8k$FUNDMOM <- DEVCON8k$SC25Q11
DEVCON8k$COUNCILMOM <- DEVCON8k$SC25Q10

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8k$TCM_STUASS[DEVCON8k$SC30Q01==1] <- 1
DEVCON8k$TCM_STUASS[DEVCON8k$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8k$TCM_PEER[DEVCON8k$SC30Q02==1] <- 1
DEVCON8k$TCM_PEER[DEVCON8k$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8k <- merge(DEVCON8k,SC31OUT.rda,by="NEWID")
DEVCON8k$TCH_INCENTV <- rescale(DEVCON8k$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8k$ASS_PROG[DEVCON8k$SC18Q01==1] <- 1
DEVCON8k$ASS_PROG[DEVCON8k$SC18Q01==2] <- 0

DEVCON8k$ASS_PROM[DEVCON8k$SC18Q02==1] <- 1
DEVCON8k$ASS_PROM[DEVCON8k$SC18Q02==2] <- 0

DEVCON8k$ASS_SCH[DEVCON8k$SC18Q05==1] <- 1
DEVCON8k$ASS_SCH[DEVCON8k$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8k$STU_FEEDB[DEVCON8k$SC39Q07==1] <- 1
DEVCON8k$STU_FEEDB[DEVCON8k$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8k$COMP_USE[DEVCON8k$SC40Q01==1] <- 1
DEVCON8k$COMP_USE[DEVCON8k$SC40Q01==2] <- 0

DEVCON8k$TXT_BOOK[DEVCON8k$SC40Q02==1] <- 1
DEVCON8k$TXT_BOOK[DEVCON8k$SC40Q02==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8k$DUM_SMLTOWN <- ifelse(DEVCON8k$SC03Q01==2,1,0)
DEVCON8k$DUM_TOWN    <- ifelse(DEVCON8k$SC03Q01==3,1,0)

DEVCON8k$TOWN <- DEVCON8k$DUM_SMLTOWN+DEVCON8k$DUM_TOWN
# DEVCON8k$TOWN[DEVCON8k$TOWN>1] <- 1 do not need to do this since mutually exclusive

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8k$EXC2_PLAY[DEVCON8k$SC16Q02==1] <- 1
DEVCON8k$EXC2_PLAY[DEVCON8k$SC16Q02==2] <- 0

DEVCON8k$EXC6_MATHCOMP[DEVCON8k$SC16Q06==1] <- 1
DEVCON8k$EXC6_MATHCOMP[DEVCON8k$SC16Q06==2] <- 0

DEVCON8k$EXC10_SPORT[DEVCON8k$SC16Q10==1] <- 1
DEVCON8k$EXC10_SPORT[DEVCON8k$SC16Q10==2] <- 0

DEVCON8k$EXC11_UNICORN[DEVCON8k$SC16Q11==1] <- 1
DEVCON8k$EXC11_UNICORN[DEVCON8k$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8k$SCL_EXTR_CL[DEVCON8k$SC20Q01==1] <- 1
DEVCON8k$SCL_EXTR_CL[DEVCON8k$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8k$SCORE_PUBLIC[DEVCON8k$SC19Q01==1] <- 1
DEVCON8k$SCORE_PUBLIC[DEVCON8k$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8k$QUAL_RECORD[DEVCON8k$SC39Q03==1] <- 1
DEVCON8k$QUAL_RECORD[DEVCON8k$SC39Q03==2] <- 0

# Now for the rotated part 3 variables 

# ST91Q02
#________________________________________________________________________________________________________
DEVCON8k$ATT_SA <- ifelse(DEVCON8k$ST91Q02==1,1,0)
DEVCON8k$ATT_A <- ifelse(DEVCON8k$ST91Q02==2,1,0)
DEVCON8k$ATT_CONTROL <-DEVCON8k$ATT_SA+DEVCON8k$ATT_A
# DEVCON8k$ATT_CONTROL[DEVCON8k$ATT_CONTROL>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8k$FAMPROB_SA <- ifelse(DEVCON8k$ST91Q03==1,1,0)
DEVCON8k$FAMPROB_A <- ifelse(DEVCON8k$ST91Q03==2,1,0)
DEVCON8k$BKGR_FAMPROB <-DEVCON8k$FAMPROB_SA+DEVCON8k$FAMPROB_A
# DEVCON8k$BKGR_FAMPROB[DEVCON8k$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8k$DIFFTCH_SA <- ifelse(DEVCON8k$ST91Q04==1,1,0)
DEVCON8k$DIFFTCH_A <- ifelse(DEVCON8k$ST91Q04==2,1,0)
DEVCON8k$TCHQUAL_DIFF <- DEVCON8k$DIFFTCH_SA+DEVCON8k$DIFFTCH_A

R159 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R159

R160 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R160 # BKGR_FAMPROB decreases
# Vietnam 73.06

R161 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R161 # BKGR_FAMPROB decreases, SCMAT increases
# Vietnam 76.40

R162 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R162 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases
# Vietnam 73.00

R163 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R163 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases
# Vietnam 74.03

R164 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R164 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases
# Vietnam 73.93

R165 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R165 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# Vietnam 72.77 

R166 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R166 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases
# Vietnam 74.15

R167 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R167 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases
# Vietnam 74.03

R168 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R168 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases
# Vietnam 73.30

R169 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R169 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases
# Vietnam 71.45

R170 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R170 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases, TCHBEHTD decreases
# Vietnam 71.44

R171 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R171 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases, TCHBEHTD decreases, TCHBEHSO decreases
# Vietnam 68.56 

R172 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO","TCHBEHFA"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R172 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases, TCHBEHTD decreases, TCHBEHSO decreases,
# TCHBEHFA decreases
# Vietnam 68.46

R173 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO","TCHBEHFA","COGACT"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R173 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases, TCHBEHTD decreases, TCHBEHSO decreases,
# TCHBEHFA decreases, COGACT increases
# Vietnam 70.90

R174 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO","TCHBEHFA","COGACT","CLSMAN"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R174 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases, TCHBEHTD decreases, TCHBEHSO decreases,
# TCHBEHFA decreases, COGACT increases, CLSMAN increases
# Vietnam 71.30

R175 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","SCMAT","ANXMAT",
                        "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO","TCHBEHFA","COGACT","CLSMAN","DISCLIMA"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R175 # BKGR_FAMPROB decreases, SCMAT increases, ANXMAT decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# ATT_CONTROL increases, MTSUP decreases, STUDREL decreases, TCHQUAL_DIFF decreases, TCHBEHTD decreases, TCHBEHSO decreases,
# TCHBEHFA decreases, COGACT increases, CLSMAN increases, DISCLIMA decreases
# Vietnam 71.02

# Testing all the 11 gap decreasing variables

R176 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","ANXMAT",
                        "ATSCHL","ATTLNACT","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO","TCHBEHFA","DISCLIMA"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R176

### 2. CREATING COLUMN (8) NON-ROTATED VARIABLES + ROTATED PART 3 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST91Q03","ST91Q04","ANXMAT","ATSCHL","ATTLNACT","MTSUP","STUDREL","TCHBEHTD",
                    "TCHBEHSO","TCHBEHFA","DISCLIMA")]
N1 <- NROW(na.omit(T1b)) 
N1 #15539
N0-N1 #32944 NA's
DEVCON8k <- DEVCON8a[complete.cases(T1b),]

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8k$PRESCHOOL[DEVCON8k$ST05Q01==1] <- 0
DEVCON8k$PRESCHOOL[DEVCON8k$ST05Q01==2] <- 1
DEVCON8k$PRESCHOOL[DEVCON8k$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==1]  <- 5
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==2]  <- 15
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==3]  <- 60
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==4]  <- 150
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==5]  <- 350
DEVCON8k$BOOK_N[DEVCON8k$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8k$PARPRESSURE[DEVCON8k$SC24Q01==1] <- 1
DEVCON8k$PARPRESSURE[DEVCON8k$SC24Q01==2] <- 0
DEVCON8k$PARPRESSURE[DEVCON8k$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8k$SC25Q10[is.na(DEVCON8k$SC25Q10)]  <- 0
DEVCON8k$SC25Q11[is.na(DEVCON8k$SC25Q11)]  <- 0
DEVCON8k$FUNDMOM <- DEVCON8k$SC25Q11
DEVCON8k$COUNCILMOM <- DEVCON8k$SC25Q10
DEVCON8k$DUTYMOM <- DEVCON8k$SC25Q02

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8k$TCM_STUASS[DEVCON8k$SC30Q01==1] <- 1
DEVCON8k$TCM_STUASS[DEVCON8k$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8k$TCM_PEER[DEVCON8k$SC30Q02==1] <- 1
DEVCON8k$TCM_PEER[DEVCON8k$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8k <- merge(DEVCON8k,SC31OUT.rda,by="NEWID")
DEVCON8k$TCH_INCENTV <- rescale(DEVCON8k$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8k$ASS_PROG[DEVCON8k$SC18Q01==1] <- 1
DEVCON8k$ASS_PROG[DEVCON8k$SC18Q01==2] <- 0

DEVCON8k$ASS_PROM[DEVCON8k$SC18Q02==1] <- 1
DEVCON8k$ASS_PROM[DEVCON8k$SC18Q02==2] <- 0

DEVCON8k$ASS_SCH[DEVCON8k$SC18Q05==1] <- 1
DEVCON8k$ASS_SCH[DEVCON8k$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8k$STU_FEEDB[DEVCON8k$SC39Q07==1] <- 1
DEVCON8k$STU_FEEDB[DEVCON8k$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8k$COMP_USE[DEVCON8k$SC40Q01==1] <- 1
DEVCON8k$COMP_USE[DEVCON8k$SC40Q01==2] <- 0

DEVCON8k$TXT_BOOK[DEVCON8k$SC40Q02==1] <- 1
DEVCON8k$TXT_BOOK[DEVCON8k$SC40Q02==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8k$DUM_SMLTOWN <- ifelse(DEVCON8k$SC03Q01==2,1,0)
DEVCON8k$DUM_TOWN    <- ifelse(DEVCON8k$SC03Q01==3,1,0)

DEVCON8k$TOWN <- DEVCON8k$DUM_SMLTOWN+DEVCON8k$DUM_TOWN
# DEVCON8k$TOWN[DEVCON8k$TOWN>1] <- 1 do not need to do this since mutually exclusive

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8k$EXC2_PLAY[DEVCON8k$SC16Q02==1] <- 1
DEVCON8k$EXC2_PLAY[DEVCON8k$SC16Q02==2] <- 0

DEVCON8k$EXC6_MATHCOMP[DEVCON8k$SC16Q06==1] <- 1
DEVCON8k$EXC6_MATHCOMP[DEVCON8k$SC16Q06==2] <- 0

DEVCON8k$EXC10_SPORT[DEVCON8k$SC16Q10==1] <- 1
DEVCON8k$EXC10_SPORT[DEVCON8k$SC16Q10==2] <- 0

DEVCON8k$EXC11_UNICORN[DEVCON8k$SC16Q11==1] <- 1
DEVCON8k$EXC11_UNICORN[DEVCON8k$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8k$SCL_EXTR_CL[DEVCON8k$SC20Q01==1] <- 1
DEVCON8k$SCL_EXTR_CL[DEVCON8k$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8k$SCORE_PUBLIC[DEVCON8k$SC19Q01==1] <- 1
DEVCON8k$SCORE_PUBLIC[DEVCON8k$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8k$QUAL_RECORD[DEVCON8k$SC39Q03==1] <- 1
DEVCON8k$QUAL_RECORD[DEVCON8k$SC39Q03==2] <- 0

# Now for the rotated part 3 variables 

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8k$FAMPROB_SA <- ifelse(DEVCON8k$ST91Q03==1,1,0)
DEVCON8k$FAMPROB_A <- ifelse(DEVCON8k$ST91Q03==2,1,0)
DEVCON8k$BKGR_FAMPROB <-DEVCON8k$FAMPROB_SA+DEVCON8k$FAMPROB_A
# DEVCON8k$BKGR_FAMPROB[DEVCON8k$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8k$DIFFTCH_SA <- ifelse(DEVCON8k$ST91Q04==1,1,0)
DEVCON8k$DIFFTCH_A <- ifelse(DEVCON8k$ST91Q04==2,1,0)
DEVCON8k$TCHQUAL_DIFF <- DEVCON8k$DIFFTCH_SA+DEVCON8k$DIFFTCH_A

R177 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "FUNDMOM", "COUNCILMOM","DUTYMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","PCGIRLS", "COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","ANXMAT",
                        "ATSCHL","ATTLNACT","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                        "TCHBEHSO","TCHBEHFA","DISCLIMA"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R177

write.csv(R177, "Math_rot3.csv")













