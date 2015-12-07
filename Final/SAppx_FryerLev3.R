#### SAppx_FryerLev3.R (Science, Table A4)

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
# questionnaire variables for SCIENCE (Table A4). The PISA dataset contains 3 rotated student questionnaire, each one
# having been adminstered to 2/3 of the students tested. Thus, in order to keep the sample size meaningfully
# large, we do not add all the rotated student questionnaire variables to the non-rotated variables (see S3_FryerLevitt)
# at once but in turn. We create three additional columns per subject.
# We add to all gap-decreasing non-rotated variables the rotated part 1 variables in turn to determine which are gap decreasing
# Then we create a sample just on gap-decreasing variables (from non-rotated and rotated part 1) and report those in Table A3

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
# TABLE A4 COLUMN (6) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 1 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 1 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","PERSEV","OPENPS")]
N1 <- NROW(na.omit(T1b)) 
N1 #17441
N0-N1 #31042 NA's
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
# First I have to generate a series of dummy variables 
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

R378 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8y,export=FALSE)
R378 
# VIETNAM: 85.13

R379 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8y,export=FALSE)
R379 # PERSEV decreases
# VIETNAM: 84.54

R380 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","PERSEV","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8y,export=FALSE)
R380 # PERSEV decreases, OPENPS increases
# VIETNAM: 86.81

### 2. CREATING COLUMN (6) NON-ROTATED VARIABLES + ROTATED PART 1 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","PERSEV")]
N1 <- NROW(na.omit(T1b)) 
N1 #17545
N0-N1 #30938 NA's
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
# First I have to generate a series of dummy variables 
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

R381 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "TCM_PEER","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PCGIRLS",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8y,export=FALSE)
R381

write.csv(R381, "Scie_rot1.csv")

########################################################################################################
# TABLE A3 COLUMN (7) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 2 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 2 variables

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","ST55Q03","LMINS","MMINS","SMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 #12670
N0-N1 #35813 NA's
DEVCON8z <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8z$FEMALE[DEVCON8z$ST04Q01==1] <- 1
DEVCON8z$FEMALE[DEVCON8z$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8z$PRESCHOOL[DEVCON8z$ST05Q01==1] <- 0
DEVCON8z$PRESCHOOL[DEVCON8z$ST05Q01==2] <- 1
DEVCON8z$PRESCHOOL[DEVCON8z$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==1]  <- 5
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==2]  <- 15
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==3]  <- 60
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==4]  <- 150
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==5]  <- 350
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8z$PARPRESSURE[DEVCON8z$SC24Q01==1] <- 1
DEVCON8z$PARPRESSURE[DEVCON8z$SC24Q01==2] <- 0
DEVCON8z$PARPRESSURE[DEVCON8z$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8z$SC25Q10[is.na(DEVCON8z$SC25Q10)]  <- 0
DEVCON8z$SC25Q11[is.na(DEVCON8z$SC25Q11)]  <- 0
DEVCON8z$FUNDMOM <-  DEVCON8z$SC25Q11
DEVCON8z$COUNCILMOM <- DEVCON8z$SC25Q10

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8z$TCM_STUASS[DEVCON8z$SC30Q01==1] <- 1
DEVCON8z$TCM_STUASS[DEVCON8z$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8z$TCM_PEER[DEVCON8z$SC30Q02==1] <- 1
DEVCON8z$TCM_PEER[DEVCON8z$SC30Q02==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8z$ASS_PROG[DEVCON8z$SC18Q01==1] <- 1
DEVCON8z$ASS_PROG[DEVCON8z$SC18Q01==2] <- 0

DEVCON8z$ASS_PROM[DEVCON8z$SC18Q02==1] <- 1
DEVCON8z$ASS_PROM[DEVCON8z$SC18Q02==2] <- 0

DEVCON8z$ASS_NAT[DEVCON8z$SC18Q04==1] <- 1
DEVCON8z$ASS_NAT[DEVCON8z$SC18Q04==2] <- 0

DEVCON8z$ASS_SCH[DEVCON8z$SC18Q05==1] <- 1
DEVCON8z$ASS_SCH[DEVCON8z$SC18Q05==2] <- 0

DEVCON8z$ASS_CUR[DEVCON8z$SC18Q07==1] <- 1
DEVCON8z$ASS_CUR[DEVCON8z$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8z$STU_FEEDB[DEVCON8z$SC39Q07==1] <- 1
DEVCON8z$STU_FEEDB[DEVCON8z$SC39Q07==2] <- 0

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8z$PRIVATESCL[DEVCON8z$SC01Q01==2] <- 1
DEVCON8z$PRIVATESCL[DEVCON8z$SC01Q01==1] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8z$DUM_SMLTOWN <- ifelse(DEVCON8z$SC03Q01==2,1,0)
DEVCON8z$DUM_TOWN    <- ifelse(DEVCON8z$SC03Q01==3,1,0)
DEVCON8z$TOWN <- DEVCON8z$DUM_SMLTOWN+DEVCON8z$DUM_TOWN
DEVCON8z$TOWN[DEVCON8z$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8z$EXC2_PLAY[DEVCON8z$SC16Q02==1] <- 1
DEVCON8z$EXC2_PLAY[DEVCON8z$SC16Q02==2] <- 0

DEVCON8z$EXC6_MATHCOMP[DEVCON8z$SC16Q06==1] <- 1
DEVCON8z$EXC6_MATHCOMP[DEVCON8z$SC16Q06==2] <- 0

DEVCON8z$EXC10_SPORT[DEVCON8z$SC16Q10==1] <- 1
DEVCON8z$EXC10_SPORT[DEVCON8z$SC16Q10==2] <- 0

DEVCON8z$EXC11_UNICORN[DEVCON8z$SC16Q11==1] <- 1
DEVCON8z$EXC11_UNICORN[DEVCON8z$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8z$SCORE_PUBLIC[DEVCON8z$SC19Q01==1] <- 1
DEVCON8z$SCORE_PUBLIC[DEVCON8z$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8z$QUAL_RECORD[DEVCON8z$SC39Q03==1] <- 1
DEVCON8z$QUAL_RECORD[DEVCON8z$SC39Q03==2] <- 0

# The rotated part 2 variables:

#ST55Q03
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==1] <- 0
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==2] <- 1
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==3] <- 3
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==4] <- 5
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==5] <- 7

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8z$SHRS <- (DEVCON8z$SMINS)/60
DEVCON8z$MHRS <- (DEVCON8z$MMINS)/60
DEVCON8z$LHRS <- (DEVCON8z$LMINS)/60

R383 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R383 
# VIETNAM: 84.94

R384 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R384 # OUTSCIE decreases
# VIETNAM: 83.51

R385 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE","LHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R385 # OUTSCIE decreases, LHRS increases
# VIETNAM: 83.52

R386 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE","LHRS","MHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R386 # OUTSCIE decreases, LHRS increases, MHRS increase
# VIETNAM: 86.28

R387 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R387 # OUTSCIE decreases, LHRS increases, MHRS increase, SHRS increases
# VIETNAM: 88.51 

### 2. CREATING COLUMN (7) NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","ST55Q03")]
N1 <- NROW(na.omit(T1b)) 
N1 # 15960
N0-N1 # 32523 NA's
DEVCON8z <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8z$FEMALE[DEVCON8z$ST04Q01==1] <- 1
DEVCON8z$FEMALE[DEVCON8z$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8z$PRESCHOOL[DEVCON8z$ST05Q01==1] <- 0
DEVCON8z$PRESCHOOL[DEVCON8z$ST05Q01==2] <- 1
DEVCON8z$PRESCHOOL[DEVCON8z$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==1]  <- 5
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==2]  <- 15
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==3]  <- 60
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==4]  <- 150
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==5]  <- 350
DEVCON8z$BOOK_N[DEVCON8z$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8z$PARPRESSURE[DEVCON8z$SC24Q01==1] <- 1
DEVCON8z$PARPRESSURE[DEVCON8z$SC24Q01==2] <- 0
DEVCON8z$PARPRESSURE[DEVCON8z$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8z$SC25Q10[is.na(DEVCON8z$SC25Q10)]  <- 0
DEVCON8z$SC25Q11[is.na(DEVCON8z$SC25Q11)]  <- 0
DEVCON8z$FUNDMOM <-  DEVCON8z$SC25Q11
DEVCON8z$COUNCILMOM <- DEVCON8z$SC25Q10
DEVCON8z$DUTYMOM <- DEVCON8z$SC25Q02

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8z$TCM_STUASS[DEVCON8z$SC30Q01==1] <- 1
DEVCON8z$TCM_STUASS[DEVCON8z$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8z$TCM_PEER[DEVCON8z$SC30Q02==1] <- 1
DEVCON8z$TCM_PEER[DEVCON8z$SC30Q02==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8z$ASS_PROG[DEVCON8z$SC18Q01==1] <- 1
DEVCON8z$ASS_PROG[DEVCON8z$SC18Q01==2] <- 0

DEVCON8z$ASS_PROM[DEVCON8z$SC18Q02==1] <- 1
DEVCON8z$ASS_PROM[DEVCON8z$SC18Q02==2] <- 0

DEVCON8z$ASS_NAT[DEVCON8z$SC18Q04==1] <- 1
DEVCON8z$ASS_NAT[DEVCON8z$SC18Q04==2] <- 0

DEVCON8z$ASS_SCH[DEVCON8z$SC18Q05==1] <- 1
DEVCON8z$ASS_SCH[DEVCON8z$SC18Q05==2] <- 0

DEVCON8z$ASS_CUR[DEVCON8z$SC18Q07==1] <- 1
DEVCON8z$ASS_CUR[DEVCON8z$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8z$STU_FEEDB[DEVCON8z$SC39Q07==1] <- 1
DEVCON8z$STU_FEEDB[DEVCON8z$SC39Q07==2] <- 0

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8z$PRIVATESCL[DEVCON8z$SC01Q01==2] <- 1
DEVCON8z$PRIVATESCL[DEVCON8z$SC01Q01==1] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8z$DUM_SMLTOWN <- ifelse(DEVCON8z$SC03Q01==2,1,0)
DEVCON8z$DUM_TOWN    <- ifelse(DEVCON8z$SC03Q01==3,1,0)
DEVCON8z$TOWN <- DEVCON8z$DUM_SMLTOWN+DEVCON8z$DUM_TOWN
DEVCON8z$TOWN[DEVCON8z$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8z$EXC2_PLAY[DEVCON8z$SC16Q02==1] <- 1
DEVCON8z$EXC2_PLAY[DEVCON8z$SC16Q02==2] <- 0

DEVCON8z$EXC6_MATHCOMP[DEVCON8z$SC16Q06==1] <- 1
DEVCON8z$EXC6_MATHCOMP[DEVCON8z$SC16Q06==2] <- 0

DEVCON8z$EXC10_SPORT[DEVCON8z$SC16Q10==1] <- 1
DEVCON8z$EXC10_SPORT[DEVCON8z$SC16Q10==2] <- 0

DEVCON8z$EXC11_UNICORN[DEVCON8z$SC16Q11==1] <- 1
DEVCON8z$EXC11_UNICORN[DEVCON8z$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8z$SCORE_PUBLIC[DEVCON8z$SC19Q01==1] <- 1
DEVCON8z$SCORE_PUBLIC[DEVCON8z$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8z$QUAL_RECORD[DEVCON8z$SC39Q03==1] <- 1
DEVCON8z$QUAL_RECORD[DEVCON8z$SC39Q03==2] <- 0

# The rotated part 2 variables:

#ST55Q03
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==1] <- 0
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==2] <- 1
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==3] <- 3
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==4] <- 5
DEVCON8z$OUTSCIE[DEVCON8z$ST55Q03==5] <- 7

R388 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "TCM_PEER","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PCGIRLS",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R388

write.csv(R388, "Scie_rot2.csv")

########################################################################################################
# TABLE A3 COLUMN (8) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 3 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 3 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 3 variables

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","ST91Q03","BELONG","ATSCHL","ATTLNACT",
                    "ST91Q02","STUDREL","ST91Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 #16764
N0-N1 #31719 NA's
DEVCON8za <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8za$FEMALE[DEVCON8za$ST04Q01==1] <- 1
DEVCON8za$FEMALE[DEVCON8za$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8za$PRESCHOOL[DEVCON8za$ST05Q01==1] <- 0
DEVCON8za$PRESCHOOL[DEVCON8za$ST05Q01==2] <- 1
DEVCON8za$PRESCHOOL[DEVCON8za$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==1]  <- 5
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==2]  <- 15
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==3]  <- 60
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==4]  <- 150
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==5]  <- 350
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8za$PARPRESSURE[DEVCON8za$SC24Q01==1] <- 1
DEVCON8za$PARPRESSURE[DEVCON8za$SC24Q01==2] <- 0
DEVCON8za$PARPRESSURE[DEVCON8za$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8za$SC25Q10[is.na(DEVCON8za$SC25Q10)]  <- 0
DEVCON8za$SC25Q11[is.na(DEVCON8za$SC25Q11)]  <- 0
DEVCON8za$FUNDMOM <-  DEVCON8za$SC25Q11
DEVCON8za$COUNCILMOM <- DEVCON8za$SC25Q10

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8za$TCM_STUASS[DEVCON8za$SC30Q01==1] <- 1
DEVCON8za$TCM_STUASS[DEVCON8za$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8za$TCM_PEER[DEVCON8za$SC30Q02==1] <- 1
DEVCON8za$TCM_PEER[DEVCON8za$SC30Q02==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8za$ASS_PROG[DEVCON8za$SC18Q01==1] <- 1
DEVCON8za$ASS_PROG[DEVCON8za$SC18Q01==2] <- 0

DEVCON8za$ASS_PROM[DEVCON8za$SC18Q02==1] <- 1
DEVCON8za$ASS_PROM[DEVCON8za$SC18Q02==2] <- 0

DEVCON8za$ASS_NAT[DEVCON8za$SC18Q04==1] <- 1
DEVCON8za$ASS_NAT[DEVCON8za$SC18Q04==2] <- 0

DEVCON8za$ASS_SCH[DEVCON8za$SC18Q05==1] <- 1
DEVCON8za$ASS_SCH[DEVCON8za$SC18Q05==2] <- 0

DEVCON8za$ASS_CUR[DEVCON8za$SC18Q07==1] <- 1
DEVCON8za$ASS_CUR[DEVCON8za$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8za$STU_FEEDB[DEVCON8za$SC39Q07==1] <- 1
DEVCON8za$STU_FEEDB[DEVCON8za$SC39Q07==2] <- 0

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8za$PRIVATESCL[DEVCON8za$SC01Q01==2] <- 1
DEVCON8za$PRIVATESCL[DEVCON8za$SC01Q01==1] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8za$DUM_SMLTOWN <- ifelse(DEVCON8za$SC03Q01==2,1,0)
DEVCON8za$DUM_TOWN    <- ifelse(DEVCON8za$SC03Q01==3,1,0)
DEVCON8za$TOWN <- DEVCON8za$DUM_SMLTOWN+DEVCON8za$DUM_TOWN
DEVCON8za$TOWN[DEVCON8za$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8za$EXC2_PLAY[DEVCON8za$SC16Q02==1] <- 1
DEVCON8za$EXC2_PLAY[DEVCON8za$SC16Q02==2] <- 0

DEVCON8za$EXC6_MATHCOMP[DEVCON8za$SC16Q06==1] <- 1
DEVCON8za$EXC6_MATHCOMP[DEVCON8za$SC16Q06==2] <- 0

DEVCON8za$EXC10_SPORT[DEVCON8za$SC16Q10==1] <- 1
DEVCON8za$EXC10_SPORT[DEVCON8za$SC16Q10==2] <- 0

DEVCON8za$EXC11_UNICORN[DEVCON8za$SC16Q11==1] <- 1
DEVCON8za$EXC11_UNICORN[DEVCON8za$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8za$SCORE_PUBLIC[DEVCON8za$SC19Q01==1] <- 1
DEVCON8za$SCORE_PUBLIC[DEVCON8za$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8za$QUAL_RECORD[DEVCON8za$SC39Q03==1] <- 1
DEVCON8za$QUAL_RECORD[DEVCON8za$SC39Q03==2] <- 0

# The rotated part 3 variables:

# ST91Q02
#________________________________________________________________________________________________________
DEVCON8za$ATT_SA <- ifelse(DEVCON8za$ST91Q02==1,1,0)
DEVCON8za$ATT_A <- ifelse(DEVCON8za$ST91Q02==2,1,0)
DEVCON8za$ATT_CONTROL <-DEVCON8za$ATT_SA+DEVCON8za$ATT_A
# DEVCON8za$ATT_CONTROL[DEVCON8za$ATT_CONTROL>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8za$FAMPROB_SA <- ifelse(DEVCON8za$ST91Q03==1,1,0)
DEVCON8za$FAMPROB_A <- ifelse(DEVCON8za$ST91Q03==2,1,0)
DEVCON8za$BKGR_FAMPROB <-DEVCON8za$FAMPROB_SA+DEVCON8za$FAMPROB_A
# DEVCON8za$BKGR_FAMPROB[DEVCON8za$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8za$DIFFTCH_SA <- ifelse(DEVCON8za$ST91Q04==1,1,0)
DEVCON8za$DIFFTCH_A <- ifelse(DEVCON8za$ST91Q04==2,1,0)
DEVCON8za$TCHQUAL_DIFF <- DEVCON8za$DIFFTCH_SA+DEVCON8za$DIFFTCH_A
# DEVCON8za$TCHQUAL_DIFF[DEVCON8za$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

# Let's try our regression with all gap decreasing variables before we add the rotated parts

R394 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R394
# VIETNAM: 87.81

R395 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R395  # BKGR_FAMPROB decreases
# VIETNAM: 84.89

R396 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R396  # BKGR_FAMPROB decreases, BELONG increases
# VIETNAM: 87.46

R397 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R397  # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases
# VIETNAM: 87.26

R398 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R398 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# VIETNAM: 85.46

R399 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT",
                        "ATT_CONTROL"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R399 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases, ATT_CONTROL increases
# VIETNAM: 87.14

R402 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT",
                        "ATT_CONTROL","STUDREL"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R402 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases, ATT_CONTROL increases,
# STUDREL decreases
# VIETNAM: 86.76

R403 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT",
                        "ATT_CONTROL","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R403 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases, ATT_CONTROL increases,
# STUDREL decreases, TCHQUAL_DIFF decreases
# VIETNAM: 84.32

# All gap decreasing variables:

R404 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","ATSCHL","ATTLNACT","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R404
# VIETNAM: 81.08

### 2. CREATING COLUMN (8) NON-ROTATED VARIABLES + ROTATED PART 3 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","ST91Q03","ATSCHL","ATTLNACT",
                    "STUDREL","ST91Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 # 16826
N0-N1 # 31657NA's
DEVCON8za <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8za$FEMALE[DEVCON8za$ST04Q01==1] <- 1
DEVCON8za$FEMALE[DEVCON8za$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8za$PRESCHOOL[DEVCON8za$ST05Q01==1] <- 0
DEVCON8za$PRESCHOOL[DEVCON8za$ST05Q01==2] <- 1
DEVCON8za$PRESCHOOL[DEVCON8za$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==1]  <- 5
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==2]  <- 15
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==3]  <- 60
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==4]  <- 150
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==5]  <- 350
DEVCON8za$BOOK_N[DEVCON8za$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8za$PARPRESSURE[DEVCON8za$SC24Q01==1] <- 1
DEVCON8za$PARPRESSURE[DEVCON8za$SC24Q01==2] <- 0
DEVCON8za$PARPRESSURE[DEVCON8za$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8za$SC25Q10[is.na(DEVCON8za$SC25Q10)]  <- 0
DEVCON8za$SC25Q11[is.na(DEVCON8za$SC25Q11)]  <- 0
DEVCON8za$FUNDMOM <-  DEVCON8za$SC25Q11
DEVCON8za$COUNCILMOM <- DEVCON8za$SC25Q10
DEVCON8za$DUTYMOM <- DEVCON8za$SC25Q02

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8za$TCM_STUASS[DEVCON8za$SC30Q01==1] <- 1
DEVCON8za$TCM_STUASS[DEVCON8za$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8za$TCM_PEER[DEVCON8za$SC30Q02==1] <- 1
DEVCON8za$TCM_PEER[DEVCON8za$SC30Q02==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8za$ASS_PROG[DEVCON8za$SC18Q01==1] <- 1
DEVCON8za$ASS_PROG[DEVCON8za$SC18Q01==2] <- 0

DEVCON8za$ASS_PROM[DEVCON8za$SC18Q02==1] <- 1
DEVCON8za$ASS_PROM[DEVCON8za$SC18Q02==2] <- 0

DEVCON8za$ASS_NAT[DEVCON8za$SC18Q04==1] <- 1
DEVCON8za$ASS_NAT[DEVCON8za$SC18Q04==2] <- 0

DEVCON8za$ASS_SCH[DEVCON8za$SC18Q05==1] <- 1
DEVCON8za$ASS_SCH[DEVCON8za$SC18Q05==2] <- 0

DEVCON8za$ASS_CUR[DEVCON8za$SC18Q07==1] <- 1
DEVCON8za$ASS_CUR[DEVCON8za$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8za$STU_FEEDB[DEVCON8za$SC39Q07==1] <- 1
DEVCON8za$STU_FEEDB[DEVCON8za$SC39Q07==2] <- 0

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8za$PRIVATESCL[DEVCON8za$SC01Q01==2] <- 1
DEVCON8za$PRIVATESCL[DEVCON8za$SC01Q01==1] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8za$DUM_SMLTOWN <- ifelse(DEVCON8za$SC03Q01==2,1,0)
DEVCON8za$DUM_TOWN    <- ifelse(DEVCON8za$SC03Q01==3,1,0)
DEVCON8za$TOWN <- DEVCON8za$DUM_SMLTOWN+DEVCON8za$DUM_TOWN
DEVCON8za$TOWN[DEVCON8za$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8za$EXC2_PLAY[DEVCON8za$SC16Q02==1] <- 1
DEVCON8za$EXC2_PLAY[DEVCON8za$SC16Q02==2] <- 0

DEVCON8za$EXC6_MATHCOMP[DEVCON8za$SC16Q06==1] <- 1
DEVCON8za$EXC6_MATHCOMP[DEVCON8za$SC16Q06==2] <- 0

DEVCON8za$EXC10_SPORT[DEVCON8za$SC16Q10==1] <- 1
DEVCON8za$EXC10_SPORT[DEVCON8za$SC16Q10==2] <- 0

DEVCON8za$EXC11_UNICORN[DEVCON8za$SC16Q11==1] <- 1
DEVCON8za$EXC11_UNICORN[DEVCON8za$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8za$SCORE_PUBLIC[DEVCON8za$SC19Q01==1] <- 1
DEVCON8za$SCORE_PUBLIC[DEVCON8za$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8za$QUAL_RECORD[DEVCON8za$SC39Q03==1] <- 1
DEVCON8za$QUAL_RECORD[DEVCON8za$SC39Q03==2] <- 0

# The rotated part 3 variables:

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8za$FAMPROB_SA <- ifelse(DEVCON8za$ST91Q03==1,1,0)
DEVCON8za$FAMPROB_A <- ifelse(DEVCON8za$ST91Q03==2,1,0)
DEVCON8za$BKGR_FAMPROB <-DEVCON8za$FAMPROB_SA+DEVCON8za$FAMPROB_A
# DEVCON8za$BKGR_FAMPROB[DEVCON8za$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8za$DIFFTCH_SA <- ifelse(DEVCON8za$ST91Q04==1,1,0)
DEVCON8za$DIFFTCH_A <- ifelse(DEVCON8za$ST91Q04==2,1,0)
DEVCON8za$TCHQUAL_DIFF <- DEVCON8za$DIFFTCH_SA+DEVCON8za$DIFFTCH_A
# DEVCON8za$TCHQUAL_DIFF[DEVCON8za$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

R405 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "TCM_PEER","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PCGIRLS",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","ATSCHL","ATTLNACT","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R405

write.csv(R405, "Scie_rot3.csv")

