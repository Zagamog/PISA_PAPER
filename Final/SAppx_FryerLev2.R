#### SAppx_FryerLev2.R (Reading, Table A3)

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
# questionnaire variables for READING (Table A3). The PISA dataset contains 3 rotated student questionnaire, each one
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
# TABLE A3 COLUMN (6) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 1 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 1 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 1 variables

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07",
                    "SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "PERSEV","OPENPS")]
N1 <- NROW(na.omit(T1b)) 
N1 #17506
N0-N1 #30977 NA's
DEVCON8q <- DEVCON8a[complete.cases(T1b),]

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

# Let's try our regression with all gap decreasing variables before we add the rotated parts

R263 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8q,export=FALSE)
R263
# VIETNAM: 58.99

R264 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8q,export=FALSE)
R264 # PERSEV decreases
# VIETNAM: 58.43

R265 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","PERSEV","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8q,export=FALSE)
R265 # PERSEV decreases, OPENPS increases
# VIETNAM: 59.27

### 2. CREATING COLUMN (6) NON-ROTATED VARIABLES + ROTATED PART 1 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07",
                    "SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "PERSEV")]
N1 <- NROW(na.omit(T1b)) 
N1 #17611
N0-N1 #30872 NA's
DEVCON8q <- DEVCON8a[complete.cases(T1b),]

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
DEVCON8q$DUTYMOM <- DEVCON8q$SC25Q02

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

R266 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PCGIRLS",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","PERSEV"),
                    weight="W_FSTUWT",
                    data=DEVCON8q,export=FALSE)
R266 

write.csv(R266, "Read_rot1.csv")

########################################################################################################
# TABLE A3 COLUMN (7) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 2 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 2 variables

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04",
                    "SC18Q07","SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "ST55Q01","LMINS","MMINS","SMINS","ST72Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #11843
N0-N1 #36640 NA's
DEVCON8r <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8r$FEMALE[DEVCON8r$ST04Q01==1] <- 1
DEVCON8r$FEMALE[DEVCON8r$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8r$PRESCHOOL[DEVCON8r$ST05Q01==1] <- 0
DEVCON8r$PRESCHOOL[DEVCON8r$ST05Q01==2] <- 1
DEVCON8r$PRESCHOOL[DEVCON8r$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==1]  <- 5
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==2]  <- 15
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==3]  <- 60
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==4]  <- 150
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==5]  <- 350
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8r$PARPRESSURE[DEVCON8r$SC24Q01==1] <- 1
DEVCON8r$PARPRESSURE[DEVCON8r$SC24Q01==2] <- 0
DEVCON8r$PARPRESSURE[DEVCON8r$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8r$SC25Q05[is.na(DEVCON8r$SC25Q05)]  <- 0
DEVCON8r$SC25Q06[is.na(DEVCON8r$SC25Q06)]  <- 0
DEVCON8r$SC25Q07[is.na(DEVCON8r$SC25Q07)]  <- 0
DEVCON8r$SC25Q09[is.na(DEVCON8r$SC25Q09)]  <- 0
DEVCON8r$SC25Q10[is.na(DEVCON8r$SC25Q10)]  <- 0
DEVCON8r$SC25Q11[is.na(DEVCON8r$SC25Q11)]  <- 0
DEVCON8r$SC25Q12[is.na(DEVCON8r$SC25Q12)]  <- 0
DEVCON8r$FUNDMOM <-  DEVCON8r$SC25Q11
DEVCON8r$COUNCILMOM <- DEVCON8r$SC25Q10
DEVCON8r$VOLUMOM <- DEVCON8r$SC25Q05+DEVCON8r$SC25Q06+DEVCON8r$SC25Q07+DEVCON8r$SC25Q09+DEVCON8r$SC25Q12
DEVCON8r$VOLUMOM[DEVCON8r$VOLUMOM>100] <- 100 

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8r$TCM_STUASS[DEVCON8r$SC30Q01==1] <- 1
DEVCON8r$TCM_STUASS[DEVCON8r$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8r$ASS_PROG[DEVCON8r$SC18Q01==1] <- 1
DEVCON8r$ASS_PROG[DEVCON8r$SC18Q01==2] <- 0

DEVCON8r$ASS_PROM[DEVCON8r$SC18Q02==1] <- 1
DEVCON8r$ASS_PROM[DEVCON8r$SC18Q02==2] <- 0

DEVCON8r$ASS_NAT[DEVCON8r$SC18Q04==1] <- 1
DEVCON8r$ASS_NAT[DEVCON8r$SC18Q04==2] <- 0

DEVCON8r$ASS_CUR[DEVCON8r$SC18Q07==1] <- 1
DEVCON8r$ASS_CUR[DEVCON8r$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8r$STU_FEEDB[DEVCON8r$SC39Q07==1] <- 1
DEVCON8r$STU_FEEDB[DEVCON8r$SC39Q07==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8r$DUM_SMLTOWN <- ifelse(DEVCON8r$SC03Q01==2,1,0)
DEVCON8r$DUM_TOWN    <- ifelse(DEVCON8r$SC03Q01==3,1,0)
DEVCON8r$TOWN <- DEVCON8r$DUM_SMLTOWN+DEVCON8r$DUM_TOWN
DEVCON8r$TOWN[DEVCON8r$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8r$EXC2_PLAY[DEVCON8r$SC16Q02==1] <- 1
DEVCON8r$EXC2_PLAY[DEVCON8r$SC16Q02==2] <- 0

DEVCON8r$EXC6_MATHCOMP[DEVCON8r$SC16Q06==1] <- 1
DEVCON8r$EXC6_MATHCOMP[DEVCON8r$SC16Q06==2] <- 0

DEVCON8r$EXC10_SPORT[DEVCON8r$SC16Q10==1] <- 1
DEVCON8r$EXC10_SPORT[DEVCON8r$SC16Q10==2] <- 0

DEVCON8r$EXC11_UNICORN[DEVCON8r$SC16Q11==1] <- 1
DEVCON8r$EXC11_UNICORN[DEVCON8r$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8r$SCORE_PUBLIC[DEVCON8r$SC19Q01==1] <- 1
DEVCON8r$SCORE_PUBLIC[DEVCON8r$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8r$QUAL_RECORD[DEVCON8r$SC39Q03==1] <- 1
DEVCON8r$QUAL_RECORD[DEVCON8r$SC39Q03==2] <- 0

# The rotated part 2 variables:

#ST55Q01
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8r$OUTREAD[DEVCON8r$ST55Q01==1] <- 0
DEVCON8r$OUTREAD[DEVCON8r$ST55Q01==2] <- 1
DEVCON8r$OUTREAD[DEVCON8r$ST55Q01==3] <- 3
DEVCON8r$OUTREAD[DEVCON8r$ST55Q01==4] <- 5
DEVCON8r$OUTREAD[DEVCON8r$ST55Q01==5] <- 7

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8r$SHRS <- (DEVCON8r$SMINS)/60
DEVCON8r$MHRS <- (DEVCON8r$MMINS)/60
DEVCON8r$LHRS <- (DEVCON8r$LMINS)/60

# Let's try our regression with all gap decreasing variables before we add the rotated parts

R268 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R268
# VIETNAM 54.22

R269 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R269 # OUTREAD increases
# VIETNAM 56.71

R275 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD","LHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R275 # OUTREAD increases, LHRS decreases
#VIETNAM 56.50

R276 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD","LHRS","MHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R276 # OUTREAD increases, LHRS decreases, MHRS increases
#VIETNAM 59.29

R277 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R277 # # OUTREAD increases, LHRS decreases, MHRS increases, SHRS increases
#VIETNAM 60.62

R278 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD","LHRS","MHRS","SHRS","ST72Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R278 # LHRS decreases, MHRS increases, SHRS increases, ST72Q01 decreases
#VIETNAM 58.72

# Let's try all gap decreasing variables

R279 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","LHRS","ST72Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R279

### 2. CREATING COLUMN (7) NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04",
                    "SC18Q07","SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "LMINS","ST72Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 14226
N0-N1 # 34257
DEVCON8r <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8r$FEMALE[DEVCON8r$ST04Q01==1] <- 1
DEVCON8r$FEMALE[DEVCON8r$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8r$PRESCHOOL[DEVCON8r$ST05Q01==1] <- 0
DEVCON8r$PRESCHOOL[DEVCON8r$ST05Q01==2] <- 1
DEVCON8r$PRESCHOOL[DEVCON8r$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==1]  <- 5
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==2]  <- 15
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==3]  <- 60
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==4]  <- 150
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==5]  <- 350
DEVCON8r$BOOK_N[DEVCON8r$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8r$PARPRESSURE[DEVCON8r$SC24Q01==1] <- 1
DEVCON8r$PARPRESSURE[DEVCON8r$SC24Q01==2] <- 0
DEVCON8r$PARPRESSURE[DEVCON8r$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8r$SC25Q05[is.na(DEVCON8r$SC25Q05)]  <- 0
DEVCON8r$SC25Q06[is.na(DEVCON8r$SC25Q06)]  <- 0
DEVCON8r$SC25Q07[is.na(DEVCON8r$SC25Q07)]  <- 0
DEVCON8r$SC25Q09[is.na(DEVCON8r$SC25Q09)]  <- 0
DEVCON8r$SC25Q10[is.na(DEVCON8r$SC25Q10)]  <- 0
DEVCON8r$SC25Q11[is.na(DEVCON8r$SC25Q11)]  <- 0
DEVCON8r$SC25Q12[is.na(DEVCON8r$SC25Q12)]  <- 0
DEVCON8r$FUNDMOM <-  DEVCON8r$SC25Q11
DEVCON8r$COUNCILMOM <- DEVCON8r$SC25Q10
DEVCON8r$VOLUMOM <- DEVCON8r$SC25Q05+DEVCON8r$SC25Q06+DEVCON8r$SC25Q07+DEVCON8r$SC25Q09+DEVCON8r$SC25Q12
DEVCON8r$VOLUMOM[DEVCON8r$VOLUMOM>100] <- 100 
DEVCON8r$DUTYMOM <- DEVCON8r$SC25Q02

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8r$TCM_STUASS[DEVCON8r$SC30Q01==1] <- 1
DEVCON8r$TCM_STUASS[DEVCON8r$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8r$ASS_PROG[DEVCON8r$SC18Q01==1] <- 1
DEVCON8r$ASS_PROG[DEVCON8r$SC18Q01==2] <- 0

DEVCON8r$ASS_PROM[DEVCON8r$SC18Q02==1] <- 1
DEVCON8r$ASS_PROM[DEVCON8r$SC18Q02==2] <- 0

DEVCON8r$ASS_NAT[DEVCON8r$SC18Q04==1] <- 1
DEVCON8r$ASS_NAT[DEVCON8r$SC18Q04==2] <- 0

DEVCON8r$ASS_CUR[DEVCON8r$SC18Q07==1] <- 1
DEVCON8r$ASS_CUR[DEVCON8r$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8r$STU_FEEDB[DEVCON8r$SC39Q07==1] <- 1
DEVCON8r$STU_FEEDB[DEVCON8r$SC39Q07==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8r$DUM_SMLTOWN <- ifelse(DEVCON8r$SC03Q01==2,1,0)
DEVCON8r$DUM_TOWN    <- ifelse(DEVCON8r$SC03Q01==3,1,0)
DEVCON8r$TOWN <- DEVCON8r$DUM_SMLTOWN+DEVCON8r$DUM_TOWN
DEVCON8r$TOWN[DEVCON8r$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8r$EXC2_PLAY[DEVCON8r$SC16Q02==1] <- 1
DEVCON8r$EXC2_PLAY[DEVCON8r$SC16Q02==2] <- 0

DEVCON8r$EXC6_MATHCOMP[DEVCON8r$SC16Q06==1] <- 1
DEVCON8r$EXC6_MATHCOMP[DEVCON8r$SC16Q06==2] <- 0

DEVCON8r$EXC10_SPORT[DEVCON8r$SC16Q10==1] <- 1
DEVCON8r$EXC10_SPORT[DEVCON8r$SC16Q10==2] <- 0

DEVCON8r$EXC11_UNICORN[DEVCON8r$SC16Q11==1] <- 1
DEVCON8r$EXC11_UNICORN[DEVCON8r$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8r$SCORE_PUBLIC[DEVCON8r$SC19Q01==1] <- 1
DEVCON8r$SCORE_PUBLIC[DEVCON8r$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8r$QUAL_RECORD[DEVCON8r$SC39Q03==1] <- 1
DEVCON8r$QUAL_RECORD[DEVCON8r$SC39Q03==2] <- 0

# The rotated part 2 variables:

# LMINS, MMINS, SMINS
DEVCON8r$LHRS <- (DEVCON8r$LMINS)/60

R280 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PCGIRLS",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","LHRS","ST72Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R280

write.csv(R280, "Read_rot2.csv")

########################################################################################################
# TABLE A3 COLUMN (8) ALL GAP DECREASING NON-ROTATED VARIABLES + ROTATED PART 3 GAP DECREASING VARIABLES
########################################################################################################

### 1.DETERMINING ROTATED PART 3 GAP DECREASING VARIABLES

# We take all the gap-decreasing non-rotated variables and all rotated part 3 variables

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04",
                    "SC18Q07","SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "ST91Q03","BELONG","ATSCHL","ATTLNACT","ST91Q02","STUDREL","ST91Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 #16829
N0-N1 #31654 NA's
DEVCON8s <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8s$FEMALE[DEVCON8s$ST04Q01==1] <- 1
DEVCON8s$FEMALE[DEVCON8s$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8s$PRESCHOOL[DEVCON8s$ST05Q01==1] <- 0
DEVCON8s$PRESCHOOL[DEVCON8s$ST05Q01==2] <- 1
DEVCON8s$PRESCHOOL[DEVCON8s$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==1]  <- 5
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==2]  <- 15
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==3]  <- 60
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==4]  <- 150
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==5]  <- 350
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8s$PARPRESSURE[DEVCON8s$SC24Q01==1] <- 1
DEVCON8s$PARPRESSURE[DEVCON8s$SC24Q01==2] <- 0
DEVCON8s$PARPRESSURE[DEVCON8s$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8s$SC25Q05[is.na(DEVCON8s$SC25Q05)]  <- 0
DEVCON8s$SC25Q06[is.na(DEVCON8s$SC25Q06)]  <- 0
DEVCON8s$SC25Q07[is.na(DEVCON8s$SC25Q07)]  <- 0
DEVCON8s$SC25Q09[is.na(DEVCON8s$SC25Q09)]  <- 0
DEVCON8s$SC25Q10[is.na(DEVCON8s$SC25Q10)]  <- 0
DEVCON8s$SC25Q11[is.na(DEVCON8s$SC25Q11)]  <- 0
DEVCON8s$SC25Q12[is.na(DEVCON8s$SC25Q12)]  <- 0
DEVCON8s$FUNDMOM <-  DEVCON8s$SC25Q11
DEVCON8s$COUNCILMOM <- DEVCON8s$SC25Q10
DEVCON8s$VOLUMOM <- DEVCON8s$SC25Q05+DEVCON8s$SC25Q06+DEVCON8s$SC25Q07+DEVCON8s$SC25Q09+DEVCON8s$SC25Q12
DEVCON8s$VOLUMOM[DEVCON8s$VOLUMOM>100] <- 100 

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8s$TCM_STUASS[DEVCON8s$SC30Q01==1] <- 1
DEVCON8s$TCM_STUASS[DEVCON8s$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8s$ASS_PROG[DEVCON8s$SC18Q01==1] <- 1
DEVCON8s$ASS_PROG[DEVCON8s$SC18Q01==2] <- 0

DEVCON8s$ASS_PROM[DEVCON8s$SC18Q02==1] <- 1
DEVCON8s$ASS_PROM[DEVCON8s$SC18Q02==2] <- 0

DEVCON8s$ASS_NAT[DEVCON8s$SC18Q04==1] <- 1
DEVCON8s$ASS_NAT[DEVCON8s$SC18Q04==2] <- 0

DEVCON8s$ASS_CUR[DEVCON8s$SC18Q07==1] <- 1
DEVCON8s$ASS_CUR[DEVCON8s$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8s$STU_FEEDB[DEVCON8s$SC39Q07==1] <- 1
DEVCON8s$STU_FEEDB[DEVCON8s$SC39Q07==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8s$DUM_SMLTOWN <- ifelse(DEVCON8s$SC03Q01==2,1,0)
DEVCON8s$DUM_TOWN    <- ifelse(DEVCON8s$SC03Q01==3,1,0)
DEVCON8s$TOWN <- DEVCON8s$DUM_SMLTOWN+DEVCON8s$DUM_TOWN
DEVCON8s$TOWN[DEVCON8s$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8s$EXC2_PLAY[DEVCON8s$SC16Q02==1] <- 1
DEVCON8s$EXC2_PLAY[DEVCON8s$SC16Q02==2] <- 0

DEVCON8s$EXC6_MATHCOMP[DEVCON8s$SC16Q06==1] <- 1
DEVCON8s$EXC6_MATHCOMP[DEVCON8s$SC16Q06==2] <- 0

DEVCON8s$EXC10_SPORT[DEVCON8s$SC16Q10==1] <- 1
DEVCON8s$EXC10_SPORT[DEVCON8s$SC16Q10==2] <- 0

DEVCON8s$EXC11_UNICORN[DEVCON8s$SC16Q11==1] <- 1
DEVCON8s$EXC11_UNICORN[DEVCON8s$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8s$SCORE_PUBLIC[DEVCON8s$SC19Q01==1] <- 1
DEVCON8s$SCORE_PUBLIC[DEVCON8s$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8s$QUAL_RECORD[DEVCON8s$SC39Q03==1] <- 1
DEVCON8s$QUAL_RECORD[DEVCON8s$SC39Q03==2] <- 0

# Now for the rotated part 3 variables

# ST91Q02
#________________________________________________________________________________________________________
DEVCON8s$ATT_SA <- ifelse(DEVCON8s$ST91Q02==1,1,0)
DEVCON8s$ATT_A <- ifelse(DEVCON8s$ST91Q02==2,1,0)
DEVCON8s$ATT_CONTROL <-DEVCON8s$ATT_SA+DEVCON8s$ATT_A
# DEVCON8s$ATT_CONTROL[DEVCON8s$ATT_CONTROL>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8s$FAMPROB_SA <- ifelse(DEVCON8s$ST91Q03==1,1,0)
DEVCON8s$FAMPROB_A <- ifelse(DEVCON8s$ST91Q03==2,1,0)
DEVCON8s$BKGR_FAMPROB <-DEVCON8s$FAMPROB_SA+DEVCON8s$FAMPROB_A
# DEVCON8s$BKGR_FAMPROB[DEVCON8s$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8s$DIFFTCH_SA <- ifelse(DEVCON8s$ST91Q04==1,1,0)
DEVCON8s$DIFFTCH_A <- ifelse(DEVCON8s$ST91Q04==2,1,0)
DEVCON8s$TCHQUAL_DIFF <- DEVCON8s$DIFFTCH_SA+DEVCON8s$DIFFTCH_A
# DEVCON8s$TCHQUAL_DIFF[DEVCON8s$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

# Let's try our regression with all gap decreasing variables before we add the rotated parts

R281 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R281

R282 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R282 # BKGR_FAMPROB decreases
# Vietnam 55.74

R283 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R283 # BKGR_FAMPROB decreases, BELONG increases
# Vietnam 58.56

R284 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R284 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases
# Vietnam 58.12

R285 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R285 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases
# Vietnam 56.39

R286 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT",
                        "ATT_CONTROL"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R286 # BKGR_FAMPROB decreases, BELONG increases, ATSCHL decreases, ATTLNACT decreases, ATT_CONTROL increases
# Vietnam 57.94

R289 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT",
                        "ATT_CONTROL","STUDREL"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R289 # STUDREL decreases
# VIETNAM 57.52

R290 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","BELONG","ATSCHL","ATTLNACT",
                        "ATT_CONTROL","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R290 # STUDREL decreases, TCHQUAL_DIFF decreases
# VIETNAM  55.49

R291 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","ATSCHL","ATTLNACT","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R291
# VIETNAM 52.72

### 2. CREATING COLUMN (8) NON-ROTATED VARIABLES + ROTATED PART 2 GAP DECREASING VARIABLES

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04",
                    "SC18Q07","SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "ST91Q03","ATSCHL","ATTLNACT","STUDREL","ST91Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 # 16891
N0-N1 # NA's
DEVCON8s <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8s$FEMALE[DEVCON8s$ST04Q01==1] <- 1
DEVCON8s$FEMALE[DEVCON8s$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8s$PRESCHOOL[DEVCON8s$ST05Q01==1] <- 0
DEVCON8s$PRESCHOOL[DEVCON8s$ST05Q01==2] <- 1
DEVCON8s$PRESCHOOL[DEVCON8s$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==1]  <- 5
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==2]  <- 15
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==3]  <- 60
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==4]  <- 150
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==5]  <- 350
DEVCON8s$BOOK_N[DEVCON8s$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8s$PARPRESSURE[DEVCON8s$SC24Q01==1] <- 1
DEVCON8s$PARPRESSURE[DEVCON8s$SC24Q01==2] <- 0
DEVCON8s$PARPRESSURE[DEVCON8s$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8s$SC25Q05[is.na(DEVCON8s$SC25Q05)]  <- 0
DEVCON8s$SC25Q06[is.na(DEVCON8s$SC25Q06)]  <- 0
DEVCON8s$SC25Q07[is.na(DEVCON8s$SC25Q07)]  <- 0
DEVCON8s$SC25Q09[is.na(DEVCON8s$SC25Q09)]  <- 0
DEVCON8s$SC25Q10[is.na(DEVCON8s$SC25Q10)]  <- 0
DEVCON8s$SC25Q11[is.na(DEVCON8s$SC25Q11)]  <- 0
DEVCON8s$SC25Q12[is.na(DEVCON8s$SC25Q12)]  <- 0
DEVCON8s$FUNDMOM <-  DEVCON8s$SC25Q11
DEVCON8s$COUNCILMOM <- DEVCON8s$SC25Q10
DEVCON8s$VOLUMOM <- DEVCON8s$SC25Q05+DEVCON8s$SC25Q06+DEVCON8s$SC25Q07+DEVCON8s$SC25Q09+DEVCON8s$SC25Q12
DEVCON8s$VOLUMOM[DEVCON8s$VOLUMOM>100] <- 100 
DEVCON8s$DUTYMOM <- DEVCON8s$SC25Q02

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8s$TCM_STUASS[DEVCON8s$SC30Q01==1] <- 1
DEVCON8s$TCM_STUASS[DEVCON8s$SC30Q01==2] <- 0

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8s$ASS_PROG[DEVCON8s$SC18Q01==1] <- 1
DEVCON8s$ASS_PROG[DEVCON8s$SC18Q01==2] <- 0

DEVCON8s$ASS_PROM[DEVCON8s$SC18Q02==1] <- 1
DEVCON8s$ASS_PROM[DEVCON8s$SC18Q02==2] <- 0

DEVCON8s$ASS_NAT[DEVCON8s$SC18Q04==1] <- 1
DEVCON8s$ASS_NAT[DEVCON8s$SC18Q04==2] <- 0

DEVCON8s$ASS_CUR[DEVCON8s$SC18Q07==1] <- 1
DEVCON8s$ASS_CUR[DEVCON8s$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8s$STU_FEEDB[DEVCON8s$SC39Q07==1] <- 1
DEVCON8s$STU_FEEDB[DEVCON8s$SC39Q07==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8s$DUM_SMLTOWN <- ifelse(DEVCON8s$SC03Q01==2,1,0)
DEVCON8s$DUM_TOWN    <- ifelse(DEVCON8s$SC03Q01==3,1,0)
DEVCON8s$TOWN <- DEVCON8s$DUM_SMLTOWN+DEVCON8s$DUM_TOWN
DEVCON8s$TOWN[DEVCON8s$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8s$EXC2_PLAY[DEVCON8s$SC16Q02==1] <- 1
DEVCON8s$EXC2_PLAY[DEVCON8s$SC16Q02==2] <- 0

DEVCON8s$EXC6_MATHCOMP[DEVCON8s$SC16Q06==1] <- 1
DEVCON8s$EXC6_MATHCOMP[DEVCON8s$SC16Q06==2] <- 0

DEVCON8s$EXC10_SPORT[DEVCON8s$SC16Q10==1] <- 1
DEVCON8s$EXC10_SPORT[DEVCON8s$SC16Q10==2] <- 0

DEVCON8s$EXC11_UNICORN[DEVCON8s$SC16Q11==1] <- 1
DEVCON8s$EXC11_UNICORN[DEVCON8s$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8s$SCORE_PUBLIC[DEVCON8s$SC19Q01==1] <- 1
DEVCON8s$SCORE_PUBLIC[DEVCON8s$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8s$QUAL_RECORD[DEVCON8s$SC39Q03==1] <- 1
DEVCON8s$QUAL_RECORD[DEVCON8s$SC39Q03==2] <- 0

# Now for the rotated part 3 variables

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8s$FAMPROB_SA <- ifelse(DEVCON8s$ST91Q03==1,1,0)
DEVCON8s$FAMPROB_A <- ifelse(DEVCON8s$ST91Q03==2,1,0)
DEVCON8s$BKGR_FAMPROB <-DEVCON8s$FAMPROB_SA+DEVCON8s$FAMPROB_A
# DEVCON8s$BKGR_FAMPROB[DEVCON8s$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8s$DIFFTCH_SA <- ifelse(DEVCON8s$ST91Q04==1,1,0)
DEVCON8s$DIFFTCH_A <- ifelse(DEVCON8s$ST91Q04==2,1,0)
DEVCON8s$TCHQUAL_DIFF <- DEVCON8s$DIFFTCH_SA+DEVCON8s$DIFFTCH_A
# DEVCON8s$TCHQUAL_DIFF[DEVCON8s$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

R292 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB","PCGIRLS",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","ATSCHL","ATTLNACT","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8s,export=FALSE)
R292

write.csv(R292, "Read_rot3.csv")

