# PISA2012_FL_part8
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 08/03/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in READ, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# PLEASE NOTE THAT THIS IS THE FILE FOR THE READING REGRESSIONS
# For the READ regressions please see PISA_2012_FL_part2 to part6
##################################################################################

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) (in part 1)
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES (in part 1)
# 3. PISA SCORES (in part 1)
# 4. REGRESSION ANALYSIS OF A MODIFIED FRYER & LEVITT (2004) APPROACH MATH (part 2 - part 6)
# 5. REGRESSION ANALYSIS OF A MODIFIED FRYER & LEVITT (2004) APPROACH READING (part 7 - part 11) 
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

############### 5. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

################### 5.2.6 Non-rotated Questions - all gap decreasing & increasing / Sensitivty ######################

# Finally, we need to see how the Vietnam dummy increases and decreases when we add all gap decreasing and gap increasing
# variables to it (separately) on a sample of non-missing values for all variables.

# So let's see first how many missing variables we have overall:

# How big is our initital sample size
T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0)) 
N0 # 48483 data points (we already know that)

# How many non-missing values for all non-rotated student (excl SC25), teacher, pedagogical practices and school variables?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","HISEI","MISCED",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS",
                    "STRATIO","PROPCERT","PROPQUAL","TCSHORT","TCFOCST", 
                    "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04","SC39Q08","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "SC18Q01","SC18Q02","SC18Q03","SC18Q04","SC18Q05","SC18Q06","SC18Q07","SC18Q08",
                    "SC39Q07","SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE",
                    "RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","SC16Q01","SC16Q02","SC16Q03","SC16Q04",
                    "SC16Q05","SC16Q06","SC16Q07","SC16Q08","SC16Q09","SC16Q10","SC16Q11","SC19Q01",
                    "SC19Q02","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC39Q03",
                    "SCHSEL","STUDCLIM","TEACCLIM","TCMORALE")]
N1 <- NROW(na.omit(T1b)) 
N1 # 18474
N0-N1 # 30009 NAs
DEVCON8p <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the variables (again)

######################### Student variables ####################

#ST04Q01
#___________________________________________________________________________________________________________
DEVCON8p$FEMALE[DEVCON8p$ST04Q01==1] <- 1
DEVCON8p$FEMALE[DEVCON8p$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8p$PRESCHOOL[DEVCON8p$ST05Q01==1] <- 0
DEVCON8p$PRESCHOOL[DEVCON8p$ST05Q01==2] <- 1
DEVCON8p$PRESCHOOL[DEVCON8p$ST05Q01==3] <- 1

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
DEVCON8p$BOOK_N[DEVCON8p$ST28Q01==1]  <- 5
DEVCON8p$BOOK_N[DEVCON8p$ST28Q01==2]  <- 15
DEVCON8p$BOOK_N[DEVCON8p$ST28Q01==3]  <- 60
DEVCON8p$BOOK_N[DEVCON8p$ST28Q01==4]  <- 150
DEVCON8p$BOOK_N[DEVCON8p$ST28Q01==5]  <- 350
DEVCON8p$BOOK_N[DEVCON8p$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8p$PARPRESSURE[DEVCON8p$SC24Q01==1] <- 1
DEVCON8p$PARPRESSURE[DEVCON8p$SC24Q01==2] <- 0
DEVCON8p$PARPRESSURE[DEVCON8p$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8p$SC25Q01[is.na(DEVCON8p$SC25Q01)]  <- 0
DEVCON8p$SC25Q02[is.na(DEVCON8p$SC25Q02)]  <- 0
DEVCON8p$SC25Q03[is.na(DEVCON8p$SC25Q03)]  <- 0
DEVCON8p$SC25Q04[is.na(DEVCON8p$SC25Q04)]  <- 0
DEVCON8p$SC25Q05[is.na(DEVCON8p$SC25Q05)]  <- 0
DEVCON8p$SC25Q06[is.na(DEVCON8p$SC25Q06)]  <- 0
DEVCON8p$SC25Q07[is.na(DEVCON8p$SC25Q07)]  <- 0
DEVCON8p$SC25Q08[is.na(DEVCON8p$SC25Q08)]  <- 0
DEVCON8p$SC25Q09[is.na(DEVCON8p$SC25Q09)]  <- 0
DEVCON8p$SC25Q10[is.na(DEVCON8p$SC25Q10)]  <- 0
DEVCON8p$SC25Q11[is.na(DEVCON8p$SC25Q11)]  <- 0
DEVCON8p$SC25Q12[is.na(DEVCON8p$SC25Q12)]  <- 0

#TIGERMOM
DEVCON8p$TIGERMOM  <- DEVCON8p$SC25Q01+DEVCON8p$SC25Q03
DEVCON8p$TIGERMOM[DEVCON8p$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8p$VOLUMOM <- DEVCON8p$SC25Q05+DEVCON8p$SC25Q06+DEVCON8p$SC25Q07+DEVCON8p$SC25Q09+DEVCON8p$SC25Q12
DEVCON8p$VOLUMOM[DEVCON8p$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8p$TEACHMOM <- DEVCON8p$SC25Q08

#FUNDMOM
DEVCON8p$FUNDMOM <-  DEVCON8p$SC25Q11

#COUNCILMOM
DEVCON8p$COUNCILMOM <- DEVCON8p$SC25Q10

######################### Teacher variables ########

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8p$TCM_STUASS[DEVCON8p$SC30Q01==1] <- 1
DEVCON8p$TCM_STUASS[DEVCON8p$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8p$TCM_PEER[DEVCON8p$SC30Q02==1] <- 1
DEVCON8p$TCM_PEER[DEVCON8p$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8p$TCM_OBSER[DEVCON8p$SC30Q03==1] <- 1
DEVCON8p$TCM_OBSER[DEVCON8p$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8p$TCM_INSPE[DEVCON8p$SC30Q04==1] <- 1
DEVCON8p$TCM_INSPE[DEVCON8p$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8p$TCH_MENT[DEVCON8p$SC39Q08==1] <- 1
DEVCON8p$TCH_MENT[DEVCON8p$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8p <- merge(DEVCON8p,SC31OUT.rda,by="NEWID")
DEVCON8p$TCH_INCENTV <- rescale(DEVCON8p$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

######################### Pedagogical Practices variables ########

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8p$ASS_PROG[DEVCON8p$SC18Q01==1] <- 1
DEVCON8p$ASS_PROG[DEVCON8p$SC18Q01==2] <- 0

DEVCON8p$ASS_PROM[DEVCON8p$SC18Q02==1] <- 1
DEVCON8p$ASS_PROM[DEVCON8p$SC18Q02==2] <- 0

DEVCON8p$ASS_INSTR[DEVCON8p$SC18Q03==1] <- 1
DEVCON8p$ASS_INSTR[DEVCON8p$SC18Q03==2] <- 0

DEVCON8p$ASS_NAT[DEVCON8p$SC18Q04==1] <- 1
DEVCON8p$ASS_NAT[DEVCON8p$SC18Q04==2] <- 0

DEVCON8p$ASS_SCH[DEVCON8p$SC18Q05==1] <- 1
DEVCON8p$ASS_SCH[DEVCON8p$SC18Q05==2] <- 0

DEVCON8p$ASS_TCH[DEVCON8p$SC18Q06==1] <- 1
DEVCON8p$ASS_TCH[DEVCON8p$SC18Q06==2] <- 0

DEVCON8p$ASS_CUR[DEVCON8p$SC18Q07==1] <- 1
DEVCON8p$ASS_CUR[DEVCON8p$SC18Q07==2] <- 0

DEVCON8p$ASS_OTH[DEVCON8p$SC18Q08==1] <- 1
DEVCON8p$ASS_OTH[DEVCON8p$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8p$STU_FEEDB[DEVCON8p$SC39Q07==1] <- 1
DEVCON8p$STU_FEEDB[DEVCON8p$SC39Q07==2] <- 0

######################### School variables ########

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8p$PRIVATESCL[DEVCON8p$SC01Q01==2] <- 1
DEVCON8p$PRIVATESCL[DEVCON8p$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8p$DUM_VILLAGE <- ifelse(DEVCON8p$SC03Q01==1,1,0)
DEVCON8p$DUM_SMLTOWN <- ifelse(DEVCON8p$SC03Q01==2,1,0)
DEVCON8p$DUM_TOWN    <- ifelse(DEVCON8p$SC03Q01==3,1,0)
DEVCON8p$DUM_CITY    <- ifelse(DEVCON8p$SC03Q01==4,1,0)
DEVCON8p$DUM_LRGCITY <- ifelse(DEVCON8p$SC03Q01==5,1,0)

DEVCON8p$TOWN <- DEVCON8p$DUM_SMLTOWN+DEVCON8p$DUM_TOWN
DEVCON8p$TOWN[DEVCON8p$TOWN>1] <- 1
DEVCON8p$CITY <- DEVCON8p$DUM_CITY+DEVCON8p$DUM_LRGCITY
DEVCON8p$CITY[DEVCON8p$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8p$EXC1_BAND[DEVCON8p$SC16Q01==1] <- 1
DEVCON8p$EXC1_BAND[DEVCON8p$SC16Q01==2] <- 0

DEVCON8p$EXC2_PLAY[DEVCON8p$SC16Q02==1] <- 1
DEVCON8p$EXC2_PLAY[DEVCON8p$SC16Q02==2] <- 0

DEVCON8p$EXC3_NEWS[DEVCON8p$SC16Q03==1] <- 1
DEVCON8p$EXC3_NEWS[DEVCON8p$SC16Q03==2] <- 0

DEVCON8p$EXC4_VOLU[DEVCON8p$SC16Q04==1] <- 1
DEVCON8p$EXC4_VOLU[DEVCON8p$SC16Q04==2] <- 0

DEVCON8p$EXC5_MCLUB[DEVCON8p$SC16Q05==1] <- 1
DEVCON8p$EXC5_MCLUB[DEVCON8p$SC16Q05==2] <- 0

DEVCON8p$EXC6_MATHCOMP[DEVCON8p$SC16Q06==1] <- 1
DEVCON8p$EXC6_MATHCOMP[DEVCON8p$SC16Q06==2] <- 0

DEVCON8p$EXC7_CHESS[DEVCON8p$SC16Q07==1] <- 1
DEVCON8p$EXC7_CHESS[DEVCON8p$SC16Q07==2] <- 0

DEVCON8p$EXC8_ICTCB[DEVCON8p$SC16Q08==1] <- 1
DEVCON8p$EXC8_ICTCB[DEVCON8p$SC16Q08==2] <- 0

DEVCON8p$EXC9_ARTCB[DEVCON8p$SC16Q09==1] <- 1
DEVCON8p$EXC9_ARTCB[DEVCON8p$SC16Q09==2] <- 0

DEVCON8p$EXC10_SPORT[DEVCON8p$SC16Q10==1] <- 1
DEVCON8p$EXC10_SPORT[DEVCON8p$SC16Q10==2] <- 0

DEVCON8p$EXC11_UNICORN[DEVCON8p$SC16Q11==1] <- 1
DEVCON8p$EXC11_UNICORN[DEVCON8p$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8p$SCORE_PUBLIC[DEVCON8p$SC19Q01==1] <- 1
DEVCON8p$SCORE_PUBLIC[DEVCON8p$SC19Q01==2] <- 0

DEVCON8p$SCORE_AUTHRITS[DEVCON8p$SC19Q02==1] <- 1
DEVCON8p$SCORE_AUTHRITS[DEVCON8p$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8p$QUAL_RECORD[DEVCON8p$SC39Q03==1] <- 1
DEVCON8p$QUAL_RECORD[DEVCON8p$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8p, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8p.rda") 

# First, remember, we have a smaller data set (18474 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R253 <- pisa.reg.pv(pvlabel="READ", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8p,export=FALSE)
R253
#Estimate Std. Error t value
#(Intercept)   417.40       3.32  125.90
#VIETNAM        94.69       6.00   15.79
#R-squared      20.92       2.31    9.04

######################################## Gap decreasing variables #################################################

# Let's try our regression with all gap decreasing variables before we do it individually per factor (student,
# teachers, pedagogical practices and schools)

R254 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R254
#Estimate Std. Error t value
#(Intercept)     353.41      25.74   13.73
#VIETNAM          52.61       6.08    8.65
#FEMALE           22.88       1.83   12.52
#PRESCHOOL        25.90       4.02    6.44
#REPEAT          -45.81       3.89  -11.76
#ST08Q01          -8.57       1.64   -5.21
#ST115Q01         -9.16       1.92   -4.77
#BOOK_N            0.06       0.01    5.20
#PARPRESSURE       1.98       4.54    0.44
#PCGIRLS          32.15      18.44    1.74
#VOLUMOM           0.02       0.06    0.32
#FUNDMOM           0.10       0.06    1.49
#COUNCILMOM       -0.20       0.06   -3.17
#PROPCERT          4.35       6.28    0.69
#TCSHORT           2.82       2.24    1.26
#TCM_STUASS        3.95       8.63    0.46
#ASS_PROG        -33.18      12.25   -2.71
#ASS_PROM         11.05       6.17    1.79
#ASS_NAT          -0.52       7.20   -0.07
#ASS_CUR          -7.50      10.48   -0.72
#STU_FEEDB         3.09       5.20    0.59
#TOWN             -6.31       4.21   -1.50
#CLSIZE            0.86       0.24    3.59
#COMPWEB          12.94       6.52    1.99
#SCMATEDU          6.18       3.24    1.91
#SCMATBUI          1.21       2.61    0.46
#EXC2_PLAY        13.37       5.00    2.67
#EXC6_MATHCOMP    11.57       5.28    2.19
#EXC10_SPORT       1.81      15.17    0.12
#EXC11_UNICORN     9.37       6.20    1.51
#SCORE_PUBLIC      4.93       5.65    0.87
#LEADINST          1.62       2.57    0.63
#QUAL_RECORD       3.25       8.56    0.38
#SCHSEL            2.08       3.31    0.63
#TEACCLIM         -1.97       2.92   -0.67
#R-squared        43.09       2.36   18.24

# The gap decreasing student-related variables:

R255 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R255
#VIETNAM: 68.54 

# The gap decreasing student and teacher variables:

R256 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R256
#VIETNAM: 66.77

# The gap decreasing student, teacher and pedagogical practices variables:

R257 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R257
#VIETNAM: 65.75

######################################## Gap increasing variables #################################################

# We look at the individual sections from PISA_20120_FL_part7 and copy the gap increasing variables

# Students:
#"ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
#"TIGERMOM","TEACHMOM"

# Teachers:
#"STRATIO","PROPQUAL","TCFOCST","TCM_PEER","TCM_OBSER",
#"TCM_INSPE","TCH_INCENTV","TCH_MENT"

# Pedagog. Practices:
#"ASS_INSTR","ASS_SCH" ,"ASS_TCH","ASS_OTH"

# Schools:
#"PRIVATESCL","SC02Q02",
#"DUM_VILLAGE","SCHSIZE","RATCMP15","SCORE_AUTHRITS","SCHAUTON",
#"TCHPARTI","LEADCOM","LEADPD","LEADTCH","STUDCLIM","TCMORALE"

# All student, teachers, pedagogical practices and school gap increasing variables:
R258 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","TEACHMOM","STRATIO","PROPQUAL","TCFOCST","TCM_PEER","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT","ASS_INSTR","ASS_SCH","ASS_TCH","ASS_OTH",
                        "PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","SCHSIZE","RATCMP15","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADPD","LEADTCH","STUDCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R258
#VIETNAM:  110.80

# All student gap increasing variables:
R259 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R259
#VIETNAM: 100.99

# All student and teachers gap increasing variables:
R260 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","TEACHMOM","STRATIO","PROPQUAL","TCFOCST","TCM_PEER","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R260
#VIETNAM: 103.35

# All student,teachers and pedagogical practices gap increasing variables:
R261 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","TEACHMOM","STRATIO","PROPQUAL","TCFOCST","TCM_PEER","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT","ASS_INSTR","ASS_SCH","ASS_TCH","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8p,export=FALSE)
R261
#VIETNAM: 102.94
