# PISA2012_FL_part13
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 08/05/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in SCIE, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# PLEASE NOTE THAT THIS IS THE FILE FOR THE SCIENCE REGRESSIONS
# For the MATH see PISA_2012_FL_part2 to part6, for SCIEING see PISA_2012_FL_part7 to part11
##################################################################################

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) (in part 1)
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES (in part 1)
# 3. PISA SCORES (in part 1)
# 4. REGRESSION ANALYSIS OF A MODIFIED FRYER & LEVITT (2004) APPROACH MATH (part 2 - part 6)
# 5. REGRESSION ANALYSIS OF A MODIFIED FRYER & LEVITT (2004) APPROACH READING (part 7 - 11)
# 6. REGRESSION ANALYSIS OF A MODIFIED FRYER & LEVITT (2004) APPROACH SCIENCE (part 12 - 16)
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

############### 6. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

################### 6.2.6 Non-rotated Questions - all gap decreasing & increasing / Sensitivty ######################

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
DEVCON8x <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the variables (again)

######################### Student variables ####################

#ST04Q01
#___________________________________________________________________________________________________________
DEVCON8x$FEMALE[DEVCON8x$ST04Q01==1] <- 1
DEVCON8x$FEMALE[DEVCON8x$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8x$PRESCHOOL[DEVCON8x$ST05Q01==1] <- 0
DEVCON8x$PRESCHOOL[DEVCON8x$ST05Q01==2] <- 1
DEVCON8x$PRESCHOOL[DEVCON8x$ST05Q01==3] <- 1

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
DEVCON8x$BOOK_N[DEVCON8x$ST28Q01==1]  <- 5
DEVCON8x$BOOK_N[DEVCON8x$ST28Q01==2]  <- 15
DEVCON8x$BOOK_N[DEVCON8x$ST28Q01==3]  <- 60
DEVCON8x$BOOK_N[DEVCON8x$ST28Q01==4]  <- 150
DEVCON8x$BOOK_N[DEVCON8x$ST28Q01==5]  <- 350
DEVCON8x$BOOK_N[DEVCON8x$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8x$PARPRESSURE[DEVCON8x$SC24Q01==1] <- 1
DEVCON8x$PARPRESSURE[DEVCON8x$SC24Q01==2] <- 0
DEVCON8x$PARPRESSURE[DEVCON8x$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8x$SC25Q01[is.na(DEVCON8x$SC25Q01)]  <- 0
DEVCON8x$SC25Q02[is.na(DEVCON8x$SC25Q02)]  <- 0
DEVCON8x$SC25Q03[is.na(DEVCON8x$SC25Q03)]  <- 0
DEVCON8x$SC25Q04[is.na(DEVCON8x$SC25Q04)]  <- 0
DEVCON8x$SC25Q05[is.na(DEVCON8x$SC25Q05)]  <- 0
DEVCON8x$SC25Q06[is.na(DEVCON8x$SC25Q06)]  <- 0
DEVCON8x$SC25Q07[is.na(DEVCON8x$SC25Q07)]  <- 0
DEVCON8x$SC25Q08[is.na(DEVCON8x$SC25Q08)]  <- 0
DEVCON8x$SC25Q09[is.na(DEVCON8x$SC25Q09)]  <- 0
DEVCON8x$SC25Q10[is.na(DEVCON8x$SC25Q10)]  <- 0
DEVCON8x$SC25Q11[is.na(DEVCON8x$SC25Q11)]  <- 0
DEVCON8x$SC25Q12[is.na(DEVCON8x$SC25Q12)]  <- 0

#TIGERMOM
DEVCON8x$TIGERMOM  <- DEVCON8x$SC25Q01+DEVCON8x$SC25Q03
DEVCON8x$TIGERMOM[DEVCON8x$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8x$VOLUMOM <- DEVCON8x$SC25Q05+DEVCON8x$SC25Q06+DEVCON8x$SC25Q07+DEVCON8x$SC25Q09+DEVCON8x$SC25Q12
DEVCON8x$VOLUMOM[DEVCON8x$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8x$TEACHMOM <- DEVCON8x$SC25Q08

#FUNDMOM
DEVCON8x$FUNDMOM <-  DEVCON8x$SC25Q11

#COUNCILMOM
DEVCON8x$COUNCILMOM <- DEVCON8x$SC25Q10

######################### Teacher variables ########

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8x$TCM_STUASS[DEVCON8x$SC30Q01==1] <- 1
DEVCON8x$TCM_STUASS[DEVCON8x$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8x$TCM_PEER[DEVCON8x$SC30Q02==1] <- 1
DEVCON8x$TCM_PEER[DEVCON8x$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8x$TCM_OBSER[DEVCON8x$SC30Q03==1] <- 1
DEVCON8x$TCM_OBSER[DEVCON8x$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8x$TCM_INSPE[DEVCON8x$SC30Q04==1] <- 1
DEVCON8x$TCM_INSPE[DEVCON8x$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8x$TCH_MENT[DEVCON8x$SC39Q08==1] <- 1
DEVCON8x$TCH_MENT[DEVCON8x$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8x <- merge(DEVCON8x,SC31OUT.rda,by="NEWID")
DEVCON8x$TCH_INCENTV <- rescale(DEVCON8x$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

######################### Pedagogical Practices variables ########

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8x$ASS_PROG[DEVCON8x$SC18Q01==1] <- 1
DEVCON8x$ASS_PROG[DEVCON8x$SC18Q01==2] <- 0

DEVCON8x$ASS_PROM[DEVCON8x$SC18Q02==1] <- 1
DEVCON8x$ASS_PROM[DEVCON8x$SC18Q02==2] <- 0

DEVCON8x$ASS_INSTR[DEVCON8x$SC18Q03==1] <- 1
DEVCON8x$ASS_INSTR[DEVCON8x$SC18Q03==2] <- 0

DEVCON8x$ASS_NAT[DEVCON8x$SC18Q04==1] <- 1
DEVCON8x$ASS_NAT[DEVCON8x$SC18Q04==2] <- 0

DEVCON8x$ASS_SCH[DEVCON8x$SC18Q05==1] <- 1
DEVCON8x$ASS_SCH[DEVCON8x$SC18Q05==2] <- 0

DEVCON8x$ASS_TCH[DEVCON8x$SC18Q06==1] <- 1
DEVCON8x$ASS_TCH[DEVCON8x$SC18Q06==2] <- 0

DEVCON8x$ASS_CUR[DEVCON8x$SC18Q07==1] <- 1
DEVCON8x$ASS_CUR[DEVCON8x$SC18Q07==2] <- 0

DEVCON8x$ASS_OTH[DEVCON8x$SC18Q08==1] <- 1
DEVCON8x$ASS_OTH[DEVCON8x$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8x$STU_FEEDB[DEVCON8x$SC39Q07==1] <- 1
DEVCON8x$STU_FEEDB[DEVCON8x$SC39Q07==2] <- 0

######################### School variables ########

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8x$PRIVATESCL[DEVCON8x$SC01Q01==2] <- 1
DEVCON8x$PRIVATESCL[DEVCON8x$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8x$DUM_VILLAGE <- ifelse(DEVCON8x$SC03Q01==1,1,0)
DEVCON8x$DUM_SMLTOWN <- ifelse(DEVCON8x$SC03Q01==2,1,0)
DEVCON8x$DUM_TOWN    <- ifelse(DEVCON8x$SC03Q01==3,1,0)
DEVCON8x$DUM_CITY    <- ifelse(DEVCON8x$SC03Q01==4,1,0)
DEVCON8x$DUM_LRGCITY <- ifelse(DEVCON8x$SC03Q01==5,1,0)

DEVCON8x$TOWN <- DEVCON8x$DUM_SMLTOWN+DEVCON8x$DUM_TOWN
DEVCON8x$TOWN[DEVCON8x$TOWN>1] <- 1
DEVCON8x$CITY <- DEVCON8x$DUM_CITY+DEVCON8x$DUM_LRGCITY
DEVCON8x$CITY[DEVCON8x$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8x$EXC1_BAND[DEVCON8x$SC16Q01==1] <- 1
DEVCON8x$EXC1_BAND[DEVCON8x$SC16Q01==2] <- 0

DEVCON8x$EXC2_PLAY[DEVCON8x$SC16Q02==1] <- 1
DEVCON8x$EXC2_PLAY[DEVCON8x$SC16Q02==2] <- 0

DEVCON8x$EXC3_NEWS[DEVCON8x$SC16Q03==1] <- 1
DEVCON8x$EXC3_NEWS[DEVCON8x$SC16Q03==2] <- 0

DEVCON8x$EXC4_VOLU[DEVCON8x$SC16Q04==1] <- 1
DEVCON8x$EXC4_VOLU[DEVCON8x$SC16Q04==2] <- 0

DEVCON8x$EXC5_MCLUB[DEVCON8x$SC16Q05==1] <- 1
DEVCON8x$EXC5_MCLUB[DEVCON8x$SC16Q05==2] <- 0

DEVCON8x$EXC6_MATHCOMP[DEVCON8x$SC16Q06==1] <- 1
DEVCON8x$EXC6_MATHCOMP[DEVCON8x$SC16Q06==2] <- 0

DEVCON8x$EXC7_CHESS[DEVCON8x$SC16Q07==1] <- 1
DEVCON8x$EXC7_CHESS[DEVCON8x$SC16Q07==2] <- 0

DEVCON8x$EXC8_ICTCB[DEVCON8x$SC16Q08==1] <- 1
DEVCON8x$EXC8_ICTCB[DEVCON8x$SC16Q08==2] <- 0

DEVCON8x$EXC9_ARTCB[DEVCON8x$SC16Q09==1] <- 1
DEVCON8x$EXC9_ARTCB[DEVCON8x$SC16Q09==2] <- 0

DEVCON8x$EXC10_SPORT[DEVCON8x$SC16Q10==1] <- 1
DEVCON8x$EXC10_SPORT[DEVCON8x$SC16Q10==2] <- 0

DEVCON8x$EXC11_UNICORN[DEVCON8x$SC16Q11==1] <- 1
DEVCON8x$EXC11_UNICORN[DEVCON8x$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8x$SCORE_PUBLIC[DEVCON8x$SC19Q01==1] <- 1
DEVCON8x$SCORE_PUBLIC[DEVCON8x$SC19Q01==2] <- 0

DEVCON8x$SCORE_AUTHRITS[DEVCON8x$SC19Q02==1] <- 1
DEVCON8x$SCORE_AUTHRITS[DEVCON8x$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8x$QUAL_RECORD[DEVCON8x$SC39Q03==1] <- 1
DEVCON8x$QUAL_RECORD[DEVCON8x$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8x, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8x.rda") 

# First, remember, we have a smaller data set (18474 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R368 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R368
#Estimate Std. Error t value
#(Intercept)   408.50       3.33  122.76
#VIETNAM       123.38       5.70   21.65
#R-squared      31.67       2.27   13.98

######################################## Gap decreasing variables #################################################

# Let's try our regression with all gap decreasing variables before we do it individually per factor (student,
# teachers, pedagogical practices and schools)

R369 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R369
# VIETNAM 80.09

write.csv(R369, "SCIE_stu_ped_teach_school.csv")

# The gap decreasing student-related variables:
R370 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R370
#VIETNAM: 94.84 

write.csv(R370, "SCIE_stu.csv")

# The gap decreasing student and teacher variables:
R371 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R371
#VIETNAM: 94.05

# The gap decreasing student, teacher and pedagogical practices variables:
R372 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R372
#VIETNAM: 92.98

write.csv(R372, "SCIE_stu_ped_teach.csv")

######################################## Gap increasing variables #################################################

# All student, teachers, pedagogical practices and school gap increasing variables:
R373 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES","VOLUMOM",
                        "TIGERMOM","TEACHMOM","STRATIO","PROPQUAL","TCFOCST","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT","ASS_INSTR","ASS_TCH","ASS_OTH",
                        "SC02Q02","DUM_VILLAGE","SCHSIZE","RATCMP15","SCMATBUI","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADPD","LEADTCH","STUDCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R373
#VIETNAM: 143.43

# All student gap increasing variables:
R374 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES","VOLUMOM",
                        "TIGERMOM","TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R374
#VIETNAM: 133.39 

# All student and teachers gap increasing variables:
R375 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES","VOLUMOM",
                        "TIGERMOM","TEACHMOM","STRATIO","PROPQUAL","TCFOCST","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R375
#VIETNAM: 134.54

#Estimate Std. Error t value
#(Intercept)   423.60      18.38   23.05
#VIETNAM       134.54       5.57   24.17
#ST09Q01       -20.34       2.67   -7.61
#HISEI           0.37       0.05    7.49
#MISCED          1.66       0.66    2.53
#WEALTH          8.99       1.26    7.12
#CULTPOS        -2.43       1.07   -2.27
#HEDRES         12.33       1.14   10.83
#VOLUMOM         0.15       0.08    1.76
#TIGERMOM       -0.04       0.06   -0.60
#TEACHMOM       -0.14       0.08   -1.67
#STRATIO        -0.09       0.25   -0.36
#PROPQUAL       14.18      15.74    0.90
#TCFOCST         0.13       2.61    0.05
#TCM_OBSER       5.13       6.07    0.85
#TCM_INSPE      -7.80       5.28   -1.48
#TCH_INCENTV     1.38       2.54    0.54
#TCH_MENT        7.32       7.05    1.04
#R-squared      42.50       2.03   20.96

# All student,teachers and pedagogical practices gap increasing variables:
R376 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES","VOLUMOM",
                        "TIGERMOM","TEACHMOM","STRATIO","PROPQUAL","TCFOCST","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT","ASS_INSTR","ASS_TCH","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8x,export=FALSE)
R376
#VIETNAM:  134.54

#Estimate Std. Error t value
#(Intercept)   421.41      18.83   22.37
#VIETNAM       134.54       5.69   23.66
#ST09Q01       -20.25       2.71   -7.47
#HISEI           0.37       0.05    7.40
#MISCED          1.66       0.67    2.47
#WEALTH          8.92       1.26    7.08
#CULTPOS        -2.41       1.09   -2.22
#HEDRES         12.29       1.14   10.79
#VOLUMOM         0.14       0.08    1.66
#TIGERMOM       -0.04       0.06   -0.63
#TEACHMOM       -0.14       0.09   -1.68
#STRATIO        -0.08       0.26   -0.30
#PROPQUAL       14.36      15.62    0.92
#TCFOCST         0.09       2.59    0.04
#TCM_OBSER       5.36       6.18    0.87
#TCM_INSPE      -7.97       5.34   -1.49
#TCH_INCENTV     0.89       2.55    0.35
#TCH_MENT        6.33       7.72    0.82
#ASS_INSTR       4.49       5.23    0.86
#ASS_TCH        -2.31       5.55   -0.42
#ASS_OTH         2.02       6.43    0.31
#R-squared      42.56       2.04   20.87



