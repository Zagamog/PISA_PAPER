# PISA2012_FL_part4
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/28/2015

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
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH (part 2 - part 6)
###################################################################################

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

############### 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

############# 4.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# NON-ROTATED PART:

# 1. Students		
# Background: FEMALE, ST05Q01, REPEAT (indexed ST07), ST08Q01, ST09Q01, ST115Q01, MISCED, HISEI,		
# --------------WEALTH, CULTPOS, HEDRES, ST28Q01		
# Home Support: SC25 (Parent Participation, SC), SC24Q01 (Parental Expectations, SC)		
# Gender Balance: PCGIRLS (Proportion of girls enrolled at school, SC)

# 2. Teachers		
# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT, SMRATIO		
# Quality: TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08, SC35Q02		

# 3. Pedagogical practices		
# General / student-perceived teaching practices: SC40Q01-SC40Q03 (Practices in Maths, SC)		
# Assessment: SC18Q01-Q08		
# Classroom Management: SC39Q07 (Seeking student feedback, SC)	

# 4. Schools		
# Type: SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SC03Q01, SCHSIZE (SC)		
# Resources: RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),		
# --------------CLSIZE (Class Size based on SC05, SC)		
# --------------SC16Q01-Q11		
# --------------SCMATEDU (Quality of educ. resources, SC), SCMATBUI (Quality of Physical Infrastructure, SC),		
# --------------SC20Q01 (Additional maths lessons offered, SC)		
#  Leadership: LEADCOM (Framing Schools goal and curriculum, SC), LEADINST (Instructional Leadership, SC), 		
# --------------LEADPD (Promoting Development, SC), LEADTCH (Teacher Participation in Leadership, SC),		
# --------------SC19Q01 & SC19Q02 (if Student Achievement data is made available, SC), SCHAUTON (School autonomy, SC), 		
# --------------TCHPARTI (Teacher participation, SC), SC39Q03 (recording of student/teacher/test data, SC)		
# Selectivity: SCHSEL (School Selectivity of students, SC)		
# Climate: STUDCLIM (Student aspects of school climate, SC), TEACCLIM (teacher aspects of school climate, SC), 		
# --------------TCMORALE (Teacher Morale, SC)		

# ROTATED PART 1	

# 1. Students	
#-----Effort: MATWKETH	
#-----Attitude: INSTMOT, INTMAT, SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS 	

# ROTATED PART 2

# 1. Students
#-----Effort: ST55Q02 (Math lessons out of school), ST57Q01-Q06 (dropped for Math)
#-----Preparation: EXAPPLM, EXPUREM, FACMCONC
# 2. Teachers
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes)

# ROTATED PART 3

# 1. Students
#-----Background: ST91Q03
#-----Attitude: SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT, ST91Q02
# 2. Teachers
#----Quality: MTSUP, STUDREL, ST91Q04
# 3. Pedagogical Practices
#-----General: TCHBEHTD, TCHBEHSO
#-----Assessment: TCHBEHFA
#-----Cognitive Activation: COGACT
#-----Classroom Management: CLSMAN, DISCLIMA

########################## 4.2.1 Explanatory Variables - rotated & non-rotated questions #######################

############################### 4.2.7 Non-rotated & PART 3 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the first (part 1) rotated part

# We will add the rotated variables according to the subsector (students, teachers, etc) they belong to (see our schematic structure)
# and within, as always, in order that they have been asked

# 1. STUDENTS
#-----Background: ST91Q03
#-----Attitude: SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT, ST91Q02

# 2. TEACHERS
#----Quality: MTSUP, STUDREL, ST91Q04

# 3. PEDAGOGICAL PRACTICES
#-----General: TCHBEHTD, TCHBEHSO
#-----Assessment: TCHBEHFA
#-----Cognitive Activation: COGACT
#-----Classroom Management: CLSMAN, DISCLIMA

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

# Surprisingly with so many variables, the sample size seems ok to work with (we don't want it to be much less than
# a third of the original sample size which was 48483 data points)

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

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
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8k <- merge(DEVCON8k,SC31OUT.rda,by="NEWID")
DEVCON8k$TCH_INCENTV <- rescale(DEVCON8k$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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
# DEVCON8k$TCHQUAL_DIFF[DEVCON8k$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8k, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8k.rda") 

R158 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R158
#Estimate Std. Error t value
#(Intercept)   394.10       3.02  130.52
#VIETNAM       123.64       6.84   18.08
#R-squared      28.08       2.56   10.98

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
#Estimate Std. Error t value
#(Intercept)     349.24      21.92   15.94
#VIETNAM          74.86       7.92    9.46
#PRESCHOOL        24.13       4.41    5.47
#REPEAT          -36.59       3.10  -11.81
#ST08Q01          -6.83       1.76   -3.89
#ST115Q01         -5.74       2.03   -2.83
#BOOK_N            0.07       0.01    5.36
#PARPRESSURE      10.36       4.64    2.23
#PCGIRLS          16.94      15.90    1.06
#FUNDMOM           0.18       0.07    2.66
#COUNCILMOM       -0.14       0.06   -2.32
#PROPCERT         16.76       7.18    2.33
#SMRATIO          -0.03       0.01   -2.15
#TCSHORT           2.08       1.85    1.13
#TCFOCST          -2.80       1.98   -1.41
#TCM_STUASS        3.06       8.66    0.35
#TCM_PEER         -7.08       5.23   -1.35
#TCH_INCENTV      -2.96       2.74   -1.08
#ASS_PROG        -15.58      10.50   -1.48
#ASS_PROM         11.05       5.97    1.85
#ASS_SCH           1.18       7.96    0.15
#STU_FEEDB         2.52       4.98    0.51
#COMP_USE         -1.51       5.58   -0.27
#TXT_BOOK        -10.04       7.46   -1.35
#TOWN             -9.63       3.98   -2.42
#CLSIZE            0.80       0.25    3.26
#COMPWEB          13.06       6.76    1.93
#SCMATEDU          5.98       3.15    1.90
#SCMATBUI          3.40       2.57    1.32
#EXC2_PLAY         8.96       3.96    2.26
#EXC6_MATHCOMP    -1.39       5.59   -0.25
#EXC10_SPORT      -7.41      10.08   -0.74
#EXC11_UNICORN     6.98       6.21    1.12
#SCL_EXTR_CL       9.07       5.18    1.75
#SCORE_PUBLIC      9.95       5.05    1.97
#QUAL_RECORD       3.26       6.95    0.47
#SCHSEL            0.60       3.37    0.18
#R-squared        43.59       2.47   17.63

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

# We have now checked all the student related variables from the rotational part 3. Let's group them into
# decreasing and increasing variables 

# Gap decreasing variables

R166a <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                         "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                         "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                         "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                         "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","ANXMAT",
                         "ATSCHL","ATTLNACT"),
                     weight="W_FSTUWT",
                     data=DEVCON8k,export=FALSE)
R166a # Vietnam 70.83 

# Gap increasing variables

R166b <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","SCMAT","BELONG","ATT_CONTROL"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R166b # Vietnam 80.53 

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

# We have now checked all the students and teachers related variables from the rotational part 3. Let's group them into
# decreasing and increasing variables 

# Gap decreasing variables

R169a <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","ANXMAT",
                        "ATSCHL","ATTLNACT","MTSUP","STUDREL","TCHQUAL_DIFF"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R169a # Vietnam 67.41

# Gap increasing variables

R169b <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","SCMAT",
                        "BELONG","ATT_CONTROL"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R169b # Vietnam 80.53 

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
#Estimate Std. Error t value
#(Intercept)     367.65      21.25   17.30
#VIETNAM          64.14       7.67    8.36
#PRESCHOOL        24.13       4.32    5.59
#REPEAT          -32.61       2.91  -11.21
#ST08Q01          -5.21       1.66   -3.14
#ST115Q01         -3.17       1.90   -1.67
#BOOK_N            0.07       0.01    5.33
#PARPRESSURE       7.27       4.40    1.65
#PCGIRLS          10.48      14.81    0.71
#FUNDMOM           0.19       0.06    2.88
#COUNCILMOM       -0.15       0.06   -2.50
#PROPCERT         18.38       7.20    2.55
#SMRATIO          -0.02       0.01   -1.79
#TCSHORT           2.06       1.76    1.17
#TCFOCST          -2.50       1.87   -1.34
#TCM_STUASS        2.88       8.89    0.32
#TCM_PEER         -4.58       5.20   -0.88
#TCH_INCENTV      -3.15       2.63   -1.20
#ASS_PROG        -21.79      10.39   -2.10
#ASS_PROM         10.13       6.20    1.64
#ASS_SCH           3.87       7.29    0.53
#STU_FEEDB         3.89       4.79    0.81
#COMP_USE          0.03       5.46    0.01
#TXT_BOOK         -8.69       6.97   -1.25
#TOWN             -8.12       3.93   -2.07
#CLSIZE            0.74       0.25    3.02
#COMPWEB          13.97       6.62    2.11
#SCMATEDU          6.67       3.07    2.17
#SCMATBUI          2.38       2.54    0.94
#EXC2_PLAY         8.42       3.72    2.27
#EXC6_MATHCOMP    -3.27       5.16   -0.63
#EXC10_SPORT      -9.00       9.04   -1.00
#EXC11_UNICORN     6.61       5.98    1.11
#SCL_EXTR_CL       6.05       5.07    1.19
#SCORE_PUBLIC     10.42       4.89    2.13
#QUAL_RECORD       5.20       6.92    0.75
#SCHSEL            1.51       3.43    0.44
#BKGR_FAMPROB      0.28       1.78    0.16
#ANXMAT          -18.78       1.58  -11.89
#ATSCHL            2.42       1.09    2.22
#ATTLNACT          0.00       1.19    0.00
#MTSUP             4.41       1.12    3.93
#STUDREL          -6.75       1.17   -5.75
#TCHQUAL_DIFF    -10.35       2.02   -5.11
#TCHBEHTD          5.48       1.25    4.39
#TCHBEHSO        -13.47       1.43   -9.44
#TCHBEHFA          0.04       1.73    0.02
#DISCLIMA          1.30       1.20    1.08
#R-squared        48.15       2.24   21.52

# Testing all the 5 gap increasing variables

R176 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","SCMAT","BELONG","ATT_CONTROL",
                        "COGACT","CLSMAN"),
                    weight="W_FSTUWT",
                    data=DEVCON8k,export=FALSE)
R176 
#Estimate Std. Error t value
#(Intercept)     336.80      22.28   15.12
#VIETNAM          81.55       7.75   10.52
#PRESCHOOL        24.07       4.50    5.35
#REPEAT          -36.83       3.11  -11.84
#ST08Q01          -6.42       1.80   -3.57
#ST115Q01         -5.31       2.06   -2.58
#BOOK_N            0.06       0.01    4.70
#PARPRESSURE      10.23       4.65    2.20
#PCGIRLS          18.37      15.90    1.15
#FUNDMOM           0.18       0.07    2.70
#COUNCILMOM       -0.16       0.06   -2.62
#PROPCERT         18.44       7.20    2.56
#SMRATIO          -0.03       0.01   -2.07
#TCSHORT           1.98       1.83    1.08
#TCFOCST          -2.65       1.97   -1.34
#TCM_STUASS        2.94       8.54    0.34
#TCM_PEER         -6.74       5.20   -1.30
#TCH_INCENTV      -2.82       2.60   -1.08
#ASS_PROG        -14.56      10.92   -1.33
#ASS_PROM         11.74       6.23    1.89
#ASS_SCH           1.19       7.94    0.15
#STU_FEEDB         2.41       4.92    0.49
#COMP_USE         -2.20       5.60   -0.39
#TXT_BOOK         -8.96       7.27   -1.23
#TOWN             -9.48       4.03   -2.36
#CLSIZE            0.80       0.24    3.26
#COMPWEB          14.16       6.77    2.09
#SCMATEDU          5.88       3.19    1.85
#SCMATBUI          2.96       2.58    1.15
#EXC2_PLAY         9.35       3.97    2.35
#EXC6_MATHCOMP    -1.47       5.52   -0.27
#EXC10_SPORT      -6.96      10.09   -0.69
#EXC11_UNICORN     6.98       6.10    1.14
#SCL_EXTR_CL       9.32       5.15    1.81
#SCORE_PUBLIC     10.06       5.07    1.98
#QUAL_RECORD       3.75       6.91    0.54
#SCHSEL            0.69       3.38    0.20
#SCMAT            12.78       1.47    8.72
#BELONG            4.41       1.13    3.90
#ATT_CONTROL       4.81       2.18    2.21
#COGACT            1.68       1.22    1.38
#CLSMAN           -2.52       1.24   -2.04
#R-squared        44.78       2.47   18.10




