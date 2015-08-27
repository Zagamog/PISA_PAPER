# PISA2012_FL_part5
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
# Type: SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SC03Q01	
# --------------CLSIZE (Class Size based on SC05, SC), SCHSIZE (based on SC07, SC)	
# Resources: RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),		
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

############################### 4.2.8 Non-rotated & PART 2 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the first (part 1) rotated part

# We will add the rotated variables according to the subsector (students, teachers, etc) they belong to (see our schematic structure)
# and within, as always, in order that they have been asked

# 1. STUDENTS
#-----Effort: ST55Q02 (Math lessons out of school), ST57Q01-Q06
#-----Preparation: EXAPPLM, EXPUREM, FACMCONC

# 2. TEACHERS
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes)

# 4. SCHOOLS
#-----Resources: ST72 (not for math but for 'test language' class) WE ARE NOT TAKING THIS FOR MATH

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

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

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
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8j <- merge(DEVCON8j,SC31OUT.rda,by="NEWID")
DEVCON8j$TCH_INCENTV <- rescale(DEVCON8j$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

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

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8j, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8j.rda") 

R134 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R134
#Estimate Std. Error t value
#(Intercept)   398.53       3.20  124.37
#VIETNAM       119.49       6.92   17.27
#R-squared      24.92       2.56    9.73

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
R136 # OUTMATH_NONE decreases
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
R142 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically (-32%)
# Vietnam 45.38

# Student Effort & Preparation testing all gap decreasing variables

R142a <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                         "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                         "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                         "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                         "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXPUREM","FAMCONC"),
                     weight="W_FSTUWT",
                     data=DEVCON8j,export=FALSE)
R142a # VIETNAM 46.20

# Student Effort & Preparation testing all gap increasing variables (EXAPPLM)

R142b <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                         "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                         "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                         "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                         "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM"),
                     weight="W_FSTUWT",
                     data=DEVCON8j,export=FALSE)
R142b # VIETNAM 74.16

R143 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXAPPLM","EXPUREM","FAMCONC","LHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R143 # OUTMATH decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically, LHRS increases
# Vietnam 45.40

R144 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH",
                        "EXAPPLM","EXPUREM","FAMCONC","LHRS","MHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R144 # OUTMATH decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically, LHRS increases, MHRS increases 
# Vietnam 50.74

R145 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXAPPLM","EXPUREM","FAMCONC","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R145 # OUTMATH_NONE decreases, OUTMATH_LESS2 decreases, OUTMATH_2TO4 decreases, OUTMATH_4TO6 decreases, EXAPPLM increases,
# EXPUREM decreases, FAMCONC decreases drastically, LHRS increases, MHRS increases, SHRS increases
# Vietnam 55.66 

# Now we are testing all the 3 gap decreasing variables

R146 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","OUTMATH","EXPUREM","FAMCONC"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R146 # Vietnam 46.20

write.csv(R146,"MATH_rot2.csv")

# Interestingly, when we did the one-by-one regression and had EXAPPLM (which we identified as gap increasing) included,
# the dummy stood at 45.93 after including FAMCONC. But again we have to pay attention to the direction and not the absolute
# values of the coefficients

# Now we are testing all the 4 gap increasing variables

R147 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","EXAPPLM","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8j,export=FALSE)
R147 # Vietnam 82.17 


