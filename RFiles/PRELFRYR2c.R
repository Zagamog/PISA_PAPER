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

############################### 4.2.7 Non-rotated & PART 1 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the first (part 1) rotated part

# We will add the rotated variables according to the subsector (students, teachers, etc) they belong to (see our schematic structure)
# and within, as always, in order that they have been asked

# 1. STUDENTS
#-----Effort: MATWKETH
#-----Attitude: INSTMOT, INTMAT, SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS 

# Here we are just double-checking we did not miss any variables, so we just look at the NA's of the rotated 
# parts and see if they are the same as in 4.2.5 Non-rotated Questions - all gap decreasing/Sensitivity 
# (File: PISA2012_FL_part2)

T1nonrot <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                         "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                         "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                         "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                         "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL")]
Nnonrot <- NROW(na.omit(T1nonrot)) 
Nnonrot #25612
N0-Nnonrot #22871 NA's

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

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

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
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8i, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8i.rda") 

# First, remember, we have a smaller data set (15616 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R103 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8i,export=FALSE)
R103
#Estimate Std. Error t value
#(Intercept)   396.11       2.92  135.69
#VIETNAM       119.92       6.76   17.73
#R-squared      27.26       2.53   10.76

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
#Estimate Std. Error t value
#(Intercept)     354.12      20.95   16.90
#VIETNAM          73.29       7.75    9.45
#PRESCHOOL        27.40       3.73    7.35
#REPEAT          -34.65       3.86   -8.99
#ST08Q01          -7.33       1.57   -4.68
#ST115Q01         -4.88       2.03   -2.40
#BOOK_N            0.07       0.01    5.06
#PARPRESSURE      10.24       4.29    2.39
#PCGIRLS           8.36      14.21    0.59
#FUNDMOM           0.17       0.07    2.58
#COUNCILMOM       -0.14       0.06   -2.31
#PROPCERT         17.05       6.76    2.52
#SMRATIO          -0.03       0.01   -2.23
#TCSHORT           3.56       1.79    1.99
#TCFOCST          -1.46       1.93   -0.75
#TCM_STUASS       -1.35       7.73   -0.17
#TCM_PEER         -5.54       5.64   -0.98
#TCH_INCENTV      -3.11       2.58   -1.21
#ASS_PROG        -24.56       7.71   -3.19
#ASS_PROM         13.83       5.86    2.36
#ASS_SCH          -2.35       7.95   -0.30
#STU_FEEDB         2.29       5.01    0.46
#COMP_USE         -0.39       5.37   -0.07
#TXT_BOOK        -10.47       7.04   -1.49
#TOWN             -8.24       3.42   -2.41
#CLSIZE            0.76       0.23    3.32
#COMPWEB          11.29       6.25    1.81
#SCMATEDU          5.87       3.06    1.92
#SCMATBUI          4.39       2.50    1.76
#EXC2_PLAY         8.23       3.97    2.07
#EXC6_MATHCOMP     0.36       5.32    0.07
#EXC10_SPORT      -4.27       9.87   -0.43
#EXC11_UNICORN     6.90       5.55    1.24
#SCL_EXTR_CL      11.37       5.21    2.18
#SCORE_PUBLIC     10.22       4.86    2.10
#QUAL_RECORD       8.25       6.43    1.28
#SCHSEL            0.66       3.10    0.21
#R-squared        43.02       2.33   18.50

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
#Estimate Std. Error t value
#(Intercept)     360.18      19.37   18.59
#VIETNAM          59.88       6.91    8.66
#PRESCHOOL        22.54       3.48    6.48
#REPEAT          -30.60       3.67   -8.34
#ST08Q01          -5.87       1.59   -3.68
#ST115Q01         -4.26       1.90   -2.24
#BOOK_N            0.05       0.01    4.15
#PARPRESSURE       9.16       3.96    2.31
#PCGIRLS           7.62      13.08    0.58
#FUNDMOM           0.16       0.06    2.76
#COUNCILMOM       -0.14       0.05   -2.65
#PROPCERT         16.28       6.19    2.63
#SMRATIO          -0.02       0.01   -2.22
#TCSHORT           2.12       1.63    1.30
#TCFOCST          -0.35       1.72   -0.20
#TCM_STUASS       -1.26       7.33   -0.17
#TCM_PEER         -4.97       5.26   -0.94
#TCH_INCENTV      -2.95       2.24   -1.32
#ASS_PROG        -16.17       7.75   -2.09
#ASS_PROM         10.42       5.48    1.90
#ASS_SCH           1.35       7.62    0.18
#STU_FEEDB         2.03       4.50    0.45
#COMP_USE         -0.64       5.04   -0.13
#TXT_BOOK         -7.18       6.31   -1.14
#TOWN             -5.97       3.18   -1.88
#CLSIZE            0.69       0.21    3.24
#COMPWEB          12.93       5.71    2.27
#SCMATEDU          4.75       2.72    1.74
#SCMATBUI          3.16       2.26    1.40
#EXC2_PLAY         5.88       3.72    1.58
#EXC6_MATHCOMP    -0.85       4.81   -0.18
#EXC10_SPORT      -4.71       9.12   -0.52
#EXC11_UNICORN     7.04       4.81    1.46
#SCL_EXTR_CL      11.23       4.98    2.26
#SCORE_PUBLIC     10.71       4.37    2.45
#QUAL_RECORD      11.71       6.24    1.88
#SCHSEL            1.67       2.88    0.58
#MATWKETH         -9.87       1.58   -6.26
#INSTMOT           5.76       1.28    4.49
#INTMAT           -3.39       1.88   -1.81
#SUBNORM         -12.14       0.90  -13.56
#MATHEFF          29.90       2.05   14.61
#MATINTFC          8.43       0.85    9.95
#PERSEV            4.37       1.10    3.97
#R-squared        49.60       2.08   23.90

# Let's check all gap increasing variables together (of course with the gap decreasing non-rotated part, as always):

R116 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                        "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                        "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","FAILMAT","MATBEH","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R116
#Estimate Std. Error t value
#(Intercept)     353.51      21.37   16.54
#VIETNAM          80.03       7.70   10.39
#PRESCHOOL        26.53       3.69    7.18
#REPEAT          -33.98       3.93   -8.65
#ST08Q01          -6.96       1.57   -4.43
#ST115Q01         -3.91       2.03   -1.93
#BOOK_N            0.07       0.01    4.81
#PARPRESSURE       9.80       4.29    2.29
#PCGIRLS           7.42      14.55    0.51
#FUNDMOM           0.17       0.07    2.61
#COUNCILMOM       -0.15       0.06   -2.47
#PROPCERT         17.93       6.74    2.66
#SMRATIO          -0.02       0.01   -1.98
#TCSHORT           3.69       1.78    2.07
#TCFOCST          -1.49       1.91   -0.78
#TCM_STUASS       -1.66       7.76   -0.21
#TCM_PEER         -4.32       5.60   -0.77
#TCH_INCENTV      -3.16       2.56   -1.24
#ASS_PROG        -25.21       7.83   -3.22
#ASS_PROM         14.02       5.98    2.35
#ASS_SCH          -2.49       8.20   -0.30
#STU_FEEDB         2.60       4.97    0.52
#COMP_USE         -0.29       5.41   -0.05
#TXT_BOOK         -9.40       7.19   -1.31
#TOWN             -7.79       3.43   -2.27
#CLSIZE            0.75       0.23    3.26
#COMPWEB          11.51       6.20    1.85
#SCMATEDU          6.17       3.06    2.02
#SCMATBUI          4.00       2.53    1.58
#EXC2_PLAY         7.80       3.96    1.97
#EXC6_MATHCOMP    -0.53       5.33   -0.10
#EXC10_SPORT      -3.86       9.95   -0.39
#EXC11_UNICORN     6.60       5.61    1.18
#SCL_EXTR_CL      11.46       5.21    2.20
#SCORE_PUBLIC     10.39       4.90    2.12
#QUAL_RECORD       9.56       6.37    1.50
#SCHSEL            0.95       3.14    0.30
#FAILMAT          -7.12       1.13   -6.30
#MATBEH           -4.21       1.67   -2.53
#OPENPS            9.40       1.25    7.50
#R-squared        44.15       2.30   19.21


