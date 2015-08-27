# PISA2012_FL_part10
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 08/04/2015

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

# How big is our initital sample size? 
T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 

############### 5. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

############# 5.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# NON ROTATED PART

# 1. Students
# Background: FEMALE, ST05Q01, REPEAT (indexed ST07), ST08Q01, ST09Q01, ST115Q01, MISCED, HISEI,
# --------------WEALTH, CULTPOS, HEDRES, ST28Q01
# Home Support: SC25 (Parent Participation, SC), SC24Q01 (Parental Expectations, SC)
# Gender Balance: PCGIRLS (Proportion of girls enrolled at school, SC)

# 2. Teachers
# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT
# Quality: TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08

# 3. Pedagogical practices
# General / student-perceived teaching practices: none
# Assessment: SC18Q01-Q08
# Classroom Management: SC39Q07 (Seeking student feedback, SC)

# 4. Schools
# Type: SC01Q01 (Public or private school, SC), SC02Q02 (Revenues from student fees, SC), SC03Q01, 
#---------------CLSIZE (Class Size based on SC05, SC), SCHSIZE (based on SC07, SC)
# Resources: RATCMP15 (Availabilit of resources, SC), COMPWEB (PC for learning connected to the internet, SC),
# --------------SC16Q01-Q11
# --------------SCMATEDU (Quality of educ. resources, SC), SCMATBUI (Quality of Physical Infrastructure, SC),
#  Leadership: LEADCOM (Framing Schools goal and curriculum, SC), LEADINST (Instructional Leadership, SC), 
# --------------LEADPD (Promoting Development, SC), LEADTCH (Teacher Participation in Leadership, SC),
# --------------SC19Q01 & SC19Q02 (if Student Achievement data is made available, SC), SCHAUTON (School autonomy, SC), 
# --------------TCHPARTI (Teacher participation, SC), SC39Q03 (recording of student/teacher/test data, SC)
# Selectivity: SCHSEL (School Selectivity of students, SC)
# Climate: STUDCLIM (Student aspects of school climate, SC), TEACCLIM (teacher aspects of school climate, SC), 
# --------------TCMORALE (Teacher Morale, SC)

# ROTATED PART 1
# 1. Students
#-----Effort: none
#-----Attitude: PERSEV, OPENPS 

# ROTATED PART 2
# 1. Students
#-----Effort: ST55Q01 (Language lessons out of school), ST57Q01-Q06
#-----Preparation: none
# 2. Teachers
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of READ classes), SMINS (minutes of science classes)

# ROTATED PART 3
# 1. Students
#-----Background: ST91Q03
#-----Attitude: BELONG, ATSCHL, ATTLNACT, ST91Q02
# 2. Teachers
#----Quality: STUDREL, ST91Q04
# 3. Pedagogical Practices
#-----General: none
#-----Assessment: none
#-----Cognitive Activation: none
#-----Classroom Management: none

########################## 5.2.1 Explanatory Variables - rotated & non-rotated questions #######################

############################### 5.2.8 Non-rotated & PART 2 rotated questions #############################

# NON- ROTATED PART:

# 11 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, VOLUMOM, FUNDMOM, COUNCILMOM       

# 3 gap decreasing teacher-related variables: 
# PROPCERT, TCSHORT, TCM_STUASS

# 5 gap decreasing pedagogical practices-related variables:
# ASS_PROG, ASS_PROM, ASS_NAT, ASS_CUR, STU_FEEDB

#  11 (or more accurately 14 with 4 combined) school variables that decrease the gap
# TOWN, CLSIZE, COMPWEB, SCMATEDU, SCMATBUI, (EXC2_PLAY + EXC6_MATHCOMP + EXC10_SPORT + EXC11_UNICORN),
# SCORE_PUBLIC, LEADINST, QUAL_RECORD, SCHSEL, TEACCLIM

# ROTATED PART 2:
# 1. Students	
#-----Effort: ST55Q01 (Language lessons out of school)
#-----Preparation: none	
# 2. Teachers	
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes),
#-----ST72Q01

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04",
                    "SC18Q07","SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM",
                    "ST55Q01","LMINS","MMINS","SMINS","ST72Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #11843
N0-N1 #36640 NA's
DEVCON8r <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8r$TCM_STUASS[DEVCON8r$SC30Q01==1] <- 1
DEVCON8r$TCM_STUASS[DEVCON8r$SC30Q01==2] <- 0

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8r, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8r.rda") 

# First, remember, we have a smaller data set (12778 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R267 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R267
# Estimate Std. Error t value
# (Intercept)   420.48       3.08  136.36
# VIETNAM        95.19       5.20   18.30
# R-squared      17.70       2.05    8.64

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

R270 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R270 # OUTREAD_NONE increases, OUTREAD_LESS2 decreases
# VIETNAM ...

R271 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R271 # OUTREAD_NONE increases, OUTREAD_LESS2 decreases, OUTREAD_2TO4 decreases
# VIETNAM ...

R272 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R272 # OUTREAD_NONE increases, OUTREAD_LESS2 decreases, OUTREAD_2TO4 decreases, OUTREAD_4TO6 decreases
# VIETNAM  ...

# Student Effort (OUTREAD) all gap decreasing:

R273 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R273
# VIETNAM   58.05
# This doesn't really make sense, that the Vietnam coefficent goes up after all. Let's check the OUTREAD variables individually

R273a <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_LESS2"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R273a # VIETNAM 58.72, OUTREAD_LESS2 increases

R273b <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                         "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                         "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                         "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_2TO4"),
                     weight="W_FSTUWT",
                     data=DEVCON8r,export=FALSE)
R273b # VIETNAM 58.33, OUTREAD_2TO4 increases

R273c <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                         "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                         "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                         "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_4TO6"),
                     weight="W_FSTUWT",
                     data=DEVCON8r,export=FALSE)
R273c # VIETNAM 57.95, OUTREAD_4TO6 increases

# So this is a case where OUTREAD_NONE increased the dummy significantly, so that adding the other OUTREAD variables
# decreased it slightly. However, without OUTREAD_NONE, they are also increasing the dummy. We really have to count them
# as increasing the dummy then!

# Student Effort (OUTREAD) all gap increasing:

R274 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R274 # VIETNAM  58.05 (same as R272)

# Now for the teacher relatd variables 

R275 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6","LHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R275 # LHRS decreases
#VIETNAM 61.37 

R276 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6","LHRS","MHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R276 # LHRS decreases, MHRS increases
#VIETNAM 63.66

R277 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6","LHRS","MHRS","SHRS","ST72Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R277 # LHRS decreases, MHRS increases, SHRS increases
#VIETNAM 64.93 

R278 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6","LHRS","MHRS","SHRS","ST72Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R277 # LHRS decreases, MHRS increases, SHRS increases, ST72Q01 decreases
#VIETNAM 60.07 

# Let's try all gap decreasing variables:

R278 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","LHRS","ST72Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R278
# VIETNAM 52.04 

# So let's quickly test the gap increasing variables and then we are done with this set

R279 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTREAD_NONE","OUTREAD_LESS2","OUTREAD_2TO4",
                        "OUTREAD_4TO6","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8r,export=FALSE)
R279
# VIETNAM 59.84 
