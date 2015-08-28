# PISA2012_FL_part16
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 08/06/2015

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

############# 6.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

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
#-----Effort: ST55Q03 (Science lessons out of school)
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

########################## 6.2.1 Explanatory Variables - rotated & non-rotated questions #######################

############################### 6.2.9 Non-rotated & PART 3 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the third (part 3) rotated part

# We will add the rotated variables according to the subsector (students, teachers, etc) they belong to (see our schematic structure)
# and within, as always, in order that they have been asked

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM       

# 4 gap decreasing teacher-related variables: 
# PROPCERT, TCSHORT, TCM_STUASS, TCM_PEER

# 6 gap decreasing pedagogical practices-related variables:
# ASS_PROG, ASS_PROM, ASS_NAT, ASS_SCH, ASS_CUR, STU_FEEDB

# 11 (or more accurately 14 with 4 combined) school variables that decrease the gap
# PRIVATESCL, TOWN, CLSIZE, COMPWEB, SCMATEDU, (EXC2_PLAY + EXC6_MATHCOMP + EXC10_SPORT + EXC11_UNICORN),
# SCORE_PUBLIC, LEADINST, QUAL_RECORD, SCHSEL, TEACCLIM

# ROTATED PART 3
# 1. Students
#-----Background: ST91Q03
#-----Attitude: BELONG, ATSCHL, ATTLNACT, ST91Q02
# 2. Teachers
#----Quality: STUDREL, ST91Q04

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","ST91Q03","BELONG","ATSCHL","ATTLNACT",
                    "ST91Q02","STUDREL","ST91Q04")]
N1 <- NROW(na.omit(T1b)) 
N1 #16764
N0-N1 #31719 NA's
DEVCON8za <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8za$TCM_STUASS[DEVCON8za$SC30Q01==1] <- 1
DEVCON8za$TCM_STUASS[DEVCON8za$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8za$TCM_PEER[DEVCON8za$SC30Q02==1] <- 1
DEVCON8za$TCM_PEER[DEVCON8za$SC30Q02==2] <- 0

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8za, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8za.rda") 

# First, remember, we have a smaller data set (16764 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R393 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R393
# Estimate Std. Error t value
# (Intercept)   402.38       2.67  150.74
# VIETNAM       132.86       5.40   24.62
# R-squared      32.63       2.20   14.84

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

# We have now checked all the student related variables from the rotational part 3. Let's group them into
# decreasing and increasing variables 

# Gap decreasing student variables:

R400 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BKGR_FAMPROB","ATSCHL","ATTLNACT"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R400
# VIETNAM: 84.34

# Gap increasing student variables:

R401 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","BELONG","ATT_CONTROL"),
                    weight="W_FSTUWT",
                    data=DEVCON8za,export=FALSE)
R401
# VIETNAM: 91.86

# Testing the Student & Teacher related variables:

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

# All gap decreasing variables (Students & Teachers):

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

write.csv(R404, "SCIE_rot3.csv")

# All gap increasing variables (Students & Teachers): same as R401





