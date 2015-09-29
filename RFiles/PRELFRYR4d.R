# PISA2012_FL_part15
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

load("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/DEVCON8a.rda")

DEVCON8a <- DEVCON8a.rda

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

############################### 6.2.8 Non-rotated & PART 2 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the second (part 2) rotated part

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

# ROTATED PART 2
# 1. Students
#-----Effort: ST55Q03 (Science lessons out of school)
#-----Preparation: none
# 2. Teachers
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes)

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","ST55Q03","LMINS","MMINS","SMINS")]
N1 <- NROW(na.omit(T1b)) 
N1 #12670
N0-N1 #35813 NA's
DEVCON8z <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8z$TCM_STUASS[DEVCON8z$SC30Q01==1] <- 1
DEVCON8z$TCM_STUASS[DEVCON8z$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8z$TCM_PEER[DEVCON8z$SC30Q02==1] <- 1
DEVCON8z$TCM_PEER[DEVCON8z$SC30Q02==2] <- 0

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8z, file = "C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/DEVCON8z.rda") 

# First, remember, we have a smaller data set (12670 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R382 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R382
#Estimate Std. Error t value
#(Intercept)   407.77       2.87  142.04
#VIETNAM       128.70       5.41   23.80
#R-squared      29.76       2.25   13.22

# Let's try our regression with all gap decreasing variables before we add the rotated parts

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

write.csv(R384, "SCIE_rot2.csv")

# Let's move on to the Teacher variables:

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

# All gap increasing variables: 

R388 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R388 # VIETNAM: 89.44

# All gap decreasing variables: R384





















####################################### FOR LATER, initiatl regressions with OUTSCIE_NONE, etc #########################

R384 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R384 # OUTSCIE_NONE decreases
# VIETNAM: 84.85 

R385 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE","OUTSCIE_LESS2"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R385 # OUTSCIE_NONE decreases, OUTSCIE_LESS2 decreases
# VIETNAM: 84.57

R386 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE","OUTSCIE_LESS2","OUTSCIE_2TO4"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R386 # OUTSCIE_NONE decreases, OUTSCIE_LESS2 decreases, OUTSCIE_2TO4 decreases
# VIETNAM: 84.47

R387 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE","OUTSCIE_LESS2","OUTSCIE_2TO4",
                        "OUTSCIE_4TO6"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R387 # OUTSCIE_NONE decreases, OUTSCIE_LESS2 decreases, OUTSCIE_2TO4 decreases, OUTSCIE_4TO6 increases (decreases)
# VIETNAM: 84.58

# This looks a lot like the problem we had with reading, these variables seem to capture the same effect and thus only marginally 
# change when taken cumulatively. They actually go the same direction if taken individually. In the case of reading, although 
# OUTREAD_NONE increased and all other (taken cumulatively) decreased, all of them individually increased. In thh case 
# of science, although OUTSCIE_4TO6 cumulatively (at the end) increases, all of the OUTSCIE variables individually decrease.

# Therefore it makes sense to count them all as decreasing, the same as we counted all reading as increasing.

# Therefore we count all Students (Effort) variables as decreasing

# Let's move on to the Teacher variables:

R388 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE","OUTSCIE_LESS2","OUTSCIE_2TO4",
                        "OUTSCIE_4TO6","LHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R388 # OUTSCIE_NONE decreases, OUTSCIE_LESS2 decreases, OUTSCIE_2TO4 decreases, OUTSCIE_4TO6 decreases, LHRS no change
# VIETNAM: 84.58

R389 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE","OUTSCIE_LESS2","OUTSCIE_2TO4",
                        "OUTSCIE_4TO6","LHRS","MHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R389 # OUTSCIE_NONE decreases, OUTSCIE_LESS2 decreases, OUTSCIE_2TO4 decreases, OUTSCIE_4TO6 decreases, LHRS no change,
# MHRS increase
# VIETNAM: 87.23

R390 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OUTSCIE_NONE","OUTSCIE_LESS2","OUTSCIE_2TO4",
                        "OUTSCIE_4TO6","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R390 # OUTSCIE_NONE decreases, OUTSCIE_LESS2 decreases, OUTSCIE_2TO4 decreases, OUTSCIE_4TO6 decreases, LHRS no change,
# MHRS increase, SHRS increases
# VIETNAM: 89.39

# Now testing to see where LHRS "belongs" to:

R391 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","LHRS","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R391 # VIETNAM: 89.44

R392 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","MHRS","SHRS"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R392 # VIETNAM: 86.82

# Although not clear from our cumulative way, we can clearly see that adding LHRS increases the gap/dummy variable,
# hence we will count LHRS as increasing the gap so that:

# All gap increasing variables: R391

# All gap decreasing variables: R387
