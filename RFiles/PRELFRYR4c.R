# PISA2012_FL_part14
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

############################### 6.2.7 Non-rotated & PART 1 rotated questions #############################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables from the non-rotated parts
# AND deleting missing data from all variables we will use from the first (part 1) rotated part

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

# 1. ROTATED PART
# Students
#-----Effort: none
#-----Attitude: PERSEV, OPENPS 

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM","PERSEV","OPENPS")]
N1 <- NROW(na.omit(T1b)) 
N1 #17441
N0-N1 #31042 NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

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

# Now for the teacher-related variables

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==1] <- 1
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==1] <- 1
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==2] <- 0

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8y, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8y.rda") 

# First, remember, we have a smaller data set (17441 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R377 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8y,export=FALSE)
R377
#Estimate Std. Error t value
#(Intercept)   402.79       2.70  149.34
#VIETNAM       130.49       5.40   24.18
#R-squared      31.66       2.18   14.55

# Let's try our regression with all gap decreasing variables before we add the rotated parts

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

write.csv(R379, "SCIE_rot1.csv")

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

# All gap decreasing is just R379

# All gap increasing (OPENPS without PERSEV):

R381 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM","OPENPS"),
                    weight="W_FSTUWT",
                    data=DEVCON8y,export=FALSE)
R381
# VIETNAM: 88.54






