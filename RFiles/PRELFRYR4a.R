# PISA2012_FL_part12
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

############### 6. REGRESSION ANALYSIS FOR SCIENCE OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

################################## 6.1 Introduction & baseline regressions ##################################

# The Main idea is quite simple: We build a regression on PISA test scores of 1) a dummy representing Vietnam 
# (which we alSCIEy created) and 2) a vector of other covariates (which we will create in this section). 
# We start with the dummy variable as the only regressor. Covariates are added to the regression subsequently. 
# In each turn, we analyze how and with which specific addition the (Vietnam) dummy variable decreases/becomes 
# insignificant in association with the test scores. 

# See: Roland Fryer and Steven Levitt 'Understanding the Black-White test score gap in the first two years of school', The Review of Economics and Statistics, 2004, Vol 86, 447-464
# Available to download here: http://www.mitpressjournals.org/doi/pdf/10.1162/003465304323031049 (July 2015)

############# 6.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# Useful tip: Take a moment and think about the specific regressors you want to choose (eg. Teaching variables,
# Student Attitutde, Parent Involvement, etc.) that best fit with your conjecture. Since our target is to 'unravel
# a secret', namely why Vietnam did so well in PISA 2012, it requires quite a rigorous and holistic approach, so
# we analyze many potential sources.

# Please see our conceptual scheme. We have arranged possible explanatory variables into four sets of factors and
# working through the codebooks, questionnaires and Technical Manual, carefully decided which variables (or indices)
# to use to proxy these factors: 

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
#-----Quantity: LMINS (minutes of language classes), MMINS (minutes of math classes), SMINS (minutes of science classes)

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

# The student questionnaires administered to students contains a common part and three rotated sections (two of
# which were given to each student). For example, Student Ann would answer the common part, rotation question set 1
# and rotation question set 2, Student Bert would answer the common part, rotation question set 3 and question set 1
# and so on, so that all rotation parts are answered by 2/3 of the sampled students. 
# For more details please SCIE: PISA 2012 technical report, p. 58-61 and p. 376-386

# The school questionnaire administered to school administration does not contain rotated parts. 

# We follow our conceptual scheme, ie first student variables, then teacher variables, and so on; we will first look at the 
# questions in the non-rotated parts (ie. questions that were administered to all students) and the school questionnaire.
# In a later step we will look at the rotated questions. 

############################### 6.2.2 Non-rotated Questions - Student variables #############################

# As we regress on the independent variables from the questionnaires (eg. hours spent learning outside school, etc.), we 
# need to first make sure that we are not faced with too many missing cases per independent variable; otherwise we cannot
# analyze it. We therefore create intermediary files with the specific variables, see how many cases are missing, drop
# the missing cases and add it back to the main file. In the process we will loose quite a few cases and our sensitivity
# analysis later on will work in a reverse order of dropping missing variables to ensure that our findings are coherent.
# For an overview of how to handle missing data in R we recommend: http://www.statmethods.net/input/missingdata.html

# How big is our initital sample: 
T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 # 48483 data points

# Let's get started with the non-rotated student variables:

# How many non-missing values for all non-rotated student variables (excl SC25)?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","MISCED", "HISEI",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 35345 (same, same)
N0-N1 # 13138 NAs
DEVCON8t <- DEVCON8a[complete.cases(T1b),]

# Preparing the student related variables

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8t$FEMALE[DEVCON8t$ST04Q01==1] <- 1
DEVCON8t$FEMALE[DEVCON8t$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8t$PRESCHOOL[DEVCON8t$ST05Q01==1] <- 0
DEVCON8t$PRESCHOOL[DEVCON8t$ST05Q01==2] <- 1
DEVCON8t$PRESCHOOL[DEVCON8t$ST05Q01==3] <- 1

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
# We want to work with actual range of number of books (please see the student coding file p. 95):
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==1]  <- 5
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==2]  <- 15
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==3]  <- 60
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==4]  <- 150
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==5]  <- 350
DEVCON8t$BOOK_N[DEVCON8t$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8t$PARPRESSURE[DEVCON8t$SC24Q01==1] <- 1
DEVCON8t$PARPRESSURE[DEVCON8t$SC24Q01==2] <- 0
DEVCON8t$PARPRESSURE[DEVCON8t$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
DEVCON8t$SC25Q01[is.na(DEVCON8t$SC25Q01)]  <- 0
DEVCON8t$SC25Q02[is.na(DEVCON8t$SC25Q02)]  <- 0
DEVCON8t$SC25Q03[is.na(DEVCON8t$SC25Q03)]  <- 0
DEVCON8t$SC25Q04[is.na(DEVCON8t$SC25Q04)]  <- 0
DEVCON8t$SC25Q05[is.na(DEVCON8t$SC25Q05)]  <- 0
DEVCON8t$SC25Q06[is.na(DEVCON8t$SC25Q06)]  <- 0
DEVCON8t$SC25Q07[is.na(DEVCON8t$SC25Q07)]  <- 0
DEVCON8t$SC25Q08[is.na(DEVCON8t$SC25Q08)]  <- 0
DEVCON8t$SC25Q09[is.na(DEVCON8t$SC25Q09)]  <- 0
DEVCON8t$SC25Q10[is.na(DEVCON8t$SC25Q10)]  <- 0
DEVCON8t$SC25Q11[is.na(DEVCON8t$SC25Q11)]  <- 0
DEVCON8t$SC25Q12[is.na(DEVCON8t$SC25Q12)]  <- 0

#TIGERMOM
DEVCON8t$TIGERMOM  <- DEVCON8t$SC25Q01+DEVCON8t$SC25Q03
DEVCON8t$TIGERMOM[DEVCON8t$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8t$VOLUMOM <- DEVCON8t$SC25Q05+DEVCON8t$SC25Q06+DEVCON8t$SC25Q07+DEVCON8t$SC25Q09+DEVCON8t$SC25Q12
DEVCON8t$VOLUMOM[DEVCON8t$VOLUMOM>100] <- 100 

#TEACHMOM
DEVCON8t$TEACHMOM <- DEVCON8t$SC25Q08

#FUNDMOM
DEVCON8t$FUNDMOM <-  DEVCON8t$SC25Q11

#COUNCILMOM
DEVCON8t$COUNCILMOM <- DEVCON8t$SC25Q10

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8t, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8t.rda") 

# First, remember, we have a smaller data set (35345 data points) compared to when we first regressed the Vietnam PISA SCIE score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R293 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R293
# Estimate Std. Error t value
# (Intercept)   400.65       2.39  167.49
# VIETNAM       129.63       4.87   26.60
# R-squared      32.03       2.09   15.34

# Just to re-iterate, on the whole data set (48483) we got:

R293a <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8a,export=FALSE)
R293a
# Estimate Std. Error t value
# (Intercept)   393.86       2.25  175.00
# VIETNAM       134.56       4.91   27.41
# R-squared      30.75       1.96   15.66

# Let's start with the regression of the student related non-rotated variables

R294 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R294 # FEMALE decreases
#VIETNAM:  129.60

R295 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R295 # FEMALE decreases, PRESCHOOL decreases
#VIETNAM:  120.24

R296 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R296 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases
#VIETNAM: 115.42

R297 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R297 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases
#VIETNAM: 112.99 

R298 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R298 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 113.06

R299 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R299 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases
#VIETNAM: 112.84

R300 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R300 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases
#VIETNAM: 120.02 

R301 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R301 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases
#VIETNAM: 120.85

# We only take HISEI and MISCED (not FISCED, HISCED) as they will be strongly correlated anyways

R302 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R302 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases
#VIETNAM: 121.36

R303 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R303 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases
#VIETNAM: 121.48 

R304 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R304 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 121.59

R305 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R305 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 121.48

R306 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R306 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 120.82

R307 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R307 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
#VIETNAM: 121.18 

R308 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R308 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases,
# VOLUMOM increases
#VIETNAM: 121.35

R309 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R309 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases
#VIETNAM: 123.64

R310 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R310 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM: 119.15

R311 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R311 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 114.35

R312 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM",
                        "PCGIRLS"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R312 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases, PCGIRLS decreases
#VIETNAM: 113.73

# Now testing all 10 variables that decreased the gap

R313 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "FUNDMOM", "COUNCILMOM","PCGIRLS"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R313
#Estimate Std. Error t value
#(Intercept)   382.77       6.65   57.52
#VIETNAM        99.68       4.42   22.54
#FEMALE         -6.82       1.42   -4.79
#PRESCHOOL      39.54       3.85   10.26
#REPEAT        -51.19       3.03  -16.91
#ST08Q01        -9.19       1.14   -8.07
#ST115Q01       -5.66       1.75   -3.23
#BOOK_N          0.08       0.01    6.34
#PARPRESSURE     5.81       4.16    1.39
#FUNDMOM         0.24       0.05    4.53
#COUNCILMOM     -0.19       0.05   -3.80
#PCGIRLS        37.82       9.56    3.96
#R-squared      44.76       1.71   26.23

# Now testing all 9 variables that increased the gap

R314 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "VOLUMOM", "TIGERMOM","TEACHMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R314
#Estimate Std. Error t value
#(Intercept)   434.68       4.39   99.12
#VIETNAM       137.99       3.98   34.67
#ST09Q01       -21.38       1.86  -11.50
#HISEI           0.41       0.05    8.50
#MISCED          2.18       0.52    4.16
#WEALTH          9.59       1.11    8.63
#CULTPOS        -4.01       0.91   -4.39
#HEDRES         11.66       0.89   13.13
#VOLUMOM         0.10       0.06    1.64
#TIGERMOM       -0.03       0.04   -0.74
#TEACHMOM       -0.13       0.06   -2.05
#R-squared      43.06       1.76   24.49

########################## 6.2.3 Non-rotated Questions - student & teacher variables ##########################

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM     

# Teacher related variables: 
# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT
# Quality: TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08

# How many non-missing values for all non-rotated teacher variables and all gap-decreasing student variables?
T1b <- DEVCON8a[, c("VIETNAM","STRATIO","PROPCERT","PROPQUAL","TCSHORT","TCFOCST", 
                    "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04","SC39Q08","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 #28960
N0-N1 #19523 NA's
DEVCON8u <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8u$FEMALE[DEVCON8u$ST04Q01==1] <- 1
DEVCON8u$FEMALE[DEVCON8u$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8u$PRESCHOOL[DEVCON8u$ST05Q01==1] <- 0
DEVCON8u$PRESCHOOL[DEVCON8u$ST05Q01==2] <- 1
DEVCON8u$PRESCHOOL[DEVCON8u$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==1]  <- 5
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==2]  <- 15
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==3]  <- 60
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==4]  <- 150
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==5]  <- 350
DEVCON8u$BOOK_N[DEVCON8u$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8u$PARPRESSURE[DEVCON8u$SC24Q01==1] <- 1
DEVCON8u$PARPRESSURE[DEVCON8u$SC24Q01==2] <- 0
DEVCON8u$PARPRESSURE[DEVCON8u$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8u$SC25Q10[is.na(DEVCON8u$SC25Q10)]  <- 0
DEVCON8u$SC25Q11[is.na(DEVCON8u$SC25Q11)]  <- 0
DEVCON8u$FUNDMOM <-  DEVCON8u$SC25Q11
DEVCON8u$COUNCILMOM <- DEVCON8u$SC25Q10
 
# And now for the teacher-related variables

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8u$TCM_STUASS[DEVCON8u$SC30Q01==1] <- 1
DEVCON8u$TCM_STUASS[DEVCON8u$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8u$TCM_PEER[DEVCON8u$SC30Q02==1] <- 1
DEVCON8u$TCM_PEER[DEVCON8u$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8u$TCM_OBSER[DEVCON8u$SC30Q03==1] <- 1
DEVCON8u$TCM_OBSER[DEVCON8u$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8u$TCM_INSPE[DEVCON8u$SC30Q04==1] <- 1
DEVCON8u$TCM_INSPE[DEVCON8u$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8u$TCH_MENT[DEVCON8u$SC39Q08==1] <- 1
DEVCON8u$TCH_MENT[DEVCON8u$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
# We will generate an OECD style rasch index that measures incentives - High incentives means high value on this WMLE measure
SC31DAT <- DEVCON8u[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(SC31DAT, "SC31DAT.csv")
# Generated Winsteps output using Winsteps control+data file SC31a.txt
# Person file Output SCIE back into R
SC31OUT.rda <- SCIE.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
# We merge back to the PISA data, except now I have to give it a c suffix.
# Merge school and student datasets 
DEVCON8u <- merge(DEVCON8u,SC31OUT.rda,by="NEWID")
DEVCON8u$TCH_INCENTV <- rescale(DEVCON8u$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Let's support R and create an intermediate file:
save(DEVCON8u, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8u.rda") 

# We start with the regression of all gap decreasing student variables and then add teacher variables one by one:

R315 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R315
#Estimate Std. Error t value
#(Intercept)   373.19       7.90   47.26
#VIETNAM       101.48       4.61   22.02
#FEMALE         -5.80       1.54   -3.77
#PRESCHOOL      38.82       4.22    9.20
#REPEAT        -51.13       3.60  -14.19
#ST08Q01        -8.09       1.32   -6.15
#ST115Q01       -7.51       1.78   -4.21
#BOOK_N          0.06       0.01    5.69
#PARPRESSURE     7.31       3.86    1.89
#PCGIRLS        56.34      12.84    4.39
#FUNDMOM         0.23       0.05    4.34
#COUNCILMOM     -0.23       0.05   -4.61
#R-squared      46.53       1.83   25.41

R316 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R316 # STRATIO increases gap
#VIETNAM: 101.64

R317 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R317 # STRATIO increases, PROPCERT decreases
#VIETNAM: 100.90 

R318 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R318 # STRATIO increases, PROPCERT decreases, PROPQUAL increases
#VIETNAM:  101.10 

R319 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT","PROPQUAL",
                        "TCSHORT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R319 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases
#VIETNAM: 100.96

R320 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPCERT","PROPQUAL",
                        "TCSHORT","TCFOCST"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R320 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases, TCFOCST increases
#VIETNAM: 101.29

R321 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R321 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases
#VIETNAM: 99.69

R322 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R322 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases
#VIETNAM: 99.62

R323 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R323 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases
#VIETNAM: 101.16

R324 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R324 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases
#VIETNAM: 102.07

R325 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                        "TCH_INCENTV"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R325 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV increases
#VIETNAM: 102.16 

R326 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO", "PROPCERT", "PROPQUAL",
                        "TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                        "TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R326 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, TCSHORT decreases,
# TCFOCST increases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV increases, TCH_MENT increases
#VIETNAM: 102.57

# Now testing all 4 variables that decreased the gap

R327 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R327
#Estimate Std. Error t value
#(Intercept)   352.69      12.70   27.77
#VIETNAM        99.36       4.74   20.96
#FEMALE         -5.52       1.49   -3.71
#PRESCHOOL      37.17       3.97    9.35
#REPEAT        -49.30       3.56  -13.86
#ST08Q01        -8.19       1.31   -6.24
#ST115Q01       -8.00       1.78   -4.50
#BOOK_N          0.06       0.01    5.59
#PARPRESSURE     6.32       3.96    1.60
#PCGIRLS        56.34      12.75    4.42
#FUNDMOM         0.21       0.05    4.11
#COUNCILMOM     -0.22       0.05   -4.39
#PROPCERT       11.94       5.09    2.34
#TCSHORT        -0.23       1.89   -0.12
#TCM_STUASS     17.36       8.21    2.11
#TCM_PEER       -0.98       6.07   -0.16
#R-squared      46.99       1.83   25.67

# Now testing all 7 variables that increased the gap

R328 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","STRATIO","PROPQUAL","TCFOCST","TCM_OBSER",
                        "TCM_INSPE","TCH_INCENTV","TCH_MENT"),
                    weight="W_FSTUWT",
                    data=DEVCON8u,export=FALSE)
R328
#Estimate Std. Error t value
#(Intercept)   375.15      17.32   21.66
#VIETNAM       103.99       5.13   20.26
#FEMALE         -5.91       1.53   -3.86
#PRESCHOOL      36.76       4.31    8.52
#REPEAT        -51.52       3.92  -13.15
#ST08Q01        -8.22       1.32   -6.22
#ST115Q01       -7.15       1.80   -3.97
#BOOK_N          0.07       0.01    5.82
#PARPRESSURE     8.00       3.96    2.02
#PCGIRLS        53.36      13.15    4.06
#FUNDMOM         0.22       0.05    4.14
#COUNCILMOM     -0.22       0.05   -4.05
#STRATIO         0.08       0.22    0.36
#PROPQUAL       13.83      12.37    1.12
#TCFOCST         1.10       2.30    0.48
#TCM_OBSER      -7.54       5.27   -1.43
#TCM_INSPE      -2.61       4.19   -0.62
#TCH_INCENTV     0.31       2.44    0.13
#TCH_MENT       -6.09       6.32   -0.96
#R-squared      46.78       1.90   24.60

################ 6.2.4 Non-rotated Questions - student & teacher, pedagogical practices variables ##############

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM       

# 4 gap decreasing teacher-related variables: 
# PROPCERT, TCSHORT, TCM_STUASS, TCM_PEER

# 3. PEDAGOGICAL PRACTICES
# 3.0 General / student-perceived teaching practices: none
# 3.1 Assessment: SC18Q01-Q08
# 3.3 Classroom Management: SC39Q07 (Seeking student feedback, SC)

# How many non-missing values for all non-rotated pedagogical practices variables and all gap-decreasing 
# student and teacher variables?

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q03","SC18Q04","SC18Q05",
                    "SC18Q06","SC18Q07","SC18Q08","SC39Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 #31723
N0-N1 #16760 NA's
DEVCON8v <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8v$FEMALE[DEVCON8v$ST04Q01==1] <- 1
DEVCON8v$FEMALE[DEVCON8v$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8v$PRESCHOOL[DEVCON8v$ST05Q01==1] <- 0
DEVCON8v$PRESCHOOL[DEVCON8v$ST05Q01==2] <- 1
DEVCON8v$PRESCHOOL[DEVCON8v$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==1]  <- 5
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==2]  <- 15
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==3]  <- 60
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==4]  <- 150
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==5]  <- 350
DEVCON8v$BOOK_N[DEVCON8v$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8v$PARPRESSURE[DEVCON8v$SC24Q01==1] <- 1
DEVCON8v$PARPRESSURE[DEVCON8v$SC24Q01==2] <- 0
DEVCON8v$PARPRESSURE[DEVCON8v$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8v$SC25Q10[is.na(DEVCON8v$SC25Q10)]  <- 0
DEVCON8v$SC25Q11[is.na(DEVCON8v$SC25Q11)]  <- 0
DEVCON8v$FUNDMOM <-  DEVCON8v$SC25Q11
DEVCON8v$COUNCILMOM <- DEVCON8v$SC25Q10

# Now for the teacher-related variables

#SC30Q01/SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8v$TCM_STUASS[DEVCON8v$SC30Q01==1] <- 1
DEVCON8v$TCM_STUASS[DEVCON8v$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8v$TCM_PEER[DEVCON8v$SC30Q02==1] <- 1
DEVCON8v$TCM_PEER[DEVCON8v$SC30Q02==2] <- 0

# And now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8v$ASS_PROG[DEVCON8v$SC18Q01==1] <- 1
DEVCON8v$ASS_PROG[DEVCON8v$SC18Q01==2] <- 0

DEVCON8v$ASS_PROM[DEVCON8v$SC18Q02==1] <- 1
DEVCON8v$ASS_PROM[DEVCON8v$SC18Q02==2] <- 0

DEVCON8v$ASS_INSTR[DEVCON8v$SC18Q03==1] <- 1
DEVCON8v$ASS_INSTR[DEVCON8v$SC18Q03==2] <- 0

DEVCON8v$ASS_NAT[DEVCON8v$SC18Q04==1] <- 1
DEVCON8v$ASS_NAT[DEVCON8v$SC18Q04==2] <- 0

DEVCON8v$ASS_SCH[DEVCON8v$SC18Q05==1] <- 1
DEVCON8v$ASS_SCH[DEVCON8v$SC18Q05==2] <- 0

DEVCON8v$ASS_TCH[DEVCON8v$SC18Q06==1] <- 1
DEVCON8v$ASS_TCH[DEVCON8v$SC18Q06==2] <- 0

DEVCON8v$ASS_CUR[DEVCON8v$SC18Q07==1] <- 1
DEVCON8v$ASS_CUR[DEVCON8v$SC18Q07==2] <- 0

DEVCON8v$ASS_OTH[DEVCON8v$SC18Q08==1] <- 1
DEVCON8v$ASS_OTH[DEVCON8v$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8v$STU_FEEDB[DEVCON8v$SC39Q07==1] <- 1
DEVCON8v$STU_FEEDB[DEVCON8v$SC39Q07==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8v, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8v.rda") 

# We start with the regression of all gap decreasing student and teacher-related
# variables and then add pedagogical practices variables one by one:

R329 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R329
#Estimate Std. Error t value
#(Intercept)   362.92      11.03   32.89
#VIETNAM       101.04       4.62   21.89
#FEMALE         -4.91       1.51   -3.24
#PRESCHOOL      36.24       3.61   10.04
#REPEAT        -49.56       3.28  -15.09
#ST08Q01        -9.17       1.34   -6.87
#ST115Q01       -7.16       1.82   -3.93
#BOOK_N          0.06       0.01    6.09
#PARPRESSURE     7.20       3.91    1.84
#PCGIRLS        40.18       9.72    4.13
#FUNDMOM         0.21       0.05    4.13
#COUNCILMOM     -0.22       0.05   -4.50
#PROPCERT       10.26       4.79    2.14
#TCSHORT        -1.06       1.77   -0.60
#TCM_STUASS     17.37       6.97    2.49
#TCM_PEER       -1.85       5.80   -0.32
#R-squared      46.74       1.84   25.45

# So let's start adding the pedagogical practices related variables

R330 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R330 # ASS_PROG decreases (same coefficient, we never had that case before, let's put as decrease then, which is what we had
# for MATH and READ as well)
#VIETNAM: 101.04

R331 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R331 # ASS_PROG decreases, ASS_PROM decreases
# VIETNAM: 100.80

R332 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R332 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases
# VIETNAM:  100.81

R333 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R333 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases
# VIETNAM: 100.21

R334 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R334 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases
# VIETNAM:  100.18

R335 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R335 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases
# VIETNAM: 101.48

R336 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R336 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR decreases
# VIETNAM: 101.25

R337 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R337 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR decreases, ASS_OTH increases
# VIETNAM: 101.41

R338 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_INSTR","ASS_NAT","ASS_SCH" ,"ASS_TCH","ASS_CUR","ASS_OTH",
                        "STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R338 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT decreases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR decreases, ASS_OTH increases, STU_FEEDB decreases
# VIETNAM: 101.09

# Now testing all 6 variables that decreased the gap:

R339 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R339
#Estimate Std. Error t value
#(Intercept)   357.85      16.83   21.27
#VIETNAM        99.58       4.61   21.61
#FEMALE         -5.08       1.53   -3.32
#PRESCHOOL      36.09       3.70    9.75
#REPEAT        -49.48       3.29  -15.03
#ST08Q01        -8.93       1.30   -6.86
#ST115Q01       -7.17       1.80   -3.97
#BOOK_N          0.06       0.01    6.12
#PARPRESSURE     6.32       4.05    1.56
#PCGIRLS        39.73       9.92    4.00
#TCM_PEER       -2.62       5.97   -0.44
#FUNDMOM         0.20       0.05    4.12
#COUNCILMOM     -0.22       0.05   -4.33
#PROPCERT       10.58       4.92    2.15
#TCSHORT        -0.79       1.73   -0.46
#TCM_STUASS     15.90       7.11    2.24
#ASS_PROG       -1.57      17.17   -0.09
#ASS_PROM        4.77       6.13    0.78
#ASS_NAT         5.92       4.68    1.27
#ASS_SCH         2.58       6.24    0.41
#ASS_CUR        -6.65       7.56   -0.88
#STU_FEEDB       5.18       4.32    1.20
#R-squared      46.89       1.85   25.38

# Now testing all 3 variables that increased the gap

R340 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_INSTR","ASS_TCH","ASS_OTH"),
                    weight="W_FSTUWT",
                    data=DEVCON8v,export=FALSE)
R340
#Estimate Std. Error t value
#(Intercept)   368.80      10.62   34.73
#VIETNAM       102.24       4.85   21.06
#FEMALE         -4.94       1.51   -3.27
#PRESCHOOL      35.83       3.49   10.25
#REPEAT        -49.99       3.24  -15.45
#ST08Q01        -9.17       1.35   -6.80
#ST115Q01       -7.07       1.79   -3.94
#BOOK_N          0.06       0.01    6.19
#PARPRESSURE     7.45       3.98    1.87
#PCGIRLS        38.92      10.22    3.81
#TCM_PEER       -0.99       6.06   -0.16
#FUNDMOM         0.20       0.05    3.99
#COUNCILMOM     -0.21       0.05   -4.47
#PROPCERT       10.45       4.89    2.14
#TCSHORT        -1.08       1.76   -0.61
#TCM_STUASS     18.04       6.97    2.59
#ASS_INSTR       1.84       4.37    0.42
#ASS_TCH        -9.35       5.31   -1.76
#ASS_OTH         0.41       4.42    0.09
#R-squared      46.81       1.85   25.34

############## 6.2.5 Non-rotated Questions - student & teacher, pedagogical practices and school variables ############

# 10 gap decreasing student-related variables: 
# FEMALE, PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM       

# 4 gap decreasing teacher-related variables: 
# PROPCERT, TCSHORT, TCM_STUASS, TCM_PEER

# 6 gap decreasing pedagogical practices-related variables:
# ASS_PROG, ASS_PROM, ASS_NAT, ASS_SCH, ASS_CUR, STU_FEEDB

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

# How many non-missing values for all non-rotated school variables and all gap-decreasing student, teacher and 
# pedagogical practices variables?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SC16Q01","SC16Q02",
                    "SC16Q03","SC16Q04","SC16Q05","SC16Q06","SC16Q07","SC16Q08","SC16Q09","SC16Q10","SC16Q11",
                    "SCMATEDU","SCMATBUI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC19Q01",
                    "SC19Q02","SCHAUTON","TCHPARTI","SC39Q03","SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" )]
N1 <- NROW(na.omit(T1b)) 
N1 #22631
N0-N1 #25852 NA's
DEVCON8w <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8w$FEMALE[DEVCON8w$ST04Q01==1] <- 1
DEVCON8w$FEMALE[DEVCON8w$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8w$PRESCHOOL[DEVCON8w$ST05Q01==1] <- 0
DEVCON8w$PRESCHOOL[DEVCON8w$ST05Q01==2] <- 1
DEVCON8w$PRESCHOOL[DEVCON8w$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==1]  <- 5
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==2]  <- 15
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==3]  <- 60
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==4]  <- 150
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==5]  <- 350
DEVCON8w$BOOK_N[DEVCON8w$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8w$PARPRESSURE[DEVCON8w$SC24Q01==1] <- 1
DEVCON8w$PARPRESSURE[DEVCON8w$SC24Q01==2] <- 0
DEVCON8w$PARPRESSURE[DEVCON8w$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8w$SC25Q10[is.na(DEVCON8w$SC25Q10)]  <- 0
DEVCON8w$SC25Q11[is.na(DEVCON8w$SC25Q11)]  <- 0
DEVCON8w$FUNDMOM <-  DEVCON8w$SC25Q11
DEVCON8w$COUNCILMOM <- DEVCON8w$SC25Q10

# Now for the teacher-related variables

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8w$TCM_STUASS[DEVCON8w$SC30Q01==1] <- 1
DEVCON8w$TCM_STUASS[DEVCON8w$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8w$TCM_PEER[DEVCON8w$SC30Q02==1] <- 1
DEVCON8w$TCM_PEER[DEVCON8w$SC30Q02==2] <- 0

# Now for the pedagogical practices-related variables

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8w$ASS_PROG[DEVCON8w$SC18Q01==1] <- 1
DEVCON8w$ASS_PROG[DEVCON8w$SC18Q01==2] <- 0

DEVCON8w$ASS_PROM[DEVCON8w$SC18Q02==1] <- 1
DEVCON8w$ASS_PROM[DEVCON8w$SC18Q02==2] <- 0

DEVCON8w$ASS_NAT[DEVCON8w$SC18Q04==1] <- 1
DEVCON8w$ASS_NAT[DEVCON8w$SC18Q04==2] <- 0

DEVCON8w$ASS_SCH[DEVCON8w$SC18Q05==1] <- 1
DEVCON8w$ASS_SCH[DEVCON8w$SC18Q05==2] <- 0

DEVCON8w$ASS_CUR[DEVCON8w$SC18Q07==1] <- 1
DEVCON8w$ASS_CUR[DEVCON8w$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8w$STU_FEEDB[DEVCON8w$SC39Q07==1] <- 1
DEVCON8w$STU_FEEDB[DEVCON8w$SC39Q07==2] <- 0

# Now for the schools-related variables

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8w$PRIVATESCL[DEVCON8w$SC01Q01==2] <- 1
DEVCON8w$PRIVATESCL[DEVCON8w$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8w$DUM_VILLAGE <- ifelse(DEVCON8w$SC03Q01==1,1,0)
DEVCON8w$DUM_SMLTOWN <- ifelse(DEVCON8w$SC03Q01==2,1,0)
DEVCON8w$DUM_TOWN    <- ifelse(DEVCON8w$SC03Q01==3,1,0)
DEVCON8w$DUM_CITY    <- ifelse(DEVCON8w$SC03Q01==4,1,0)
DEVCON8w$DUM_LRGCITY <- ifelse(DEVCON8w$SC03Q01==5,1,0)

DEVCON8w$TOWN <- DEVCON8w$DUM_SMLTOWN+DEVCON8w$DUM_TOWN
DEVCON8w$TOWN[DEVCON8w$TOWN>1] <- 1
DEVCON8w$CITY <- DEVCON8w$DUM_CITY+DEVCON8w$DUM_LRGCITY
DEVCON8w$CITY[DEVCON8w$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI leave as is

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8w$EXC1_BAND[DEVCON8w$SC16Q01==1] <- 1
DEVCON8w$EXC1_BAND[DEVCON8w$SC16Q01==2] <- 0

DEVCON8w$EXC2_PLAY[DEVCON8w$SC16Q02==1] <- 1
DEVCON8w$EXC2_PLAY[DEVCON8w$SC16Q02==2] <- 0

DEVCON8w$EXC3_NEWS[DEVCON8w$SC16Q03==1] <- 1
DEVCON8w$EXC3_NEWS[DEVCON8w$SC16Q03==2] <- 0

DEVCON8w$EXC4_VOLU[DEVCON8w$SC16Q04==1] <- 1
DEVCON8w$EXC4_VOLU[DEVCON8w$SC16Q04==2] <- 0

DEVCON8w$EXC5_MCLUB[DEVCON8w$SC16Q05==1] <- 1
DEVCON8w$EXC5_MCLUB[DEVCON8w$SC16Q05==2] <- 0

DEVCON8w$EXC6_MATHCOMP[DEVCON8w$SC16Q06==1] <- 1
DEVCON8w$EXC6_MATHCOMP[DEVCON8w$SC16Q06==2] <- 0

DEVCON8w$EXC7_CHESS[DEVCON8w$SC16Q07==1] <- 1
DEVCON8w$EXC7_CHESS[DEVCON8w$SC16Q07==2] <- 0

DEVCON8w$EXC8_ICTCB[DEVCON8w$SC16Q08==1] <- 1
DEVCON8w$EXC8_ICTCB[DEVCON8w$SC16Q08==2] <- 0

DEVCON8w$EXC9_ARTCB[DEVCON8w$SC16Q09==1] <- 1
DEVCON8w$EXC9_ARTCB[DEVCON8w$SC16Q09==2] <- 0

DEVCON8w$EXC10_SPORT[DEVCON8w$SC16Q10==1] <- 1
DEVCON8w$EXC10_SPORT[DEVCON8w$SC16Q10==2] <- 0

DEVCON8w$EXC11_UNICORN[DEVCON8w$SC16Q11==1] <- 1
DEVCON8w$EXC11_UNICORN[DEVCON8w$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8w$SCORE_PUBLIC[DEVCON8w$SC19Q01==1] <- 1
DEVCON8w$SCORE_PUBLIC[DEVCON8w$SC19Q01==2] <- 0

DEVCON8w$SCORE_AUTHRITS[DEVCON8w$SC19Q02==1] <- 1
DEVCON8w$SCORE_AUTHRITS[DEVCON8w$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8w$QUAL_RECORD[DEVCON8w$SC39Q03==1] <- 1
DEVCON8w$QUAL_RECORD[DEVCON8w$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8w, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8w.rda")

# First, remember, we have a smaller data set (22631 data points) compared to when we first regressed the Vietnam PISA READ score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R341 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R341
# Estimate Std. Error t value
# (Intercept)   403.23       2.98  135.24
# VIETNAM       128.54       5.70   22.56
# R-squared      31.50       2.24   14.09

# Regressing on all gap decreasing student, teacher and pedagogical variables

R342 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R342
# VIETNAM: 97.61

# So let's get started on the school-related variables

R343 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R343 # PRIVATESCL decreases the gap (does this make sense?)
#VIETNAM: 97.08 

R344 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R344 # PRIVATESCL decreases, SC02Q02 increases 
#VIETNAM: 98.25

R345 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02","DUM_VILLAGE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R345 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases
#VIETNAM: 101.60

R345a <- pisa.reg.pv(pvlabel="SCIE",  
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                         "PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN"),
                     weight="W_FSTUWT",
                     data=DEVCON8w,export=FALSE)
R345a # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases
#VIETNAM: 100.41

R346 <- pisa.reg.pv(pvlabel="SCIE",  
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R346 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases
#VIETNAM: 95.81

R347 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R347 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, 
# SCHSIZE increases
#VIETNAM: 99.82

R348 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R348 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases
#VIETNAM: 100.91 

R349 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R349 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases
#VIETNAM: 100.00

R350 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R350 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases
#VIETNAM: 98.49 

R351 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R351 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increases
#VIETNAM: 98.67

# For the school activities, we first group them together depending on endowment, so we look at the means
mean1A <- t(sapply(DEVCON8w[c("EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN")], function(x) 
  unlist(t.test(x~DEVCON8w$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#                       estimate.mean in group 0      estimate.mean in group 1      
#EXC1_BAND                    0.5495384                0.1847625  
#EXC2_PLAY                    0.6302543                0.9062307    x
#EXC3_NEWS                    0.5951313                0.5573720 
#EXC4_VOLU                    0.8510496                0.8454658  
#EXC5_MCLUB                   0.4987880                0.2501542 
#EXC6_MATHCOMP                0.6040023                0.8047502    x
#EXC7_CHESS                   0.3669091                0.1964837 
#EXC8_ICTCB                   0.5765640                0.1958667  
#EXC9_ARTCB                   0.7247408                0.4438618 
#EXC10_SPORT                  0.9548713                0.9984577    x
#EXC11_UNICORN                0.7700758                0.9466379    x

# So let's regress with the one where Vietnam does more (ie has a higher mean)

R352 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R352 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease
#VIETNAM: 96.46

# go from here to R353

# just to double check the ones where Vietnam has less of (lower means)
R352a <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("VIETNAM",
                         "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                         "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                         "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                         "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC1_BAND",
                         "EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB"),
                     weight="W_FSTUWT",
                     data=DEVCON8w,export=FALSE)
R352a # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC1_BAND + EXC3_NEWS
# + EXC4_VOLU + EXC5_MCLUB + EXC7_CHESS + EXC8_ICTCB + EXC9_ARTCB) increases
#VIETNAM: 111.12

R353 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R353 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases
#VIETNAM: 94.78 

R354 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC",
                        "SCORE_AUTHRITS"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R354 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases
#VIETNAM: 95.44

R355 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R355 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases
#VIETNAM: 101.52

R356 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON","TCHPARTI"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R356 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases
#VIETNAM: 108.21

R357 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R357 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases
#VIETNAM: 108.89

R358 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R358 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases
#VIETNAM: 108.84

R359 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R359 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases
#VIETNAM: 109.07 

R360 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R360 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases
#VIETNAM: 111.49 

R361 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R361 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases
#VIETNAM: 111.40

R362 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R362 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases
#VIETNAM:  111.08

R363 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R363 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases
#VIETNAM: 112.03

R364 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R364 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease,SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM decreases
#VIETNAM: 111.67

R365 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM",
                        "TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R365 # PRIVATESCL decreases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI increase, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM decreases, TCMORALE increases
#VIETNAM: 112.67

# Now testing all 11 (or more accurately 14 with 4 combined) variables that decrease the gap

# PRIVATESCL, TOWN, CLSIZE, COMPWEB, SCMATEDU, (EXC2_PLAY + EXC6_MATHCOMP + EXC10_SPORT + EXC11_UNICORN),
# SCORE_PUBLIC, LEADINST, QUAL_RECORD, SCHSEL, TEACCLIM

R366 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R366
# VIETNAM: 84.36

# Now testing for all 13 variables that increased the gap

# PRIVATESCL, SC02Q02, DUM_VILLAGE, SCHSIZE, RATCMP15, SCORE_AUTHRITS, SCHAUTON, TCHPARTI, LEADCOM, 
# LEADPD, LEADTCH, STUDCLIM, TCMORALE

R367 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB","SC02Q02",
                        "DUM_VILLAGE","SCHSIZE","RATCMP15","SCMATBUI","SCORE_AUTHRITS","SCHAUTON",
                        "TCHPARTI","LEADCOM","LEADPD","LEADTCH","STUDCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R367
# VIETNAM: 117.84
