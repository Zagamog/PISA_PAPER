# PISA2012_FL_part2
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
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH Math (Math: part 2 - part 6)
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

############### 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

################################## 4.1 Introduction & baseline regressions ##################################

# The Main idea is quite simple: We build a regression on PISA test scores of 1) a dummy representing Vietnam 
# (which we already created) and 2) a vector of other covariates (which we will create in this section). 
# We start with the dummy variable as the only regressor. Covariates are added to the regression subsequently. 
# In each turn, we analyze how and with which specific addition the (Vietnam) dummy variable decreases/becomes 
# insignificant in association with the test scores. 

# See: Roland Fryer and Steven Levitt 'Understanding the Black-White test score gap in the first two years of school', The Review of Economics and Statistics, 2004, Vol 86, 447-464
# Available to download here: http://www.mitpressjournals.org/doi/pdf/10.1162/003465304323031049 (July 2015)

############# 4.2 Explanatory variables - Students, teachers, pedagogical practices and schools #############

# Useful tip: Take a moment and think about the specific regressors you want to choose (eg. Teaching variables,
# Student Attitutde, Parent Involvement, etc.) that best fit with your conjecture. Since our target is to 'unravel
# a secret', namely why Vietnam did so well in PISA 2012, it requires quite a rigorous and holistic approach, so
# we analyze many potential sources.

# Please see our conceptual scheme. We have arranged possible explanatory variables into four sets of factors and
# working through the codebooks, questionnaires and Technical Manual, carefully decided which variables (or indices)
# to use to proxy these factors: 

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

# The student questionnaires administered to students contains a common part and three rotated sections (two of
# which were given to each student). For example, Student Ann would answer the common part, rotation question set 1
# and rotation question set 2, Student Bert would answer the common part, rotation question set 3 and question set 1
# and so on, so that all rotation parts are answered by 2/3 of the sampled students. 
# For more details please read: PISA 2012 technical report, p. 58-61 and p. 376-386

# The school questionnaire administered to school administration does not contain rotated parts. 

# We follow our conceptual scheme, ie first student variables, then teacher variables, and so on; we will first look at the 
# questions in the non-rotated parts (ie. questions that were administered to all students) and the school questionnaire.
# In a later step we will look at the rotated questions. 

############################### 4.2.2 Non-rotated Questions - Student variables #############################

# As we regress on the independent variables from the questionnaires (eg. hours spent learning outside school, etc.), we 
# need to first make sure that we are not faced with too many missing cases per independent variable; otherwise we cannot
# analyze it. We therefore create intermediary files with the specific variables, see how many cases are missing, drop
# the missing cases and add it back to the main file. In the process we will loose quite a few cases and our sensitivity
# analysis later on will work in a reverse order of dropping missing variables to ensure that our findings are coherent.
# For an overview of how to handle missing data in R we recommend: http://www.statmethods.net/input/missingdata.html

# How big is our initital sample size? Two ways to check: 
T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0 # display, we find that we have 48483 data points, which is a good sample size to start with

# OR (if we know there are no missing variables, you can simply look at the length of the vector: 
length(DEVCON8a$VIETNAM) # 48483 data points. Carefull, though, this is the length of the vector! so...

length(DEVCON8a$SC40Q03) # will give you 48483 "data points", but ...
TSC40 <- DEVCON8a[, c("SC40Q03")]
NSC40 <- NROW(na.omit(TSC40))
NSC40 # deleting all NA's will give you "only" 47504 data points

# Let's get started with the non-rotated student variables:

# How many non-missing values for all non-rotated student variables (excl SC25)?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "HISCED", "MISCED", "FISCED",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 34405
N0-N1 # 14078 NAs

# And now we are completing the missing cases, to work on a dataframe of non-missing variabels for these regressors
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# As you can and will see, we are working mostly with these incdices as regressors; however we still need to prepare some
# variables (eg. ST05Q01, ST28Q01, SC25) to be meaningfully used as regressors. Let's quickly do that:

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8b$FEMALE[DEVCON8b$ST04Q01==1] <- 1
DEVCON8b$FEMALE[DEVCON8b$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==1] <- 0
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==2] <- 1
DEVCON8b$PRESCHOOL[DEVCON8b$ST05Q01==3] <- 1

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
# We want do not want to work with categories (1-6) but with actual range of number of books, hence we assign 
# each category its average number of books (please see the student coding file p. 95) 
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==1]  <- 5
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==2]  <- 15
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==3]  <- 60
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==4]  <- 150
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==5]  <- 350
DEVCON8b$BOOK_N[DEVCON8b$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# This question asked principal to tick if there is constant, little or largely absent pressure from parents on them to 
# set high academic standards AND the expectation of parents to achieve them. We think this gives a good proxy to measure
# how much pressure 'from home' also rests on the students and hence included it in the student variables.
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8b$PARPRESSURE[DEVCON8b$SC24Q01==1] <- 1
DEVCON8b$PARPRESSURE[DEVCON8b$SC24Q01==2] <- 0
DEVCON8b$PARPRESSURE[DEVCON8b$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We intentionally did not delete missing variables from the 'SC25' (Parent Participation) variable. In this specific case,
# we replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
# This may generate some spurious data, but we still find it safe to replace N/A with 0's, indicating that no parents 
# fall in this category. 
DEVCON8b$SC25Q01[is.na(DEVCON8b$SC25Q01)]  <- 0
DEVCON8b$SC25Q02[is.na(DEVCON8b$SC25Q02)]  <- 0
DEVCON8b$SC25Q03[is.na(DEVCON8b$SC25Q03)]  <- 0
DEVCON8b$SC25Q04[is.na(DEVCON8b$SC25Q04)]  <- 0
DEVCON8b$SC25Q05[is.na(DEVCON8b$SC25Q05)]  <- 0
DEVCON8b$SC25Q06[is.na(DEVCON8b$SC25Q06)]  <- 0
DEVCON8b$SC25Q07[is.na(DEVCON8b$SC25Q07)]  <- 0
DEVCON8b$SC25Q08[is.na(DEVCON8b$SC25Q08)]  <- 0
DEVCON8b$SC25Q09[is.na(DEVCON8b$SC25Q09)]  <- 0
DEVCON8b$SC25Q10[is.na(DEVCON8b$SC25Q10)]  <- 0
DEVCON8b$SC25Q11[is.na(DEVCON8b$SC25Q11)]  <- 0
DEVCON8b$SC25Q12[is.na(DEVCON8b$SC25Q12)]  <- 0

# SC25Q01 is quite rich in information, so we create sub-variables
#TIGERMOM
DEVCON8b$TIGERMOM  <- DEVCON8b$SC25Q01+DEVCON8b$SC25Q03
DEVCON8b$TIGERMOM[DEVCON8b$TIGERMOM>100] <- 100 

# Since asking for students behaviour and students progress can be viewed complementary, we add them up (and do not average them).
# We then account for the fact that not more than 100% of parents can logically be a 'TIGERMOM'. You can do that differently, 
# as long as you are consistent with the other created out of the 'SC25' questions.

#VOLUMOM
DEVCON8b$VOLUMOM <- DEVCON8b$SC25Q05+DEVCON8b$SC25Q06+DEVCON8b$SC25Q07+DEVCON8b$SC25Q09+DEVCON8b$SC25Q12
DEVCON8b$VOLUMOM[DEVCON8b$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8b$TEACHMOM <- DEVCON8b$SC25Q08

#FUNDMOM
DEVCON8b$FUNDMOM <-  DEVCON8b$SC25Q11

#COUNCILMOM
DEVCON8b$COUNCILMOM <- DEVCON8b$SC25Q10

# As you might have noticed, we did not include SCQ02 and SCQ04, which resembles Q01 and Q03 but from the teachers inititative,
# not parents initiative. Theoretically, these could be seen as complementary; ie. progress/behaviour is either discussed on
# the parents initiative or not at all/on the teachers initiative (if deemed necessary), given that there is no formal process
# in place of discussing students behaviour/progress. In fact, if you add these two together, the mean is 80%, which seems
# satisfactory to only focus on parent initiated discussions.

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8b, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8b.rda") 

# Now that we have all independent variables ready, let's check the means for each variable in the group of 7 and for Vietnam

mean1A <- t(sapply(DEVCON8b[c("FEMALE", "PRESCHOOL","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "MISCED",
                              "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS", "TIGERMOM", "VOLUMOM",
                              "TEACHMOM", "FUNDMOM", "COUNCILMOM")], function(x) 
                                unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# You can see that, on average, students in Vietnam have more pre-schooling (at least one year, PRESCHOOL), 
# higher parental pressure (on schools and student achievement, PARPRESSURE), higher ratio of girls in school (PCGIRLS),
# much higher percentage of parents initiative to discuss students behaviour and/or progress (TIGERMOM), higher parent
# participation in volunterring activities (VOLUMOM), much higher parent participation in assisting in teaching (TEACHMOM),
# much higher assistance of parents in fundraising (FUNDMOM).

# Please refer to the file of "descriptive statistics" to see means and NA's for each variable individually

# Regardless, these are very interesting findings, so let's see how well they explain Vietnam's high performance (and decrease the gap, ie the
# Vietnam dummy variable) along the way:

# First, remember, we have a smaller data set (34405 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# That gives us ...
#            Estimate Std. Error t value
#(Intercept)   389.97       2.78  140.44
#VIETNAM       124.14       5.69   21.80
#R-squared      28.19       2.42   11.62

# So let's start with the regression of the student related non-rotated variables

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R1 # FEMALE increases the gap
#VIETNAM: 124.22

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R2 # FEMALE increases, PRESCHOOL decreases
#VIETNAM: 114.16

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R3 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases
#VIETNAM: 109.49

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R4 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases
#VIETNAM: 107.08

R5 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R5 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 107.10

R6 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R6 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases
#VIETNAM: 106.83

R7 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R7 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases
#VIETNAM: 114.88

R8 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                      "MISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R8 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases
#VIETNAM: 115.83

# We only take HISEI and MISCED (not FISCED, HISCED) as they will be strongly correlated anyways

R9 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                      "MISCED", "WEALTH"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R9 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases
#VIETNAM: 116.24

R10 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                      "MISCED", "WEALTH", "CULTPOS"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R10 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases
#VIETNAM: 116.34

R11 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R11 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 116.50

R12 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R12 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 116.37

R13 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R13 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 115.21

R14 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R14 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases
#VIETNAM: 114.19

R15 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R15 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
#VIETNAM: 114.45

R16 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R16 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases
#VIETNAM: 114.66 

R17 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R17 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases
#VIETNAM: 115.83

R18 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R18 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM: 110.92

R19 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R19 # FEMALE increases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 107.22

# Now testing all 9 variables that decreased the gap

R20 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R20
#Estimate Std. Error t value
#(Intercept)   366.69       7.16   51.25
#VIETNAM        94.52       5.41   17.49
#PRESCHOOL      42.56       4.18   10.17
#REPEAT        -48.57       2.87  -16.89
#ST08Q01        -8.61       1.23   -6.98
#ST115Q01       -4.60       1.73   -2.65
#BOOK_N          0.09       0.01    6.95
#PARPRESSURE    10.92       5.27    2.07
#PCGIRLS        22.79      10.98    2.08
#FUNDMOM         0.26       0.06    4.26
#COUNCILMOM     -0.16       0.06   -2.82
#R-squared      40.97       1.90   21.56

# Now testing all 10 variables that increased the gap

R21 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                       "TIGERMOM","VOLUMOM","TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R21
#Estimate Std. Error t value
#(Intercept)   424.48       5.44   78.09
#VIETNAM       131.25       4.54   28.91
#FEMALE         -8.32       1.97   -4.23
#ST09Q01       -19.93       2.12   -9.39
#HISEI           0.46       0.05    8.78
#MISCED          2.53       0.64    3.96
#WEALTH          9.89       1.26    7.86
#CULTPOS        -3.89       0.84   -4.64
#HEDRES         12.89       0.97   13.34
#TIGERMOM       -0.02       0.06   -0.32
#VOLUMOM         0.10       0.08    1.18
#TEACHMOM       -0.06       0.07   -0.89
#R-squared      40.64       1.95   20.86

########################## 4.2.2 Non-rotated Questions - student & teacher variables ##########################

# 9 gap decreasing student-related variables: 
# PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM     

# Teacher related variables: 
# Quantity: STRATIO, PROPCERT, PROPQUAL, TCSHORT, SMRATIO
# Quality: TCFOCST, SC30Q01, SC30Q02, SC30Q03, SC30Q04, SC31Q01-Q07 (TCH incentive), SC39Q08, SC35Q02

# How many non-missing values for all non-rotated teacher variables and all gap-decreasing student variables?
T1b <- DEVCON8a[, c("VIETNAM","STRATIO","PROPCERT","PROPQUAL","TCSHORT","SMRATIO","TCFOCST", 
                    "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04", "SC35Q02", "SC39Q08","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 #27447
N0-N1 #21036 NA's
DEVCON8d <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8d$PRESCHOOL[DEVCON8d$ST05Q01==1] <- 0
DEVCON8d$PRESCHOOL[DEVCON8d$ST05Q01==2] <- 1
DEVCON8d$PRESCHOOL[DEVCON8d$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==1]  <- 5
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==2]  <- 15
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==3]  <- 60
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==4]  <- 150
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==5]  <- 350
DEVCON8d$BOOK_N[DEVCON8d$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8d$PARPRESSURE[DEVCON8d$SC24Q01==1] <- 1
DEVCON8d$PARPRESSURE[DEVCON8d$SC24Q01==2] <- 0
DEVCON8d$PARPRESSURE[DEVCON8d$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8d$SC25Q10[is.na(DEVCON8d$SC25Q10)]  <- 0
DEVCON8d$SC25Q11[is.na(DEVCON8d$SC25Q11)]  <- 0
DEVCON8d$FUNDMOM <-  DEVCON8d$SC25Q11
DEVCON8d$COUNCILMOM <- DEVCON8d$SC25Q10

# And now for the teacher-related variables

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8d$TCM_STUASS[DEVCON8d$SC30Q01==1] <- 1
DEVCON8d$TCM_STUASS[DEVCON8d$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8d$TCM_PEER[DEVCON8d$SC30Q02==1] <- 1
DEVCON8d$TCM_PEER[DEVCON8d$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8d$TCM_OBSER[DEVCON8d$SC30Q03==1] <- 1
DEVCON8d$TCM_OBSER[DEVCON8d$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8d$TCM_INSPE[DEVCON8d$SC30Q04==1] <- 1
DEVCON8d$TCM_INSPE[DEVCON8d$SC30Q04==2] <- 0
                    
#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8d$TCH_MENT[DEVCON8d$SC39Q08==1] <- 1
DEVCON8d$TCH_MENT[DEVCON8d$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
# We will generate an OECD style rasch index that measures incentives - High incentives means high value on this WMLE measure
SC31DAT <- DEVCON8d[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(SC31DAT, "SC31DAT.csv")
# Generated Winsteps output using Winsteps control+data file SC31a.txt
# Person file Output read back into R
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
# We merge back to the PISA data, except now I have to give it a c suffix.
# Merge school and student datasets 
DEVCON8d <- merge(DEVCON8d,SC31OUT.rda,by="NEWID")
DEVCON8d$TCH_INCENTV <- rescale(DEVCON8d$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8d, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8d.rda") 

# First, remember, we have a smaller data set (27477 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R22 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8d,export=FALSE)
R22
# Estimate Std. Error t value
# (Intercept)   386.85       2.83  136.88
# VIETNAM       126.59       6.20   20.41
# R-squared      30.79       2.34   13.13

# We start with the regression of all gap decreasing student variables and then add teacher variables one by one:

R23 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R23
# Estimate Std. Error t value
# (Intercept)   356.78       9.36   38.14
# VIETNAM        96.50       5.31   18.17
# PRESCHOOL      40.57       4.15    9.77
# REPEAT        -45.89       3.46  -13.28
# ST08Q01        -7.14       1.32   -5.41
# ST115Q01       -5.65       1.76   -3.20
# BOOK_N          0.08       0.01    6.54
# PARPRESSURE    13.76       4.36    3.16
# PCGIRLS        40.15      16.04    2.50
# FUNDMOM         0.26       0.06    4.54
# COUNCILMOM     -0.22       0.06   -3.86
# R-squared      43.50       2.10   20.72

R24 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R24 # STRATIO increases gap
#VIETNAM: 96.53

R25 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R25 # STRATIO increases, PROPCERT decreases
#VIETNAM: 95.17

R26 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R26 # STRATIO increases, PROPCERT decreases, PROPQUAL increases
#VIETNAM:  95.18

R27 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R27 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases
#VIETNAM: 93.15

R28 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R28 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases
#VIETNAM: 92.86

R29 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R29 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases
#VIETNAM: 92.74

R30 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R30 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases
#VIETNAM: 91.41

R31 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R31 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases, TCM_PEER decreases
#VIETNAM: 91.29

R32 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R32 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases
#VIETNAM: 92.86

R33 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R33 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases
#VIETNAM: 93.73

R34 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                       "TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R34 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV decreases
#VIETNAM: 93.67

R35 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                       "TCH_INCENTV","SC35Q02"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R35 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV decreases, SC35Q02 increases
#VIETNAM: 93.85

R36 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO", "PROPCERT", "PROPQUAL",
                       "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                       "TCH_INCENTV","SC35Q02","TCH_MENT"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R36 # STRATIO increases, PROPCERT decreases, PROPQUAL increases, SMRATIO decreases, TCSHORT decreases,
# TCFOCST decreases, TCM_STUASS decreases, TCM_PEER decreases, TCM_OBSER increases, TCM_INSPE increases,
# TCH_INCENTV decreases, SC35Q02 increases, TCH_MENT increases
#VIETNAM: 94.11 

# Now testing all 7 variables that decreased the gap

R37 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R37
# Estimate Std. Error t value
# (Intercept)   342.18      13.23   25.87
# VIETNAM        91.84       5.73   16.04
# PRESCHOOL      39.10       4.03    9.69
# REPEAT        -44.12       3.51  -12.58
# ST08Q01        -7.03       1.30   -5.40
# ST115Q01       -6.05       1.76   -3.43
# BOOK_N          0.07       0.01    6.34
# PARPRESSURE    12.48       4.41    2.83
# PCGIRLS        37.76      15.31    2.47
# FUNDMOM         0.25       0.06    4.19
# COUNCILMOM     -0.21       0.06   -3.78
# PROPCERT       17.85       5.67    3.15
# SMRATIO        -0.02       0.01   -1.88
# TCSHORT        -1.19       1.74   -0.68
# TCFOCST        -0.81       2.18   -0.37
# TCM_STUASS     13.03       8.90    1.46
# TCM_PEER       -1.75       6.46   -0.27
# TCH_INCENTV     0.23       2.16    0.11
# R-squared      44.19       2.05   21.53

# Now testing all 6 variables that increased the gap

R38 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM", "STRATIO","PROPQUAL","TCM_OBSER",
                       "TCM_INSPE","SC35Q02","TCH_MENT"),
                   weight="W_FSTUWT",
                   data=DEVCON8d,export=FALSE)
R38
# Estimate Std. Error t value
# (Intercept)   365.80      21.03   17.40
# VIETNAM        98.10       5.87   16.72
# PRESCHOOL      38.76       4.45    8.71
# REPEAT        -46.73       3.95  -11.84
# ST08Q01        -7.44       1.31   -5.69
# ST115Q01       -5.44       1.76   -3.10
# BOOK_N          0.08       0.01    6.57
# PARPRESSURE    13.97       4.52    3.09
# PCGIRLS        37.16      15.22    2.44
# FUNDMOM         0.25       0.06    4.15
# COUNCILMOM     -0.21       0.05   -3.86
# STRATIO        -0.04       0.26   -0.17
# PROPQUAL        2.87      15.57    0.18
# TCM_OBSER      -8.88       5.22   -1.70
# TCM_INSPE      -2.33       4.74   -0.49
# SC35Q02         0.07       0.05    1.41
# TCH_MENT       -1.79       7.98   -0.22
# R-squared      43.67       2.16   20.25

######### 4.2.3 Non-rotated Questions - student & teacher, pedagogical practices variables #########

# 9 gap decreasing student-related variables: 
# PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM     

# 7 gap decreasing teacher-related variables: 
# PROPCERT, SMRATIO, TCSHORT, TCFOCST, TCM_STUASS, TCM_PEER, TCH_INCENTV

# 3. PEDAGOGICAL PRACTICES
# 3.0 General / student-perceived teaching practices: SC40Q01-SC40Q03 (Practices in Maths, SC)
# 3.1 Assessment: ASSESS (SC, see p. 309) or better SC18Q01-Q08
# 3.3 Classroom Management: SC39Q07 (Seeking student feedback, SC)

# How many non-missing values for all non-rotated pedagogical practices variables and all gap-decreasing 
# student and teacher variables?
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01", "SC30Q02","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01",
                    "REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01",
                    "SC18Q02","SC18Q03","SC18Q04","SC18Q05","SC18Q06","SC18Q07","SC18Q08",
                    "SC39Q07","SC40Q01","SC40Q02","SC40Q03")]
N1 <- NROW(na.omit(T1b)) 
N1 #29428
N0-N1 #19055 NA's
DEVCON8e <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8e$PRESCHOOL[DEVCON8e$ST05Q01==1] <- 0
DEVCON8e$PRESCHOOL[DEVCON8e$ST05Q01==2] <- 1
DEVCON8e$PRESCHOOL[DEVCON8e$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8e$BOOK_N[DEVCON8e$ST28Q01==1]  <- 5
DEVCON8e$BOOK_N[DEVCON8e$ST28Q01==2]  <- 15
DEVCON8e$BOOK_N[DEVCON8e$ST28Q01==3]  <- 60
DEVCON8e$BOOK_N[DEVCON8e$ST28Q01==4]  <- 150
DEVCON8e$BOOK_N[DEVCON8e$ST28Q01==5]  <- 350
DEVCON8e$BOOK_N[DEVCON8e$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8e$PARPRESSURE[DEVCON8e$SC24Q01==1] <- 1
DEVCON8e$PARPRESSURE[DEVCON8e$SC24Q01==2] <- 0
DEVCON8e$PARPRESSURE[DEVCON8e$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8e$SC25Q10[is.na(DEVCON8e$SC25Q10)]  <- 0
DEVCON8e$SC25Q11[is.na(DEVCON8e$SC25Q11)]  <- 0
DEVCON8e$FUNDMOM <- DEVCON8e$SC25Q11
DEVCON8e$COUNCILMOM <- DEVCON8e$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8e$TCM_STUASS[DEVCON8e$SC30Q01==1] <- 1
DEVCON8e$TCM_STUASS[DEVCON8e$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8e$TCM_PEER[DEVCON8e$SC30Q02==1] <- 1
DEVCON8e$TCM_PEER[DEVCON8e$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8e <- merge(DEVCON8e,SC31OUT.rda,by="NEWID")
DEVCON8e$TCH_INCENTV <- rescale(DEVCON8e$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# And now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8e$ASS_PROG[DEVCON8e$SC18Q01==1] <- 1
DEVCON8e$ASS_PROG[DEVCON8e$SC18Q01==2] <- 0

DEVCON8e$ASS_PROM[DEVCON8e$SC18Q02==1] <- 1
DEVCON8e$ASS_PROM[DEVCON8e$SC18Q02==2] <- 0

DEVCON8e$ASS_INSTR[DEVCON8e$SC18Q03==1] <- 1
DEVCON8e$ASS_INSTR[DEVCON8e$SC18Q03==2] <- 0

DEVCON8e$ASS_NAT[DEVCON8e$SC18Q04==1] <- 1
DEVCON8e$ASS_NAT[DEVCON8e$SC18Q04==2] <- 0

DEVCON8e$ASS_SCH[DEVCON8e$SC18Q05==1] <- 1
DEVCON8e$ASS_SCH[DEVCON8e$SC18Q05==2] <- 0

DEVCON8e$ASS_TCH[DEVCON8e$SC18Q06==1] <- 1
DEVCON8e$ASS_TCH[DEVCON8e$SC18Q06==2] <- 0

DEVCON8e$ASS_CUR[DEVCON8e$SC18Q07==1] <- 1
DEVCON8e$ASS_CUR[DEVCON8e$SC18Q07==2] <- 0

DEVCON8e$ASS_OTH[DEVCON8e$SC18Q08==1] <- 1
DEVCON8e$ASS_OTH[DEVCON8e$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8e$STU_FEEDB[DEVCON8e$SC39Q07==1] <- 1
DEVCON8e$STU_FEEDB[DEVCON8e$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8e$COMP_USE[DEVCON8e$SC40Q01==1] <- 1
DEVCON8e$COMP_USE[DEVCON8e$SC40Q01==2] <- 0

DEVCON8e$TXT_BOOK[DEVCON8e$SC40Q02==1] <- 1
DEVCON8e$TXT_BOOK[DEVCON8e$SC40Q02==2] <- 0

DEVCON8e$STD_CUR[DEVCON8e$SC40Q03==1] <- 1
DEVCON8e$STD_CUR[DEVCON8e$SC40Q03==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8e, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8e.rda") 

# First, remember, we have a smaller data set (29428 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R39 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R39
# Estimate Std. Error t value
# (Intercept)   386.35       2.86  135.11
# VIETNAM       127.87       6.08   21.05
# R-squared      30.32       2.41   12.56

# We start with the regression of all gap decreasing student and teacher-related
# variables and then add pedagogical practices variables one by one:

R40 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R40
# Estimate Std. Error t value
# (Intercept)   346.51      12.65   27.39
# VIETNAM        94.47       5.70   16.59
# PRESCHOOL      39.38       4.16    9.47
# REPEAT        -44.91       3.34  -13.45
# ST08Q01        -8.53       1.45   -5.89
# ST115Q01       -6.06       1.82   -3.33
# BOOK_N          0.08       0.01    6.53
# PARPRESSURE    13.21       4.84    2.73
# PCGIRLS        32.76      14.65    2.24
# FUNDMOM         0.22       0.06    3.70
# COUNCILMOM     -0.21       0.06   -3.64
# PROPCERT       15.79       5.49    2.88
# SMRATIO        -0.01       0.01   -1.43
# TCSHORT        -2.22       1.93   -1.15
# TCFOCST        -0.47       2.16   -0.22
# TCM_STUASS     12.53       7.72    1.62
# TCM_PEER        0.23       6.29    0.04
# TCH_INCENTV    -0.91       2.62   -0.35
# R-squared      43.29       2.14   20.26

# The Vietnam dummy went up a bit. It makes sense that it is not the same as before, since we are 
# technically working on a different data set. Remember, before we worked on a dataset of a) only gap
# decreasing student variabes and b) all teacher variables. Now, we are working on a data set of a)
# only gap decreasing student variables b) only gap decreasing teacher variables and c) all pedagogical
# practice variables. 

# So let's start adding the pedagogical practices related variables

R41 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R41 # ASS_PROG decreases gap
#VIETNAM: 94.37

R42 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R42 # ASS_PROG decreases, ASS_PROM decreases
# VIETNAM: 94.13 

R43 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R43 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases
# VIETNAM: 94.21

R44 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R44 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases
# VIETNAM: 94.25

R45 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH" ),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R45 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases
# VIETNAM: 94.09

R46 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R46 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases
# VIETNAM: 95.09

R47 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R47 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR increases
# VIETNAM: 95.13

R48 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R48 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR increases, ASS_OTH increases
# VIETNAM: 95.29  

R49 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH","STU_FEEDB"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R49 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR increases, ASS_OTH increases, STU_FEEDB decreases
# VIETNAM: 95.01

R50 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH","STU_FEEDB",
                       "COMP_USE"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R50 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR increases, ASS_OTH increases, STU_FEEDB decreases, COMP_USE decreases
# VIETNAM: 94.72

R51 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH","STU_FEEDB",
                       "COMP_USE","TXT_BOOK"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R51 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR increases, ASS_OTH increases, STU_FEEDB decreases, COMP_USE decreases,
# TXT_BOOK decreases
# VIETNAM: 94.31 

R52 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH","STU_FEEDB",
                       "COMP_USE","TXT_BOOK","STD_CUR"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R52 # ASS_PROG decreases, ASS_PROM decreases, ASS_INSTR increases, ASS_NAT increases, ASS_SCH decreases,
# ASS_TCH increases, ASS_CUR increases, ASS_OTH increases, STU_FEEDB decreases, COMP_USE decreases,
# TXT_BOOK decreases, STD_CUR increases
# VIETNAM: 95.47

# Now testing all 6 variables that decreased the gap

R53 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R53
#Estimate Std. Error t value
#(Intercept)   343.40      18.02   19.06
#VIETNAM        93.06       5.74   16.22
#PRESCHOOL      38.08       4.18    9.10
#REPEAT        -44.79       3.32  -13.48
#ST08Q01        -8.48       1.40   -6.04
#ST115Q01       -5.74       1.85   -3.10
#BOOK_N          0.08       0.01    6.51
#PARPRESSURE    11.92       4.92    2.42
#PCGIRLS        31.98      14.56    2.20
#FUNDMOM         0.22       0.06    3.77
#COUNCILMOM     -0.21       0.06   -3.68
#PROPCERT       17.28       5.98    2.89
#SMRATIO        -0.01       0.01   -1.30
#TCSHORT        -1.86       1.92   -0.97
#TCFOCST        -0.58       2.15   -0.27
#TCM_STUASS     13.81       8.01    1.72
#TCM_PEER        0.87       6.28    0.14
#TCH_INCENTV    -1.05       2.84   -0.37
#ASS_PROG       -1.19      14.61   -0.08
#ASS_PROM        5.58       6.27    0.89
#ASS_SCH         4.38       7.49    0.59
#STU_FEEDB       2.03       5.11    0.40
#COMP_USE        2.68       4.58    0.58
#TXT_BOOK      -11.44       6.17   -1.85
#R-squared      43.60       2.16   20.19

# Arguably, this is not the nicest output for the Vietnam dummy, since we already had it down to 91.84 
# when we only had the gap decreasing student and gap decreasing teacher variables. But again be aware,
# that we are regressing on different data sets; that is why we always do the initial regression again on 
# then new data set and see where we are starting from. For consistency reasons we have to cumulatively 
# keep on adding variables. Let's see how far we can get the Vietnam dummy down after we have added 
# the last section, school-related variables. 

# Now testing all 6 variables that increased the gap

R54 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV","ASS_INSTR","ASS_NAT",
                       "ASS_TCH","ASS_CUR","ASS_OTH","STD_CUR"),
                   weight="W_FSTUWT",
                   data=DEVCON8e,export=FALSE)
R54
#Estimate Std. Error t value
#(Intercept)   358.32      14.32   25.03
#VIETNAM        96.80       5.87   16.50
#PRESCHOOL      38.48       3.93    9.79
#REPEAT        -44.78       3.20  -14.01
#ST08Q01        -8.53       1.48   -5.75
#ST115Q01       -6.05       1.79   -3.38
#BOOK_N          0.08       0.01    6.68
#PARPRESSURE    13.19       4.79    2.75
#PCGIRLS        29.45      15.51    1.90
#FUNDMOM         0.21       0.06    3.48
#COUNCILMOM     -0.19       0.06   -3.35
#PROPCERT       14.80       5.83    2.54
#SMRATIO        -0.01       0.01   -1.43
#TCSHORT        -2.31       1.90   -1.21
#TCFOCST        -0.68       2.29   -0.30
#TCM_STUASS     12.74       7.88    1.62
#TCM_PEER        1.96       6.47    0.30
#TCH_INCENTV    -0.87       2.24   -0.39
#ASS_INSTR       3.69       5.16    0.71
#ASS_NAT         0.69       5.36    0.13
#ASS_TCH        -4.64       6.51   -0.71
#ASS_CUR         5.05       7.94    0.64
#ASS_OTH        -2.78       5.74   -0.48
#STD_CUR       -13.76       8.57   -1.61
#R-squared      43.53       2.16   20.11

# At least it is higher than the initial regression. Let's move on to our last part.

###### 4.2.4 Non-rotated Questions - student & teacher, pedagogical practices and school variables ######

# 9 gap decreasing student-related variables: 
# PRESCHOOL, REPEAT, ST08Q01, ST115Q01, BOOK_N, PARPRESSURE, PCGIRLS, FUNDMOM, COUNCILMOM     

# 7 gap decreasing teacher-related variables: 
# PROPCERT, SMRATIO, TCSHORT, TCFOCST, TCM_STUASS, TCM_PEER, TCH_INCENTV

# 6 gap decreasing pedagogical practices-related variables:
# ASS_PROG, ASS_PROM, ASS_SCH, STU_FEEDB, COMP_USE, TXT_BOOK

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

# How many non-missing values for all non-rotated school variables and all gap-decreasing student, teacher and 
# pedagogical practices variables?
T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01",
                    "REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02",
                    "SC18Q05","SC39Q07","SC40Q01","SC40Q02","SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE",
                    "RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","SC16Q01","SC16Q02","SC16Q03","SC16Q04",
                    "SC16Q05","SC16Q06","SC16Q07","SC16Q08","SC16Q09","SC16Q10","SC16Q11","SC20Q01","SC19Q01",
                    "SC19Q02","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC39Q03",
                    "SCHSEL","STUDCLIM","TEACCLIM","TCMORALE")]
N1 <- NROW(na.omit(T1b)) 
N1 #21443
N0-N1 #27040 NA's
DEVCON8f <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8f$PRESCHOOL[DEVCON8f$ST05Q01==1] <- 0
DEVCON8f$PRESCHOOL[DEVCON8f$ST05Q01==2] <- 1
DEVCON8f$PRESCHOOL[DEVCON8f$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8f$BOOK_N[DEVCON8f$ST28Q01==1]  <- 5
DEVCON8f$BOOK_N[DEVCON8f$ST28Q01==2]  <- 15
DEVCON8f$BOOK_N[DEVCON8f$ST28Q01==3]  <- 60
DEVCON8f$BOOK_N[DEVCON8f$ST28Q01==4]  <- 150
DEVCON8f$BOOK_N[DEVCON8f$ST28Q01==5]  <- 350
DEVCON8f$BOOK_N[DEVCON8f$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8f$PARPRESSURE[DEVCON8f$SC24Q01==1] <- 1
DEVCON8f$PARPRESSURE[DEVCON8f$SC24Q01==2] <- 0
DEVCON8f$PARPRESSURE[DEVCON8f$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8f$SC25Q10[is.na(DEVCON8f$SC25Q10)]  <- 0
DEVCON8f$SC25Q11[is.na(DEVCON8f$SC25Q11)]  <- 0
DEVCON8f$FUNDMOM <- DEVCON8f$SC25Q11
DEVCON8f$COUNCILMOM <- DEVCON8f$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8f$TCM_STUASS[DEVCON8f$SC30Q01==1] <- 1
DEVCON8f$TCM_STUASS[DEVCON8f$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8f$TCM_PEER[DEVCON8f$SC30Q02==1] <- 1
DEVCON8f$TCM_PEER[DEVCON8f$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8f <- merge(DEVCON8f,SC31OUT.rda,by="NEWID")
DEVCON8f$TCH_INCENTV <- rescale(DEVCON8f$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8f$ASS_PROG[DEVCON8f$SC18Q01==1] <- 1
DEVCON8f$ASS_PROG[DEVCON8f$SC18Q01==2] <- 0

DEVCON8f$ASS_PROM[DEVCON8f$SC18Q02==1] <- 1
DEVCON8f$ASS_PROM[DEVCON8f$SC18Q02==2] <- 0

DEVCON8f$ASS_SCH[DEVCON8f$SC18Q05==1] <- 1
DEVCON8f$ASS_SCH[DEVCON8f$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8f$STU_FEEDB[DEVCON8f$SC39Q07==1] <- 1
DEVCON8f$STU_FEEDB[DEVCON8f$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8f$COMP_USE[DEVCON8f$SC40Q01==1] <- 1
DEVCON8f$COMP_USE[DEVCON8f$SC40Q01==2] <- 0

DEVCON8f$TXT_BOOK[DEVCON8f$SC40Q02==1] <- 1
DEVCON8f$TXT_BOOK[DEVCON8f$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8f$PRIVATESCL[DEVCON8f$SC01Q01==2] <- 1
DEVCON8f$PRIVATESCL[DEVCON8f$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8f$DUM_VILLAGE <- ifelse(DEVCON8f$SC03Q01==1,1,0)
DEVCON8f$DUM_SMLTOWN <- ifelse(DEVCON8f$SC03Q01==2,1,0)
DEVCON8f$DUM_TOWN    <- ifelse(DEVCON8f$SC03Q01==3,1,0)
DEVCON8f$DUM_CITY    <- ifelse(DEVCON8f$SC03Q01==4,1,0)
DEVCON8f$DUM_LRGCITY <- ifelse(DEVCON8f$SC03Q01==5,1,0)

DEVCON8f$TOWN <- DEVCON8f$DUM_SMLTOWN+DEVCON8f$DUM_TOWN
DEVCON8f$TOWN[DEVCON8f$TOWN>1] <- 1
DEVCON8f$CITY <- DEVCON8f$DUM_CITY+DEVCON8f$DUM_LRGCITY
DEVCON8f$CITY[DEVCON8f$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8f$EXC1_BAND[DEVCON8f$SC16Q01==1] <- 1
DEVCON8f$EXC1_BAND[DEVCON8f$SC16Q01==2] <- 0

DEVCON8f$EXC2_PLAY[DEVCON8f$SC16Q02==1] <- 1
DEVCON8f$EXC2_PLAY[DEVCON8f$SC16Q02==2] <- 0

DEVCON8f$EXC3_NEWS[DEVCON8f$SC16Q03==1] <- 1
DEVCON8f$EXC3_NEWS[DEVCON8f$SC16Q03==2] <- 0

DEVCON8f$EXC4_VOLU[DEVCON8f$SC16Q04==1] <- 1
DEVCON8f$EXC4_VOLU[DEVCON8f$SC16Q04==2] <- 0

DEVCON8f$EXC5_MCLUB[DEVCON8f$SC16Q05==1] <- 1
DEVCON8f$EXC5_MCLUB[DEVCON8f$SC16Q05==2] <- 0

DEVCON8f$EXC6_MATHCOMP[DEVCON8f$SC16Q06==1] <- 1
DEVCON8f$EXC6_MATHCOMP[DEVCON8f$SC16Q06==2] <- 0

DEVCON8f$EXC7_CHESS[DEVCON8f$SC16Q07==1] <- 1
DEVCON8f$EXC7_CHESS[DEVCON8f$SC16Q07==2] <- 0

DEVCON8f$EXC8_ICTCB[DEVCON8f$SC16Q08==1] <- 1
DEVCON8f$EXC8_ICTCB[DEVCON8f$SC16Q08==2] <- 0

DEVCON8f$EXC9_ARTCB[DEVCON8f$SC16Q09==1] <- 1
DEVCON8f$EXC9_ARTCB[DEVCON8f$SC16Q09==2] <- 0

DEVCON8f$EXC10_SPORT[DEVCON8f$SC16Q10==1] <- 1
DEVCON8f$EXC10_SPORT[DEVCON8f$SC16Q10==2] <- 0

DEVCON8f$EXC11_UNICORN[DEVCON8f$SC16Q11==1] <- 1
DEVCON8f$EXC11_UNICORN[DEVCON8f$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8f$SCL_EXTR_CL[DEVCON8f$SC20Q01==1] <- 1
DEVCON8f$SCL_EXTR_CL[DEVCON8f$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8f$SCORE_PUBLIC[DEVCON8f$SC19Q01==1] <- 1
DEVCON8f$SCORE_PUBLIC[DEVCON8f$SC19Q01==2] <- 0

DEVCON8f$SCORE_AUTHRITS[DEVCON8f$SC19Q02==1] <- 1
DEVCON8f$SCORE_AUTHRITS[DEVCON8f$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8f$QUAL_RECORD[DEVCON8f$SC39Q03==1] <- 1
DEVCON8f$QUAL_RECORD[DEVCON8f$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8f, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8f.rda") 

# First, remember, we have a smaller data set (21443 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R55 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R55
# Estimate Std. Error t value
# (Intercept)   393.12       3.35  117.29
# VIETNAM       120.68       6.94   17.38
# R-squared      27.25       2.59   10.54

R56 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R56
#Estimate Std. Error t value
#(Intercept)   374.27      18.19   20.58
#VIETNAM        87.52       7.50   11.67
#PRESCHOOL      34.91       4.92    7.10
#REPEAT        -46.72       4.35  -10.74
#ST08Q01        -9.33       1.72   -5.41
#ST115Q01       -4.66       2.05   -2.28
#BOOK_N          0.08       0.01    5.95
#PARPRESSURE    13.27       5.09    2.61
#PCGIRLS        21.50      18.41    1.17
#FUNDMOM         0.20       0.06    3.19
#COUNCILMOM     -0.26       0.07   -3.86
#PROPCERT       15.56       7.71    2.02
#SMRATIO        -0.02       0.01   -1.49
#TCSHORT        -0.61       2.15   -0.29
#TCFOCST        -1.25       2.38   -0.53
#TCM_STUASS      1.65       6.69    0.25
#TCM_PEER        1.89       6.61    0.29
#TCH_INCENTV    -2.47       3.38   -0.73
#ASS_PROG      -16.90       9.10   -1.86
#ASS_PROM       12.98       6.03    2.15
#ASS_SCH         6.44      10.00    0.64
#STU_FEEDB       2.52       5.83    0.43
#COMP_USE        5.85       5.76    1.02
#TXT_BOOK      -13.20       8.06   -1.64
#R-squared      40.80       2.33   17.50

# It is quite interesting that the Vietnam dummy went down from 93.06 to 87.52 by just regressing on a different
# data set. This data set is much smaller than the previous one (21443 vis-a-vis 29428), since we deleted all missing
# cells for the school related variables. Although the previous data set had deleted cells for the gap-increasing
# pedagogical practices, we have much more schools variables; hence the much smaller data set. We will get back to
# this when we finally have all the gap decreasing variables and regress on a data set where we only delet cells
# with missing data for gap reducing variables.

# So let's get started on the school-related variables

R57 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R57 # PRIVATESCL increases the gap
#VIETNAM: 88.46

R58 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R58 # PRIVATESCL increases, SC02Q02 increases 
#VIETNAM: 90.80

R59 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R59 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases
#VIETNAM: 95.23

R59a <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R59a # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases
#VIETNAM: 93.46

# go from here directly to R60

R59b <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","CITY"),
                    weight="W_FSTUWT",
                    data=DEVCON8f,export=FALSE)
R59b # PRIVATESCL increases, SC02Q02 increases, CITY increases slightly
#VIETNAM: 90.84

R59c <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02",
                        "CITY","TOWN"),
                    weight="W_FSTUWT",
                    data=DEVCON8f,export=FALSE)
R59c # PRIVATESCL increases, SC02Q02 increases, (TOWN + CITY) increases
#VIETNAM:  93.46

R60 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02",
                        "DUM_VILLAGE","TOWN","CLSIZE"),
                    weight="W_FSTUWT",
                    data=DEVCON8f,export=FALSE)
R60 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases
#VIETNAM: 89.20

R61 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R61 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, 
# SCHSIZE increases
#VIETNAM: 91.10

R62 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R62 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases
#VIETNAM: 91.86

R63 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R63 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases
#VIETNAM: 90.99

R64 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R64 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases
#VIETNAM: 88.69

R65 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU", "SCMATBUI"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R65 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases
#VIETNAM: 88.42

# For the school activities, we first group them together depending on endowment, so we look at the means
mean1A <- t(sapply(DEVCON8f[c("EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN")], function(x) 
  unlist(t.test(x~DEVCON8f$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

#                     estimate.mean in group 0      estimate.mean in group 1      
#EXC1_BAND                    0.5566146                0.1836470  
#EXC2_PLAY                    0.6386362                0.9208306  x
#EXC3_NEWS                    0.6104243                0.5434783  
#EXC4_VOLU                    0.8536027                0.8465282  
#EXC5_MCLUB                   0.5120091                0.2426995 
#EXC6_MATHCOMP                0.6038342                0.8049968  x
#EXC7_CHESS                   0.3832035                0.1862427 
#EXC8_ICTCB                   0.5855890                0.1855938  
#EXC9_ARTCB                   0.7308970                0.4555483 
#EXC10_SPORT                  0.9565383                0.9983777  x
#EXC11_UNICORN                0.7789881                0.9438676  x

# So let's regress with the one where Vietnam does more (ie has a higher mean)

R66a <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R66a # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease
#VIETNAM: 87.52

# move from here directly to R67

# And now the ones where Vietnam has less of (lower means)

R66b <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","TOWN",
                        "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC1_BAND",
                        "EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB"),
                    weight="W_FSTUWT",
                    data=DEVCON8f,export=FALSE)
R66b # PRIVATESCL increases, SC02Q02 increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC1_BAND + EXC3_NEWS
# + EXC4_VOLU + EXC5_MCLUB + EXC7_CHESS + EXC8_ICTCB + EXC9_ARTCB) increases
#VIETNAM: 104.23

# Just to double check I try Mathcompetition

R66c <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","TOWN",
                        "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC6_MATHCOMP"),
                    weight="W_FSTUWT",
                    data=DEVCON8f,export=FALSE)
R66c # PRIVATESCL increases, SC02Q02 increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, EXC6_MATHCOMP increase
#VIETNAM: 85.85

R70 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                        "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                        "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                        "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL"),
                    weight="W_FSTUWT",
                    data=DEVCON8f,export=FALSE)
R70 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases
#VIETNAM: 86.78

R71 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R71 # PRIVATESCL increases, SC02Q02 increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases
#VIETNAM: 84.91

R72 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R72 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases
#VIETNAM: 85.46

R73 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R73 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases
#VIETNAM: 94.27

R74 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R74 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases
#VIETNAM: 104.17

R75 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R75 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases
#VIETNAM: 105.69 

R76 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R76 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases
#VIETNAM: 105.81

R77 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R77 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases, LEADPD increases
#VIETNAM: 105.92

R78 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R78 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases, LEADPD increases, LEADTCH increases
#VIETNAM: 106.75

R79 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH",
                       "QUAL_RECORD"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R79 # PRIVATESCL increases, SC02Q02 increases, DUm_VILLAGE increases,TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases
#VIETNAM: 106.24

R80 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH",
                       "QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R80 # PRIVATESCL increases, SC02Q02 increases, DUM_VILALGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases
#VIETNAM: 106.02

R81 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH",
                       "QUAL_RECORD","SCHSEL","STUDCLIM"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R81 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases
#VIETNAM: 107.29

R82 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH",
                       "QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R82 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST increases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM increases
#VIETNAM: 107.32

R83 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN",
                       "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC",
                       "SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH",
                       "QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM","TCMORALE"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R83 # PRIVATESCL increases, SC02Q02 increases, DUM_VILLAGE increases, TOWN decreases, CLSIZE decreases, SCHSIZE increases,
#RATCMP15 increases, COMPWEB decreases, SCMATEDU decreases, SCMATBUI decreases, (EXC2_PLAY + EXC6_MATHCOMP
# + EXC10_SPORT + EXC11_UNICORN) decrease, SCL_EXTR_CL decreases, SCORE_PUBLIC decreases, SCORE_AUTHRITS increases,
#SCHAUTON increases, TCHPARTI increases, LEADCOM increases, LEADINST decreases, LEADPD increases, LEADTCH increases,
#QUAL_RECORD decreases, SCHSEL decreases, STUDCLIM increases, TEACCLIM increases, TCMORALE increases
#VIETNAM: 108.41 

# Now testing all 14 (or more accurately 11 with 4 combined) variables that decreased the gap

# TOWN, CLSIZE, COMPWEB, SCMATEDU, SCMATBUI, (EXC2_PLAY + EXC6_MATHCOMP + EXC10_SPORT + EXC11_UNICORN),
# SCL_EXTR_CL, SCORE_PUBLIC, QUAL_RECORD, SCHSEL

R84 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R84
#Estimate Std. Error t value
#(Intercept)     369.21      21.57   17.12
#VIETNAM          72.86       8.07    9.02
#PRESCHOOL        26.42       4.23    6.25
#REPEAT          -38.66       3.53  -10.94
#ST08Q01          -8.76       1.53   -5.73
#ST115Q01         -5.23       1.98   -2.64
#BOOK_N            0.07       0.01    5.04
#PARPRESSURE      12.39       4.88    2.54
#PCGIRLS           9.20      17.66    0.52
#FUNDMOM           0.15       0.07    2.14
#COUNCILMOM       -0.17       0.06   -2.69
#PROPCERT         15.19       7.53    2.02
#SMRATIO          -0.02       0.01   -1.84
#TCSHORT           5.39       1.95    2.77
#TCFOCST          -2.92       1.99   -1.47
#TCM_STUASS       -8.50       6.28   -1.35
#TCM_PEER         -4.37       5.56   -0.78
#TCH_INCENTV      -3.42       2.86   -1.19
#ASS_PROG        -27.08       8.04   -3.37
#ASS_PROM         12.02       6.04    1.99
#ASS_SCH          -9.01       9.77   -0.92
#STU_FEEDB         2.61       5.58    0.47
#COMP_USE          1.00       5.93    0.17
#TXT_BOOK        -12.46       8.23   -1.51
#TOWN             -7.87       4.06   -1.94
#CLSIZE            0.72       0.24    2.98
#COMPWEB          12.04       6.96    1.73
#SCMATEDU          8.33       3.48    2.39
#SCMATBUI          2.88       2.70    1.07
#EXC2_PLAY         8.44       4.64    1.82
#EXC6_MATHCOMP     0.52       5.56    0.09
#EXC10_SPORT      -0.66       9.63   -0.07
#EXC11_UNICORN     4.64       6.51    0.71
#SCL_EXTR_CL      13.50       5.32    2.54
#SCORE_PUBLIC      9.30       5.35    1.74
#QUAL_RECORD      10.47       6.47    1.62
#SCHSEL            1.10       3.33    0.33
#R-squared        44.14       2.35   18.77

# Remember that with this data set and no other regressors the Vietnam dummy was at 120.68,
# regressing all gap decreasing student, teacher, pedagogical practices and school variables, we could bring
# it down to 72.86, which means explaining about 40% of variation. We will create a data set, where we just
# account for the missing data of the gap decreasing variables and then see how in each step, the Vietnam
# dummy goes down, so that we have a cumulative step by step regression on one single data set. Remember,
# in our last data set (DEVCON8f) we also have deleted missing data for the gap increasing school-related
# variables.

# Before we do aforementioned, lets quickly compare, testing for all 13 variables that increased the gap

# PRIVATESCL, SC02Q02, DUM_VILLAGE, SCHSIZE, RATCMP15, SCORE_AUTHRITS, SCHAUTON, TCHPARTI, LEADCOM, LEADINST
# LEADPD, LEADTCH, STUDCLIM, TEACCLIM, TCMORALE

R85 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","PRIVATESCL","SC02Q02","DUM_VILLAGE",
                       "SCHSIZE","RATCMP15","SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST",
                       "LEADPD","LEADTCH","STUDCLIM","TEACCLIM","TCMORALE"),
                   weight="W_FSTUWT",
                   data=DEVCON8f,export=FALSE)
R85
#Estimate Std. Error t value
#(Intercept)      387.17      18.32   21.14
#VIETNAM          116.07       8.98   12.93
#PRESCHOOL         23.25       4.02    5.78
#REPEAT           -39.33       3.35  -11.73
#ST08Q01           -7.52       1.24   -6.05
#ST115Q01          -3.88       1.82   -2.13
#BOOK_N             0.06       0.01    4.87
#PARPRESSURE       11.60       4.46    2.60
#PCGIRLS           17.91      15.79    1.13
#FUNDMOM            0.15       0.06    2.41
#COUNCILMOM        -0.15       0.06   -2.59
#PROPCERT           5.13       6.37    0.81
#SMRATIO           -0.05       0.01   -3.31
#TCSHORT            2.97       1.91    1.55
#TCFOCST           -7.42       2.55   -2.91
#TCM_STUASS       -13.37       6.70   -2.00
#TCM_PEER           1.25       5.37    0.23
#TCH_INCENTV       -4.74       2.96   -1.60
#ASS_PROG         -14.68       8.04   -1.83
#ASS_PROM           0.13       6.19    0.02
#ASS_SCH           -5.59      10.84   -0.52
#STU_FEEDB          1.19       5.08    0.24
#COMP_USE          -0.66       4.69   -0.14
#TXT_BOOK         -12.52       6.71   -1.86
#PRIVATESCL       -15.34       6.94   -2.21
#SC02Q02            0.34       0.08    4.41
#DUM_VILLAGE      -14.19       4.99   -2.84
#SCHSIZE            0.02       0.00    7.46
#RATCMP15           7.85       7.16    1.10
#SCORE_AUTHRITS    15.54       5.49    2.83
#SCHAUTON           5.51       2.76    2.00
#TCHPARTI           3.71       1.68    2.20
#LEADCOM            3.38       2.95    1.14
#LEADINST          -7.43       3.76   -1.97
#LEADPD             7.83       3.36    2.33
#LEADTCH            1.89       3.29    0.57
#STUDCLIM           6.40       3.08    2.08
#TEACCLIM           3.62       3.30    1.10
#TCMORALE           2.82       2.79    1.01
#R-squared         47.06       2.32   20.32


# That is quite an indicative output. Let's move on to just regressing on gap decreasing variables and
# one data set (where missing data is deleted just for the gap decreasing variables)

###################### 4.2.5 Non-rotated Questions - all gap decreasing/Sensitivity #########################

# Let's prepare our data set by deleting the missing data for all gap decreasing variables

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03",
                    "SCHSEL")]
N1 <- NROW(na.omit(T1b)) 
N1 #25612
N0-N1 #22871 NA's
DEVCON8g <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8g$PRESCHOOL[DEVCON8g$ST05Q01==1] <- 0
DEVCON8g$PRESCHOOL[DEVCON8g$ST05Q01==2] <- 1
DEVCON8g$PRESCHOOL[DEVCON8g$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8g$BOOK_N[DEVCON8g$ST28Q01==1]  <- 5
DEVCON8g$BOOK_N[DEVCON8g$ST28Q01==2]  <- 15
DEVCON8g$BOOK_N[DEVCON8g$ST28Q01==3]  <- 60
DEVCON8g$BOOK_N[DEVCON8g$ST28Q01==4]  <- 150
DEVCON8g$BOOK_N[DEVCON8g$ST28Q01==5]  <- 350
DEVCON8g$BOOK_N[DEVCON8g$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8g$PARPRESSURE[DEVCON8g$SC24Q01==1] <- 1
DEVCON8g$PARPRESSURE[DEVCON8g$SC24Q01==2] <- 0
DEVCON8g$PARPRESSURE[DEVCON8g$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8g$SC25Q10[is.na(DEVCON8g$SC25Q10)]  <- 0
DEVCON8g$SC25Q11[is.na(DEVCON8g$SC25Q11)]  <- 0
DEVCON8g$FUNDMOM <- DEVCON8g$SC25Q11
DEVCON8g$COUNCILMOM <- DEVCON8g$SC25Q10

# Now for the teacher-related variables

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8g$TCM_STUASS[DEVCON8g$SC30Q01==1] <- 1
DEVCON8g$TCM_STUASS[DEVCON8g$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8g$TCM_PEER[DEVCON8g$SC30Q02==1] <- 1
DEVCON8g$TCM_PEER[DEVCON8g$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8g <- merge(DEVCON8g,SC31OUT.rda,by="NEWID")
DEVCON8g$TCH_INCENTV <- rescale(DEVCON8g$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Now for the pedagogical practices-related variables

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8g$ASS_PROG[DEVCON8g$SC18Q01==1] <- 1
DEVCON8g$ASS_PROG[DEVCON8g$SC18Q01==2] <- 0

DEVCON8g$ASS_PROM[DEVCON8g$SC18Q02==1] <- 1
DEVCON8g$ASS_PROM[DEVCON8g$SC18Q02==2] <- 0

DEVCON8g$ASS_SCH[DEVCON8g$SC18Q05==1] <- 1
DEVCON8g$ASS_SCH[DEVCON8g$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8g$STU_FEEDB[DEVCON8g$SC39Q07==1] <- 1
DEVCON8g$STU_FEEDB[DEVCON8g$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8g$COMP_USE[DEVCON8g$SC40Q01==1] <- 1
DEVCON8g$COMP_USE[DEVCON8g$SC40Q01==2] <- 0

DEVCON8g$TXT_BOOK[DEVCON8g$SC40Q02==1] <- 1
DEVCON8g$TXT_BOOK[DEVCON8g$SC40Q02==2] <- 0

# Now for the schools-related variables

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8g$DUM_SMLTOWN <- ifelse(DEVCON8g$SC03Q01==2,1,0)
DEVCON8g$DUM_TOWN    <- ifelse(DEVCON8g$SC03Q01==3,1,0)

DEVCON8g$TOWN <- DEVCON8g$DUM_SMLTOWN+DEVCON8g$DUM_TOWN
DEVCON8g$TOWN[DEVCON8g$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8g$EXC2_PLAY[DEVCON8g$SC16Q02==1] <- 1
DEVCON8g$EXC2_PLAY[DEVCON8g$SC16Q02==2] <- 0

DEVCON8g$EXC6_MATHCOMP[DEVCON8g$SC16Q06==1] <- 1
DEVCON8g$EXC6_MATHCOMP[DEVCON8g$SC16Q06==2] <- 0

DEVCON8g$EXC10_SPORT[DEVCON8g$SC16Q10==1] <- 1
DEVCON8g$EXC10_SPORT[DEVCON8g$SC16Q10==2] <- 0

DEVCON8g$EXC11_UNICORN[DEVCON8g$SC16Q11==1] <- 1
DEVCON8g$EXC11_UNICORN[DEVCON8g$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8g$SCL_EXTR_CL[DEVCON8g$SC20Q01==1] <- 1
DEVCON8g$SCL_EXTR_CL[DEVCON8g$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8g$SCORE_PUBLIC[DEVCON8g$SC19Q01==1] <- 1
DEVCON8g$SCORE_PUBLIC[DEVCON8g$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8g$QUAL_RECORD[DEVCON8g$SC39Q03==1] <- 1
DEVCON8g$QUAL_RECORD[DEVCON8g$SC39Q03==2] <- 0

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8g, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8g.rda") 

# First, remember, we have a smaller data set (25612 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R86 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R86
#Estimate Std. Error t value
#(Intercept)   390.54       2.96  131.72
#VIETNAM       125.98       6.72   18.74
#R-squared      27.64       2.45   11.27

# Let's try our regression with all gap decreasing variables before we do it individually per factor (student,
# teacher, etc)

R87 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R87
#Estimate Std. Error t value
#(Intercept)     342.68      20.00   17.13
#VIETNAM          76.20       7.73    9.86
#PRESCHOOL        25.32       3.84    6.60
#REPEAT          -37.40       3.10  -12.05
#ST08Q01          -7.95       1.35   -5.87
#ST115Q01         -5.32       1.88   -2.83
#BOOK_N            0.07       0.01    5.27
#PARPRESSURE      10.38       4.38    2.37
#PCGIRLS          12.59      14.78    0.85
#FUNDMOM           0.17       0.07    2.63
#COUNCILMOM       -0.13       0.06   -2.18
#PROPCERT         17.03       6.79    2.51
#SMRATIO          -0.03       0.01   -2.17
#TCSHORT           2.50       1.83    1.36
#TCFOCST          -2.05       1.89   -1.09
#TCM_STUASS        0.34       8.15    0.04
#TCM_PEER         -4.74       5.41   -0.88
#TCH_INCENTV      -2.82       2.71   -1.04
#ASS_PROG        -21.79       7.85   -2.77
#ASS_PROM         12.66       5.65    2.24
#ASS_SCH           0.65       7.55    0.09
#STU_FEEDB         1.91       4.92    0.39
#COMP_USE         -1.65       5.38   -0.31
#TXT_BOOK         -9.30       7.18   -1.30
#TOWN             -9.14       3.64   -2.51
#CLSIZE            0.83       0.24    3.47
#COMPWEB          15.57       6.23    2.50
#SCMATEDU          5.68       3.03    1.88
#SCMATBUI          3.53       2.49    1.42
#EXC2_PLAY         8.30       3.97    2.09
#EXC6_MATHCOMP    -0.93       5.33   -0.17
#EXC10_SPORT      -5.60       9.19   -0.61
#EXC11_UNICORN     6.66       5.58    1.19
#SCL_EXTR_CL      10.82       5.09    2.13
#SCORE_PUBLIC      9.62       4.89    1.97
#QUAL_RECORD       7.09       6.73    1.05
#SCHSEL            1.31       3.17    0.41
#R-squared        43.70       2.35   18.57

# Quite a good output/ decrease of the Vietnam dummy from 125.99 to 76.22, which is a 40% decrease.
# So let's see how the individual factors add to this 40%

# Just the student-related variables:

R88 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R88
#Estimate Std. Error t value
#(Intercept)   369.21       9.34   39.53
#VIETNAM        97.33       6.43   15.14
#PRESCHOOL      37.12       4.69    7.92
#REPEAT        -44.88       3.75  -11.96
#ST08Q01        -8.88       1.61   -5.51
#ST115Q01       -5.03       1.94   -2.59
#BOOK_N          0.08       0.01    6.14
#PARPRESSURE    13.53       4.58    2.95
#PCGIRLS        28.97      15.97    1.81
#FUNDMOM         0.22       0.06    3.58
#COUNCILMOM     -0.22       0.06   -3.60
#R-squared      39.29       2.22   17.67

# As expected, and seen from our previous regressions, student related-variables add a large part of decreasing
# the gap.

# The student-related variables and the teacher-related variables:

R89 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R89
#Estimate Std. Error t value
#(Intercept)   357.34      12.98   27.52
#VIETNAM        93.48       6.97   13.41
#PRESCHOOL      36.54       4.42    8.26
#REPEAT        -43.78       3.76  -11.64
#ST08Q01        -8.65       1.56   -5.55
#ST115Q01       -5.27       1.91   -2.76
#BOOK_N          0.08       0.01    6.10
#PARPRESSURE    13.06       4.62    2.83
#PCGIRLS        26.97      15.67    1.72
#FUNDMOM         0.22       0.06    3.44
#COUNCILMOM     -0.22       0.06   -3.42
#PROPCERT       15.41       6.10    2.53
#SMRATIO        -0.01       0.01   -1.63
#TCSHORT        -2.93       2.06   -1.42
#TCFOCST        -0.75       2.27   -0.33
#TCM_STUASS      9.55       9.04    1.06
#TCM_PEER       -1.12       6.33   -0.18
#TCH_INCENTV    -1.94       2.86   -0.68
#R-squared      39.92       2.24   17.81

# The student, teacher and pedagogical practices-related variables:

R90 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R90
#Estimate Std. Error t value
#(Intercept)   361.52      16.18   22.35
#VIETNAM        92.39       7.00   13.19
#PRESCHOOL      35.12       4.43    7.93
#REPEAT        -43.76       3.81  -11.48
#ST08Q01        -8.70       1.53   -5.70
#ST115Q01       -5.00       1.94   -2.57
#BOOK_N          0.08       0.01    5.97
#PARPRESSURE    11.50       4.83    2.38
#PCGIRLS        25.84      15.49    1.67
#FUNDMOM         0.21       0.06    3.41
#COUNCILMOM     -0.21       0.07   -3.26
#PROPCERT       16.70       6.91    2.42
#SMRATIO        -0.01       0.01   -1.43
#TCSHORT        -2.33       2.02   -1.15
#TCFOCST        -0.71       2.25   -0.32
#TCM_STUASS     10.38       9.19    1.13
#TCM_PEER       -0.52       6.33   -0.08
#TCH_INCENTV    -2.36       3.05   -0.77
#ASS_PROG      -20.49       8.36   -2.45
#ASS_PROM       12.86       5.62    2.29
#ASS_SCH         9.31       8.74    1.06
#STU_FEEDB       1.74       5.46    0.32
#COMP_USE        4.46       5.23    0.85
#TXT_BOOK      -10.63       7.19   -1.48
#R-squared      40.40       2.29   17.68

# A bit dissapointing but at least it is going down

# So it is easier to compare, let's just do here the student, teacher, pedagogical practices and school-related
# variables again:

R91 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R91
#Estimate Std. Error t value
#(Intercept)     342.68      20.00   17.13
#VIETNAM          76.20       7.73    9.86
#PRESCHOOL        25.32       3.84    6.60
#REPEAT          -37.40       3.10  -12.05
#ST08Q01          -7.95       1.35   -5.87
#ST115Q01         -5.32       1.88   -2.83
#BOOK_N            0.07       0.01    5.27
#PARPRESSURE      10.38       4.38    2.37
#PCGIRLS          12.59      14.78    0.85
#FUNDMOM           0.17       0.07    2.63
#COUNCILMOM       -0.13       0.06   -2.18
#PROPCERT         17.03       6.79    2.51
#SMRATIO          -0.03       0.01   -2.17
#TCSHORT           2.50       1.83    1.36
#TCFOCST          -2.05       1.89   -1.09
#TCM_STUASS        0.34       8.15    0.04
#TCM_PEER         -4.74       5.41   -0.88
#TCH_INCENTV      -2.82       2.71   -1.04
#ASS_PROG        -21.79       7.85   -2.77
#ASS_PROM         12.66       5.65    2.24
#ASS_SCH           0.65       7.55    0.09
#STU_FEEDB         1.91       4.92    0.39
#COMP_USE         -1.65       5.38   -0.31
#TXT_BOOK         -9.30       7.18   -1.30
#TOWN             -9.14       3.64   -2.51
#CLSIZE            0.83       0.24    3.47
#COMPWEB          15.57       6.23    2.50
#SCMATEDU          5.68       3.03    1.88
#SCMATBUI          3.53       2.49    1.42
#EXC2_PLAY         8.30       3.97    2.09
#EXC6_MATHCOMP    -0.93       5.33   -0.17
#EXC10_SPORT      -5.60       9.19   -0.61
#EXC11_UNICORN     6.66       5.58    1.19
#SCL_EXTR_CL      10.82       5.09    2.13
#SCORE_PUBLIC      9.62       4.89    1.97
#QUAL_RECORD       7.09       6.73    1.05
#SCHSEL            1.31       3.17    0.41
#R-squared        43.70       2.35   18.57

# Overall, the four different sets decreased the Vietnam gap as follows:
#                                                 abs	  abs_cum   %	  %_cum
# Original:                       VIETNAM 125.99
# Students:                       VIETNAM 97.33   -28.65	-28.65	23%	23%
# Students & Teachers:            VIETNAM 93.48   -3.85	  -32.5	  4%	26%
# Student & Teach & Ped.Pract:    VIETNAM 92.39   -1.09	  -33.59	1%	27%
# Stu & Tea & Ped.Prac & School:  VIETNAM 76.20   -16.19	-49.78	18%	40%

# Please be careful how you use these figures, especially the non-cumulative percentages!


























################################ OLD REGRESSIONS AND/OR FOR LATER USE ################################

############### OLD REGRESSIONS (with HISCED, FISCED) ####################

R1 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM",
                         "PRESCHOOL"),
                     weight="W_FSTUWT",
                     data=DEVCON8b,export=FALSE)
R1 # PRESCHOOL decreases the gap
#VIETNAM: 114.10

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R2 # PRESCHOOL, REPEAT decreases the gap
#VIETNAM: 109.49

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R3 # PRESCHOOL, REPEAT, ST08Q01 decreases the gap
#VIETNAM: 107.19 

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R4 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases
#VIETNAM: 107.23 

R5 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R5 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases
#VIETNAM: 107.02

R6 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R6 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases
#VIETNAM: 115.12

R7 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R7 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases,
# HISCED increases
#VIETNAM: 116.07

R8 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                      "MISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R8 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases,
# HISCED increases, MISCED increases
#VIETNAM: 116.20

R9 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                      "MISCED", "FISCED"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R9 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases,
# HISCED increases,MISCED increases, FISCED decreases (slightly)
#VIETNAM: 116.11

R10 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                      "MISCED", "FISCED", "WEALTH"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R10 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases
#VIETNAM: 116.46

R11 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R11 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases
#VIETNAM: 116.57

R12 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R12 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases
#VIETNAM: 116.71

R13 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R13 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases
#VIETNAM: 116.59

R14 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R14 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases
#VIETNAM: 115.46

R15 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R15 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases
#VIETNAM: 114.98

R16 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R16 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases
#VIETNAM: 115.26

R17 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R17 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases
#VIETNAM: 115.46

R18 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R18 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases, TEACHMOM increases
#VIETNAM: 116.62

R19 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R19 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases
#VIETNAM: 111.74

R20 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI", "HISCED",
                       "MISCED", "FISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R20 # PRESCHOOL, REPEAT, ST08Q01 decreases, ST09Q01 increases, ST115Q01 decreases, HISEI increases, 
# HISCED increases,MISCED increases, FISCED decreases (slightly), WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, PCGIRLS decreases, TIGERMOM increases,
# VOLUMOM increases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 108.20

# Now testing all 10 variables that decreased the gap

R21 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","FISCED","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R21
#Estimate Std. Error t value
#(Intercept)   355.78       7.35   48.38
#VIETNAM        98.75       5.02   19.67
#PRESCHOOL      36.36       3.94    9.22
#REPEAT        -46.48       2.90  -16.04
#ST08Q01        -9.67       1.19   -8.11
#ST115Q01       -4.92       1.70   -2.90
#FISCED          7.12       0.73    9.69
#BOOK_N          0.07       0.01    6.11
#PARPRESSURE    10.10       5.02    2.01
#PCGIRLS        18.44      10.77    1.71
#FUNDMOM         0.26       0.06    4.51
#COUNCILMOM     -0.16       0.05   -3.09
#R-squared      42.86       1.84   23.32

# Now testing all 10 variables that increased the gap

R22 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "ST09Q01", "HISEI", "HISCED", "MISCED", "WEALTH", "CULTPOS", "HEDRES",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R22
#Estimate Std. Error t value
#(Intercept)   419.25       5.20   80.55
#VIETNAM       131.30       4.51   29.10
#ST09Q01       -18.99       2.10   -9.04
#HISEI           0.47       0.05    8.94
#HISCED         -0.74       0.71   -1.04
#MISCED          3.20       0.74    4.34
#WEALTH          9.93       1.27    7.83
#CULTPOS        -3.87       0.85   -4.57
#HEDRES         12.68       0.94   13.46
#TIGERMOM       -0.02       0.06   -0.35
#VOLUMOM         0.10       0.08    1.19
#TEACHMOM       -0.07       0.07   -0.94
#R-squared      40.46       1.96   20.67


################ FOR LATER / NOTES ###############################################

# So let"s do our first regression on the student related non-rotated variables

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL","REPEAT","ST08Q01","ST09Q01","ST115Q01","HISEI", "HISCED", "MISCED","FISCED",
                      "WEALTH","CULTPOS","HEDRES","BOOK_N", "PARPRESSURE", "PCGIRLS","TIGERMOM","VOLUMOM",
                      "TEACHMOM", "FUNDMOM", "COUNCILMOM"), 
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R1



# As opposed to our original regression of ...

MATH0 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
MATH0

# that gives us ...
# Estimate Std. Error t value
# (Intercept)   383.29       2.50  153.26
# VIETNAM       128.05       5.65   22.68
# R-squared      27.21       2.25   12.07


########################################################### JUST TESTS #################################

length(DEVCON8a$REPEAT)
T1test <- DEVCON8a[, c("VIETNAM", "ST07Q01")]
N1test <- NROW(na.omit(T1test))
N1test

# 47359

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01","HISEI","MISCED","FISCED", 
                    "WEALTH","CULTPOS","HEDRES","ST28Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 # 33,500
N0-N1 # 14983 NAs

# to test for the complementary of SC25Q01 and SC25Q02
T1test <- DEVCON8b[, c("VIETNAM", "SC25Q01","SC25Q02", "SC25Q03", "SC25Q04")]
T1test

T1test$FIRST <- T1test$SC25Q01+T1test$SC25Q02
T1test$SECOND <- T1test$SC25Q03+T1test$SC25Q04

mean(T1test$FIRST)
mean(T1test$SECOND)

############ THIS IS JUST NOTES or tests or for later use ##############

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# We will work closely with these indices as regressors; to see on which questionnaire items they are based on, please
# consult the PISA 2012 Technical Manual, Ch. 16

# We group the rotated questions not according to our conceptual scheme but in which  

TP2 <- DEVCON8a[, c("ANXMAT", "SCMAT", "ATSCHL", "ATTLNACT", "BELONG")]
NP2 <- NROW(na.omit(TP2))
NP2 # 29384

TP3 <- DEVCON8a[, c("ST55Q02", "ST57Q01", "ST57Q02", "ST57Q03", "ST57Q04", "ST57Q05", "ST57Q06", "EXAPPLM", "EXPUREM", "FAMCONC")]
NP3 <- NROW(na.omit(TP3))
NP3 # 15311

TP1 <- DEVCON8a[, c("MATWKETH", "PERSEV", "OPENPS", "INTMAT", "INSTMOT", "SUBNORM", "MATHEFF", "FAILMAT", "MATINTFC", "MATBEH")]
NP1 <- NROW(na.omit(TP1))
NP1 # 28364

############ THIS IS JUST NOTES or tests or for later use ##############

# ST01 - GRADE
# We omit as an analytical variable as we already capture that aspect through Preschool and Age variable


Tanne <- DEVCON8a[, c("NEWID")]
Nanne <- NROW(na.omit(T0))
Nanne


DEVCON8a$SC47Q01
length(DEVCON8a$SC47Q01)
class(DEVCON8a$SC47Q01)

T1b <- DEVCON8a[, c("SC47Q01")]
N1 <- NROW(na.omit(T1b)) # removes all NA's
N1 # 0
N0-N1 # 48483 NAs ---> no Financial Education data for (at least one of these) countries, lets leave it

################################### FOR LATER: #######################################

# Some of the PISA items were designed to be used in analyses as single items (for example, gender). However, most questionnaire
# items were designed to be combined in some way in order to measure latent constructs that cannot be observed directly.
# We will work closely with these indices as regressors; to see on which questionnaire items they are based on, please
# consult the PISA 2012 Technical Manual, Ch. 16


####################### JUST for TESTING out: with and without deleting NA's ####################

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM", 
                      "COGACT",
                      "CULTPOS",
                      "HOMEPOS",
                      "INTMAT",
                      "INSTMOT"),
                  weight="W_FSTUWT",
                  data=DEVCON8a,export=FALSE)
R0
# Estimate Std. Error t value
# (Intercept)   430.10       4.56   94.34
# VIETNAM       128.94       5.14   25.10
# COGACT          5.02       1.30    3.86
# CULTPOS        -9.98       1.24   -8.03
# HOMEPOS        26.12       1.70   15.34
# INTMAT         -2.52       2.09   -1.20
# INSTMOT         2.84       1.95    1.46
# R-squared      37.46       2.07   18.07

T1b <- DEVCON8a[, c("VIETNAM","COGACT","CULTPOS", "HOMEPOS", "INTMAT", "INSTMOT")]
N1 <- NROW(na.omit(T1b)) 
N1 #15035
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM", 
                      "COGACT",
                      "CULTPOS",
                      "HOMEPOS",
                      "INTMAT",
                      "INSTMOT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# Estimate Std. Error t value
# (Intercept)   430.10       4.56   94.34
# VIETNAM       128.94       5.14   25.10
# COGACT          5.02       1.30    3.86
# CULTPOS        -9.98       1.24   -8.03
# HOMEPOS        26.12       1.70   15.34
# INTMAT         -2.52       2.09   -1.20
# INSTMOT         2.84       1.95    1.46
# R-squared      37.46       2.07   18.07

mean1A <- t(sapply(DEVCON8a[c("COGACT","CULTPOS","HOMEPOS", "INTMAT", "INSTMOT")], function(x) 
  unlist(t.test(x~DEVCON8a$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# COGACT                 0.2961395               -0.3261360  0.000000e+00   47.778674
# CULTPOS               -0.1461614               -0.2364387  3.840693e-09    5.899749
# HOMEPOS               -1.3157310               -1.8168381 3.646975e-208   31.925199
# INTMAT                 0.7187600                0.6919566  3.412589e-02    2.119206
# INSTMOT                0.4258199                0.3689303  3.473342e-05    4.144245

mean1B <- t(sapply(DEVCON8b[c("COGACT","CULTPOS","HOMEPOS", "INTMAT", "INSTMOT")], function(x) 
  unlist(t.test(x~DEVCON8b$VIETNAM,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1B

# estimate.mean in group 0 estimate.mean in group 1       p.value statistic.t
# COGACT                 0.2515972               -0.3329091 6.802506e-176   30.778643
# CULTPOS               -0.1498809               -0.2633874  2.148253e-05    4.259201
# HOMEPOS               -1.2993032               -1.8394074  4.382984e-77   19.358243
# INTMAT                 0.7369364                0.6680100  1.401528e-04    3.814131
# INSTMOT                0.4056477                0.3180724  3.931470e-06    4.626535

###############################################################################


DEVCON8a$FAILMAT
DEVCON8a

T1b <- DEVCON8a[, c("VIETNAM","COGACT","CULTPOS","HOMEPOS","FAILMAT","EXAPPLM")]
N1 <- NROW(na.omit(T1b)) 
N1 # 0
N0-N1 # 48483
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

R0 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM", 
                      "COGACT",
                      "CULTPOS",
                      "HOMEPOS",
                      "FAILMAT"),
                  weight="W_FSTUWT",
                  data=DEVCON8b,export=FALSE)
R0

# I work on dataframe of non-missing
DEVCON8b <- DEVCON8a[complete.cases(T1b),]


length(DEVCON8$ST74Q01)

is.na(DEVCON8$ST74Q01)

# How many cases ?
T0 <- DEVCON8a[, c("VIETNAM")]
N0<- NROW(na.omit(T0)) # 48483 data points - we have scores on these students for sure

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","ST07Q01","ST08Q01","ST09Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) # removes all NA's
N1 # 43,626
N0-N1 # 4857 NAs

T1b[complete.cases(T1b),]
length(T1b)
T1c <- na.omit(T1b)
length(T1c$VIETNAM) # 43626

# I work on dataframe of non-missing
DEVCON8test <- DEVCON8a[complete.cases(T1b),]
length(DEVCON8test$SC22Q13)

T10 <- DEVCON8a[, c("VIETNAM")]
N0 <- NROW(na.omit(T10))
N0

# Not applicable anymore, see page 376 is.na(DEVCON8a$PERSEV)
# T1z <- DEVCON8a[, c("VIETNAM", "PERSEV")]
# N1 <- NROW(na.omit)
# N1

#How many missing variables? 
# length(which(is.na(DEVCON8a$PERSEV)))



