# PISA2012_FL_part3
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/21/2015

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

############### 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH ################

################### 4.2.6 Non-rotated Questions - all gap decreasing & increasing / Sensitivty ######################

# Finally, we need to see how the Vietnam dummy increases and decreases when we add all gap decreasing and gap increasing
# variables to it (separately) on a sample of non-missing values for all variables.

# So let's see first how many missing variables we have overall:

# How big is our initital sample size? Two ways to check: 
T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0)) 
N0 # 48483 data points (we already know that)

# Here is an overview of all the variables we will be using:

# Students:"ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","HISEI","MISCED",
#           "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS", (SC25 left out)
# Teachers: "STRATIO","PROPCERT","PROPQUAL","TCSHORT","SMRATIO","TCFOCST", 
#           "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04", "SC35Q02", "SC39Q08","SC31Q01", 
#           "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07"
# Pedagogical Practices: "SC18Q02","SC18Q03","SC18Q04","SC18Q05","SC18Q06","SC18Q07","SC18Q08",
#           "SC39Q07","SC40Q01","SC40Q02","SC40Q03"
# Schools:"SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU",
#           "SCMATBUI","SC16Q01","SC16Q02","SC16Q03","SC16Q04","SC16Q05","SC16Q06","SC16Q07",
#           "SC16Q08","SC16Q09","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC19Q02","SCHAUTON",
#           "TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC39Q03",
#           "SCHSEL","STUDCLIM","TEACCLIM","TCMORALE"

# How many non-missing values for all non-rotated student (excl SC25), teacher, pedagogical practices and school variables?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","HISEI","MISCED",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS",
                    "STRATIO","PROPCERT","PROPQUAL","TCSHORT","SMRATIO","TCFOCST", 
                    "SC30Q01", "SC30Q02", "SC30Q03", "SC30Q04", "SC35Q02", "SC39Q08","SC31Q01", 
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "SC18Q02","SC18Q03","SC18Q04","SC18Q05","SC18Q06","SC18Q07","SC18Q08",
                    "SC39Q07","SC40Q01","SC40Q02","SC40Q03",
                    "SC01Q01","SC02Q02","SC03Q01","CLSIZE","SCHSIZE",
                    "RATCMP15","COMPWEB","SCMATEDU","SCMATBUI","SC16Q01","SC16Q02","SC16Q03","SC16Q04",
                    "SC16Q05","SC16Q06","SC16Q07","SC16Q08","SC16Q09","SC16Q10","SC16Q11","SC20Q01","SC19Q01",
                    "SC19Q02","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH","SC39Q03",
                    "SCHSEL","STUDCLIM","TEACCLIM","TCMORALE")]
N1 <- NROW(na.omit(T1b)) 
N1 # 17492
N0-N1 # 30991 NAs
DEVCON8h <- DEVCON8a[complete.cases(T1b),]

# A sample size of 17492 data points/rows seems overall acceptable. 
# Let's prepare the variables (again)

######################### Student variables ########

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8h$FEMALE[DEVCON8h$ST04Q01==1] <- 1
DEVCON8h$FEMALE[DEVCON8h$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8h$PRESCHOOL[DEVCON8h$ST05Q01==1] <- 0
DEVCON8h$PRESCHOOL[DEVCON8h$ST05Q01==2] <- 1
DEVCON8h$PRESCHOOL[DEVCON8h$ST05Q01==3] <- 1

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
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==1]  <- 5
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==2]  <- 15
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==3]  <- 60
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==4]  <- 150
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==5]  <- 350
DEVCON8h$BOOK_N[DEVCON8h$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# This question asked principal to tick if there is constant, little or largely absent pressure from parents on them to 
# set high academic standards AND the expectation of parents to achieve them. We think this gives a good proxy to measure
# how much pressure 'from home' also rests on the students and hence included it in the student variables.
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8h$PARPRESSURE[DEVCON8h$SC24Q01==1] <- 1
DEVCON8h$PARPRESSURE[DEVCON8h$SC24Q01==2] <- 0
DEVCON8h$PARPRESSURE[DEVCON8h$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We intentionally did not delete missing variables from the 'SC25' (Parent Participation) variable. In this specific case,
# we replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
# This may generate some spurious data, but we still find it safe to replace N/A with 0's, indicating that no parents 
# fall in this category. 
DEVCON8h$SC25Q01[is.na(DEVCON8h$SC25Q01)]  <- 0
DEVCON8h$SC25Q02[is.na(DEVCON8h$SC25Q02)]  <- 0
DEVCON8h$SC25Q03[is.na(DEVCON8h$SC25Q03)]  <- 0
DEVCON8h$SC25Q04[is.na(DEVCON8h$SC25Q04)]  <- 0
DEVCON8h$SC25Q05[is.na(DEVCON8h$SC25Q05)]  <- 0
DEVCON8h$SC25Q06[is.na(DEVCON8h$SC25Q06)]  <- 0
DEVCON8h$SC25Q07[is.na(DEVCON8h$SC25Q07)]  <- 0
DEVCON8h$SC25Q08[is.na(DEVCON8h$SC25Q08)]  <- 0
DEVCON8h$SC25Q09[is.na(DEVCON8h$SC25Q09)]  <- 0
DEVCON8h$SC25Q10[is.na(DEVCON8h$SC25Q10)]  <- 0
DEVCON8h$SC25Q11[is.na(DEVCON8h$SC25Q11)]  <- 0
DEVCON8h$SC25Q12[is.na(DEVCON8h$SC25Q12)]  <- 0

# SC25Q01 is quite rich in information, so we create sub-variables
#TIGERMOM
DEVCON8h$TIGERMOM  <- DEVCON8h$SC25Q01+DEVCON8h$SC25Q03
DEVCON8h$TIGERMOM[DEVCON8h$TIGERMOM>100] <- 100 

# Since asking for students behaviour and students progress can be viewed complementary, we add them up (and do not average them).
# We then account for the fact that not more than 100% of parents can logically be a 'TIGERMOM'. You can do that differently, 
# as long as you are consistent with the other created out of the 'SC25' questions.

#VOLUMOM
DEVCON8h$VOLUMOM <- DEVCON8h$SC25Q05+DEVCON8h$SC25Q06+DEVCON8h$SC25Q07+DEVCON8h$SC25Q09+DEVCON8h$SC25Q12
DEVCON8h$VOLUMOM[DEVCON8h$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8h$TEACHMOM <- DEVCON8h$SC25Q08

#FUNDMOM
DEVCON8h$FUNDMOM <-  DEVCON8h$SC25Q11

#COUNCILMOM
DEVCON8h$COUNCILMOM <- DEVCON8h$SC25Q10

######################### Teacher variables ########

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8h$TCM_STUASS[DEVCON8h$SC30Q01==1] <- 1
DEVCON8h$TCM_STUASS[DEVCON8h$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8h$TCM_PEER[DEVCON8h$SC30Q02==1] <- 1
DEVCON8h$TCM_PEER[DEVCON8h$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8h$TCM_OBSER[DEVCON8h$SC30Q03==1] <- 1
DEVCON8h$TCM_OBSER[DEVCON8h$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8h$TCM_INSPE[DEVCON8h$SC30Q04==1] <- 1
DEVCON8h$TCM_INSPE[DEVCON8h$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8h$TCH_MENT[DEVCON8h$SC39Q08==1] <- 1
DEVCON8h$TCH_MENT[DEVCON8h$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8h <- merge(DEVCON8h,SC31OUT.rda,by="NEWID")
DEVCON8h$TCH_INCENTV <- rescale(DEVCON8h$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

######################### Pedagogical Practices variables ########

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8h$ASS_PROG[DEVCON8h$SC18Q01==1] <- 1
DEVCON8h$ASS_PROG[DEVCON8h$SC18Q01==2] <- 0

DEVCON8h$ASS_PROM[DEVCON8h$SC18Q02==1] <- 1
DEVCON8h$ASS_PROM[DEVCON8h$SC18Q02==2] <- 0

DEVCON8h$ASS_INSTR[DEVCON8h$SC18Q03==1] <- 1
DEVCON8h$ASS_INSTR[DEVCON8h$SC18Q03==2] <- 0

DEVCON8h$ASS_NAT[DEVCON8h$SC18Q04==1] <- 1
DEVCON8h$ASS_NAT[DEVCON8h$SC18Q04==2] <- 0

DEVCON8h$ASS_SCH[DEVCON8h$SC18Q05==1] <- 1
DEVCON8h$ASS_SCH[DEVCON8h$SC18Q05==2] <- 0

DEVCON8h$ASS_TCH[DEVCON8h$SC18Q06==1] <- 1
DEVCON8h$ASS_TCH[DEVCON8h$SC18Q06==2] <- 0

DEVCON8h$ASS_CUR[DEVCON8h$SC18Q07==1] <- 1
DEVCON8h$ASS_CUR[DEVCON8h$SC18Q07==2] <- 0

DEVCON8h$ASS_OTH[DEVCON8h$SC18Q08==1] <- 1
DEVCON8h$ASS_OTH[DEVCON8h$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8h$STU_FEEDB[DEVCON8h$SC39Q07==1] <- 1
DEVCON8h$STU_FEEDB[DEVCON8h$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8h$COMP_USE[DEVCON8h$SC40Q01==1] <- 1
DEVCON8h$COMP_USE[DEVCON8h$SC40Q01==2] <- 0

DEVCON8h$TXT_BOOK[DEVCON8h$SC40Q02==1] <- 1
DEVCON8h$TXT_BOOK[DEVCON8h$SC40Q02==2] <- 0

DEVCON8h$STD_CUR[DEVCON8h$SC40Q03==1] <- 1
DEVCON8h$STD_CUR[DEVCON8h$SC40Q03==2] <- 0

######################### School variables ########

# Now for the schools-related variables

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8h$PRIVATESCL[DEVCON8h$SC01Q01==2] <- 1
DEVCON8h$PRIVATESCL[DEVCON8h$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8h$DUM_VILLAGE <- ifelse(DEVCON8h$SC03Q01==1,1,0)
DEVCON8h$DUM_SMLTOWN <- ifelse(DEVCON8h$SC03Q01==2,1,0)
DEVCON8h$DUM_TOWN    <- ifelse(DEVCON8h$SC03Q01==3,1,0)
DEVCON8h$DUM_CITY    <- ifelse(DEVCON8h$SC03Q01==4,1,0)
DEVCON8h$DUM_LRGCITY <- ifelse(DEVCON8h$SC03Q01==5,1,0)

DEVCON8h$TOWN <- DEVCON8h$DUM_SMLTOWN+DEVCON8h$DUM_TOWN
DEVCON8h$TOWN[DEVCON8h$TOWN>1] <- 1
DEVCON8h$CITY <- DEVCON8h$DUM_CITY+DEVCON8h$DUM_LRGCITY
DEVCON8h$CITY[DEVCON8h$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8h$EXC1_BAND[DEVCON8h$SC16Q01==1] <- 1
DEVCON8h$EXC1_BAND[DEVCON8h$SC16Q01==2] <- 0

DEVCON8h$EXC2_PLAY[DEVCON8h$SC16Q02==1] <- 1
DEVCON8h$EXC2_PLAY[DEVCON8h$SC16Q02==2] <- 0

DEVCON8h$EXC3_NEWS[DEVCON8h$SC16Q03==1] <- 1
DEVCON8h$EXC3_NEWS[DEVCON8h$SC16Q03==2] <- 0

DEVCON8h$EXC4_VOLU[DEVCON8h$SC16Q04==1] <- 1
DEVCON8h$EXC4_VOLU[DEVCON8h$SC16Q04==2] <- 0

DEVCON8h$EXC5_MCLUB[DEVCON8h$SC16Q05==1] <- 1
DEVCON8h$EXC5_MCLUB[DEVCON8h$SC16Q05==2] <- 0

DEVCON8h$EXC6_MATHCOMP[DEVCON8h$SC16Q06==1] <- 1
DEVCON8h$EXC6_MATHCOMP[DEVCON8h$SC16Q06==2] <- 0

DEVCON8h$EXC7_CHESS[DEVCON8h$SC16Q07==1] <- 1
DEVCON8h$EXC7_CHESS[DEVCON8h$SC16Q07==2] <- 0

DEVCON8h$EXC8_ICTCB[DEVCON8h$SC16Q08==1] <- 1
DEVCON8h$EXC8_ICTCB[DEVCON8h$SC16Q08==2] <- 0

DEVCON8h$EXC9_ARTCB[DEVCON8h$SC16Q09==1] <- 1
DEVCON8h$EXC9_ARTCB[DEVCON8h$SC16Q09==2] <- 0

DEVCON8h$EXC10_SPORT[DEVCON8h$SC16Q10==1] <- 1
DEVCON8h$EXC10_SPORT[DEVCON8h$SC16Q10==2] <- 0

DEVCON8h$EXC11_UNICORN[DEVCON8h$SC16Q11==1] <- 1
DEVCON8h$EXC11_UNICORN[DEVCON8h$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8h$SCL_EXTR_CL[DEVCON8h$SC20Q01==1] <- 1
DEVCON8h$SCL_EXTR_CL[DEVCON8h$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8h$SCORE_PUBLIC[DEVCON8h$SC19Q01==1] <- 1
DEVCON8h$SCORE_PUBLIC[DEVCON8h$SC19Q01==2] <- 0

DEVCON8h$SCORE_AUTHRITS[DEVCON8h$SC19Q02==1] <- 1
DEVCON8h$SCORE_AUTHRITS[DEVCON8h$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8h$QUAL_RECORD[DEVCON8h$SC39Q03==1] <- 1
DEVCON8h$QUAL_RECORD[DEVCON8h$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# Let's support R and create an intermediate file we will just load when we come back here, so that the
# R memory does not get all worked up:
save(DEVCON8h, file = "C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON8h.rda") 

# Let's try the gap decreasing variables first. We already have them from _part2, the new work will be compiling the 
# gap increasing variables.

# First, remember, we have a smaller data set (17492 data points) compared to when we first regressed the Vietnam PISA Math score
# (48483 data points); hence we regress again to get the correct size of the Vietnam dummy. 

R92 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R92
#Estimate Std. Error t value
#(Intercept)   396.95       3.31  119.87
#VIETNAM       117.68       6.92   17.00
#R-squared      28.68       2.60   11.02

######################################## Gap decreasing variables ########

# Let's try our regression with all gap decreasing variables before we do it individually per factor (student,
# teachers, pedagogical practices and schools)

R93 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R93
#Estimate Std. Error t value
#(Intercept)     374.97      23.82   15.74
#VIETNAM          71.91       7.64    9.41
#PRESCHOOL        26.96       4.07    6.62
#REPEAT          -39.43       3.68  -10.73
#ST08Q01          -7.77       1.38   -5.63
#ST115Q01         -4.54       2.03   -2.24
#BOOK_N            0.07       0.01    5.20
#PARPRESSURE      10.93       4.72    2.32
#PCGIRLS          19.91      18.41    1.08
#FUNDMOM           0.17       0.06    2.58
#COUNCILMOM       -0.17       0.06   -2.71
#PROPCERT         15.80       6.75    2.34
#SMRATIO          -0.03       0.01   -1.96
#TCSHORT           5.77       2.01    2.87
#TCFOCST          -2.80       2.17   -1.29
#TCM_STUASS       -6.04       7.36   -0.82
#TCM_PEER         -5.44       5.88   -0.92
#TCH_INCENTV      -1.50       2.20   -0.68
#ASS_PROG        -31.95      10.15   -3.15
#ASS_PROM          8.75       6.15    1.42
#ASS_SCH         -12.41      10.17   -1.22
#STU_FEEDB         0.07       5.86    0.01
#COMP_USE          1.08       5.21    0.21
#TXT_BOOK        -18.30       8.53   -2.15
#TOWN             -5.30       4.31   -1.23
#CLSIZE            0.74       0.23    3.21
#COMPWEB          12.01       6.88    1.74
#SCMATEDU          6.47       3.18    2.03
#SCMATBUI          2.24       2.70    0.83
#EXC2_PLAY         6.14       4.82    1.27
#EXC6_MATHCOMP     2.65       5.79    0.46
#EXC10_SPORT      -1.87      10.23   -0.18
#EXC11_UNICORN     7.43       5.80    1.28
#SCL_EXTR_CL      13.84       5.28    2.62
#SCORE_PUBLIC      9.65       5.65    1.71
#QUAL_RECORD       7.45       7.87    0.95
#SCHSEL            1.63       3.27    0.50
#R-squared        45.37       2.38   19.03

# The gap decrease on this sample is roughly the same as on the previous ones, with a gap decrease of approx. 39% (from 117.68 to 71.91)

# Just the gap decreasing student-related variables:

R94 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R94
# Estimate Std. Error t value
#(Intercept)   371.40      11.05   33.62
#VIETNAM        90.35       6.38   14.16
#PRESCHOOL      37.57       4.44    8.47
#REPEAT        -47.50       4.36  -10.89
#ST08Q01        -8.65       1.63   -5.32
#ST115Q01       -4.66       2.00   -2.33
#BOOK_N          0.09       0.01    6.20
#PARPRESSURE    13.83       4.75    2.91
#PCGIRLS        31.55      20.97    1.50
#FUNDMOM         0.24       0.06    3.96
#COUNCILMOM     -0.27       0.06   -4.60
#R-squared      41.14       2.37   17.37

# As expected and seen in previous regressions on different samples, student variables decreased the gap significantly

# The gap decreasing student and teacher variables:

R95 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R95
#Estimate Std. Error t value
#(Intercept)   368.63      12.52   29.43
#VIETNAM        86.56       7.20   12.02
#PRESCHOOL      36.97       4.39    8.42
#REPEAT        -46.53       4.33  -10.75
#ST08Q01        -8.32       1.57   -5.29
#ST115Q01       -4.62       2.05   -2.25
#BOOK_N          0.09       0.01    6.06
#PARPRESSURE    13.33       4.89    2.72
#PCGIRLS        29.18      20.28    1.44
#FUNDMOM         0.22       0.06    3.53
#COUNCILMOM     -0.26       0.06   -4.29
#PROPCERT       17.19       6.46    2.66
#SMRATIO        -0.02       0.01   -1.93
#TCSHORT         0.44       2.02    0.22
#TCFOCST        -1.67       2.55   -0.66
#TCM_STUASS     -0.38       7.17   -0.05
#TCM_PEER       -2.32       6.21   -0.37
#TCH_INCENTV     0.16       2.35    0.07
#R-squared      41.74       2.30   18.17

# The gap decreasing student, teacher and pedagogical practices-related variables:

R96 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R96
#Estimate Std. Error t value
#(Intercept)   388.87      17.75   21.91
#VIETNAM        85.06       7.07   12.04
#PRESCHOOL      34.47       4.26    8.09
#REPEAT        -47.06       4.46  -10.55
#ST08Q01        -8.19       1.54   -5.33
#ST115Q01       -4.13       2.05   -2.01
#BOOK_N          0.09       0.01    6.09
#PARPRESSURE    12.06       4.69    2.57
#PCGIRLS        31.22      18.70    1.67
#FUNDMOM         0.22       0.06    3.68
#COUNCILMOM     -0.25       0.06   -4.19
#PROPCERT       17.57       6.90    2.55
#SMRATIO        -0.02       0.01   -1.69
#TCSHORT         1.16       1.89    0.61
#TCFOCST        -1.42       2.59   -0.55
#TCM_STUASS      3.20       7.89    0.41
#TCM_PEER       -0.16       6.54   -0.02
#TCH_INCENTV    -0.43       2.45   -0.18
#ASS_PROG      -21.77       9.91   -2.20
#ASS_PROM        9.52       6.17    1.54
#ASS_SCH         0.65       9.94    0.07
#STU_FEEDB      -0.55       6.01   -0.09
#COMP_USE        5.93       5.53    1.07
#TXT_BOOK      -19.49       8.28   -2.35
#R-squared      42.61       2.35   18.15

# So it is easier to compare, let's just do here the student, teacher, pedagogical practices and school-related
# variables again:

R97 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R97
# see above

######################################## Gap increasing variables ########

# Let's try our regression with all gap increasing variables before we do it individually per factor (student,
# teachers, pedagogical practices and schools)

R98 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","VOLUMOM","TEACHMOM","STRATIO","PROPQUAL","TCM_OBSER",
                        "TCM_INSPE","SC35Q02","TCH_MENT","ASS_INSTR","ASS_NAT",
                        "ASS_TCH","ASS_CUR","ASS_OTH","STD_CUR","PRIVATESCL","SC02Q02","DUM_VILLAGE",
                        "SCHSIZE","RATCMP15","SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST",
                        "LEADPD","LEADTCH","STUDCLIM","TEACCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8h,export=FALSE)
R98
#Estimate Std. Error t value
#(Intercept)      400.18      16.43   24.36
#VIETNAM          133.62       8.62   15.50
#FEMALE            -8.41       2.16   -3.90
#ST09Q01          -17.76       2.62   -6.78
#HISEI              0.44       0.06    7.13
#MISCED             1.63       0.58    2.81
#WEALTH             4.00       1.29    3.11
#CULTPOS           -2.03       1.06   -1.92
#HEDRES             8.81       1.13    7.81
#TIGERMOM           0.06       0.06    0.89
#VOLUMOM           -0.01       0.08   -0.16
#TEACHMOM          -0.03       0.09   -0.29
#STRATIO           -1.08       0.32   -3.35
#PROPQUAL         -10.70      14.36   -0.74
#TCM_OBSER         -7.70       6.26   -1.23
#TCM_INSPE         -3.63       5.06   -0.72
#SC35Q02            0.11       0.06    1.76
#TCH_MENT          -2.88       7.91   -0.36
#ASS_INSTR         13.03       5.17    2.52
#ASS_NAT            0.46       5.49    0.08
#ASS_TCH            0.07       6.35    0.01
#ASS_CUR           -2.73       9.68   -0.28
#ASS_OTH            4.88       5.05    0.97
#STD_CUR            2.99       6.51    0.46
#PRIVATESCL       -23.29       7.33   -3.18
#SC02Q02            0.29       0.08    3.51
#DUM_VILLAGE       -2.88       5.86   -0.49
#SCHSIZE            0.02       0.00    7.73
#RATCMP15           0.48       9.97    0.05
#SCORE_AUTHRITS    11.47       5.73    2.00
#SCHAUTON           6.26       3.10    2.02
#TCHPARTI           3.59       1.70    2.11
#LEADCOM            1.64       3.48    0.47
#LEADINST          -1.74       3.90   -0.45
#LEADPD             1.58       3.13    0.50
#LEADTCH            1.42       3.22    0.44
#STUDCLIM           6.59       2.75    2.40
#TEACCLIM          -1.52       3.18   -0.48
#TCMORALE          -3.45       2.47   -1.40
#R-squared         46.29       2.30   20.15

# All the gap increasing student variables:

R99 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                       "TIGERMOM","VOLUMOM","TEACHMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R99
#Estimate Std. Error t value
#(Intercept)   426.10       5.96   71.46
#VIETNAM       125.40       6.19   20.27
#FEMALE         -5.36       2.84   -1.89
#ST09Q01       -18.10       2.93   -6.17
#HISEI           0.42       0.06    6.94
#MISCED          1.71       0.66    2.59
#WEALTH          9.00       1.62    5.56
#CULTPOS        -2.56       1.15   -2.23
#HEDRES         12.67       1.31    9.64
#TIGERMOM        0.03       0.08    0.35
#VOLUMOM         0.09       0.11    0.81
#TEACHMOM       -0.06       0.09   -0.61
#R-squared      39.47       2.49   15.88

# All the gap increasing student and teacher variables: 

R100 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                       "TIGERMOM","VOLUMOM","TEACHMOM","STRATIO","PROPQUAL","TCM_OBSER",
                       "TCM_INSPE","SC35Q02","TCH_MENT"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R100
#Estimate Std. Error t value
#(Intercept)   422.61      24.61   17.17
#VIETNAM       126.64       6.72   18.86
#FEMALE         -6.01       2.44   -2.47
#ST09Q01       -19.09       2.91   -6.57
#HISEI           0.43       0.06    6.94
#MISCED          1.96       0.63    3.09
#WEALTH          8.31       1.32    6.30
#CULTPOS        -2.50       1.14   -2.20
#HEDRES         12.37       1.31    9.46
#TIGERMOM        0.03       0.08    0.37
#VOLUMOM         0.06       0.11    0.55
#TEACHMOM       -0.06       0.10   -0.61
#STRATIO        -0.26       0.27   -0.97
#PROPQUAL       -5.53      19.95   -0.28
#TCM_OBSER       1.64       6.87    0.24
#TCM_INSPE      -9.40       5.58   -1.68
#SC35Q02         0.20       0.06    3.27
#TCH_MENT        8.63       8.45    1.02
#R-squared      40.36       2.65   15.24

# All the gap increasing student, teacher and pedagogical practices variables:

R101 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                       "TIGERMOM","VOLUMOM","TEACHMOM","STRATIO","PROPQUAL","TCM_OBSER",
                       "TCM_INSPE","SC35Q02","TCH_MENT","ASS_INSTR","ASS_NAT",
                       "ASS_TCH","ASS_CUR","ASS_OTH","STD_CUR"),
                   weight="W_FSTUWT",
                   data=DEVCON8h,export=FALSE)
R101
#Estimate Std. Error t value
#(Intercept)   423.56      23.96   17.68
#VIETNAM       126.09       6.71   18.78
#FEMALE         -6.84       2.50   -2.74
#ST09Q01       -18.95       2.93   -6.46
#HISEI           0.43       0.06    6.87
#MISCED          1.95       0.66    2.94
#WEALTH          7.53       1.34    5.62
#CULTPOS        -2.43       1.10   -2.20
#HEDRES         12.15       1.30    9.32
#TIGERMOM        0.03       0.08    0.35
#VOLUMOM         0.03       0.10    0.26
#TEACHMOM       -0.06       0.10   -0.56
#STRATIO        -0.21       0.30   -0.70
#PROPQUAL       -7.32      20.04   -0.37
#TCM_OBSER       3.15       6.34    0.50
#TCM_INSPE      -9.49       5.54   -1.71
#SC35Q02         0.19       0.06    3.10
#TCH_MENT        5.87       9.42    0.62
#ASS_INSTR      12.86       4.97    2.59
#ASS_NAT         4.70       6.26    0.75
#ASS_TCH        -4.34       5.96   -0.73
#ASS_CUR        -0.23      10.97   -0.02
#ASS_OTH         2.70       5.87    0.46
#STD_CUR       -11.11       6.28   -1.77
#R-squared      40.97       2.65   15.48

# So it is easier to compare, let's just do here the student, teacher, pedagogical practices and school-related
# variables that increase the gap again:

R102 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "FEMALE", "ST09Q01", "HISEI","MISCED","WEALTH","CULTPOS","HEDRES",
                        "TIGERMOM","VOLUMOM","TEACHMOM","STRATIO","PROPQUAL","TCM_OBSER",
                        "TCM_INSPE","SC35Q02","TCH_MENT","ASS_INSTR","ASS_NAT",
                        "ASS_TCH","ASS_CUR","ASS_OTH","STD_CUR","PRIVATESCL","SC02Q02","DUM_VILLAGE",
                        "SCHSIZE","RATCMP15","SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST",
                        "LEADPD","LEADTCH","STUDCLIM","TEACCLIM","TCMORALE"),
                    weight="W_FSTUWT",
                    data=DEVCON8h,export=FALSE)
R102
#Estimate Std. Error t value
#(Intercept)      400.18      16.43   24.36
#VIETNAM          133.62       8.62   15.50
#FEMALE            -8.41       2.16   -3.90
#ST09Q01          -17.76       2.62   -6.78
#HISEI              0.44       0.06    7.13
#MISCED             1.63       0.58    2.81
#WEALTH             4.00       1.29    3.11
#CULTPOS           -2.03       1.06   -1.92
#HEDRES             8.81       1.13    7.81
#TIGERMOM           0.06       0.06    0.89
#VOLUMOM           -0.01       0.08   -0.16
#TEACHMOM          -0.03       0.09   -0.29
#STRATIO           -1.08       0.32   -3.35
#PROPQUAL         -10.70      14.36   -0.74
#TCM_OBSER         -7.70       6.26   -1.23
#TCM_INSPE         -3.63       5.06   -0.72
#SC35Q02            0.11       0.06    1.76
#TCH_MENT          -2.88       7.91   -0.36
#ASS_INSTR         13.03       5.17    2.52
#ASS_NAT            0.46       5.49    0.08
#ASS_TCH            0.07       6.35    0.01
#ASS_CUR           -2.73       9.68   -0.28
#ASS_OTH            4.88       5.05    0.97
#STD_CUR            2.99       6.51    0.46
#PRIVATESCL       -23.29       7.33   -3.18
#SC02Q02            0.29       0.08    3.51
#DUM_VILLAGE       -2.88       5.86   -0.49
#SCHSIZE            0.02       0.00    7.73
#RATCMP15           0.48       9.97    0.05
#SCORE_AUTHRITS    11.47       5.73    2.00
#SCHAUTON           6.26       3.10    2.02
#TCHPARTI           3.59       1.70    2.11
#LEADCOM            1.64       3.48    0.47
#LEADINST          -1.74       3.90   -0.45
#LEADPD             1.58       3.13    0.50
#LEADTCH            1.42       3.22    0.44
#STUDCLIM           6.59       2.75    2.40
#TEACCLIM          -1.52       3.18   -0.48
#TCMORALE          -3.45       2.47   -1.40
#R-squared         46.29       2.30   20.15



