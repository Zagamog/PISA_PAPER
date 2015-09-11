# PRELFRYR_DUTYMOM
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

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

load("DEVCON8a.RDA")

# How big is our initital sample size? Two ways to check: 
T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0 # display, we find that we have 48483 data points, which is a good sample size to start with



######### MATHEMATICS #############

# Let's get started with the non-rotated student variables:

# How many non-missing values for all non-rotated student variables (excl SC25)?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01", "HISEI", "HISCED", "MISCED", "FISCED",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 34405
N0-N1 # 14078 NAs

# And now we are completing the missing cases, to work on a dataframe of non-missing variabels for these regressors
DEVCON8b <- DEVCON8a[complete.cases(T1b),]

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

# DUTYMOM

DEVCON8b$DUTYMOM <- DEVCON8b$SC25Q02

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

R19b <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                       "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                       "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM","DUTYMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8b,export=FALSE)
R19b # VIETNAM:  106.79 DUTYMOM decreases further

######################### READING ################################

# How many non-missing values for all non-rotated student variables (excl SC25)?
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST09Q01","ST115Q01","MISCED", "HISEI",
                    "WEALTH", "CULTPOS", "HEDRES", "ST28Q01", "SC24Q01", "PCGIRLS")]
N1 <- NROW(na.omit(T1b)) 
N1 # 35345
N0-N1 # 13138 NAs

# And now we are completing the missing cases, to work on a dataframe of non-missing variabels for these regressors
DEVCON8l <- DEVCON8a[complete.cases(T1b),]

# Preparing the student related variables

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8l$FEMALE[DEVCON8l$ST04Q01==1] <- 1
DEVCON8l$FEMALE[DEVCON8l$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8l$PRESCHOOL[DEVCON8l$ST05Q01==1] <- 0
DEVCON8l$PRESCHOOL[DEVCON8l$ST05Q01==2] <- 1
DEVCON8l$PRESCHOOL[DEVCON8l$ST05Q01==3] <- 1

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
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==1]  <- 5
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==2]  <- 15
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==3]  <- 60
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==4]  <- 150
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==5]  <- 350
DEVCON8l$BOOK_N[DEVCON8l$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8l$PARPRESSURE[DEVCON8l$SC24Q01==1] <- 1
DEVCON8l$PARPRESSURE[DEVCON8l$SC24Q01==2] <- 0
DEVCON8l$PARPRESSURE[DEVCON8l$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
DEVCON8l$SC25Q01[is.na(DEVCON8l$SC25Q01)]  <- 0
DEVCON8l$SC25Q02[is.na(DEVCON8l$SC25Q02)]  <- 0
DEVCON8l$SC25Q03[is.na(DEVCON8l$SC25Q03)]  <- 0
DEVCON8l$SC25Q04[is.na(DEVCON8l$SC25Q04)]  <- 0
DEVCON8l$SC25Q05[is.na(DEVCON8l$SC25Q05)]  <- 0
DEVCON8l$SC25Q06[is.na(DEVCON8l$SC25Q06)]  <- 0
DEVCON8l$SC25Q07[is.na(DEVCON8l$SC25Q07)]  <- 0
DEVCON8l$SC25Q08[is.na(DEVCON8l$SC25Q08)]  <- 0
DEVCON8l$SC25Q09[is.na(DEVCON8l$SC25Q09)]  <- 0
DEVCON8l$SC25Q10[is.na(DEVCON8l$SC25Q10)]  <- 0
DEVCON8l$SC25Q11[is.na(DEVCON8l$SC25Q11)]  <- 0
DEVCON8l$SC25Q12[is.na(DEVCON8l$SC25Q12)]  <- 0

#TIGERMOM
DEVCON8l$TIGERMOM  <- DEVCON8l$SC25Q01+DEVCON8l$SC25Q03
DEVCON8l$TIGERMOM[DEVCON8l$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8l$VOLUMOM <- DEVCON8l$SC25Q05+DEVCON8l$SC25Q06+DEVCON8l$SC25Q07+DEVCON8l$SC25Q09+DEVCON8l$SC25Q12
DEVCON8l$VOLUMOM[DEVCON8l$VOLUMOM>100] <- 100 

#TEACHMOM
DEVCON8l$TEACHMOM <- DEVCON8l$SC25Q08

#FUNDMOM
DEVCON8l$FUNDMOM <-  DEVCON8l$SC25Q11

#COUNCILMOM
DEVCON8l$COUNCILMOM <- DEVCON8l$SC25Q10

#DUTYMOM
DEVCON8l$DUTYMOM <- DEVCON8l$SC25Q02

R195 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R195 # FEMALE decreases, PRESCHOOL decreases, REPEAT decreases, ST08Q01 decreases, ST09Q01 increases,
# ST115Q01 decreases, HISEI increases, MISCED increases, WEALTH increases, CULTPOS increases,
# HEDRES increases, BOOK_N decreases, PARPRESSURE decreases, TIGERMOM increases
# VOLUMOM decreases, TEACHMOM increases, FUNDMOM decreases, COUNCILMOM decreases
#VIETNAM: 84.67

R195b <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM","DUTYMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8l,export=FALSE)
R195b # VIETNAM: 84.38 DUTYMOM decreases

######################################### SCIENCE #####################################

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

#DUTYMOM
DEVCON8t$DUTYMOM <- DEVCON8t$SC25Q02

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

R311b <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                        "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", 
                        "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM","DUTYMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8t,export=FALSE)
R311b # VIETNAM: DUTYMOM decreases







