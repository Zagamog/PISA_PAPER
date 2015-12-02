#### S2_Endowments.R

#### Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 ####

# Prepared by Suhas D. Parandekar and Elisabeth K. Sedmik (The World Bank Group)
# Accompanying code to research paper
# Date of this version: 12/02/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam.

##################################################################################
# Outline:
# S0_Prelims      Generating data sets (merging, cleaning) 
# S1_Figures      SECTION 1: Introduction (Descriptive statistics, plots, etc)
### S2_Endowments SECTION 2: Endowments tables
# S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
# S4_OaxacaBlin   SECTION 4: Regressions following Oaxaca-Blinder approach
##################################################################################

# We create the Latex output for the Endowments tables in Section 2 (Tables 1-9 and Table A1). We split it into our conceptual
# scheme of Students, Parents, Teachers, Schools. In addition, we create a comparison of means (p value, t-stats) 
# to be used to determine which variable means of Vietnam and Dev7 are statistically different at the 5% level 

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
library(data.table) # For enhanced dataframe faster processing
library(dplyr) # For faster data crunching

load("DEVCON8.rda")

# Since we will now alter most of the initial file (delete missing cases, etc.) we create a new file (DEVON8a)  
# to have the masterfile (DEVCON8) as a back-up:

DEVCON8a <- DEVCON8

### 1. DATA PREPARATION FOR ENDOWMENTS TABLES

# We have to create some new variables.Some of the PISA items were designed to be used in analyses as single items 
# (for example, gender). However, most questionnaire items were designed to be combined in some way in order to measure 
# latent constructs that cannot be observed directly. As you will see, we will be working with these indices to a large extent,
# however we still need to prepare some variables (eg. ST05Q01, ST28Q01, SC25) to be meaningfully analyzed:

# Student and Parents-related Variables

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8a$FEMALE[DEVCON8a$ST04Q01==1] <- 1
DEVCON8a$FEMALE[DEVCON8a$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==1] <- 0
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==2] <- 1
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==3] <- 1

#ST08Q01
#_______________________________________________________________________________________________________________
# we leave as is

#ST09Q01
#_______________________________________________________________________________________________________________
# we leave as is 

# ST115Q01 
#______________________________________________________________________________________________________________
# we leave as is

#ST28Q01
#______________________________________________________________________________________________________________
# We want do not want to work with categories (1-6) but with actual range of number of books, hence we assign 
# each category its average number of books (please see the student coding file p. 95) 
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==1]  <- 5
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==2]  <- 15
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==3]  <- 60
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==4]  <- 150
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==5]  <- 350
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
# This question asked principal to tick if there is constant, little or largely absent pressure from parents on them to 
# set high academic standards AND the expectation of parents to achieve them. We think this gives a good proxy to measure
# how much pressure 'from home' also rests on the students and hence included it in the student variables.
# We create a dummy variable, whether parental achievement pressure is observed amongst many parents or few/nearly none
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==1] <- 1
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==2] <- 0
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
# We intentionally did not delete missing variables from the 'SC25' (Parent Participation) variable. In this specific case,
# we replace N/A's with 0's, principles were asked to indicate the percetnage of parents that fall in each category. 
# This may generate some spurious data, but we still find it safe to replace N/A with 0's, indicating that no parents 
# fall in this category. 
DEVCON8a$SC25Q01[is.na(DEVCON8a$SC25Q01)]  <- 0
DEVCON8a$SC25Q02[is.na(DEVCON8a$SC25Q02)]  <- 0
DEVCON8a$SC25Q03[is.na(DEVCON8a$SC25Q03)]  <- 0
DEVCON8a$SC25Q04[is.na(DEVCON8a$SC25Q04)]  <- 0
DEVCON8a$SC25Q05[is.na(DEVCON8a$SC25Q05)]  <- 0
DEVCON8a$SC25Q06[is.na(DEVCON8a$SC25Q06)]  <- 0
DEVCON8a$SC25Q07[is.na(DEVCON8a$SC25Q07)]  <- 0
DEVCON8a$SC25Q08[is.na(DEVCON8a$SC25Q08)]  <- 0
DEVCON8a$SC25Q09[is.na(DEVCON8a$SC25Q09)]  <- 0
DEVCON8a$SC25Q10[is.na(DEVCON8a$SC25Q10)]  <- 0
DEVCON8a$SC25Q11[is.na(DEVCON8a$SC25Q11)]  <- 0
DEVCON8a$SC25Q12[is.na(DEVCON8a$SC25Q12)]  <- 0

# SC25Q01 is quite rich in information, so we create sub-variables
#TIGERMOM
DEVCON8a$TIGERMOM  <- DEVCON8a$SC25Q01+DEVCON8a$SC25Q03
DEVCON8a$TIGERMOM[DEVCON8a$TIGERMOM>100] <- 100 

# Since asking for students behaviour and students progress can be viewed complementary, we add them up.
# We then account for the fact that not more than 100% of parents can logically be a 'TIGERMOM'. You can do that differently, 
# as long as you are consistent with the other created out of the 'SC25' questions.

#VOLUMOM
DEVCON8a$VOLUMOM <- DEVCON8a$SC25Q05+DEVCON8a$SC25Q06+DEVCON8a$SC25Q07+DEVCON8a$SC25Q09+DEVCON8a$SC25Q12
DEVCON8a$VOLUMOM[DEVCON8a$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8a$TEACHMOM <- DEVCON8a$SC25Q08

#FUNDMOM
DEVCON8a$FUNDMOM <-  DEVCON8a$SC25Q11

#COUNCILMOM
DEVCON8a$COUNCILMOM <- DEVCON8a$SC25Q10

#DUTYMOM
DEVCON8a$DUTYMOM  <- DEVCON8a$SC25Q02+DEVCON8a$SC25Q04
DEVCON8a$DUTYMOM[DEVCON8a$DUTYMOM>100] <- 100 

# Teacher-related variables 

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8a$TCM_STUASS[DEVCON8a$SC30Q01==1] <- 1
DEVCON8a$TCM_STUASS[DEVCON8a$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8a$TCM_PEER[DEVCON8a$SC30Q02==1] <- 1
DEVCON8a$TCM_PEER[DEVCON8a$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8a$TCM_OBSER[DEVCON8a$SC30Q03==1] <- 1
DEVCON8a$TCM_OBSER[DEVCON8a$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8a$TCM_INSPE[DEVCON8a$SC30Q04==1] <- 1
DEVCON8a$TCM_INSPE[DEVCON8a$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8a$TCH_MENT[DEVCON8a$SC39Q08==1] <- 1
DEVCON8a$TCH_MENT[DEVCON8a$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
# We created an OECD style rasch index that measures incentives - High incentives means high value on this WMLE measure
S2_SC31DAT <- DEVCON8a[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(S2_SC31DAT, file="S2_SC31DAT.csv")
# We then generated Winsteps output using Winsteps control+data file SC31a.txt, that we are now reading back into R:
SC31OUT.rda <- read.csv("S2_SC31DATOUT.csv")
DEVCON8a <- merge(DEVCON8a,SC31OUT.rda,by="NEWID")
DEVCON8a$TCH_INCENTV <- rescale(DEVCON8a$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8a$ASS_PROG[DEVCON8a$SC18Q01==1] <- 1
DEVCON8a$ASS_PROG[DEVCON8a$SC18Q01==2] <- 0

DEVCON8a$ASS_PROM[DEVCON8a$SC18Q02==1] <- 1
DEVCON8a$ASS_PROM[DEVCON8a$SC18Q02==2] <- 0

DEVCON8a$ASS_INSTR[DEVCON8a$SC18Q03==1] <- 1
DEVCON8a$ASS_INSTR[DEVCON8a$SC18Q03==2] <- 0

DEVCON8a$ASS_NAT[DEVCON8a$SC18Q04==1] <- 1
DEVCON8a$ASS_NAT[DEVCON8a$SC18Q04==2] <- 0

DEVCON8a$ASS_SCH[DEVCON8a$SC18Q05==1] <- 1
DEVCON8a$ASS_SCH[DEVCON8a$SC18Q05==2] <- 0

DEVCON8a$ASS_TCH[DEVCON8a$SC18Q06==1] <- 1
DEVCON8a$ASS_TCH[DEVCON8a$SC18Q06==2] <- 0

DEVCON8a$ASS_CUR[DEVCON8a$SC18Q07==1] <- 1
DEVCON8a$ASS_CUR[DEVCON8a$SC18Q07==2] <- 0

DEVCON8a$ASS_OTH[DEVCON8a$SC18Q08==1] <- 1
DEVCON8a$ASS_OTH[DEVCON8a$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==1] <- 1
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8a$COMP_USE[DEVCON8a$SC40Q01==1] <- 1
DEVCON8a$COMP_USE[DEVCON8a$SC40Q01==2] <- 0

DEVCON8a$TXT_BOOK[DEVCON8a$SC40Q02==1] <- 1
DEVCON8a$TXT_BOOK[DEVCON8a$SC40Q02==2] <- 0

DEVCON8a$STD_CUR[DEVCON8a$SC40Q03==1] <- 1
DEVCON8a$STD_CUR[DEVCON8a$SC40Q03==2] <- 0

# School variables 

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==2] <- 1
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==1] <- 0

#SC02Q02 - we leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8a$DUM_VILLAGE <- ifelse(DEVCON8a$SC03Q01==1,1,0)
DEVCON8a$DUM_SMLTOWN <- ifelse(DEVCON8a$SC03Q01==2,1,0)
DEVCON8a$DUM_TOWN    <- ifelse(DEVCON8a$SC03Q01==3,1,0)
DEVCON8a$DUM_CITY    <- ifelse(DEVCON8a$SC03Q01==4,1,0)
DEVCON8a$DUM_LRGCITY <- ifelse(DEVCON8a$SC03Q01==5,1,0)

DEVCON8a$TOWN <- DEVCON8a$DUM_SMLTOWN+DEVCON8a$DUM_TOWN
DEVCON8a$TOWN[DEVCON8a$TOWN>1] <- 1
DEVCON8a$CITY <- DEVCON8a$DUM_CITY+DEVCON8a$DUM_LRGCITY
DEVCON8a$CITY[DEVCON8a$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI we leave as is

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8a$EXC1_BAND[DEVCON8a$SC16Q01==1] <- 1
DEVCON8a$EXC1_BAND[DEVCON8a$SC16Q01==2] <- 0

DEVCON8a$EXC2_PLAY[DEVCON8a$SC16Q02==1] <- 1
DEVCON8a$EXC2_PLAY[DEVCON8a$SC16Q02==2] <- 0

DEVCON8a$EXC3_NEWS[DEVCON8a$SC16Q03==1] <- 1
DEVCON8a$EXC3_NEWS[DEVCON8a$SC16Q03==2] <- 0

DEVCON8a$EXC4_VOLU[DEVCON8a$SC16Q04==1] <- 1
DEVCON8a$EXC4_VOLU[DEVCON8a$SC16Q04==2] <- 0

DEVCON8a$EXC5_MCLUB[DEVCON8a$SC16Q05==1] <- 1
DEVCON8a$EXC5_MCLUB[DEVCON8a$SC16Q05==2] <- 0

DEVCON8a$EXC6_MATHCOMP[DEVCON8a$SC16Q06==1] <- 1
DEVCON8a$EXC6_MATHCOMP[DEVCON8a$SC16Q06==2] <- 0

DEVCON8a$EXC7_CHESS[DEVCON8a$SC16Q07==1] <- 1
DEVCON8a$EXC7_CHESS[DEVCON8a$SC16Q07==2] <- 0

DEVCON8a$EXC8_ICTCB[DEVCON8a$SC16Q08==1] <- 1
DEVCON8a$EXC8_ICTCB[DEVCON8a$SC16Q08==2] <- 0

DEVCON8a$EXC9_ARTCB[DEVCON8a$SC16Q09==1] <- 1
DEVCON8a$EXC9_ARTCB[DEVCON8a$SC16Q09==2] <- 0

DEVCON8a$EXC10_SPORT[DEVCON8a$SC16Q10==1] <- 1
DEVCON8a$EXC10_SPORT[DEVCON8a$SC16Q10==2] <- 0

DEVCON8a$EXC11_UNICORN[DEVCON8a$SC16Q11==1] <- 1
DEVCON8a$EXC11_UNICORN[DEVCON8a$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==1] <- 1
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8a$SCORE_PUBLIC[DEVCON8a$SC19Q01==1] <- 1
DEVCON8a$SCORE_PUBLIC[DEVCON8a$SC19Q01==2] <- 0

DEVCON8a$SCORE_AUTHRITS[DEVCON8a$SC19Q02==1] <- 1
DEVCON8a$SCORE_AUTHRITS[DEVCON8a$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" we leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8a$QUAL_RECORD[DEVCON8a$SC39Q03==1] <- 1
DEVCON8a$QUAL_RECORD[DEVCON8a$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" we leave as is

# Additional variables from the rotated student questionnaire (please see research paper for detailed explanations)

#ST55Q02
#________________________________________________________________________________________________________
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==1] <- 0
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==2] <- 1
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==3] <- 3
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==4] <- 5
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==5] <- 7

#ST55Q01
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==1] <- 0
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==2] <- 1
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==3] <- 3
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==4] <- 5
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==5] <- 7

#ST55Q03
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==1] <- 0
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==2] <- 1
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==3] <- 3
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==4] <- 5
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==5] <- 7

# ST57 and ST72Q01 we leave as is

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8a$SHRS <- (DEVCON8a$SMINS)/60
DEVCON8a$MHRS <- (DEVCON8a$MMINS)/60
DEVCON8a$LHRS <- (DEVCON8a$LMINS)/60

# ST91Q02
#________________________________________________________________________________________________________
DEVCON8a$ATT_SA <- ifelse(DEVCON8a$ST91Q02==1,1,0)
DEVCON8a$ATT_A <- ifelse(DEVCON8a$ST91Q02==2,1,0)
DEVCON8a$ATT_CONTROL <-DEVCON8a$ATT_SA+DEVCON8a$ATT_A
# DEVCON8a$ATT_CONTROL[DEVCON8a$ATT_CONTROL>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8a$FAMPROB_SA <- ifelse(DEVCON8a$ST91Q03==1,1,0)
DEVCON8a$FAMPROB_A <- ifelse(DEVCON8a$ST91Q03==2,1,0)
DEVCON8a$BKGR_FAMPROB <-DEVCON8a$FAMPROB_SA+DEVCON8a$FAMPROB_A
# DEVCON8a$BKGR_FAMPROB[DEVCON8a$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8a$DIFFTCH_SA <- ifelse(DEVCON8a$ST91Q04==1,1,0)
DEVCON8a$DIFFTCH_A <- ifelse(DEVCON8a$ST91Q04==2,1,0)
DEVCON8a$TCHQUAL_DIFF <- DEVCON8a$DIFFTCH_SA+DEVCON8a$DIFFTCH_A
# DEVCON8a$TCHQUAL_DIFF[DEVCON8a$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

# For the next steps, we need to have DEVCON8a as a data.table (point 2 below), however to test for the p-values (point 3),
# we need to keep a DEVCON8a version as a data.frame, so we create DEVCON8b

DEVCON8b <- DEVCON8a

# To create the endowment tables, we generate a data.table from DEVCON8a - note that this property that can be checked with the class() command
DEVCON8a <- data.table(DEVCON8a)

### 2. CREATING THE ENDOWMENTS TABLES (Section 2: Tables 1 - 9 and Table A1)

# We create the count function
Count <- function(x) base::length(which(complete.cases(x) == TRUE)) 

# Please note that since first writing the R code below, we have split the original four tables (Students, Parents, 
# Teachers, Schools) into various subtables (Tables 1 - 9 and Appendix Table A1)
# As you will see in the research paper and accompanying Latex code, we decided to create, for example 5 subtables 
# that relate to student and parents-related factors, and so on, to make it more comprehensible for the reader. 
# Below original code (for foru tables) can be easily adjusted to contain any sets of variables you see fit.

###############################################################
## The initial Student- and Parents-related table (Tables 1 - 5)
###############################################################

# A. The DEV7 countries (all countries in the DEVCON8 set excluding Vietnam):

# We generate an extract from the DEVCON8a set:
DEV7stu1a <- DEVCON8a[VIETNAM==0, .(FEMALE , AGE, PRESCHOOL ,  REPEAT ,  ST08Q01 ,  ST09Q01 ,  ST115Q01 ,  HISEI ,
                                    MISCED ,  WEALTH ,  CULTPOS ,  HEDRES ,  BOOK_N, MATWKETH, OUTMATH,
                                    OUTREAD, OUTSCIE, ST57Q01, ST57Q02, ST57Q03, ST57Q04, ST57Q05, ST57Q06, INSTMOT, INTMAT,
                                    SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS, SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT,
                                    ATT_CONTROL, EXAPPLM, EXPUREM, FAMCON, FAMCONC, PARPRESSURE,  
                                    TIGERMOM,  VOLUMOM,  TEACHMOM,  FUNDMOM,  COUNCILMOM, DUTYMOM, BKGR_FAMPROB)]

DEV7stu1b1 <- summarise_each(DEV7stu1a, funs(mean(.,na.rm=TRUE)))
DEV7stu1b2 <- summarise_each(DEV7stu1a,funs(sd(.,na.rm=TRUE)))
DEV7stu1b3 <- summarise_each(DEV7stu1a,funs(Count(.)))

DEV7stu1b1
DEV7stu1b2
DEV7stu1b3

t1 <- rbind(round(DEV7stu1b1,4),round(DEV7stu1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- DEV7stu1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # we convert blix into a matrix so we can cbind it to mt1
flax1stu<- cbind(mt1,blax) # Now we have the format we need with extra elements to delete
setnames(flax1stu,c("V1"),c("Valid N"))

seq <- seq(2,100,by=2) # we will need to use 2,100 for the actual version as there are 50 variables

flax1stu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,
           72,74,76,78,80,82,84,86,88,90,92,94,96,98,100),
         c("Variable","Valid N"):=""] # this eliminates the values

flax1stu[, MS:=as.character(MS)]
flax1stu[c(seq),MS:=paste0("(",MS,")")]

# B.Vietnam: 

# We generate a Vietnam extract from the DEVCON8a set:
VNstu1a <- DEVCON8a[VIETNAM==1, .(FEMALE ,  AGE, PRESCHOOL ,  REPEAT ,  ST08Q01 ,  ST09Q01 ,  ST115Q01 ,  HISEI ,
                                  MISCED ,  WEALTH ,  CULTPOS ,  HEDRES ,  BOOK_N, MATWKETH, OUTMATH,
                                  OUTREAD, OUTSCIE, ST57Q01, ST57Q02, ST57Q03, ST57Q04, ST57Q05, ST57Q06, INSTMOT, INTMAT,
                                  SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS, SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT,
                                  ATT_CONTROL, EXAPPLM, EXPUREM, FAMCON, FAMCONC, PARPRESSURE,  
                                  TIGERMOM,  VOLUMOM,  TEACHMOM,  FUNDMOM,  COUNCILMOM, DUTYMOM,BKGR_FAMPROB)]

VNstu1b1 <- summarise_each(VNstu1a, funs(mean(.,na.rm=TRUE)))
VNstu1b2 <- summarise_each(VNstu1a,funs(sd(.,na.rm=TRUE)))
VNstu1b3 <- summarise_each(VNstu1a,funs(Count(.)))

VNstu1b1
VNstu1b2
VNstu1b3

t1 <- rbind(round(VNstu1b1,4),round(VNstu1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- VNstu1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # we have to convert blix into a matrix so we can cbind it to mt1
flax2stu<- cbind(mt1,blax) # Now we have the format we need with extra elements we have to delete
setnames(flax2stu,c("Variable","V1"),c("Variable1","Valid N"))

flax2stu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,
           72,74,76,78,80,82,84,86,88,90,92,94,96,98,100),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2stu[, MS:=as.character(MS)]
flax2stu[c(seq),MS:=paste0("(",MS,")")]

# Combining A. DEV7 countries and B. Vietnam:

flaxstu <- cbind(flax1stu,flax2stu)
flaxstu$Variable1 <- NULL
flaxstu

# And now we create the latex table output
print(xtable(flaxstu),include.rownames = FALSE) 

########################################################
## The initial teacher-related tables (Table 6, Table A1)
########################################################

# A. The DEV7 countries:

DEV7tch1a <- DEVCON8a[VIETNAM==0, .(PROPCERT ,  PROPQUAL ,
                                    SMRATIO , TCSHORT , LHRS, SHRS, MHRS,TCFOCST , TCM_STUASS , TCM_PEER , TCM_OBSER , TCM_INSPE ,
                                    TCH_INCENTV , SC35Q02 , TCH_MENT, MTSUP, STUDREL, TCHQUAL_DIFF)]

DEV7tch1b1 <- summarise_each(DEV7tch1a, funs(mean(.,na.rm=TRUE)))
DEV7tch1b2 <- summarise_each(DEV7tch1a,funs(sd(.,na.rm=TRUE)))
DEV7tch1b3 <- summarise_each(DEV7tch1a,funs(Count(.)))

DEV7tch1b1
DEV7tch1b2
DEV7tch1b3

t1 <- rbind(round(DEV7tch1b1,4),round(DEV7tch1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

s <- DEV7tch1b3
blix <- c(rep(s,each=2)) 
as.matrix(blix) -> blax 
flax1tch<- cbind(mt1,blax) 
setnames(flax1tch,c("V1"),c("Valid N"))

seq <- seq(2,36,by=2) 

flax1tch[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,36),
         c("Variable","Valid N"):=""]
flax1tch[, MS:=as.character(MS)]
flax1tch[c(seq),MS:=paste0("(",MS,")")]

# B. Vietnam: 

VNtch1a <- DEVCON8a[VIETNAM==1, .(PROPCERT ,  PROPQUAL ,
                                  SMRATIO , TCSHORT , LHRS, SHRS, MHRS,TCFOCST , TCM_STUASS , TCM_PEER , TCM_OBSER , TCM_INSPE ,
                                  TCH_INCENTV , SC35Q02 , TCH_MENT, MTSUP, STUDREL, TCHQUAL_DIFF)]

VNtch1b1 <- summarise_each(VNtch1a, funs(mean(.,na.rm=TRUE)))
VNtch1b2 <- summarise_each(VNtch1a,funs(sd(.,na.rm=TRUE)))
VNtch1b3 <- summarise_each(VNtch1a,funs(Count(.)))

VNtch1b1
VNtch1b2
VNtch1b3

t1 <- rbind(round(VNtch1b1,4),round(VNtch1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

s <- VNtch1b3 
blix <- c(rep(s,each=2)) 
as.matrix(blix) -> blax 
flax2tch<- cbind(mt1,blax) 
setnames(flax2tch,c("Variable","V1"),c("Variable1","Valid N"))

flax2tch[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2tch[, MS:=as.character(MS)]
flax2tch[c(seq),MS:=paste0("(",MS,")")]

# Combining A. DEV7 countries and B. Vietnam:

flaxtch <- cbind(flax1tch,flax2tch)
flaxtch$Variable1 <- NULL
print(xtable(flaxtch),include.rownames = FALSE) # this generates the latex table input

########################################################
## The initial teacher-related tables (Table 7, Table A1)
#######################################################

# A.The DEV7 countries:

DEV7ped1a <- DEVCON8a[VIETNAM==0, .(COMP_USE , TXT_BOOK , STD_CUR, TCHBEHTD, TCHBEHSO, ASS_PROG , ASS_PROM ,
                                    ASS_INSTR , ASS_NAT , ASS_SCH , ASS_TCH , ASS_CUR , ASS_OTH , TCHBEHFA, COGACT,
                                    STU_FEEDB , CLSMAN, DISCLIMA)]

DEV7ped1b1 <- summarise_each(DEV7ped1a, funs(mean(.,na.rm=TRUE)))
DEV7ped1b2 <- summarise_each(DEV7ped1a,funs(sd(.,na.rm=TRUE)))
DEV7ped1b3 <- summarise_each(DEV7ped1a,funs(Count(.)))

DEV7ped1b1
DEV7ped1b2
DEV7ped1b3

t1 <- rbind(round(DEV7ped1b1,4),round(DEV7ped1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

s <- DEV7ped1b3 # just to use easier name
blix <- c(rep(s,each=2)) 
as.matrix(blix) -> blax 
flax1ped<- cbind(mt1,blax) 
setnames(flax1ped,c("V1"),c("Valid N"))

seq <- seq(2,36,by=2) 
flax1ped[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36),
         c("Variable","Valid N"):=""] # this eliminates the values

flax1ped[, MS:=as.character(MS)]
flax1ped[c(seq),MS:=paste0("(",MS,")")]

# B. Vietnam: 

VNped1a <- DEVCON8a[VIETNAM==1, .(COMP_USE , TXT_BOOK , STD_CUR, TCHBEHTD, TCHBEHSO, ASS_PROG , ASS_PROM ,
                                  ASS_INSTR , ASS_NAT , ASS_SCH , ASS_TCH , ASS_CUR , ASS_OTH , TCHBEHFA, COGACT,
                                  STU_FEEDB , CLSMAN, DISCLIMA)]

VNped1b1 <- summarise_each(VNped1a, funs(mean(.,na.rm=TRUE)))
VNped1b2 <- summarise_each(VNped1a,funs(sd(.,na.rm=TRUE)))
VNped1b3 <- summarise_each(VNped1a,funs(Count(.)))

VNped1b1
VNped1b2
VNped1b3

t1 <- rbind(round(VNped1b1,4),round(VNped1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

s <- VNped1b3 # just to use easier name
blix <- c(rep(s,each=2)) 
as.matrix(blix) -> blax 
flax2ped<- cbind(mt1,blax) 
setnames(flax2ped,c("Variable","V1"),c("Variable1","Valid N"))

flax2ped[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2ped[, MS:=as.character(MS)]
flax2ped[c(seq),MS:=paste0("(",MS,")")]

# Combining A. DEV7 countries and B. Vietnam:

flaxped <- cbind(flax1ped,flax2ped)
flaxped$Variable1 <- NULL
print(xtable(flaxped),include.rownames = FALSE) # this generates the latex table input

########################################################
## The initial schools-related tables (Table 8-9, Table A1)
#######################################################

# A. The DEV7 countries:

DEV7scu1a <- DEVCON8a[VIETNAM==0, .(PRIVATESCL , SC02Q02 , DUM_VILLAGE , TOWN , CITY ,
                                    CLSIZE , SCHSIZE , PCGIRLS, ST72Q01, SCHSEL, RATCMP15 , COMPWEB , SCMATEDU , SCMATBUI ,
                                    EXC1_BAND , EXC2_PLAY , EXC3_NEWS , EXC4_VOLU , EXC5_MCLUB , EXC6_MATHCOMP ,
                                    EXC7_CHESS , EXC8_ICTCB , EXC9_ARTCB , EXC10_SPORT , EXC11_UNICORN , SCL_EXTR_CL ,
                                    SCORE_PUBLIC , SCORE_AUTHRITS , SCHAUTON , TCHPARTI , LEADCOM , LEADINST , LEADPD ,
                                    LEADTCH , QUAL_RECORD , STUDCLIM , TEACCLIM , TCMORALE)]

DEV7scu1b1 <- summarise_each(DEV7scu1a, funs(mean(.,na.rm=TRUE)))
DEV7scu1b2 <- summarise_each(DEV7scu1a,funs(sd(.,na.rm=TRUE)))
DEV7scu1b3 <- summarise_each(DEV7scu1a,funs(Count(.)))

DEV7scu1b1
DEV7scu1b2
DEV7scu1b3

t1 <- rbind(round(DEV7scu1b1,4),round(DEV7scu1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

s <- DEV7scu1b3 # just to use easier name
blix <- c(rep(s,each=2)) 
as.matrix(blix) -> blax 
flax1scu<- cbind(mt1,blax) 
setnames(flax1scu,c("V1"),c("Valid N"))

seq <- seq(2,76,by=2) 
flax1scu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,
           64,66,68,70,72,74,76),
         c("Variable","Valid N"):=""] 

flax1scu[, MS:=as.character(MS)]
flax1scu[c(seq),MS:=paste0("(",MS,")")]

# B. Vietnam: 

VNscu1a <- DEVCON8a[VIETNAM==1, .(PRIVATESCL , SC02Q02 , DUM_VILLAGE , TOWN , CITY ,
                                  CLSIZE , SCHSIZE , PCGIRLS, ST72Q01, SCHSEL, RATCMP15 , COMPWEB , SCMATEDU , SCMATBUI ,
                                  EXC1_BAND , EXC2_PLAY , EXC3_NEWS , EXC4_VOLU , EXC5_MCLUB , EXC6_MATHCOMP ,
                                  EXC7_CHESS , EXC8_ICTCB , EXC9_ARTCB , EXC10_SPORT , EXC11_UNICORN , SCL_EXTR_CL ,
                                  SCORE_PUBLIC , SCORE_AUTHRITS , SCHAUTON , TCHPARTI , LEADCOM , LEADINST , LEADPD ,
                                  LEADTCH , QUAL_RECORD ,STUDCLIM , TEACCLIM , TCMORALE)]

VNscu1b1 <- summarise_each(VNscu1a, funs(mean(.,na.rm=TRUE)))
VNscu1b2 <- summarise_each(VNscu1a,funs(sd(.,na.rm=TRUE)))
VNscu1b3 <- summarise_each(VNscu1a,funs(Count(.)))

VNscu1b1
VNscu1b2
VNscu1b3

t1 <- rbind(round(VNscu1b1,4),round(VNscu1b2,4))
mt1 <- melt(t1)
mt1
setnames(mt1,c("variable","value"), c("Variable", "MS"))
mt1[2,.(Variable)]

s <- VNscu1b3 # just to use easier name
blix <- c(rep(s,each=2)) 
as.matrix(blix) -> blax 
flax2scu<- cbind(mt1,blax) 
setnames(flax2scu,c("Variable","V1"),c("Variable1","Valid N"))
flax2scu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,
           64,66,68,70,72,74,76),
         c("Variable1","Valid N"):=""] 

flax2scu[, MS:=as.character(MS)]
flax2scu[c(seq),MS:=paste0("(",MS,")")]

# Combining A. DEV7 countries and B. Vietnam:

flaxscu <- cbind(flax1scu,flax2scu)
flaxscu$Variable1 <- NULL
print(xtable(flaxscu),include.rownames = FALSE) 

### 3. TESTING FOR THE P-VALUE/T-STATS OF THE VARIABLES USED IN THE ENDOWMENTS TABLES (Section 2: Tables 1 - 9 and Table A1)

S2_pvalues <- t(sapply(DEVCON8b[c("FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                              "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                              "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM","DUTYMOM",
                              "STRATIO", "PROPCERT", "PROPQUAL",
                              "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                              "TCH_INCENTV","SC35Q02","TCH_MENT",
                              "ASS_PROG","ASS_PROM",
                              "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH","STU_FEEDB",
                              "COMP_USE","TXT_BOOK","STD_CUR",
                              "PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN","CITY",
                              "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI",
                              "EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP",
                              "EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL",
                              "SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD",
                              "LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM","TCMORALE",
                              "MATWKETH","INSTMOT","INTMAT",
                              "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH","PERSEV","OPENPS",
                              "OUTMATH",
                              "OUTREAD",
                              "OUTSCIE","ST57Q01","ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06",
                              "EXAPPLM","EXPUREM","FAMCON","FAMCONC","LHRS","MHRS","SHRS",
                              "BKGR_FAMPROB","SCMAT","ANXMAT",
                              "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                              "TCHBEHSO","TCHBEHFA","COGACT","CLSMAN","DISCLIMA","ST72Q01","AGE")], function (x)
                                unlist(t.test(x~DEVCON8b$VIETNAM, paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
S2_pvalues

write.csv(S2_pvalues, "S2_pvalues.csv")

#### End of S2_Endowments.R