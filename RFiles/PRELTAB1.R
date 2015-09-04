# ALLTAB1a. R

# Prepared by Suhas Monday, August 10, 2015
# and Elisabeth Sedmik

# Creating the stargazer output for the means between Vietnam and DEV7 countries

# Contains variables separated by sections (students, teachers, ped.practice, schools, rotated part 1
# rotated part 2, rotated part 3)

# Admin packages
library(foreign)# To import and export data to and from R (eg. txt files)
library(xlsx)# To generate MS-Excel output
library(epicalc)# For producing descriptives of data
library(tables) # Computes and displays complex tables of summary statistics
library(stargazer)# For latex regression and summary statistics tables

# Modeling packages
library(intsvy)# For PISA (and TIMSS, PIRLS, etc) analysis with Plausible Values (PV) and Balanced Repeated Replication (BRR)
library(TDMR)# For tuned data mining in R - eg. detect column of constants in dataframe
library(gmodels)# For model fitting, contains various R programming tools (eg. PROC FREQ like tables)
library(psych)# For rescaling variables to given mean and sd
library(sm)# for locally smoothed regressions and density estimation
library(lme4)# To run mixed-effects models using Eigen and S4

library(data.table) # to generate tables
library(dplyr) # to perform aggregations easily
library(xtable) # to generate latex table from R primitive
library(reshape2) # For melting and recasting data 

load("DEVCON8a")

# Preparing the newly created variables in DEVCON8a:

# NON - ROTATED PART:

# Student Variables

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
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==1]  <- 5
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==2]  <- 15
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==3]  <- 60
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==4]  <- 150
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==5]  <- 350
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==1] <- 1
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==2] <- 0
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
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

#VOLUMOM
DEVCON8a$VOLUMOM <- DEVCON8a$SC25Q05+DEVCON8a$SC25Q06+DEVCON8a$SC25Q07+DEVCON8a$SC25Q09+DEVCON8a$SC25Q12
DEVCON8a$VOLUMOM[DEVCON8a$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8a$TEACHMOM <- DEVCON8a$SC25Q08

#FUNDMOM
DEVCON8a$FUNDMOM <-  DEVCON8a$SC25Q11

#COUNCILMOM
DEVCON8a$COUNCILMOM <- DEVCON8a$SC25Q10

# Teacher variables 

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
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8a <- merge(DEVCON8a,SC31OUT.rda,by="NEWID")
DEVCON8a$TCH_INCENTV <- rescale(DEVCON8a$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Pedagogical Practices variables 

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

# School variables #

# Now for the schools-related variables

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==2] <- 1
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==1] <- 0

#SC02Q02 - leave as is

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

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

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

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8a$QUAL_RECORD[DEVCON8a$SC39Q03==1] <- 1
DEVCON8a$QUAL_RECORD[DEVCON8a$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# ROTATED PART 1:

# nothing to prepare

# ROTATED PART 2:

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

#ST57 leave as is, ST72Q01 leave as is

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8a$SHRS <- (DEVCON8a$SMINS)/60
DEVCON8a$MHRS <- (DEVCON8a$MMINS)/60
DEVCON8a$LHRS <- (DEVCON8a$LMINS)/60

# ROTATED PART 3:

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

# I generate a data.table from DEVCON8a - note that this is merely a property that can be checked with a class() command
DEVCON8a <- data.table(DEVCON8a)

# Creating the count function
Count <- function(x) base::length(which(complete.cases(x) == TRUE)) 

######################### The STUDENT PART ##########################

### The DEV7 countries:

# We generate an extract from the DEVCON8a set with all variables we used in our regressions
DEV7stu1a <- DEVCON8a[VIETNAM==0, .(FEMALE , AGE, PRESCHOOL ,  REPEAT ,  ST08Q01 ,  ST09Q01 ,  ST115Q01 ,  HISEI ,
                                    MISCED ,  WEALTH ,  CULTPOS ,  HEDRES ,  BOOK_N, MATWKETH, OUTMATH,
                                    OUTREAD, OUTSCIE, ST57Q01, ST57Q02, ST57Q03, ST57Q04, ST57Q05, ST57Q06, INSTMOT, INTMAT,
                                    SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS, SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT,
                                    ATT_CONTROL, EXAPPLM, EXPUREM, FAMCON, FAMCONC, PARPRESSURE,  
                                    TIGERMOM,  VOLUMOM,  TEACHMOM,  FUNDMOM,  COUNCILMOM, BKGR_FAMPROB)]

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
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax1stu<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax1stu,c("V1"),c("Valid N"))

seq <- seq(2,96,by=2) # I will need to use 2,112 for the actual version as there are 47 variables

flax1stu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,
           72,74,76,78,80,82,84,86,88,90,92,94,96),
         c("Variable","Valid N"):=""] # this eliminates the values

flax1stu[, MS:=as.character(MS)]
flax1stu[c(seq),MS:=paste0("(",MS,")")]

### Vietnam: 

# We generate a Vietnam extract from the DEVCON8a set with all variables we used in our regressions

VNstu1a <- DEVCON8a[VIETNAM==1, .(FEMALE ,  AGE, PRESCHOOL ,  REPEAT ,  ST08Q01 ,  ST09Q01 ,  ST115Q01 ,  HISEI ,
                                  MISCED ,  WEALTH ,  CULTPOS ,  HEDRES ,  BOOK_N, MATWKETH, OUTMATH,
                                  OUTREAD, OUTSCIE, ST57Q01, ST57Q02, ST57Q03, ST57Q04, ST57Q05, ST57Q06, INSTMOT, INTMAT,
                                  SUBNORM, MATHEFF, FAILMAT, MATINTFC, MATBEH, PERSEV, OPENPS, SCMAT, ANXMAT, BELONG, ATSCHL, ATTLNACT,
                                  ATT_CONTROL, EXAPPLM, EXPUREM, FAMCON, FAMCONC, PARPRESSURE,  
                                  TIGERMOM,  VOLUMOM,  TEACHMOM,  FUNDMOM,  COUNCILMOM, BKGR_FAMPROB)]

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
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax2stu<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax2stu,c("Variable","V1"),c("Variable1","Valid N"))

flax2stu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,
           72,74,76,78,80,82,84,86,88,90,92,94,96),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2stu[, MS:=as.character(MS)]
flax2stu[c(seq),MS:=paste0("(",MS,")")]

# Combining DEV7 countries and Vietnam:

flaxstu <- cbind(flax1stu,flax2stu)
flaxstu$Variable1 <- NULL
flaxstu

print(xtable(flaxstu),include.rownames = FALSE) # this generates the latex table input

######################### The TEACHER PART ##########################

### The DEV7 countries:

# We generate an extract from the DEVCON8a set with all variables we used in our regressions
DEV7tch1a <- DEVCON8a[VIETNAM==0, .(STRATIO ,  PROPCERT ,  PROPQUAL ,
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

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- DEV7tch1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax1tch<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax1tch,c("V1"),c("Valid N"))

seq <- seq(2,38,by=2) # I will need to use 2,38 for the actual version as there are 19 variables

flax1tch[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38),
         c("Variable","Valid N"):=""] # this eliminates the values

flax1tch[, MS:=as.character(MS)]
flax1tch[c(seq),MS:=paste0("(",MS,")")]

### Vietnam: 

# We generate a Vietnam extract from the DEVCON8a set with all variables we used in our regressions

VNtch1a <- DEVCON8a[VIETNAM==1, .(STRATIO ,  PROPCERT ,  PROPQUAL ,
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

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- VNtch1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax2tch<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax2tch,c("Variable","V1"),c("Variable1","Valid N"))

flax2tch[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2tch[, MS:=as.character(MS)]
flax2tch[c(seq),MS:=paste0("(",MS,")")]

# Combining DEV7 countries and Vietnam:

flaxtch <- cbind(flax1tch,flax2tch)
flaxtch$Variable1 <- NULL

print(xtable(flaxtch),include.rownames = FALSE) # this generates the latex table input

######################### The PEDAGOGICAL PRACTICES PART ##########################

### The DEV7 countries:

# We generate an extract from the DEVCON8a set with all variables we used in our regressions
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

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- DEV7ped1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax1ped<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax1ped,c("V1"),c("Valid N"))

seq <- seq(2,36,by=2) # I will need to use 2,38 for the actual version as there are 18 variables

flax1ped[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36),
         c("Variable","Valid N"):=""] # this eliminates the values

flax1ped[, MS:=as.character(MS)]
flax1ped[c(seq),MS:=paste0("(",MS,")")]

### Vietnam: 

# We generate a Vietnam extract from the DEVCON8a set with all variables we used in our regressions

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

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- VNped1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax2ped<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax2ped,c("Variable","V1"),c("Variable1","Valid N"))

flax2ped[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2ped[, MS:=as.character(MS)]
flax2ped[c(seq),MS:=paste0("(",MS,")")]

# Combining DEV7 countries and Vietnam:

flaxped <- cbind(flax1ped,flax2ped)
flaxped$Variable1 <- NULL

print(xtable(flaxped),include.rownames = FALSE) # this generates the latex table input

######################### The SCHOOLS PART ##########################

### The DEV7 countries:

# We generate an extract from the DEVCON8a set with all variables we used in our regressions
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

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- DEV7scu1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax1scu<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax1scu,c("V1"),c("Valid N"))

seq <- seq(2,76,by=2) # I will need to use 2,38 for the actual version as there are 36 variables

flax1scu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,
           64,66,68,70,72,74,76),
         c("Variable","Valid N"):=""] # this eliminates the values

flax1scu[, MS:=as.character(MS)]
flax1scu[c(seq),MS:=paste0("(",MS,")")]
### Vietnam: 

# We generate a Vietnam extract from the DEVCON8a set with all variables we used in our regressions

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

# Now to add the third column of "Count" output and eliminate the extra entries (even numbered in 1st and 3rd column)
s <- VNscu1b3 # just to use easier name

blix <- c(rep(s,each=2)) # generates doubled values
as.matrix(blix) -> blax # I have to convert blix into a matrix so I can cbind it to mt1
flax2scu<- cbind(mt1,blax) # Now I have the format I need with extra elements I have to delete
setnames(flax2scu,c("Variable","V1"),c("Variable1","Valid N"))

flax2scu[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,
           64,66,68,70,72,74,76),
         c("Variable1","Valid N"):=""] # this eliminates the values

flax2scu[, MS:=as.character(MS)]
flax2scu[c(seq),MS:=paste0("(",MS,")")]

# Combining DEV7 countries and Vietnam:

flaxscu <- cbind(flax1scu,flax2scu)
flaxscu$Variable1 <- NULL

print(xtable(flaxscu),include.rownames = FALSE) # this generates the latex table input

















