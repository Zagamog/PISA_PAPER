
# PRELFRYR6a.R

# New structure for Regressions

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

load("DEVCON8a")

T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 

############################## MATHEMATICS 

##################################### MATH - Students
# "PRESCHOOL","REPEAT", "ST08Q01","ST115Q01"

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","REPEAT","ST08Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #
N0-N1 # NA's
DEVCON8i <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R1
write.csv(R1, "Math_stu.csv")

##################################### MATH - Parents
# "PRESCHOOL","REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM"

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #
N0-N1 # NA's

DEVCON8i <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10
DEVCON8i$DUTYMOM <- DEVCON8i$SC25Q02

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R2
write.csv(R2, "Math_stu_par.csv")

##################################### MATH - Teachers
# "PRESCHOOL","REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM"
# "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
# "ASS_SCH","STU_FEEDB"

T1b <- DEVCON8a[, c("VIETNAM","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01",
                    "PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02",
                    "SC31Q01","SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "SC18Q01","SC18Q02","SC18Q05","SC39Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 #
N0-N1 # NA's

DEVCON8i <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10
DEVCON8i$DUTYMOM <- DEVCON8i$SC25Q02

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==1] <- 1
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==1] <- 1
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==1] <- 1
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==2] <- 0

DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==1] <- 1
DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==2] <- 0

DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==1] <- 1
DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==1] <- 1
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==2] <- 0

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM",
                      "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R3
write.csv(R3, "Math_stu_par_teach.csv")

##################################### MATH -  SCHOOLS
# "PRESCHOOL","REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM"
# "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
# "ASS_SCH","STU_FEEDB"
# "PCGIRLS","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL")]
N1 <- NROW(na.omit(T1b)) 
N1 #15660
N0-N1 #32823 NA's

DEVCON8i <- DEVCON8a[complete.cases(T1b),]

# Let's prepare the relevant student variables again:
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10
DEVCON8i$DUTYMOM <- DEVCON8i$SC25Q02

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==1] <- 1
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==1] <- 1
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==1] <- 1
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==2] <- 0

DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==1] <- 1
DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==2] <- 0

DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==1] <- 1
DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==1] <- 1
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==1] <- 1
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==2] <- 0

DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==1] <- 1
DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$DUM_SMLTOWN <- ifelse(DEVCON8i$SC03Q01==2,1,0)
DEVCON8i$DUM_TOWN    <- ifelse(DEVCON8i$SC03Q01==3,1,0)

DEVCON8i$TOWN <- DEVCON8i$DUM_SMLTOWN+DEVCON8i$DUM_TOWN
# DEVCON8i$TOWN[DEVCON8i$TOWN>1] <- 1 do not need to do this since mutually exclusive

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==1] <- 1
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==2] <- 0

DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==1] <- 1
DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==2] <- 0

DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==1] <- 1
DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==2] <- 0

DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==1] <- 1
DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==1] <- 1
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==1] <- 1
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==1] <- 1
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==2] <- 0

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N","PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM",
                      "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","PCGIRLS","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
                      "EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R4
write.csv(R4, "Math_stu_par_teach_school.csv")

############################## READING

######################### READING - STUDENTS
# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #
N0-N1 # NA's
DEVCON8q <- DEVCON8a[complete.cases(T1b),]
#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1

R5 <- pisa.reg.pv(pvlabel="READ", 
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R5
write.csv(R5, "Read_stu.csv")

########################## READING - PARENTS
# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #17506
N0-N1 #30977 NA's
DEVCON8q <- DEVCON8a[complete.cases(T1b),]

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==1]  <- 5
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==2]  <- 15
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==3]  <- 60
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==4]  <- 150
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==5]  <- 350
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==1] <- 1
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==2] <- 0
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8q$SC25Q05[is.na(DEVCON8q$SC25Q05)]  <- 0
DEVCON8q$SC25Q06[is.na(DEVCON8q$SC25Q06)]  <- 0
DEVCON8q$SC25Q07[is.na(DEVCON8q$SC25Q07)]  <- 0
DEVCON8q$SC25Q09[is.na(DEVCON8q$SC25Q09)]  <- 0
DEVCON8q$SC25Q10[is.na(DEVCON8q$SC25Q10)]  <- 0
DEVCON8q$SC25Q11[is.na(DEVCON8q$SC25Q11)]  <- 0
DEVCON8q$SC25Q12[is.na(DEVCON8q$SC25Q12)]  <- 0
DEVCON8q$FUNDMOM <-  DEVCON8q$SC25Q11
DEVCON8q$COUNCILMOM <- DEVCON8q$SC25Q10
DEVCON8q$VOLUMOM <- DEVCON8q$SC25Q05+DEVCON8q$SC25Q06+DEVCON8q$SC25Q07+DEVCON8q$SC25Q09+DEVCON8q$SC25Q12
DEVCON8q$VOLUMOM[DEVCON8q$VOLUMOM>100] <- 100
DEVCON8q$DUTYMOM <-  DEVCON8q$SC25Q02
R6 <- pisa.reg.pv(pvlabel="READ",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R6
write.csv(R6, "Read_stu_par.csv")

############################### READING -  TEACHERS
# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07","SC39Q07")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8q <- DEVCON8a[complete.cases(T1b),]
#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==1]  <- 5
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==2]  <- 15
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==3]  <- 60
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==4]  <- 150
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==5]  <- 350
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==1] <- 1
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==2] <- 0
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8q$SC25Q05[is.na(DEVCON8q$SC25Q05)]  <- 0
DEVCON8q$SC25Q06[is.na(DEVCON8q$SC25Q06)]  <- 0
DEVCON8q$SC25Q07[is.na(DEVCON8q$SC25Q07)]  <- 0
DEVCON8q$SC25Q09[is.na(DEVCON8q$SC25Q09)]  <- 0
DEVCON8q$SC25Q10[is.na(DEVCON8q$SC25Q10)]  <- 0
DEVCON8q$SC25Q11[is.na(DEVCON8q$SC25Q11)]  <- 0
DEVCON8q$SC25Q12[is.na(DEVCON8q$SC25Q12)]  <- 0
DEVCON8q$FUNDMOM <-  DEVCON8q$SC25Q11
DEVCON8q$COUNCILMOM <- DEVCON8q$SC25Q10
DEVCON8q$VOLUMOM <- DEVCON8q$SC25Q05+DEVCON8q$SC25Q06+DEVCON8q$SC25Q07+DEVCON8q$SC25Q09+DEVCON8q$SC25Q12
DEVCON8q$VOLUMOM[DEVCON8q$VOLUMOM>100] <- 100
DEVCON8q$DUTYMOM <-  DEVCON8q$SC25Q02
#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==1] <- 1
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==2] <- 0
#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==1] <- 1
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==2] <- 0
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==1] <- 1
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==2] <- 0
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==1] <- 1
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==2] <- 0
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==1] <- 1
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==2] <- 0
#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==1] <- 1
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==2] <- 0
R7 <- pisa.reg.pv(pvlabel="READ",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL","REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                      "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R7
write.csv(R7, "Read_stu_par_tea.csv")

############################## READING - SCHOOLS
# "FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"
# "PCGIRLS","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCORE_PUBLIC","LEADINST","QUAL_RECORD","SCHSEL","TEACCLIM"
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC18Q01","SC18Q02","SC18Q04","SC18Q07",
                    "SC39Q07","CLSIZE","COMPWEB","SC03Q01","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC19Q01","SC39Q03","LEADINST", "SCHSEL", "TEACCLIM")]
N1 <- NROW(na.omit(T1b))
N1 #17506
N0-N1 #30977 NA's
DEVCON8q <- DEVCON8a[complete.cases(T1b),]
#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==1] <- 1
DEVCON8q$FEMALE[DEVCON8q$ST04Q01==2] <- 0
#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==1] <- 0
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==2] <- 1
DEVCON8q$PRESCHOOL[DEVCON8q$ST05Q01==3] <- 1
#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==1]  <- 5
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==2]  <- 15
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==3]  <- 60
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==4]  <- 150
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==5]  <- 350
DEVCON8q$BOOK_N[DEVCON8q$ST28Q01==6]  <- 500
#SC24Q01
#________________________________________________________________________________________________________________
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==1] <- 1
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==2] <- 0
DEVCON8q$PARPRESSURE[DEVCON8q$SC24Q01==3] <- 0
#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8q$SC25Q05[is.na(DEVCON8q$SC25Q05)]  <- 0
DEVCON8q$SC25Q06[is.na(DEVCON8q$SC25Q06)]  <- 0
DEVCON8q$SC25Q07[is.na(DEVCON8q$SC25Q07)]  <- 0
DEVCON8q$SC25Q09[is.na(DEVCON8q$SC25Q09)]  <- 0
DEVCON8q$SC25Q10[is.na(DEVCON8q$SC25Q10)]  <- 0
DEVCON8q$SC25Q11[is.na(DEVCON8q$SC25Q11)]  <- 0
DEVCON8q$SC25Q12[is.na(DEVCON8q$SC25Q12)]  <- 0
DEVCON8q$FUNDMOM <-  DEVCON8q$SC25Q11
DEVCON8q$COUNCILMOM <- DEVCON8q$SC25Q10
DEVCON8q$VOLUMOM <- DEVCON8q$SC25Q05+DEVCON8q$SC25Q06+DEVCON8q$SC25Q07+DEVCON8q$SC25Q09+DEVCON8q$SC25Q12
DEVCON8q$VOLUMOM[DEVCON8q$VOLUMOM>100] <- 100
DEVCON8q$DUTYMOM <-  DEVCON8q$SC25Q02
#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==1] <- 1
DEVCON8q$TCM_STUASS[DEVCON8q$SC30Q01==2] <- 0
#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==1] <- 1
DEVCON8q$ASS_PROG[DEVCON8q$SC18Q01==2] <- 0
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==1] <- 1
DEVCON8q$ASS_PROM[DEVCON8q$SC18Q02==2] <- 0
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==1] <- 1
DEVCON8q$ASS_NAT[DEVCON8q$SC18Q04==2] <- 0
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==1] <- 1
DEVCON8q$ASS_CUR[DEVCON8q$SC18Q07==2] <- 0
#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==1] <- 1
DEVCON8q$STU_FEEDB[DEVCON8q$SC39Q07==2] <- 0
#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables
DEVCON8q$DUM_SMLTOWN <- ifelse(DEVCON8q$SC03Q01==2,1,0)
DEVCON8q$DUM_TOWN    <- ifelse(DEVCON8q$SC03Q01==3,1,0)
DEVCON8q$TOWN <- DEVCON8q$DUM_SMLTOWN+DEVCON8q$DUM_TOWN
DEVCON8q$TOWN[DEVCON8q$TOWN>1] <- 1
#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8q$EXC2_PLAY[DEVCON8q$SC16Q02==1] <- 1
DEVCON8q$EXC2_PLAY[DEVCON8q$SC16Q02==2] <- 0
DEVCON8q$EXC6_MATHCOMP[DEVCON8q$SC16Q06==1] <- 1
DEVCON8q$EXC6_MATHCOMP[DEVCON8q$SC16Q06==2] <- 0
DEVCON8q$EXC10_SPORT[DEVCON8q$SC16Q10==1] <- 1
DEVCON8q$EXC10_SPORT[DEVCON8q$SC16Q10==2] <- 0
DEVCON8q$EXC11_UNICORN[DEVCON8q$SC16Q11==1] <- 1
DEVCON8q$EXC11_UNICORN[DEVCON8q$SC16Q11==2] <- 0
#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8q$SCORE_PUBLIC[DEVCON8q$SC19Q01==1] <- 1
DEVCON8q$SCORE_PUBLIC[DEVCON8q$SC19Q01==2] <- 0
#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8q$QUAL_RECORD[DEVCON8q$SC39Q03==1] <- 1
DEVCON8q$QUAL_RECORD[DEVCON8q$SC39Q03==2] <- 0
R8 <- pisa.reg.pv(pvlabel="READ",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                      "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                      "PCGIRLS","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                      "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                      "QUAL_RECORD","SCHSEL","TEACCLIM"),
                  weight="W_FSTUWT",
                  data=DEVCON8q,export=FALSE)
R8
write.csv(R8, "Read_stu_par_tea_school.csv")

############################## SCIENCE

######################### SCIENCE - STUDENTS
# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]
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
R9 <- pisa.reg.pv(pvlabel="SCIE",
                  x=c("VIETNAM",
                      "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"),
                  weight="W_FSTUWT",
                  data=DEVCON8y,export=FALSE)
R9
write.csv(R9, "Science_stu.csv")

########################## SCIENCE - PARENTS
# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]
#ST04Q01
#___________________________________________________________________________________________________________
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
DEVCON8y$DUTYMOM <- DEVCON8y$SC25Q02
R10 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "TCM_PEER","FUNDMOM","COUNCILMOM","DUTYMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R10
write.csv(R10, "Science_stu_par.csv")
R10 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "FUNDMOM","COUNCILMOM","DUTYMOM"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R10
write.csv(R10, "Science_stu_par.csv")

########################### SCIENCE - TEACHERS
# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01",
                    "PROPCERT","TCSHORT","SC39Q07","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07",
                    "SC30Q01","SC30Q02")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]
ST04Q01
#___________________________________________________________________________________________________________
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
DEVCON8y$DUTYMOM <- DEVCON8y$SC25Q02
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
R11 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                       "TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R11
write.csv(R11, "Science_stu_par_tea.csv")

################################# SCIENCE - SCHOOLS
# "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"
# "PROPCERT","TCSHORT","TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
# "PCGIRLS","PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCORE_PUBLIC","LEADINST","QUAL_RECORD","SCHSEL","TEACCLIM"

T1b <- DEVCON8a[, c("VIETNAM","ST04Q01","ST05Q01","REPEAT","ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS",
                    "PROPCERT","TCSHORT","SC30Q01","SC30Q02","SC18Q01","SC18Q02","SC18Q04","SC18Q05","SC18Q07","SC39Q07",
                    "SC01Q01","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SC16Q02","SC16Q06","SC16Q10","SC16Q11",
                    "SC19Q01","LEADINST","SC39Q03","SCHSEL","TEACCLIM")]
N1 <- NROW(na.omit(T1b))
N1 #
N0-N1 # NA's
DEVCON8y <- DEVCON8a[complete.cases(T1b),]

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
DEVCON8y$DUTYMOM <- DEVCON8y$SC25Q02
#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==1] <- 1
DEVCON8y$TCM_STUASS[DEVCON8y$SC30Q01==2] <- 0
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==1] <- 1
DEVCON8y$TCM_PEER[DEVCON8y$SC30Q02==2] <- 0
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

R12 <- pisa.reg.pv(pvlabel="SCIE",
                   x=c("VIETNAM",
                       "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "FUNDMOM","COUNCILMOM","DUTYMOM","PROPCERT","TCSHORT",
                       "TCM_STUASS","TCM_PEER","ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                       "PCGIRLS","PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                       "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                       "QUAL_RECORD","SCHSEL","TEACCLIM"),
                   weight="W_FSTUWT",
                   data=DEVCON8y,export=FALSE)
R12
write.csv(R12, "Science_stu_par_tea_school.csv")


