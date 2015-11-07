# PRELOB3n.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Revised on November 6, 2015

## OAXACA package with final specifications
## Shanghai for math

library(foreign) # to import and export data from R
library(epicalc) # for use in descriptives
library(stargazer) # For latex summary tables
library(sm) # for locally smoothed regressions
library(lme4) # To run mixed models
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(Hmisc)# What can I say about this Man Friday/factotum
library(TDMR)# Need this for tuning data mining in R - eg. detect column of constants in dataframe

library(intsvy) # For PISA analysis with PVs and BRRs
library(xlsx)# To generate MS-Excel output
library(psych) # for the rescaling of TCH_INCENTIV variable

library(ggplot2) # For graphs 
library(reshape2)
library(gmodels) # For PROC FREQ like tables

library(dplyr)
library(data.table)
library(oaxaca)
library(gcookbook)

# DEVCON9a <- DEVCON8

T0 <- DEVCON9a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0

# Final specifications:

### The Wealth variables

# EXAPPLM
# EXPUREM

# LHRS, MHRS
DEVCON9a$MHRS <- (DEVCON9a$MMINS)/60
DEVCON9a$LHRS <- (DEVCON9a$LMINS)/60

# HISEI, MISCED, WEALTH, CULTPOS, HEDRES

# BOOK_N
#ST28Q01
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==1]  <- 5
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==2]  <- 15
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==3]  <- 60
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==4]  <- 150
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==5]  <- 350
DEVCON9a$BOOK_N[DEVCON9a$ST28Q01==6]  <- 500

#TXT_BOOK
DEVCON9a$TXT_BOOK[DEVCON9a$SC40Q02==1] <- 1
DEVCON9a$TXT_BOOK[DEVCON9a$SC40Q02==2] <- 0

# CLSIZE
# TCFOCST
# TCMORALE
# TCHPARTI

# TOWN, CITY
DEVCON9a$DUM_SMLTOWN <- ifelse(DEVCON9a$SC03Q01==2,1,0)
DEVCON9a$DUM_TOWN    <- ifelse(DEVCON9a$SC03Q01==3,1,0)
DEVCON9a$DUM_CITY    <- ifelse(DEVCON9a$SC03Q01==4,1,0)
DEVCON9a$DUM_LRGCITY <- ifelse(DEVCON9a$SC03Q01==5,1,0)

DEVCON9a$TOWN <- DEVCON9a$DUM_SMLTOWN+DEVCON9a$DUM_TOWN
DEVCON9a$TOWN[DEVCON9a$TOWN>1] <- 1
DEVCON9a$CITY <- DEVCON9a$DUM_CITY+DEVCON9a$DUM_LRGCITY
DEVCON9a$CITY[DEVCON9a$CITY>1] <- 1

# PRIVATESCL
DEVCON9a$PRIVATESCL[DEVCON9a$SC01Q01==2] <- 1
DEVCON9a$PRIVATESCL[DEVCON9a$SC01Q01==1] <- 0

#SC02Q02
DEVCON9a$STU_FEES <- DEVCON9a$SC02Q02

# EXC1_BAND
DEVCON9a$EXC1_BAND[DEVCON9a$SC16Q01==1] <- 1
DEVCON9a$EXC1_BAND[DEVCON9a$SC16Q01==2] <- 0

# RATCMP15
# SCHAUTON
# TCHPARTI

### The ED Wealth variables

# PRESCHOOL 
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==1] <- 0
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==2] <- 1
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==3] <- 1

# ST08Q01

# NOREPEAT
DEVCON9a$NOREPEAT <- factor(-(DEVCON9a$REPEAT-1))

# SHRS
DEVCON9a$SHRS <- (DEVCON9a$SMINS)/60

# OUTMATH 
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==1] <- 0
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==2] <- 1
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==3] <- 3
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==4] <- 5
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==5] <- 7

# PARPRESSURE
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==1] <- 1
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==2] <- 0
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==3] <- 0

#TIGERMOM
DEVCON9a$TIGERMOM  <- DEVCON9a$SC25Q01+DEVCON9a$SC25Q03
DEVCON9a$TIGERMOM[DEVCON9a$TIGERMOM>100] <- 100 

#TEACHMOM
DEVCON9a$TEACHMOM <- DEVCON9a$SC25Q08

# PROPCERT

# SC35Q02

# TCH_INCENTV
SC31OUTN.rda <- read.csv("C:/Users/WB484284/Desktop/PISA_PAPER/Excel/SC31DAT9OUT2.csv")
DEVCON9a <- merge(DEVCON9a,SC31OUTN.rda,by="NEWID")
DEVCON9a$TCH_INCENTV <- rescale(DEVCON9a$WMLE, mean = 0, sd = 1,df=FALSE)

# TCM_INSPE
DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==1] <- 1
DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==2] <- 0

# TCM_OBSER
DEVCON9a$TCM_OBSER[DEVCON9a$SC30Q03==1] <- 1
DEVCON9a$TCM_OBSER[DEVCON9a$SC30Q03==2] <- 0

# COMP_USE
DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==1] <- 1
DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==2] <- 0

# STU_FEEDB
DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==1] <- 1
DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==2] <- 0

# EXC6_MATHCOMP
DEVCON9a$EXC6_MATHCOMP[DEVCON9a$SC16Q06==1] <- 1
DEVCON9a$EXC6_MATHCOMP[DEVCON9a$SC16Q06==2] <- 0

# SCMATBUI

# VILLAGE 
DEVCON9a$DUM_VILLAGE <- ifelse(DEVCON9a$SC03Q01==1,1,0)

# SCL_EXTR_CL
DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==1] <- 1
DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==2] <- 0

#SCORE_PUBLIC
DEVCON9a$SCORE_PUBLIC[DEVCON9a$SC19Q01==1] <- 1
DEVCON9a$SCORE_PUBLIC[DEVCON9a$SC19Q01==2] <- 0

### Creating the subsets

PISA_VN <- subset(DEVCON9a,CNT==c("VNM")) 
PISA_SH <- subset(DEVCON9a,CNT==c("QCN")) 

###### VIETNAM & SHANGHAI #######

PISA_VNSH <- rbind(PISA_SH,PISA_VN)
PISA_VNSH$OTHER <- factor(-(PISA_VNSH$VIETNAM-1))

# Test for NA's: SHANGHAI
T1b <- PISA_SH[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                   "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                   "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                   "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                   "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                   "TCHPARTI","EXC1_BAND")]
PISA_SH1 <- PISA_SH[complete.cases(T1b),]
summary(T1b)

# Ususal suspects, from the rotated, but apart from that, all good. 

# Third set 
T1b <- PISA_VNSH[, c("PRESCHOOL","ST08Q01","NOREPEAT","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                     "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                     "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                     "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                     "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                     "TCHPARTI","EXC1_BAND")]
PISA_VNSH1 <- PISA_VNSH[complete.cases(T1b),]

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results8 <- oaxaca(PV1MATH ~ PRESCHOOL+ST08Q01+NOREPEAT+SHRS+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM+PROPCERT+
                     SC35Q02+TCH_INCENTV+TCM_INSPE+TCM_OBSER+COMP_USE+STU_FEEDB+EXC6_MATHCOMP+SCMATBUI+
                     DUM_VILLAGE+SCL_EXTR_CL+SCORE_PUBLIC+
                     EXAPPLM+EXPUREM+LHRS+MHRS+HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+TXT_BOOK+
                     CLSIZE+TCFOCST+TCMORALE+TCHPARTI+TOWN+CITY+PRIVATESCL+STU_FEES+RATCMP15+SCHAUTON+
                     TCHPARTI+EXC1_BAND| OTHER,
                   data=PISA_VNSH1, R=30,reg.fun=Marek) 

plot(results8,
     variables=c("PRESCHOOL","ST08Q01","NOREPEAT1","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                 "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                 "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                 "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                 "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                 "TCHPARTI","EXC1_BAND"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results8,
     variables=c("PRESCHOOL","ST08Q01","NOREPEAT1","SHRS","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM","PROPCERT",
                 "SC35Q02","TCH_INCENTV","TCM_INSPE","TCM_OBSER","COMP_USE","STU_FEEDB","EXC6_MATHCOMP","SCMATBUI",
                 "DUM_VILLAGE","SCL_EXTR_CL","SCORE_PUBLIC",
                 "EXAPPLM","EXPUREM","LHRS","MHRS","HISEI","MISCED","WEALTH","CULTPOS","HEDRES","BOOK_N","TXT_BOOK",
                 "CLSIZE","TCFOCST","TCMORALE","TCHPARTI","TOWN","CITY","PRIVATESCL","STU_FEES","RATCMP15","SCHAUTON",
                 "TCHPARTI","EXC1_BAND"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

