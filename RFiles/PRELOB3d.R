# PRELOB3d.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Revised on October 1,2015

library(foreign) # to import and export data from R
library(epicalc) # for use in descriptives
library(stargazer) # For latex summary tables
library(sm) # for locally smoothed regressions
library(lme4) # To run mixed models
library(xtable)# To generate Latex inputs
library(xlsx)# To generate MS-Excel output
library(HMISC)# What can I say about this Man Friday/factotum
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

# DEVCON8a <- DEVCON8

T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0

DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==1] <- 0
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==2] <- 1
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==3] <- 1


DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==1] <- 1
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==2] <- 0
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==3] <- 0

DEVCON8a$ASS_PROM[DEVCON8a$SC18Q02==1] <- 1
DEVCON8a$ASS_PROM[DEVCON8a$SC18Q02==2] <- 0

DEVCON8a$ASS_SCH[DEVCON8a$SC18Q05==1] <- 1
DEVCON8a$ASS_SCH[DEVCON8a$SC18Q05==2] <- 0

DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==1] <- 1
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==2] <- 0

DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==1] <- 1
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==2] <- 0

DEVCON8a$FEMALE[DEVCON8a$ST04Q01==1] <- 1
DEVCON8a$FEMALE[DEVCON8a$ST04Q01==2] <- 0

DEVCON8a$FEMALE <- factor(DEVCON8a$FEMALE)

# Endowments need to be positive
DEVCON8a$NOREPEAT <- factor(-(DEVCON8a$REPEAT-1))

DEVCON8a$NOLATE[DEVCON8a$ST08Q01==1] <- 10
DEVCON8a$NOLATE[DEVCON8a$ST08Q01==2] <- 8.5
DEVCON8a$NOLATE[DEVCON8a$ST08Q01==3] <- 6.5
DEVCON8a$NOLATE[DEVCON8a$ST08Q01==4] <- 4

DEVCON8a$NOMISS[DEVCON8a$ST09Q01==1] <- 10
DEVCON8a$NOMISS[DEVCON8a$ST09Q01==2] <- 8.5
DEVCON8a$NOMISS[DEVCON8a$ST09Q01==3] <- 6.5
DEVCON8a$NOMISS[DEVCON8a$ST09Q01==4] <- 4

DEVCON8a$NOSKIP[DEVCON8a$ST115Q01==1] <- 10
DEVCON8a$NOSKIP[DEVCON8a$ST115Q01==2] <- 8.5
DEVCON8a$NOSKIP[DEVCON8a$ST115Q01==3] <- 6.5
DEVCON8a$NOSKIP[DEVCON8a$ST115Q01==4] <- 4

DEVCON8a$MSRATIO <- 100/DEVCON8a$SMRATIO

#DEVCON8a$NUMGIRLS <- DEVCON8a$PCGIRLS*DEVCON8a$SCHSIZE


#T1b <- DEVCON8a[, c("VIETNAM","PRESCHOOL","REPEAT",
#                    "ST08Q01","ST09Q01","ST115Q01",
#                    "OUTMATH","OUTREAD","OUTSCIE",
#                    "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
#                   "TCH_INCENTV","TCM_INSPE","COMP_USE", "STU_FEEDB"
#                    )]
#N1 <- NROW(na.omit(T1b)) 
#N1 
# N0-N1 
#DEVCON8z <- DEVCON8a[complete.cases(T1b),]

# Additionally, need to create variables OUTMATH, OUTREAD, OUTSCIE, TIGERMOM, 
# TCH_INCENTV, TCM_INSPE, COMP_USE

PISA_VN <- subset(DEVCON8a,CNT==c("VNM")) 
PISA_AL <- subset(DEVCON8a,CNT==c("ALB")) 
PISA_CO <- subset(DEVCON8a,CNT==c("COL")) 
PISA_ID <- subset(DEVCON8a,CNT==c("IDN")) 
PISA_JO <- subset(DEVCON8a,CNT==c("JOR")) 
PISA_PE <- subset(DEVCON8a,CNT==c("PER")) 
PISA_TH <- subset(DEVCON8a,CNT==c("THA")) 
PISA_TU <- subset(DEVCON8a,CNT==c("TUN")) 

PISA_VNID <- rbind(PISA_ID,PISA_VN)
PISA_VNID$OTHER <- factor(-(PISA_VNID$VIETNAM-1))
PISA_VNID$NOREPEAT <- as.numeric(-(PISA_VNID$REPEAT-1))



T1b <- PISA_VNID[, c("VIETNAM","PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                    "OUTMATH","OUTREAD","OUTSCIE",
                    "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                    "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
                    )]
PISA_VNID2 <- PISA_VNID[complete.cases(T1b),]

####### MATHEMATICS #######

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ PRESCHOOL+REPEAT+ST08Q01+ST09Q01+ST115Q01+
                  OUTMATH+OUTREAD+OUTSCIE+ST57Q04+PARPRESSURE + TIGERMOM+PROPCERT+SC35Q02
                  + TCH_INCENTV+TCM_INSPE+COMP_USE+STU_FEEDB| OTHER,
                   data=PISA_VNID2, R=2,reg.fun=Marek) 
plot(results2,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
                ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
             )

plot(results2,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

####### READING #######

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1READ ~ PRESCHOOL+REPEAT+ST08Q01+ST09Q01+ST115Q01+
                     OUTMATH+OUTREAD+OUTSCIE+ST57Q04+PARPRESSURE + TIGERMOM+PROPCERT+SC35Q02
                   + TCH_INCENTV+TCM_INSPE+COMP_USE+STU_FEEDB| OTHER,
                   data=PISA_VNID2, R=2,reg.fun=Marek) 
plot(results3,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia (reading): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia (reading): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

####### SCIENCE #######

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results4 <- oaxaca(PV1SCIE ~ PRESCHOOL+REPEAT+ST08Q01+ST09Q01+ST115Q01+
                     OUTMATH+OUTREAD+OUTSCIE+ST57Q04+PARPRESSURE + TIGERMOM+PROPCERT+SC35Q02
                   + TCH_INCENTV+TCM_INSPE+COMP_USE+STU_FEEDB| OTHER,
                   data=PISA_VNID2, R=2,reg.fun=Marek) 
plot(results4,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia (science): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results4,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia (science): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

