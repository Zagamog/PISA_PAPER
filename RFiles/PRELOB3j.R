# PRELOB3j.R # Shanghai as BASE
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Revised on October 5,2015

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

#### Sourcing DEVCON9a (please see R script DEVCON9a.R)

load("DEVCON9a.RDA")

DEVCON9a$SHANGHAI[DEVCON9a$COUNTRY==9] <- 1 # dummy takes value = 1, if the country is Shanghai
DEVCON9a$SHANGHAI[DEVCON9a$COUNTRY!=9] <- 0 # dummy takes value = 0, if the country is not Shanghai

#### Now the subsets ####

PISA_VN <- subset(DEVCON9a,CNT==c("VNM")) 
PISA_AL <- subset(DEVCON9a,CNT==c("ALB")) 
PISA_CO <- subset(DEVCON9a,CNT==c("COL")) 
PISA_ID <- subset(DEVCON9a,CNT==c("IDN")) 
PISA_JO <- subset(DEVCON9a,CNT==c("JOR")) 
PISA_PE <- subset(DEVCON9a,CNT==c("PER")) 
PISA_TH <- subset(DEVCON9a,CNT==c("THA")) 
PISA_TU <- subset(DEVCON9a,CNT==c("TUN")) 
PISA_SH <- subset(DEVCON9a,CNT==c("QCN"))

PISA_VNSH <- rbind(PISA_SH,PISA_VN)
# Making Shanghai the base:
PISA_VNSH$OTHER <- factor(-(PISA_VNSH$SHANGHAI-1))
PISA_VNSH$NOREPEAT <- as.numeric(-(PISA_VNSH$REPEAT-1))

T1b <- PISA_VNSH[, c("SHANGHAI","PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                     "OUTMATH","OUTREAD","OUTSCIE",
                     "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                     "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
)]
PISA_VNSH2 <- PISA_VNSH[complete.cases(T1b),]

########### MATHEMATICS, Shanghai as base ##########

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ PRESCHOOL+REPEAT+ST08Q01+ST09Q01+ST115Q01+
                     OUTMATH+OUTREAD+OUTSCIE+ST57Q04+PARPRESSURE + TIGERMOM+PROPCERT+SC35Q02
                   + TCH_INCENTV+TCM_INSPE+COMP_USE+STU_FEEDB| OTHER,
                   data=PISA_VNSH2, R=2,reg.fun=Marek) 

plot(results2,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai: Shanghai as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai: Shanghai as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

########### READING, Shanghai as base ##########

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1READ ~ PRESCHOOL+REPEAT+ST08Q01+ST09Q01+ST115Q01+
                     OUTMATH+OUTREAD+OUTSCIE+ST57Q04+PARPRESSURE + TIGERMOM+PROPCERT+SC35Q02
                   + TCH_INCENTV+TCM_INSPE+COMP_USE+STU_FEEDB| OTHER,
                   data=PISA_VNSH2, R=2,reg.fun=Marek) 

plot(results3,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai (reading): Shanghai as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai (reading): Shanghai as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

########### SCIENCE, Shanghai as base ##########

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results4 <- oaxaca(PV1SCIE ~ PRESCHOOL+REPEAT+ST08Q01+ST09Q01+ST115Q01+
                     OUTMATH+OUTREAD+OUTSCIE+ST57Q04+PARPRESSURE + TIGERMOM+PROPCERT+SC35Q02
                   + TCH_INCENTV+TCM_INSPE+COMP_USE+STU_FEEDB| OTHER,
                   data=PISA_VNSH2, R=2,reg.fun=Marek) 

plot(results4,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai (science): Shanghai as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results4,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai (science): Shanghai as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)