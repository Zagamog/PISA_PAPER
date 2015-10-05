# PRELOB3i.R # Shanghai
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Revised on October 2,2015

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

stu <- read.dta("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/stu.dta")
sch <- read.dta("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/sch.dta")

QCN_T <- filter(stu, cnt == "QCN") # filter by "cnt" and create a new student file just for 'Albania'
QCN_S <- filter(sch, cnt == "QCN") # filter by "cnt" and create a new school file just for 'Albania'
QCN_P <- merge(QCN_T, QCN_S, by = "schoolid") # merge both files into one file just for 'QCNania'
QCN_P$cnt <- QCN_P$cnt.x # we duplicate "cnt.x" as "cnt" (it is only called "cnt.x" since we merged QCN_S and QCN_T, not that in QCN_S and QCN_T it was called "cnt")
QCN_P$cnt.x <- NULL # we delete the column "cnt.x"
QCN_P$subnatio <- QCN_P$subnatio.x # we duplicate "subnatio.x" as "subnatio" (to have the same nomenclature as in the original data!)
QCN_P$subnatio.x <- NULL # we delete the column "subnatio.x"
QCN_P$stratum <- QCN_P$stratum.x # same as above
QCN_P$stratum.x <- NULL # same as above
QCN_P$oecd <- QCN_P$oecd.x # same as above
QCN_P$oecd.x <- NULL # same as above
QCN_P$nc <- QCN_P$nc.x  # same as above
QCN_P$nc.x <- NULL # same as above
QCN_P$COUNTRY <-9
QCN_P$NEWID <- (QCN_P$COUNTRY*10000000)+((as.numeric(QCN_P$schoolid))*10000)+(as.numeric(QCN_P$stidstd))

save(DEVCON8a, file="C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/Devcon8a.rda")

DEVCON9a <- rbind(DEVCON8a,QCN_P)

DEVCON9a$VIETNAM[DEVCON9a$COUNTRY==8] <- 1 # dummy takes value = 1, if the country is Vietnam
DEVCON9a$VIETNAM[DEVCON9a$COUNTRY!=8] <- 0 # dummy takes value = 0, if the country is not Vietnam

# The 'intsvy' package (the most important one for the PISA analysis) requires variable names to be in upper case; 
# you might have noticed how we put all newly created variables already in upper case; no we convert all 

names(DEVCON9a) <- toupper(names(DEVCON9a)) 

T0 <- DEVCON9a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0

DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==1] <- 0
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==2] <- 1
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==3] <- 1

DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==1] <- 1
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==2] <- 0
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==3] <- 0

DEVCON9a$ASS_PROM[DEVCON9a$SC18Q02==1] <- 1
DEVCON9a$ASS_PROM[DEVCON9a$SC18Q02==2] <- 0

DEVCON9a$ASS_SCH[DEVCON9a$SC18Q05==1] <- 1
DEVCON9a$ASS_SCH[DEVCON9a$SC18Q05==2] <- 0

DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==1] <- 1
DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==2] <- 0

DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==1] <- 1
DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==2] <- 0

DEVCON9a$FEMALE[DEVCON9a$ST04Q01==1] <- 1
DEVCON9a$FEMALE[DEVCON9a$ST04Q01==2] <- 0

DEVCON9a$FEMALE <- factor(DEVCON9a$FEMALE)

# Endowments need to be positive
DEVCON9a$NOREPEAT <- factor(-(DEVCON9a$REPEAT-1))

DEVCON9a$NOLATE[DEVCON9a$ST08Q01==1] <- 10
DEVCON9a$NOLATE[DEVCON9a$ST08Q01==2] <- 8.5
DEVCON9a$NOLATE[DEVCON9a$ST08Q01==3] <- 6.5
DEVCON9a$NOLATE[DEVCON9a$ST08Q01==4] <- 4

DEVCON9a$NOMISS[DEVCON9a$ST09Q01==1] <- 10
DEVCON9a$NOMISS[DEVCON9a$ST09Q01==2] <- 8.5
DEVCON9a$NOMISS[DEVCON9a$ST09Q01==3] <- 6.5
DEVCON9a$NOMISS[DEVCON9a$ST09Q01==4] <- 4

DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==1] <- 10
DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==2] <- 8.5
DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==3] <- 6.5
DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==4] <- 4

DEVCON9a$MSRATIO <- 100/DEVCON9a$SMRATIO

#DEVCON9a$NUMGIRLS <- DEVCON9a$PCGIRLS*DEVCON9a$SCHSIZE


#T1b <- DEVCON9a[, c("VIETNAM","PRESCHOOL","REPEAT",
#                    "ST08Q01","ST09Q01","ST115Q01",
#                    "OUTMATH","OUTREAD","OUTSCIE",
#                    "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
#                   "TCH_INCENTV","TCM_INSPE","COMP_USE", "STU_FEEDB"
#                    )]
#N1 <- NROW(na.omit(T1b)) 
#N1 
# N0-N1 
#DEVCON8z <- DEVCON9a[complete.cases(T1b),]

# Additionally, need to create variables OUTMATH, OUTREAD, OUTSCIE, TIGERMOM, 
# TCH_INCENTV, TCM_INSPE, COMP_USE

DEVCON9a$TIGERMOM  <- DEVCON9a$SC25Q01+DEVCON9a$SC25Q03
DEVCON9a$TIGERMOM[DEVCON9a$TIGERMOM>100] <- 100

DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==1] <- 1
DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==2] <- 0

T1b <- DEVCON9a[, c("SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 # 51,315
N0-N1 # 2345
# Data set with non NAs 
DEVCON9a <- DEVCON9a[complete.cases(T1b),]

SC31DAT9OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISA_PAPER/Excel/SC31DAT9OUT.csv")
DEVCON9a <- merge(DEVCON9a,SC31DAT9OUT.rda,by="NEWID")
DEVCON9a$TCH_INCENTV <- rescale(DEVCON9a$WMLE, mean = 0, sd = 1,df=FALSE)

DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==1] <- 1
DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==2] <- 0

DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==1] <- 0
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==2] <- 1
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==3] <- 3
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==4] <- 5
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==5] <- 7

DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==1] <- 0
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==2] <- 1
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==3] <- 3
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==4] <- 5
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==5] <- 7

DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==1] <- 0
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==2] <- 1
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==3] <- 3
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==4] <- 5
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==5] <- 7

#### Also see DEVCON9a.R for separate way of creating DEVCON9a #####

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
PISA_VNSH$OTHER <- factor(-(PISA_VNSH$VIETNAM-1))
PISA_VNSH$NOREPEAT <- as.numeric(-(PISA_VNSH$REPEAT-1))

T1b <- PISA_VNSH[, c("VIETNAM","PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                    "OUTMATH","OUTREAD","OUTSCIE",
                    "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                    "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
                    )]
PISA_VNSH2 <- PISA_VNSH[complete.cases(T1b),]

########### MATHEMATICS ##########

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
     weight=0,title="Vietnam compared to Shanghai: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
             )

plot(results2,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

########### READING ##########

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
     weight=0,title="Vietnam compared to Shanghai (reading): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai (reading): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

########### SCIENCE ##########

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
     weight=0,title="Vietnam compared to Shanghai (science): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results4,
     variables=c("PRESCHOOL","REPEAT", "ST08Q01","ST09Q01","ST115Q01",
                 "OUTMATH","OUTREAD","OUTSCIE",
                 "PARPRESSURE","TIGERMOM","PROPCERT","SC35Q02",
                 "TCH_INCENTV","TCM_INSPE","COMP_USE","STU_FEEDB"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Shanghai (science): Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)