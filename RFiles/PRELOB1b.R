# PRELOB1a.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Prepared by Elisabeth Sedmik on Wednesday, September 1 2015
# Based on code by Suhas D. Parandekar

# Revised on September 25  2015

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

library(ggplot2) # For graphs 
library(reshape2)

library(dplyr)
library(data.table)

DEVCON8a <- DEVCON8

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


T1b <- DEVCON8a[, c("VIETNAM","PRESCHOOL","REPEAT","ST08Q01","ST09Q01",
                    "ST115Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 
# N0-N1 
DEVCON8z <- DEVCON8a[complete.cases(T1b),]



PISA_VN <- subset(DEVCON8z,CNT==c("VNM")) 
PISA_AL <- subset(DEVCON8z,CNT==c("ALB")) 
PISA_CO <- subset(DEVCON8z,CNT==c("COL")) 
PISA_ID <- subset(DEVCON8z,CNT==c("IDN")) 
PISA_JO <- subset(DEVCON8z,CNT==c("JOR")) 
PISA_PE <- subset(DEVCON8z,CNT==c("PER")) 
PISA_TH <- subset(DEVCON8z,CNT==c("THA")) 
PISA_TU <- subset(DEVCON8z,CNT==c("TUN")) 
PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) 


# Pairs
PISA_VNAL <- rbind(PISA_AL,PISA_VN)
PISA_VNCO <- rbind(PISA_CO,PISA_VN)

#################### For Math  ##############

results2 <- oaxaca(PV1MATH ~ VIETNAM + PRESCHOOL + REPEAT
                             + ST08Q01 + ST09Q01 +ST115Q01| VIETNAM,
                             data=PISA_VNCO, R=10)
plot(results2,variables=c("PRESCHOOL", "REPEAT", "ST08Q01"),decomposition="threefold")

blix <- results2$threefold




write.xlsx(blix, file="C:/Country/Vietnam/Data/PISA/MS-Excel/res2.xlsx")
# manipulate manually and reimport
#blax <- read.xlsx("C:/Country/Vietnam/Data/PISA/MS-Excel/res2.xlsx","Sheet1")

# blax <- read.csv("C:/Country/Vietnam/Data/PISA/MS-Excel/res2.csv",
#                 header=TRUE,sep=",", row.names=1, 
#colClasses=c(NA, NA, "NULL",NA, "NULL",NA, "NULL"))

#klax <- read.csv("C:/Country/Vietnam/Data/PISA/MS-Excel/res2.csv",
#                 header=TRUE,sep=",", row.names=1, 
#                 colClasses=c(NA,"NULL",NA, "NULL",NA, "NULL", NA))

blax <- read.csv("C:/Country/Vietnam/Data/PISA/MS-Excel/res2.csv",
                  header=TRUE,sep=",", row.names=1)

glax <- as.matrix(blax)
DT4 <- melt(glax,value.name="toplots")
A <- subset(DT4,  %in% c("endow","coeff","inter"))
A <- subset(DT4, Var2 %in% c("endow", "coeff", "inter"))



# convert to datatable
DT <- tbl_df(blax)
DT <- mutate(DT,endP=(endow+3*(se.endow)), endN=(endow-3*(se.endow)),coeffP=(coeff+3*(se.coeff)), 
                coeffN=(coeff-3*(se.coeff)),interP=(inter+3*(se.inter)), interN=(inter-3*(se.inter))) 
# Now to get a variable to allow easy panel in ggplot2
flax <- read.xlsx("C:/Country/Vietnam/Data/PISA/MS-Excel/res2b.xlsx","Sheet1")
DT_ <- tbl_df(flax)
DT2 <- rbind(DT,DT_)

library(reshape2)
# DT3 <- melt(DT2,variable.name="colnames")
flax<-as.matrix(blax)
DT3 <- melt(flax,variable.name="colnames",value.name="toplots")

library(reshape2)
# DT3 <- melt(DT2,variable.name="colnames")
plax<-as.matrix(klax)
DT3b <- melt(klax,variable.name="colnames",value.name="toplots")

A <- subset(DT4, colnames %in% c("endow", "coeff", "inter"))

ggplot(subset(DT4, Var2 %in% c("endow", "coeff", "inter")),aes(x=Var1, y=toplots,fill=Var1)) +
 geom_bar(stat="identity")  + 
  coord_flip() + facet_wrap(~Var2,nrow=3) +
  scale_x_discrete(limits=c("REPEAT", "PRESCHOOL","ST09Q01",
                            "ST115Q01", "ST08Q01")) +
geom_point(aes(ymin=toplots, ymax=toplots), width=.2,
              position=position_dodge(.9)) +
  geom_errorbar(data=DT4,aes(ymin=toplots-1, ymax=toplots+2), width=.2,
                position=position_dodge(.9))



rbind(DT,panel)                            
                             ge + female + LTHS + some.college + 
                               college + advanced.degree | foreign.born |
                               LTHS + some.college + college + advanced.degree,
                             data = chicago, weights = c(0.2, 0.4), R = 30)



REG_A <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("PRESCHOOL","REPEAT","ST08Q01","ST115Q01",
                    "PARPRESSURE","PROPCERT","ASS_PROM","ASS_SCH","STU_FEEDB",
                        "PCGIRLS","SCMATEDU","SCMATBUI","SCL_EXTR_CL"), 
                    data=PISA_DEV7,
                    export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("PRESCHOOL","REPEAT","ST08Q01","ST115Q01",
                        "PARPRESSURE","PROPCERT","ASS_PROM","ASS_SCH","STU_FEEDB",
                        "SCMATEDU","SCMATBUI","SCL_EXTR_CL","PCGIRLS"), 
                    data=DEVCON8a,
                    export=TRUE, name="OUTPUT_B")

# gots some crazy results on PCGIRLS coeff is 337 for Vietnam and 8.357 for Dev7. 

ggplot(PISA_VN, aes(x=ST08Q01, y=PV1SCIE,color=FEMALE)) + geom_point()+stat_smooth(method=lm) 

DB_BETA

DF_BETA_CS <- subset(R_CS, select="Estimate")  # as dataframe

BETA_CS <- R_CS[1:21,1] # as matrix (minus one, not to get the R squared in it)
BETA_VS <- R_VS[1:21,1]

# Now for the X_ part
spec1 <- c("DISCLIMA","TCH_INCENTV","TCMORALE","TCHPARTI","BELONG",
           "ATTLNACT","EXAPPLM","SCHAUTON","DUM_VILLAGE","EXPUREM",
           "TEACHMOM","SCMAT","COGACT","LEADTCH","FAMCONC","LEADINST","FUNDMOM",
           "LEADCOM","LEADPD","ST57Q04")

X_CSj <- apply(PISA_CO[,spec1], 2, mean, na.rm = TRUE)

X_VSj <- apply(PISA_VN[,spec1], 2, mean, na.rm = TRUE)

X_CS <- append(1,X_CSj)
X_VS <- append(1,X_VSj)

# SPEC 1

SDIFF_SYS1 <- (BETA_VS - BETA_CS)
SDIFF_SYS <- (X_CS)%*%(SDIFF_SYS1)

SDIFF_END1 <- (X_VS - X_CS)
SDIFF_END <- SDIFF_END1%*%BETA_VS

# SPEC 2

SDIFFJ_SYS1 <- (BETA_CS-BETA_VS)
SDIFFJ_SYS <- (X_VS)%*%(SDIFFJ_SYS1)

SDIFFJ_END1 <- (X_VS - X_CS)
SDIFFJ_END <- (SDIFFJ_END1)%*%(BETA_CS)

R_CS
R_VS

SDIFF_SYS
SDIFF_END

SDIFFJ_SYS
SDIFFJ_END

#SDIFF_SYS
#[,1]
#[1,] 100.8055
#SDIFF_END
#[,1]
#[1,] 19.83437

#SDIFFJ_SYS
#[,1]
#[1,] -102.298
#SDIFFJ_END
#[,1]
#[1,] 18.34189

ggplot(PISA_VN, aes(x=PCGIRLS, y=PV1MATH,color=FEMALE)) + geom_point() +
 scale_colour_brewer(palette="Set1")

ggplot(PISA_VN, aes(x=PCGIRLS, y=PV1READ,color=FEMALE)) + geom_point() +
  scale_colour_brewer(palette="Set1")

mean1A <- t(sapply(PISA_VN[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH")], function (x)
  unlist(t.test(x~PISA_VN$FEMALE, paired=FALSE,weight="W_FSTUWT")
         [c("estimate","p.value","statistic")])))
mean1A

mean1B <- t(sapply(PISA_VN[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")], function (x)
  unlist(t.test(x~PISA_VN$FEMALE, paired=FALSE,weight="W_FSTUWT")
         [c("estimate","p.value","statistic")])))
mean1B


mean1A <- t(sapply(DEVCON8a[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH")], function (x)
  unlist(t.test(x~DEVCON8a$FEMALE, paired=FALSE,weight="W_FSTUWT")
         [c("estimate","p.value","statistic")])))
mean1A


mean1B <- t(sapply(DEVCON8a[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")], function (x)
  unlist(t.test(x~DEVCON8a$FEMALE, paired=FALSE,weight="W_FSTUWT")
         [c("estimate","p.value","statistic")])))
mean1B


ggplot(DEVCON8z, aes(x=PCGIRLS, y=PV1READ,color=FEMALE)) + geom_point() +
  facet_grid(VNM.f ~.)+ scale_colour_brewer(palette="Set1")


