# PRELOB3k.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Prepared by Elisabeth Sedmik on October 14th 2015
# Based on code by Suhas D. Parandekar

# Revised on ...

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


T1b <- DEVCON8a[, c("VIETNAM","PRESCHOOL","NOREPEAT","NOLATE",
                    "NOMISS","NOSKIP")]
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



#################### For Math  ##############
################################################# ###########################


REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_AL,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_AL$NOREPEAT <- as.numeric(-(PISA_AL$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")
#

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_AL[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

# Just to check:
# WMean1 <- function(x) stats::weighted.mean(x, na.rm=TRUE) # same as above without weights
# XA1_ <- apply(PISA_VN[,spec1], 2, FUN=WMean1)
#> XA_
#PRESCHOOL  NOREPEAT    NOLATE    NOMISS    NOSKIP 
#0.9112559 0.9324242 9.7040505 9.8391003 9.8774679 
#> XA1_
#PRESCHOOL  NOREPEAT    NOLATE    NOMISS    NOSKIP 
#0.9112559 0.9324242 9.7040505 9.8391003 9.8774679 
# gives the same

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

# Read the beta values in
# blaxA <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/OUTPUT_A.csv",
#                  header=TRUE,sep=",") # no row.names=1 for not variable

# instead of above:
blaxA <- REG_A

# blaxB <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/OUTPUT_B.csv",
#                  header=TRUE,sep=",") # no row.names=1 for not variable

# instead of above:
blaxB <- REG_B

###### FOR THE VARIANCE: 

# Variance of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_AL[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

# Variance of the regression coefficients:

blaxA$variance <- blaxA$`Std. Error`^2
Var_coff_A <- blaxA[(1:6),4] 
blaxB$variance <- blaxB$`Std. Error`^2
Var_coff_B <- blaxB[(1:6),4] 

####### Moving on: 

# BETA_B <- blaxB[(1:6),2] # is this the correct column? Should be estimates, ie column 1
BETA_B <- blaxB[(1:6),1] 
# BETA_A <- blaxA[(1:6),2] # is this the correct column? Should be estimates, ie column 1
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
# Variance of endowments:
EndowmentsA_var <- XA_XB_T*Var_coff_A*+BETA_A*(XAvar_T+XBvar_T)
# ? or EndowmentsA_var <- Var_coff_A*+(XAvar_T+XBvar_T) ?
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
# Variance for coefficients:
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B)+(BETA_AminusBETA_B)*XBvar_T

####### B as reference
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
# Variance of endowments:
EndowmentsB_var <- XA_XB_T*Var_coff_B*+BETA_B*(XAvar_T+XBvar_T)
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
# Variance for coefficients:
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B)+(BETA_AminusBETA_B)*XAvar_T


DELTA_Y <- (XA_T%*%BETA_A)-(XB_T%*%BETA_B)
DELTA_Y  # compare with
S_EndowmentsA+S_CoefficientsA # Perfect match 
S_EndowmentsB+S_CoefficientsB # Perfect match 


# Data ready for ggplot2
blix <- rbind(EndowmentsA[2:6],EndowmentsA_var[2:6],CoefficientsA[2:6],CoefficientsA_var[2:6])
blax <- t(blix)  
rownames(blax) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
colnames(blax) <- c("Endowments", "Endowments Variance","Coefficients","Coefficients Variance")
flax <- melt(blax,value.name="toplots")

# Var2 "not found" (when trying the ggplot below), maybe to do with the OUTPUT_A, OUTPUT_B 
# manipulation in Excel? Added/ Created Var1 and Var2?

ggplot(flax,aes(x=Var1, y=toplots,fill=Var1)) +
  geom_bar(stat="identity",position = position_dodge(0.9),width=0.75)  + 
  coord_flip() + facet_wrap(~Var2,nrow=3) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  geom_errorbar(aes(ymin=toplots-3*toplots.1, ymax=toplots+3*toplots.1), width=.2,
                position=position_dodge(.9)) +
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  guides(fill=FALSE)  +
  ylab(NULL) + xlab(NULL) +
  labs(title = " VIETNAM with Albania - Mathematics: VN Reference") 


# Data ready for ggplot2
blix <- rbind(EndowmentsB[2:6],EndowmentsB_var[2:6],CoefficientsB[2:6],CoefficientsB_var[2:6])
blax <- t(blix)  
rownames(blax) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
colnames(blax) <- c("Endowments", "Endowments Variance","Coefficients","Coefficients Variance")
flax <- melt(blax,value.name="toplots")

ggplot(flax,aes(x=Var1, y=toplots,fill=Var1)) +
  geom_bar(stat="identity",position = position_dodge(0.9),width=0.75)  + 
  coord_flip() + facet_wrap(~Var2,nrow=3) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  geom_errorbar(aes(ymin=toplots-3*toplots.1, ymax=toplots+3*toplots.1), width=.2,
                position=position_dodge(.9)) +
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  guides(fill=FALSE)  +
  ylab(NULL) + xlab(NULL) +
  labs(title = " VIETNAM with Albania - Mathematics: AL Reference") 










