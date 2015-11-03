# PRELOB3k.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

### GGPLOT for all other countries (except Albania) ###

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
library(gcookbook)

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

###############################################
############ COLOMBIA - MATH  #################
###############################################

REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_CO,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_CO$NOREPEAT <- as.numeric(-(PISA_CO$REPEAT-1))

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_CO[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# FOR THE SE: 
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_CO[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

# Moving on: 
BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference (vietnam as refernce)
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Colombia - Mathematics: VN as Reference") 

###############################################
############ Colombia - Reading  ###############
###############################################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_CO,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_CO$NOREPEAT <- as.numeric(-(PISA_CO$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_CO[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_CO[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Colombia - Reading: VN as Reference") 

###############################################
############ Colombia - Science  ##############
###############################################

REG_A <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_CO,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_CO$NOREPEAT <- as.numeric(-(PISA_CO$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_CO[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_CO[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:

Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Colombia - Science: VN as Reference") 

###############################################
############ Indonesia - MATH  #################
###############################################

REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_ID,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_ID$NOREPEAT <- as.numeric(-(PISA_ID$REPEAT-1))

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_ID[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# FOR THE SE: 
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_ID[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

# Moving on: 
BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference (vietnam as refernce)
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Indonesia - Mathematics: VN as Reference") 

###############################################
############ Indonesia - Reading  ###############
###############################################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_ID,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_ID$NOREPEAT <- as.numeric(-(PISA_ID$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_ID[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_ID[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Indonesia - Reading: VN as Reference") 

###############################################
############ Indonesia - Science  ##############
###############################################

REG_A <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_ID,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_ID$NOREPEAT <- as.numeric(-(PISA_ID$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_ID[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_ID[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:

Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Indonesia - Science: VN as Reference") 

###############################################
############ Jordan - MATH  #################
###############################################

REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_JO,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_JO$NOREPEAT <- as.numeric(-(PISA_JO$REPEAT-1))

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_JO[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# FOR THE SE: 
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_JO[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

# Moving on: 
BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference (vietnam as refernce)
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Jordan - Mathematics: VN as Reference") 

###############################################
############ Jordan - Reading  ###############
###############################################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_JO,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_JO$NOREPEAT <- as.numeric(-(PISA_JO$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_JO[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_JO[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Jordan - Reading: VN as Reference") 

###############################################
############ Jordan - Science  ##############
###############################################

REG_A <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_JO,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_JO$NOREPEAT <- as.numeric(-(PISA_JO$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_JO[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_JO[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:

Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Jordan - Science: VN as Reference") 

###############################################
############ PERU - MATH  #################
###############################################

REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_PE,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_PE$NOREPEAT <- as.numeric(-(PISA_PE$REPEAT-1))

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_PE[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# FOR THE SE: 
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_PE[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

# Moving on: 
BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference (vietnam as refernce)
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Peru - Mathematics: VN as Reference") 

###############################################
############ PERU - Reading  ###############
###############################################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_PE,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_PE$NOREPEAT <- as.numeric(-(PISA_PE$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_PE[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_PE[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Peru - Reading: VN as Reference") 

###############################################
############ PERU - Science  ##############
###############################################

REG_A <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_PE,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_PE$NOREPEAT <- as.numeric(-(PISA_PE$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_PE[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_PE[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:

Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Peru - Science: VN as Reference") 

###############################################
############ Thailand - MATH  #################
###############################################

REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_TH,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_TH$NOREPEAT <- as.numeric(-(PISA_TH$REPEAT-1))

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_TH[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# FOR THE SE: 
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_TH[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

# Moving on: 
BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference (vietnam as refernce)
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Thailand - Mathematics: VN as Reference") 

###############################################
############ Thailand - Reading  ###############
###############################################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_TH,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_TH$NOREPEAT <- as.numeric(-(PISA_TH$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_TH[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_TH[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Thailand - Reading: VN as Reference") 

###############################################
############ Thailand - Science  ##############
###############################################

REG_A <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_TH,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_TH$NOREPEAT <- as.numeric(-(PISA_TH$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_TH[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_TH[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:

Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Thailand - Science: VN as Reference") 

###############################################
############ Tunisia - MATH  #################
###############################################

REG_A <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_TU,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_TU$NOREPEAT <- as.numeric(-(PISA_TU$REPEAT-1))

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_TU[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# FOR THE SE: 
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_TU[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

# Moving on: 
BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

####### A as reference (vietnam as refernce)
S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Tunisia - Mathematics: VN as Reference") 

###############################################
############ Tunisia - Reading  ###############
###############################################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_TU,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_TU$NOREPEAT <- as.numeric(-(PISA_TU$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_TU[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_TU[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:
Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Tunisia - Reading: VN as Reference") 

###############################################
############ Tunisia - Science  ##############
###############################################

REG_A <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_TU,
                     export=TRUE,name="OUTPUT_B")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_TU$NOREPEAT <- as.numeric(-(PISA_TU$REPEAT-1))

# From file PRELOB1c:
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_TU[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- REG_A
blaxB <- REG_B

# SE of the means:

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)
XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_TU[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

XAvar_T <- sqrt(XAvar_T)
XBvar_T <- sqrt(XBvar_T)

# SE of the regression coefficients:

Var_coff_A <- blaxA[(1:6),2] # this is the Standard Error
Var_coff_B <- blaxB[(1:6),2] # this is the Standard Error

BETA_B <- blaxB[(1:6),1] 
BETA_A <- blaxA[(1:6),1] 
BETA_AminusBETA_B <- BETA_A-BETA_B

# A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A
EndowmentsA_var <- XAvar_T
S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B
CoefficientsA_var <- XB_T*(Var_coff_A+Var_coff_B) 

# B as reference (Albania as reference)
S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B
EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 
CoefficientsB <- XA_T*BETA_AminusBETA_B 
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) 

######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(EndowmentsA_var[2:6], CoefficientsA_var[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
colnames(flax2) <- c("variable1","des1","col2")

flax3 <- cbind(flax1,flax2)
flax3$variable1 <- NULL
flax3$des1 <- NULL

flax3$des <- factor(flax3$des, levels = c("Endowments","Coefficients"))

ggplot(flax3, aes(x=variable, y=col1, fill=variable)) + geom_bar(stat="identity",width=0.75) + coord_flip() +
  geom_errorbar(aes(ymin=col1-col2, ymax=col1+col2), width=.2) +
  geom_point(size=2.5) + 
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ des, ncol=1) +
  guides(fill=FALSE) + ylab(NULL) + xlab(NULL) +
  scale_x_discrete(limits=c("NOSKIP","NOMISS","NOLATE","NOREPEAT","PRESCHOOL")) +
  labs(title = " VIETNAM with Tunisia - Science: VN as Reference") 























