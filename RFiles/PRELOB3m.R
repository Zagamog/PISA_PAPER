# PRELOB3k.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Prepared by Elisabeth Sedmik on October 14th 2015
# Based on code by Suhas D. Parandekar

# Revised by Suhas on Thursday, November 4, 2015

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

###############################################
############ Albania - MATH  ##################
###############################################

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

WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean) 
XB_ <- apply(PISA_AL[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

blaxA <- read.csv("C:/Users/WB484284/Documents/OUTPUT_A.csv",
                  header=TRUE,sep=",") # no row.names=1 for not variable

blaxB <- read.csv("C:/Users/WB484284/Documents/OUTPUT_B.csv",
                  header=TRUE,sep=",") # no row.names=1 for not variable

# WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE, weight="W_FSTUWT") # not working with weights, apply without (as above with means)
WVar <- function(x) Hmisc::wtd.var(x, na.rm=TRUE)

XAvar_ <- apply(PISA_VN[,spec1], 2, FUN=WVar)
XBvar_ <- apply(PISA_AL[,spec1], 2, FUN=WVar)
XAvar <- append(1,XAvar_)
XBvar <- append(1,XBvar_)
XAvar_T <- t(XAvar)
XBvar_T <- t(XBvar)

SE_coeff_A <- blaxA[(1:6),3] # this is the Standard Error
SE_coeff_B <- blaxB[(1:6),3] # this is the Standard Error

Var_coeff_A <- SE_coeff_A^2
Var_coeff_B <- SE_coeff_B^2

BETA_A <- blaxA[(1:6),2] 
BETA_A_T <- t(BETA_A)
BETA_B <- blaxB[(1:6),2] 
BETA_B_T <- t(BETA_B)

BETA_AminusBETA_B <- BETA_A-BETA_B
BETA_AminusBETA_B_T <- t(BETA_AminusBETA_B)

####### A as reference (vietnam as refernce)

S_EndowmentsA <- XA_XB_T%*%BETA_A
EndowmentsA <- XA_XB_T*BETA_A

S_CoefficientsA <- XB_T%*%BETA_AminusBETA_B
CoefficientsA <- XB_T*BETA_AminusBETA_B

# Now for those pesky standard errors for the OB constructs on Endowments and Coefficients

# Preliminaries

XAvarVV <- append(0,XAvar_) # to account for the fact that variance term on intercept is 0
XBvarVV <- append(0,XBvar_)

XAVarVVPlusXBVarVV <- diag(XAvarVV+XBvarVV) # Implied in Equation (10) though not clear from notation used
VarbDIAG <- diag(Var_coeff_B)

VBAPlusVBB <- diag(Var_coeff_A+Var_coeff_B)
XBvarVVDIAG <- diag(XBvarVV)

# Implementing Equation (10) of Jann, 2008

Var_EndowmentsA <- (XA_XB_T%*%VarbDIAG%*%XAminusXB) + (BETA_A_T)%*%(XAVarVVPlusXBVarVV)%*%(BETA_A)
SE_endowmentsA <- sqrt(Var_EndowmentsA)

Var_Endowment_A <- crossprod(XAminusXB,VarbDIAG)*XAminusXB + crossprod(BETA_A,XAVarVVPlusXBVarVV)*(BETA_A)
SE_endowmentA <- sqrt(Var_Endowment_A)

# Implementing Equation (11) of Jann, 2008
# I have to give a prefix to name for second term because Var_Coeff_A is already taken for the simple vector

SOBVar_Coeff_A <- XB_T%*%VBAPlusVBB%*%XB + BETA_AminusBETA_B_T%*%XBvarVVDIAG%*%BETA_AminusBETA_B
SOBSE_Coeff_A  <- sqrt(SOBVar_Coeff_A)

OBVar_Coeff_A <- crossprod(XB,VBAPlusVBB)*XB + crossprod(BETA_AminusBETA_B,XBvarVVDIAG)*BETA_AminusBETA_B
OBSE_Coeff_A  <- sqrt(OBVar_Coeff_A)

# All in one place

EndowmentsA 
SE_endowmentA 
CoefficientsA 
OBSE_Coeff_A 

# This is the scalar:
S_EndowmentsA 
SE_endowmentsA
S_CoefficientsA
SOBSE_Coeff_A


######## ggplot: A as reference (Vietnam)

blix1 <- rbind(EndowmentsA[2:6], CoefficientsA[2:6])
blax1 <- t(blix1) 
colnames(blax1) <- c("Endowments","Coefficients")
rownames(blax1) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax1 <- melt(blax1,value.name="toplots")
# flax1$X2 <- NULL
colnames(flax1) <- c("variable","des","col1")

blix2 <- rbind(SE_endowmentA[2:6], OBSE_Coeff_A[2:6])
blax2 <- t(blix2)  
colnames(blax2) <- c("Endowments","Coefficients")
rownames(blax2) <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")
flax2 <- melt(blax2,value.name="toplots")
# flax2$X2 <- NULL 
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
  labs(title = " VIETNAM with Albania - Mathematics: VN as Reference")























##################### FOR LATER #######################

####### B as reference (Albania as reference)

S_EndowmentsB <- XA_XB_T%*%BETA_B
EndowmentsB <- XA_XB_T*BETA_B

EndowmentsB_var <- XBvar_T
S_CoefficientsB <- XA_T%*%BETA_AminusBETA_B 

CoefficientsB <- XA_T*BETA_AminusBETA_B 
# Variance for coefficients:
# CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B)+(BETA_AminusBETA_B)*XAvar_T
# CoefficientsB_var <- (Var_coff_A+Var_coff_B)+XAvar_T
# CoefficientsB_var <- Var_coff_B
CoefficientsB_var <- XA_T*(Var_coff_A+Var_coff_B) # closest!

DELTA_Y <- (XA_T%*%BETA_A)-(XB_T%*%BETA_B)
DELTA_Y  # compare with
S_EndowmentsA+S_CoefficientsA # Perfect match 
S_EndowmentsB+S_CoefficientsB # Perfect match 








