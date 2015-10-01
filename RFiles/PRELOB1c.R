# PRELOB1c.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Prepared by Elisabeth Sedmik on Wednesday, September 1 2015
# Based on code by Suhas D. Parandekar

# Revised on September 27  2015

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


# Pairs
PISA_VNAL <- rbind(PISA_AL,PISA_VN)
PISA_VNAL$OTHER <- factor(-(PISA_VNAL$VIETNAM-1))

PISA_VNCO <- rbind(PISA_CO,PISA_VN)
PISA_VNCO$OTHER <- factor(-(PISA_VNCO$VIETNAM-1))


#################### For Math  ##############
# Testing package oaxaca 
Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ VIETNAM + PRESCHOOL + NOREPEAT +
                              NOLATE + NOMISS + NOSKIP | OTHER,
                             data=PISA_VNAL, R=2,reg.fun=Marek) 

# For some strange reason a 1 appeared in a variable name
# NOREPEAT became NOREPEAT1
plot(results2,
     variables=c("PRESCHOOL","NOREPEAT1","NOLATE","NOMISS","NOSKIP"), decomposition="twofold",
     weight=1)

blix <- results2$threefold


write.csv(blix, file="C:/Country/Vietnam/Data/PISA/MS-Excel/res2.csv")
# manipulate manually and reimport

blax <- read.csv("C:/Country/Vietnam/Data/PISA/MS-Excel/res2.csv",
                  header=TRUE,sep=",", row.names=1) # row.names for not variable

glax <- as.matrix(blax)
flax <- melt(glax,value.name="toplots")
slax <- subset(flax, Var2 %in% c("Endowments", "Coefficients", "Interaction"))
tlax <- subset(flax, Var2 %in% c("se.endow", "se.coeff", "se.inter"))
brax_ <- cbind(slax,tlax)
brax <- brax_[,c(1,2,3,6)]


ggplot(brax,aes(x=Var1, y=toplots,fill=Var1)) +
 geom_bar(stat="identity",position = position_dodge(0.9),width=0.75)  + 
  coord_flip() + facet_wrap(~Var2,nrow=3) +
  scale_x_discrete(limits=c("ST08Q01","ST115Q01","ST09Q01","PRESCHOOL","REPEAT")) +
geom_point(aes(ymin=toplots, ymax=toplots), width=.2,
              position=position_dodge(.9)) + 
  geom_errorbar(aes(ymin=toplots-3*toplots.1, ymax=toplots+3*toplots.1), width=.2,
                position=position_dodge(.9)) +
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  guides(fill=FALSE)  +
  ylab(NULL) + xlab(NULL) +
  labs(title = " VIETNAM with ALBANIA - Mathematics") 
       
  
#### ######## ############# ######### ############ ###############
#Now that I have figured out how to get the plots, 
#I want to generate the numbers I need for the plots 

# Test for differences  for discrete variables 

# Preliminary explorations 
# CROSSTABLE
CrossTable(PISA_VNAL$OTHER,PISA_VNAL$NOREPEAT,  prop.r=TRUE, prop.c=FALSE, prop.t=FALSE) 

#MEANDIFF
mean1A <- t(sapply(PISA_VNAL[c("PRESCHOOL","NOREPEAT")], function(x) 
  unlist(t.test(x~PISA_VNAL$OTHER,paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A

PISA_VNAL$VIETNAM <- factor(PISA_VNAL$VIETNAM)
ggplot(PISA_VNAL, aes(x=NOLATE, y=PV1MATH,color=VIETNAM)) + 
  geom_point() # + stat_smooth(method=lm) 
################################################ ###########################


REG_A <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                    weight="W_FSTUWT", data=PISA_VN,
                    export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                    weight="W_FSTUWT", data=PISA_AL,
                    export=TRUE,name="OUTPUT_B")


# LHS 
means <- pisa.mean.pv(pvlabel="MATH",by="VIETNAM", data=PISA_VNAL,weight="W_FSTUWT")

DELTA_Y <- means[2,3] - means[1,3]
DELTA_Y # comes out as 119

# RHS

pisa.mean("PRESCHOOL",data=PISA_VN,weight="W_FSTUWT") # is same as just weighted mean
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

# will get error message if all variables are not numeric and you try mean.
#  1: In mean.default(newX[, i], ...) :
#  argument is not numeric or logical: returning NA

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_AL$NOREPEAT <- as.numeric(-(PISA_AL$REPEAT-1))

PISA_VNAL$NOREPEAT <- as.numeric(-(PISA_VNAL$REPEAT-1))

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean)
XB_ <- apply(PISA_AL[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

# Read the beta values in
blaxA <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/OUTPUT_A.csv",
                  header=TRUE,sep=",") # no row.names=1 for not variable
blaxB <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/OUTPUT_B.csv",
                  header=TRUE,sep=",") # no row.names=1 for not variable
BETA_B <- blaxB[(1:6),2]
BETA_A <- blaxA[(1:6),2]

BETAA_BETAB <- BETA_A-BETA_B


S_Endowments <- XA_XB_T%*%BETA_B
Endowments <- XA_XB*BETA_B  # by component
S_Coefficients <- XB_T%*%BETAA_BETAB
Coefficients <- XB_T*BETAA_BETAB
S_interactions <- XA_XB_T%*%BETAA_BETAB
Interactions <- XA_XB_T*BETAA_BETAB

# Another LHS  
#XA_T.BETA_A minus XB_T.BETA_B 
DELTA_Y_NEW <- (XA_T%*%BETA_A)-(XB_T%*%BETA_B) # compare with
S_Endowments+S_Coefficients+S_interactions # Perfect match 

End_T <- t(Endowments)
Coeff_T <- Coefficients[1,]
Inter_T <- Interactions[1,]
spec1_T <- t(append(1,spec1)) 

blix <- rbind(End_T,Coefficients,Interactions)[,2:6]
blax <- t(blix) # raw material for the bar diagrams, without standard errors. 
colnames(blax) <- c("Endowments","Coefficients", "Interactions")

flax <- melt(blax,value.name="toplots")

ggplot(flax,aes(x=Var1, y=toplots,fill=Var1)) +
  geom_bar(stat="identity",position = position_dodge(0.9),width=0.75)  + 
  coord_flip() + facet_wrap(~Var2,nrow=3) +
  scale_x_discrete(limits=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")) +
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  guides(fill=FALSE)  +
  ylab(NULL) + xlab(NULL) +
  labs(title = " VIETNAM with ALBANIA - Mathematics") 

################################################ ###########################
##### READING ##########################

REG_A <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_VN,
                     export=TRUE,name="OUTPUT_A")

REG_B <- pisa.reg.pv(pvlabel="READ", 
                     x=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP"), 
                     weight="W_FSTUWT", data=PISA_AL,
                     export=TRUE,name="OUTPUT_B")

# LHS 
means <- pisa.mean.pv(pvlabel="READ",by="VIETNAM", data=PISA_VNAL,weight="W_FSTUWT")

DELTA_Y <- means[2,3] - means[1,3]
DELTA_Y # 115.57

# RHS

pisa.mean("PRESCHOOL",data=PISA_VN,weight="W_FSTUWT") # is same as just weighted mean
WMean <- function(x) stats::weighted.mean(x, na.rm=TRUE,weight="W_FSTUWT")

spec1 <- c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")

# will get error message if all variables are not numeric and you try mean.
#  1: In mean.default(newX[, i], ...) :
#  argument is not numeric or logical: returning NA

PISA_VN$NOREPEAT <- as.numeric(-(PISA_VN$REPEAT-1))
PISA_AL$NOREPEAT <- as.numeric(-(PISA_AL$REPEAT-1))
PISA_VNAL$NOREPEAT <- as.numeric(-(PISA_VNAL$REPEAT-1))

XA_ <- apply(PISA_VN[,spec1], 2, FUN=WMean)
XB_ <- apply(PISA_AL[,spec1], 2, FUN=WMean)
XA <- append(1,XA_)
XB <- append(1,XB_)
XA_T <- t(XA)
XB_T <- t(XB)

XAminusXB <- XA-XB
XA_XB_T <- t(XAminusXB) # transpose for pre-multiplication

# Read the beta values in
blaxA <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/OUTPUT_A.csv",
                  header=TRUE,sep=",") # no row.names=1 for not variable
blaxB <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/OUTPUT_B.csv",
                  header=TRUE,sep=",") # no row.names=1 for not variable
BETA_B <- blaxB[(1:6),2]
BETA_A <- blaxA[(1:6),2]

BETAA_BETAB <- BETA_A-BETA_B


S_Endowments <- XA_XB_T%*%BETA_B
Endowments <- XA_XB*BETA_B  # by component
S_Coefficients <- XB_T%*%BETAA_BETAB
Coefficients <- XB_T*BETAA_BETAB
S_interactions <- XA_XB_T%*%BETAA_BETAB
Interactions <- XA_XB_T*BETAA_BETAB

# Another LHS  
#XA_T.BETA_A minus XB_T.BETA_B 
DELTA_Y_NEW <- (XA_T%*%BETA_A)-(XB_T%*%BETA_B) # compare with
S_Endowments+S_Coefficients+S_interactions # Perfect match 

End_T <- t(Endowments)
Coeff_T <- Coefficients[1,]
Inter_T <- Interactions[1,]
spec1_T <- t(append(1,spec1)) 

blix <- rbind(End_T,Coefficients,Interactions)[,2:6]
blax <- t(blix) # raw material for the bar diagrams, without standard errors. 

colnames(blax) <- c("Endowments","Coefficients", "Interactions")

flax <- melt(blax,value.name="toplots")

ggplot(flax,aes(x=Var1, y=toplots,fill=Var1)) +
  geom_bar(stat="identity",position = position_dodge(0.9),width=0.75)  + 
  coord_flip() + facet_wrap(~Var2,nrow=3) +
  scale_x_discrete(limits=c("PRESCHOOL","NOREPEAT","NOLATE","NOMISS","NOSKIP")) +
  geom_hline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  guides(fill=FALSE)  +
  ylab(NULL) + xlab(NULL) +
  labs(title = " VIETNAM with ALBANIA - Language") 
  
################################################################


















