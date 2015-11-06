# PRELOB3n.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Revised on November 6, 2015

## OAXACA package with initital specifications
## All countries for MATH

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

# DEVCON8a <- DEVCON8

T0 <- DEVCON8a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0

# Preliminary specifications

# HISEI, MISCED, WEALTH, CULTPOS, HEDRES already in dataset

# New variables:

# BOOK_N
#ST28Q01
#______________________________________________________________________________________________________________
# We want do not want to work with categories (1-6) but with actual range of number of books, hence we assign 
# each category its average number of books (please see the student coding file p. 95) 
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==1]  <- 5
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==2]  <- 15
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==3]  <- 60
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==4]  <- 150
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==5]  <- 350
DEVCON8a$BOOK_N[DEVCON8a$ST28Q01==6]  <- 500

# OUTMATH (from rotated part 2)
#ST55Q02
#________________________________________________________________________________________________________
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==1] <- 0
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==2] <- 1
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==3] <- 3
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==4] <- 5
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==5] <- 7

# PARPRESSURE
#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==1] <- 1
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==2] <- 0
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==3] <- 0

#TIGERMOM
#SC25Q01 
#________________________________________________________________________________________________________
DEVCON8a$TIGERMOM  <- DEVCON8a$SC25Q01+DEVCON8a$SC25Q03
DEVCON8a$TIGERMOM[DEVCON8a$TIGERMOM>100] <- 100 

#TEACHMOM
#SC25Q08
#________________________________________________________________________________________________________
DEVCON8a$TEACHMOM <- DEVCON8a$SC25Q08

PISA_VN <- subset(DEVCON8a,CNT==c("VNM")) 
PISA_AL <- subset(DEVCON8a,CNT==c("ALB")) 
PISA_CO <- subset(DEVCON8a,CNT==c("COL")) 
PISA_ID <- subset(DEVCON8a,CNT==c("IDN")) 
PISA_JO <- subset(DEVCON8a,CNT==c("JOR")) 
PISA_PE <- subset(DEVCON8a,CNT==c("PER")) 
PISA_TH <- subset(DEVCON8a,CNT==c("THA")) 
PISA_TU <- subset(DEVCON8a,CNT==c("TUN")) 
PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) 

####### VIETNAM & ALBANIA #######

PISA_VNAL <- rbind(PISA_AL,PISA_VN)
PISA_VNAL$OTHER <- factor(-(PISA_VNAL$VIETNAM-1))

# First set
T1b <- PISA_VNAL[, c("MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNAL1 <- PISA_VNAL[complete.cases(T1b),]

# including "HISEI" does not work to be included for Albania since all NA's (gives error message "0 (non-NA) cases)

# Second set
T1b <- PISA_VNAL[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNAL2 <- PISA_VNAL[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNAL[, c("MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNAL3 <- PISA_VNAL[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNAL1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Albania: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Albania: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNAL2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Albania: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Albania: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNAL3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Albania: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Albania: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)


