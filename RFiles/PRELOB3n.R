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

# Double check
T1b <- PISA_AL[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_AL1 <- PISA_AL[complete.cases(T1b),]
summary(T1b)

# First set
T1b <- PISA_VNAL[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
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

####### VIETNAM & COLOMBIA #######

PISA_VNCO <- rbind(PISA_CO,PISA_VN)
PISA_VNCO$OTHER <- factor(-(PISA_VNCO$VIETNAM-1))

# First set
T1b <- PISA_VNCO[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNCO1 <- PISA_VNCO[complete.cases(T1b),]

# Second set
T1b <- PISA_VNCO[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNCO2 <- PISA_VNCO[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNCO[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNCO3 <- PISA_VNCO[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNCO1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Colombia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Colombia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNCO2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Colombia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Colombia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNCO3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Colombia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Colombia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

####### VIETNAM & Indonesia #######

PISA_VNID <- rbind(PISA_ID,PISA_VN)
PISA_VNID$OTHER <- factor(-(PISA_VNID$VIETNAM-1))

# First set
T1b <- PISA_VNID[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNID1 <- PISA_VNID[complete.cases(T1b),]

# Second set
T1b <- PISA_VNID[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNID2 <- PISA_VNID[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNID[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNID3 <- PISA_VNID[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNID1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNID2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNID3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Indonesia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)


####### VIETNAM & Jordan #######

PISA_VNJO <- rbind(PISA_JO,PISA_VN)
PISA_VNJO$OTHER <- factor(-(PISA_VNJO$VIETNAM-1))

# First set
T1b <- PISA_VNJO[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNJO1 <- PISA_VNJO[complete.cases(T1b),]

# Second set
T1b <- PISA_VNJO[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNJO2 <- PISA_VNJO[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNJO[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNJO3 <- PISA_VNJO[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNJO1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Jordan: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Jordan: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNJO2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Jordan: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Jordan: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNJO3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Jordan: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Jordan: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

####### VIETNAM & Peru #######

PISA_VNPE <- rbind(PISA_PE,PISA_VN)
PISA_VNPE$OTHER <- factor(-(PISA_VNPE$VIETNAM-1))

# First set
T1b <- PISA_VNPE[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNPE1 <- PISA_VNPE[complete.cases(T1b),]

# Second set
T1b <- PISA_VNPE[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNPE2 <- PISA_VNPE[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNPE[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNPE3 <- PISA_VNPE[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNPE1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Peru: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Peru: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNPE2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Peru: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Peru: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNPE3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Peru: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Peru: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)


####### VIETNAM & Thailand #######

PISA_VNTH <- rbind(PISA_TH,PISA_VN)
PISA_VNTH$OTHER <- factor(-(PISA_VNTH$VIETNAM-1))

# First set
T1b <- PISA_VNTH[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNTH1 <- PISA_VNTH[complete.cases(T1b),]

# Second set
T1b <- PISA_VNTH[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNTH2 <- PISA_VNTH[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNTH[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNTH3 <- PISA_VNTH[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNTH1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Thailand: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Thailand: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNTH2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Thailand: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Thailand: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNTH3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Thailand: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Thailand: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)


####### VIETNAM & Tunisia #######

PISA_VNTU <- rbind(PISA_TU,PISA_VN)
PISA_VNTU$OTHER <- factor(-(PISA_VNTU$VIETNAM-1))

# First set
T1b <- PISA_VNTU[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES","BOOK_N")]
PISA_VNTU1 <- PISA_VNTU[complete.cases(T1b),]

# Second set
T1b <- PISA_VNTU[, c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNTU2 <- PISA_VNTU[complete.cases(T1b),]

# Third set (first & second)
T1b <- PISA_VNTU[, c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM")]
PISA_VNTU3 <- PISA_VNTU[complete.cases(T1b),]

#### First set

Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results1 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N| OTHER,
                   data=PISA_VNTU1, R=30,reg.fun=Marek) 

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Tunisia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results1,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Tunisia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Second set

#Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results2 <- oaxaca(PV1MATH ~ OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNTU2, R=30,reg.fun=Marek) 

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Tunisia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results2,
     variables=c("OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Tunisia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)

#### Third Set (First & second)

# Marek <- function(formula,data,weights) stats::lm(formula=formula,data=data,weights=W_FSTUWT)
results3 <- oaxaca(PV1MATH ~ HISEI+MISCED+WEALTH+CULTPOS+HEDRES+BOOK_N+OUTMATH+PARPRESSURE+TIGERMOM+TEACHMOM| OTHER,
                   data=PISA_VNTU3, R=30,reg.fun=Marek) 

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Tunisia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB")
)

plot(results3,
     variables=c("HISEI","MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N","OUTMATH","PARPRESSURE","TIGERMOM","TEACHMOM"
     ), decomposition="twofold",
     weight=0,title="Vietnam compared to Tunisia: Vietnam as reference",
     component.labels = c("explained"="xA-xB.BetaA", "unexplained"="xB.BetaA-BetaB"),
     type="overall"
)



