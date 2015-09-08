# PRELOB1a.R
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Oaxaca-Blinder approach

# Prepared by Elisabeth Sedmik on Wednesday, September 1 2015
# Based on code by Suhas D. Parandekar

# Revised on September 2 2015

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

load("DEVCON8.rda")

# PISA_DEV7 <- subset(DEVCON8,CNT==c("ALB","COL","IDN","JOR","PER","THA","TUN"))
# does not work, only gives 6217 observations, this cannot be the case, rather, it should 
# be 48483 - 4959 (Vietnam) = 43524 observations, so lets do the countries individually and 'rbind'

PISA_VN <- subset(DEVCON8,CNT==c("VNM")) # 4959 observations

PISA_AL <- subset(DEVCON8,CNT==c("ALB")) # 4743 observations

PISA_CO <- subset(DEVCON8,CNT==c("COL")) # 9073 observations

PISA_ID <- subset(DEVCON8,CNT==c("IDN")) # 5622 observations

PISA_JO <- subset(DEVCON8,CNT==c("JOR")) # 7038 observations

PISA_PE <- subset(DEVCON8,CNT==c("PER")) # 6035 observations

PISA_TH <- subset(DEVCON8,CNT==c("THA")) # 6606 observations

PISA_TU <- subset(DEVCON8,CNT==c("TUN")) # 4407 observations

PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) # 43524 observations q.e.d.

# The difference to Suhas' files is that
# PISA_VN1 (Suhas) contains 2480 observations
# PISA_CO1 (Suhas file) contains 3383 observations
# That is significantly less for both countries, WHY? Should be 14032 observations in total ...

# Start to create our Oaxaca-Blinder Output. Let's start by trying it out with random variables

# For SCIENCE

R_CS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("ASSESS","COGACT","LMINS","MMINS","SMINS",
                        "BELONG","STUDREL","STUDCLIM","TEACCLIM",
                        "LEADCOM","LEADPD","LEADTCH"), 
                    data=PISA_DEV7,
                    export=FALSE)

R_VS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("ASSESS","COGACT","LMINS","MMINS","SMINS",
                        "BELONG","STUDREL","STUDCLIM","TEACCLIM",
                        "LEADCOM","LEADPD","LEADTCH"), 
                    data=PISA_VN,
                    export=FALSE)

DF_BETA_CS <- subset(R_CS, select="Estimate")  # as dataframe

BETA_CS <- R_CS[1:13,1] # as matrix (minus one, (14-1) not to get the R squared in it)
BETA_VS <- R_VS[1:13,1]

# Now for the X_ part
spec1 <- c("ASSESS","COGACT","LMINS","MMINS","SMINS",
           "BELONG","STUDREL","STUDCLIM","TEACCLIM",
           "LEADCOM","LEADPD","LEADTCH")

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

# SDIFF_SYS
#[,1]
#[1,] 88.15255
# SDIFF_END
#[,1]
#[1,] 31.23882

#SDIFFJ_SYS
#[,1]
#[1,] -120.8374
#SDIFFJ_END
#[,1]
#[1,] -1.446052

############ Looks ok, lets sense check with a Regression we actually did for the regressions models ...

load("DEVCON8z.rda")

PISA_VN <- subset(DEVCON8z,CNT==c("VNM")) 
PISA_AL <- subset(DEVCON8z,CNT==c("ALB")) 
PISA_CO <- subset(DEVCON8z,CNT==c("COL")) 
PISA_ID <- subset(DEVCON8z,CNT==c("IDN")) 
PISA_JO <- subset(DEVCON8z,CNT==c("JOR")) 
PISA_PE <- subset(DEVCON8z,CNT==c("PER")) 
PISA_TH <- subset(DEVCON8z,CNT==c("THA")) 
PISA_TU <- subset(DEVCON8z,CNT==c("TUN")) 
PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) 

# For SCIENCE

R_CS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"), 
                    data=PISA_DEV7,
                    export=FALSE)

R_VS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"), 
                    data=PISA_VN,
                    export=FALSE)

DF_BETA_CS <- subset(R_CS, select="Estimate")  # as dataframe

BETA_CS <- R_CS[1:34,1] # as matrix (minus one, 35-1 not to get the R squared in it)
BETA_VS <- R_VS[1:34,1]

# Now for the X_ part
spec1 <- c("FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
           "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
           "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
           "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
           "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
           "QUAL_RECORD","SCHSEL","TEACCLIM")

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
# 68.13938
#SDIFF_END
# 49.8033
#SDIFFJ_SYS
# -82.74815
#SDIFFJ_END
# 35.19454

R1 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8z,export=FALSE)
R1 

#               Estimate Std. Error t value
#(Intercept)     351.20      18.71   18.77
#VIETNAM          84.75       6.02   14.08
#FEMALE           -4.53       2.17   -2.09
#PRESCHOOL        22.99       3.88    5.92
#REPEAT          -36.01       4.48   -8.04
#ST08Q01          -8.21       1.94   -4.24
#ST115Q01         -8.60       2.95   -2.91
#BOOK_N            0.05       0.02    2.99
#PARPRESSURE      -0.37       4.07   -0.09
#PCGIRLS          20.90      12.18    1.72
#TCM_PEER         -7.77       6.49   -1.20
#FUNDMOM           0.14       0.06    2.30
#COUNCILMOM       -0.15       0.06   -2.50
#PROPCERT          0.16       6.66    0.02
#TCSHORT           3.41       2.07    1.65
#TCM_STUASS        7.34       8.74    0.84
#ASS_PROM          8.17       7.00    1.17
#ASS_NAT          -1.95       5.79   -0.34
#ASS_SCH          -3.06       8.83   -0.35
#ASS_CUR          -2.63       8.86   -0.30
#STU_FEEDB         6.03       5.34    1.13
#PRIVATESCL       -4.19       6.03   -0.70
#TOWN             -8.50       3.26   -2.60
#CLSIZE            0.89       0.22    4.07
#COMPWEB          18.21       5.89    3.09
#SCMATEDU          4.70       1.77    2.65
#EXC2_PLAY         8.45       3.87    2.18
#EXC6_MATHCOMP     3.16       5.01    0.63
#EXC10_SPORT      -4.90       9.43   -0.52
#EXC11_UNICORN    11.33       5.86    1.94
#SCORE_PUBLIC     11.51       4.67    2.47
#LEADINST          2.29       2.00    1.14
#QUAL_RECORD      -3.98       6.44   -0.62
#SCHSEL            1.80       3.42    0.53
#TEACCLIM         -1.94       2.79   -0.70
#R-squared        45.08       1.99   22.66

# All coefficients added = 119.46
# SDIFF_SYS+SDIFF_END = 117.94
































############################# for later #######################################################

load("DEVCON8.rda")

# Let's prepare some variables

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8$FEMALE[DEVCON8$ST04Q01==1] <- 1
DEVCON8$FEMALE[DEVCON8$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8$PRESCHOOL[DEVCON8$ST05Q01==1] <- 0
DEVCON8$PRESCHOOL[DEVCON8$ST05Q01==2] <- 1
DEVCON8$PRESCHOOL[DEVCON8$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8$BOOK_N[DEVCON8$ST28Q01==1]  <- 5
DEVCON8$BOOK_N[DEVCON8$ST28Q01==2]  <- 15
DEVCON8$BOOK_N[DEVCON8$ST28Q01==3]  <- 60
DEVCON8$BOOK_N[DEVCON8$ST28Q01==4]  <- 150
DEVCON8$BOOK_N[DEVCON8$ST28Q01==5]  <- 350
DEVCON8$BOOK_N[DEVCON8$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8$PARPRESSURE[DEVCON8$SC24Q01==1] <- 1
DEVCON8$PARPRESSURE[DEVCON8$SC24Q01==2] <- 0
DEVCON8$PARPRESSURE[DEVCON8$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8$SC25Q10[is.na(DEVCON8$SC25Q10)]  <- 0
DEVCON8$SC25Q11[is.na(DEVCON8$SC25Q11)]  <- 0
DEVCON8$FUNDMOM <-  DEVCON8$SC25Q11
DEVCON8$COUNCILMOM <- DEVCON8$SC25Q10

# Now for the teacher-related variables

#SC30Q01
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8$TCM_STUASS[DEVCON8$SC30Q01==1] <- 1
DEVCON8$TCM_STUASS[DEVCON8$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8$TCM_PEER[DEVCON8$SC30Q02==1] <- 1
DEVCON8$TCM_PEER[DEVCON8$SC30Q02==2] <- 0

# Now for the pedagogical practices-related variables

#SC18Q01/Q02/Q04/Q07
#________________________________________________________________________________________________________________
DEVCON8$ASS_PROG[DEVCON8$SC18Q01==1] <- 1
DEVCON8$ASS_PROG[DEVCON8$SC18Q01==2] <- 0

DEVCON8$ASS_PROM[DEVCON8$SC18Q02==1] <- 1
DEVCON8$ASS_PROM[DEVCON8$SC18Q02==2] <- 0

DEVCON8$ASS_NAT[DEVCON8$SC18Q04==1] <- 1
DEVCON8$ASS_NAT[DEVCON8$SC18Q04==2] <- 0

DEVCON8$ASS_SCH[DEVCON8$SC18Q05==1] <- 1
DEVCON8$ASS_SCH[DEVCON8$SC18Q05==2] <- 0

DEVCON8$ASS_CUR[DEVCON8$SC18Q07==1] <- 1
DEVCON8$ASS_CUR[DEVCON8$SC18Q07==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8$STU_FEEDB[DEVCON8$SC39Q07==1] <- 1
DEVCON8$STU_FEEDB[DEVCON8$SC39Q07==2] <- 0

# Now for the schools-related variables

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8$PRIVATESCL[DEVCON8$SC01Q01==2] <- 1
DEVCON8$PRIVATESCL[DEVCON8$SC01Q01==1] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8$DUM_SMLTOWN <- ifelse(DEVCON8$SC03Q01==2,1,0)
DEVCON8$DUM_TOWN    <- ifelse(DEVCON8$SC03Q01==3,1,0)
DEVCON8$TOWN <- DEVCON8$DUM_SMLTOWN+DEVCON8$DUM_TOWN
DEVCON8$TOWN[DEVCON8$TOWN>1] <- 1

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8$EXC2_PLAY[DEVCON8$SC16Q02==1] <- 1
DEVCON8$EXC2_PLAY[DEVCON8$SC16Q02==2] <- 0

DEVCON8$EXC6_MATHCOMP[DEVCON8$SC16Q06==1] <- 1
DEVCON8$EXC6_MATHCOMP[DEVCON8$SC16Q06==2] <- 0

DEVCON8$EXC10_SPORT[DEVCON8$SC16Q10==1] <- 1
DEVCON8$EXC10_SPORT[DEVCON8$SC16Q10==2] <- 0

DEVCON8$EXC11_UNICORN[DEVCON8$SC16Q11==1] <- 1
DEVCON8$EXC11_UNICORN[DEVCON8$SC16Q11==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8$SCORE_PUBLIC[DEVCON8$SC19Q01==1] <- 1
DEVCON8$SCORE_PUBLIC[DEVCON8$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8$QUAL_RECORD[DEVCON8$SC39Q03==1] <- 1
DEVCON8$QUAL_RECORD[DEVCON8$SC39Q03==2] <- 0

# The rotated part 2 variables:

#ST55Q03
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8$OUTSCIE[DEVCON8$ST55Q03==1] <- 0
DEVCON8$OUTSCIE[DEVCON8$ST55Q03==2] <- 1
DEVCON8$OUTSCIE[DEVCON8$ST55Q03==3] <- 3
DEVCON8$OUTSCIE[DEVCON8$ST55Q03==4] <- 5
DEVCON8$OUTSCIE[DEVCON8$ST55Q03==5] <- 7

# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8$SHRS <- (DEVCON8$SMINS)/60
DEVCON8$MHRS <- (DEVCON8$MMINS)/60
DEVCON8$LHRS <- (DEVCON8$LMINS)/60

## Lets do it again:

PISA_VN <- subset(DEVCON8,CNT==c("VNM")) 

PISA_AL <- subset(DEVCON8,CNT==c("ALB")) 

PISA_CO <- subset(DEVCON8,CNT==c("COL")) 

PISA_ID <- subset(DEVCON8,CNT==c("IDN")) 

PISA_JO <- subset(DEVCON8,CNT==c("JOR")) 

PISA_PE <- subset(DEVCON8,CNT==c("PER")) 

PISA_TH <- subset(DEVCON8,CNT==c("THA")) 

PISA_TU <- subset(DEVCON8,CNT==c("TUN")) 

PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) 

# For SCIENCE

R_CS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"), 
                    data=PISA_DEV7,
                    export=FALSE)

R_VS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"), 
                    data=PISA_VN,
                    export=FALSE)

DF_BETA_CS <- subset(R_CS, select="Estimate")  # as dataframe

BETA_CS <- R_CS[1:34,1] # as matrix (minus one, 35-1 not to get the R squared in it)
BETA_VS <- R_VS[1:34,1]

# Now for the X_ part
spec1 <- c("FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
           "PCGIRLS","TCM_PEER","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
           "TCM_STUASS","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
           "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
           "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
           "QUAL_RECORD","SCHSEL","TEACCLIM")

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












################################# NOTES for later use ############################################



################################# NOTES / FOR LATER USE ##########################################

# Import PISA data into R and generate DEV7 and Vietnam extracts

student.rda <- read.dta("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/stu.dta")
STU12_DEV7_VN <- subset(student.rda,cnt==c("ALB","COL","IDN","JOR","PER","THA","TUN","VNM"))

school.rda <- read.dta("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/sch.dta")
SCH12_DEV7_VN <- subset(school.rda,cnt==c("ALB","COL","IDN","JOR","PER","THA","TUN","VNM"))

# Extract Vietnam and DEV7 data
SCH12_DEV7_VN <- subset(SCH12_DEV7_VN,cnt== "COL" | cnt== "VNM" | cnt== "ALB" | cnt== "IDN" | cnt== "JOR" | cnt== "PER" | cnt== "THA" | cnt== "TUN")

# In need variable names in upper case for later application of INTSVY package
names(STU12_DEV7_VN) <- toupper(names(STU12_DEV7_VN))
names(SCH12_DEV7_VN) <- toupper(names(SCH12_DEV7_VN))

# Merge school and student datasets 
PISA_DEV7_VN <- merge(STU12_DEV7_VN,SCH12_DEV7_VN,by="SCHOOLID")

# generate DEV7 and VN extracts
PISA_DEV7 <- subset(PISA_DEV7_VN,CNT.x==c("ALB","COL","IDN","JOR","PER","THA","TUN"))
PISA_VN <- subset(PISA_DEV7_VN,CNT.x== "VNM")

save(PISA_DEV7, file="C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/PISA_DEV7.rda")
save(PISA_VN, file="C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/PISA_VN.rda")

# Oaxaca Blinder for SCIENCE

#Science Regression
R_CS <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("ASSESS","COGACT","LMINS","MMINS","SMINS",
                        "BELONG","STUDREL","STUDCLIM","TEACCLIM",
                        "LEADCOM","LEADPD","LEADTCH",
                        "SCMATBUI","SCMATEDU","COMPWEB","RATCMP15",
                        "SCHAUTON","SCHSEL","RESPCUR","RESPRES",
                        "CREACTIV","MACTIV","STRATIO","SMRATIO",
                        "PROPCERT","PROPQUAL",
                        "TCHBEHFA","TCHBEHSO","TCHBEHTD","TEACHSUP",
                        "TCMORALE","TCFOCST",
                        "ATSCHL","ATTLNACT", 
                        "EXAPPLM","EXPUREM","FAMCONC","PCGIRLS",
                        "WEALTH","HOMEPOS","CULTPOS","HEDRES",
                        "ICTRES","OUTHOURS","ANXMAT","SCMAT",
                        "HISCED","HISEI"), 
                    weight="W_FSTUWT",
                    data=PISA_VN,
                    export=FALSE)


