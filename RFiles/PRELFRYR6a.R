# PRELFRYR6a.R

# New structure for Regressions

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

load("DEVCON8a")

T0 <- DEVCON8a[, c("VIETNAM")] 
N0<- NROW(na.omit(T0))
N0 

###################### MATHEMATICS #######################

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                         "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                         "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                         "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                         "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL")]
N1 <- NROW(na.omit(T1b)) 
N1 #15660
N0-N1 #32823 NA's
DEVCON8i <- DEVCON8a[complete.cases(T1b),]
# Let's prepare the relevant student variables again:

#ST05Q01
#_________________________________________________________________________________________________________
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==1] <- 0
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==2] <- 1
DEVCON8i$PRESCHOOL[DEVCON8i$ST05Q01==3] <- 1

#ST28Q01
#______________________________________________________________________________________________________________
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==1]  <- 5
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==2]  <- 15
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==3]  <- 60
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==4]  <- 150
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==5]  <- 350
DEVCON8i$BOOK_N[DEVCON8i$ST28Q01==6]  <- 500

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==1] <- 1
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==2] <- 0
DEVCON8i$PARPRESSURE[DEVCON8i$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8i$SC25Q10[is.na(DEVCON8i$SC25Q10)]  <- 0
DEVCON8i$SC25Q11[is.na(DEVCON8i$SC25Q11)]  <- 0
DEVCON8i$FUNDMOM <- DEVCON8i$SC25Q11
DEVCON8i$COUNCILMOM <- DEVCON8i$SC25Q10
DEVCON8i$DUTYMOM <- DEVCON8i$SC25Q02

#SC30Q01, SC30Q02
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==1] <- 1
DEVCON8i$TCM_STUASS[DEVCON8i$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==1] <- 1
DEVCON8i$TCM_PEER[DEVCON8i$SC30Q02==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8i <- merge(DEVCON8i,SC31OUT.rda,by="NEWID")
DEVCON8i$TCH_INCENTV <- rescale(DEVCON8i$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==1] <- 1
DEVCON8i$ASS_PROG[DEVCON8i$SC18Q01==2] <- 0

DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==1] <- 1
DEVCON8i$ASS_PROM[DEVCON8i$SC18Q02==2] <- 0

DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==1] <- 1
DEVCON8i$ASS_SCH[DEVCON8i$SC18Q05==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==1] <- 1
DEVCON8i$STU_FEEDB[DEVCON8i$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==1] <- 1
DEVCON8i$COMP_USE[DEVCON8i$SC40Q01==2] <- 0

DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==1] <- 1
DEVCON8i$TXT_BOOK[DEVCON8i$SC40Q02==2] <- 0

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$DUM_SMLTOWN <- ifelse(DEVCON8i$SC03Q01==2,1,0)
DEVCON8i$DUM_TOWN    <- ifelse(DEVCON8i$SC03Q01==3,1,0)

DEVCON8i$TOWN <- DEVCON8i$DUM_SMLTOWN+DEVCON8i$DUM_TOWN
# DEVCON8i$TOWN[DEVCON8i$TOWN>1] <- 1 do not need to do this since mutually exclusive

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==1] <- 1
DEVCON8i$EXC2_PLAY[DEVCON8i$SC16Q02==2] <- 0

DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==1] <- 1
DEVCON8i$EXC6_MATHCOMP[DEVCON8i$SC16Q06==2] <- 0

DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==1] <- 1
DEVCON8i$EXC10_SPORT[DEVCON8i$SC16Q10==2] <- 0

DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==1] <- 1
DEVCON8i$EXC11_UNICORN[DEVCON8i$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==1] <- 1
DEVCON8i$SCL_EXTR_CL[DEVCON8i$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==1] <- 1
DEVCON8i$SCORE_PUBLIC[DEVCON8i$SC19Q01==2] <- 0

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==1] <- 1
DEVCON8i$QUAL_RECORD[DEVCON8i$SC39Q03==2] <- 0

##### STUDENTS
# "PRESCHOOL","REPEAT", "ST08Q01","ST115Q01"

R1 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"),
                   weight="W_FSTUWT",
                   data=DEVCON8i,export=FALSE)
R1
write.csv(R1, "Math_stu.csv")

##### PARENTS
# "BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM"

R2 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM",
                        "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM"),
                    weight="W_FSTUWT",
                    data=DEVCON8i,export=FALSE)
R2
write.csv(R2, "Math_stu_par.csv")

##### TEACHERS
# "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
# "ASS_SCH","STU_FEEDB"

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM",
                      "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R3
write.csv(R3, "Math_stu_par_teach.csv")

##### SCHOOLS
# "PCGIRLS","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE","FUNDMOM", "COUNCILMOM","DUTYMOM",
                      "PROPCERT","SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","PCGIRLS","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
                      "EXC11_UNICORN","SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R4
write.csv(R4, "Math_stu_par_teach_school.csv")



R87 <- pisa.reg.pv(pvlabel="MATH", 
                   x=c("VIETNAM",
                       "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                       "PCGIRLS", "FUNDMOM", "COUNCILMOM","DUTYMOM","PROPCERT","SMRATIO","TCSHORT",
                       "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                       "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                       "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                       "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL"),
                   weight="W_FSTUWT",
                   data=DEVCON8g,export=FALSE)
R87
#Estimate Std. Error t value
#(Intercept)     342.68      20.00   17.13
#VIETNAM          76.20       7.73    9.86
#PRESCHOOL        25.32       3.84    6.60
#REPEAT          -37.40       3.10  -12.05
#ST08Q01          -7.95       1.35   -5.87
#ST115Q01         -5.32       1.88   -2.83
#BOOK_N            0.07       0.01    5.27
#PARPRESSURE      10.38       4.38    2.37
#PCGIRLS          12.59      14.78    0.85
#FUNDMOM           0.17       0.07    2.63
#COUNCILMOM       -0.13       0.06   -2.18
#PROPCERT         17.03       6.79    2.51
#SMRATIO          -0.03       0.01   -2.17
#TCSHORT           2.50       1.83    1.36
#TCFOCST          -2.05       1.89   -1.09
#TCM_STUASS        0.34       8.15    0.04
#TCM_PEER         -4.74       5.41   -0.88
#TCH_INCENTV      -2.82       2.71   -1.04
#ASS_PROG        -21.79       7.85   -2.77
#ASS_PROM         12.66       5.65    2.24
#ASS_SCH           0.65       7.55    0.09
#STU_FEEDB         1.91       4.92    0.39
#COMP_USE         -1.65       5.38   -0.31
#TXT_BOOK         -9.30       7.18   -1.30
#TOWN             -9.14       3.64   -2.51
#CLSIZE            0.83       0.24    3.47
#COMPWEB          15.57       6.23    2.50
#SCMATEDU          5.68       3.03    1.88
#SCMATBUI          3.53       2.49    1.42
#EXC2_PLAY         8.30       3.97    2.09
#EXC6_MATHCOMP    -0.93       5.33   -0.17
#EXC10_SPORT      -5.60       9.19   -0.61
#EXC11_UNICORN     6.66       5.58    1.19
#SCL_EXTR_CL      10.82       5.09    2.13
#SCORE_PUBLIC      9.62       4.89    1.97
#QUAL_RECORD       7.09       6.73    1.05
#SCHSEL            1.31       3.17    0.41
#R-squared        43.70       2.35   18.57

###################### READING #######################

##### STUDENTS
# "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01", 

##### PARENTS
# "BOOK_N", "PARPRESSURE","VOLUMOM","FUNDMOM","COUNCILMOM","DUTYMOM"

##### TEACHERS
# "PROPCERT","TCSHORT","TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB"

##### SCHOOLS
# "PCGIRLS","TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCORE_PUBLIC","LEADINST","QUAL_RECORD","SCHSEL","TEACCLIM"

R251 <- pisa.reg.pv(pvlabel="READ", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","VOLUMOM","FUNDMOM","COUNCILMOM","PROPCERT","TCSHORT",
                        "TCM_STUASS","ASS_PROG","ASS_PROM","ASS_NAT","ASS_CUR","STU_FEEDB",
                        "TOWN","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8o,export=FALSE)
R251
# VIETNAM: 57.11

###################### SCIENCE #######################

##### STUDENTS
# "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01"

##### PARENTS
# "BOOK_N", "PARPRESSURE","FUNDMOM","COUNCILMOM","DUTYMOM"

##### TEACHERS
# "PROPCERT","TCSHORT","TCM_STUASS","TCM_PEER", "ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",

##### SCHOOLS
# "PCGIRLS","PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT",
# "EXC11_UNICORN","SCORE_PUBLIC","LEADINST","QUAL_RECORD","SCHSEL","TEACCLIM"

R366 <- pisa.reg.pv(pvlabel="SCIE", 
                    x=c("VIETNAM",
                        "FEMALE","PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                        "PCGIRLS","FUNDMOM","COUNCILMOM","DUTYMOM", "PROPCERT","TCSHORT",
                        "TCM_STUASS","TCM_PEER", "ASS_PROG","ASS_PROM","ASS_NAT","ASS_SCH","ASS_CUR","STU_FEEDB",
                        "PRIVATESCL","TOWN","CLSIZE","COMPWEB","SCMATEDU","EXC2_PLAY",
                        "EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN","SCORE_PUBLIC","LEADINST",
                        "QUAL_RECORD","SCHSEL","TEACCLIM"),
                    weight="W_FSTUWT",
                    data=DEVCON8w,export=FALSE)
R366
# VIETNAM: 84.36
