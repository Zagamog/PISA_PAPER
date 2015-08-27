
# PISA FL PART 17

# Taking the varibales with the highest mean differences

# Disregarding whether they are gap decreasing or increasing (we know that from our previous regressions
# for each individual variable)

# First 10 (excluding EXAPPLM that is from rotated 2): non rotated, r1, r3
DISCLIMA
TCH_INCENTV
TCMORALE
TCHPARTI
BELONG
ATTLNACT
OPENPS
SCHAUTON
MATINTFC
DUM_VILLAGE

T0 <- DEVCON8a[, c("VIETNAM")]
N0<- NROW(na.omit(T0)) 
N0 

T1b <- DEVCON8a[, c("VIETNAM","DISCLIMA","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "TCMORALE","TCHPARTI","BELONG","ATTLNACT","OPENPS","SCHAUTON","MATINTFC","SC03Q01")]
N1 <- NROW(na.omit(T1b)) 
N1 #13032
N0-N1 #35451 NA's
DEVCON8ab <- DEVCON8a[complete.cases(T1b),]

# Variable prep
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8ab <- merge(DEVCON8ab,SC31OUT.rda,by="NEWID")
DEVCON8ab$TCH_INCENTV <- rescale(DEVCON8ab$WMLE_SC31, mean = 0, sd = 1,df=FALSE)
DEVCON8ab$DUM_VILLAGE <- ifelse(DEVCON8ab$SC03Q01==1,1,0)

R1 <- pisa.reg.pv(pvlabel="MATH", 
                    x=c("VIETNAM"),
                    weight="W_FSTUWT",
                    data=DEVCON8ab,export=FALSE)
R1 # Vietnam: 121.47

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM","DISCLIMA","TCH_INCENTV",
                      "TCMORALE","TCHPARTI","BELONG","ATTLNACT","OPENPS","SCHAUTON","MATINTFC","DUM_VILLAGE"),
                  weight="W_FSTUWT",
                  data=DEVCON8ab,export=FALSE)
R2 # Vietnam: 151.73

#Estimate Std. Error t value
#(Intercept)   390.42       2.58  151.16
#VIETNAM       151.73       7.11   21.33
#DISCLIMA        1.91       1.85    1.03
#TCH_INCENTV     2.37       2.71    0.87
#TCMORALE        6.44       2.29    2.82
#TCHPARTI        7.88       2.05    3.85
#BELONG          6.18       1.61    3.83
#ATTLNACT       -4.64       1.34   -3.47
#OPENPS          5.18       1.30    3.98
#SCHAUTON        2.66       2.38    1.12
#MATINTFC       11.20       1.25    8.95
#DUM_VILLAGE   -33.42       7.44   -4.49
#R-squared      34.42       2.72   12.64

# the vietnam dummy actually increases, makes sense, some of these variables don't increase but decrease the gap

R3 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "DISCLIMA"),
                  weight="W_FSTUWT",
                  data=DEVCON8ab,export=FALSE)
R3 # Vietnam: 120.47

DEVCON8ab$DISCLIMA_2 <- (DEVCON8ab$DISCLIMA)^2

R4 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "DISCLIMA_2"),
                  weight="W_FSTUWT",
                  data=DEVCON8ab,export=FALSE)
R4 # Vietnam: 121.21

# this is not going well

# Let's try this then, although it will give us similar results, I am afraid we need to filter out the decreasing variables
# and only work with them. But let's just double check on another list:

# Non rotated and r3 (in the order of highest to lowest from our list)
DISCLIMA
TCH_INCENTV
TCMORALE
TCHPARTI
BELONG
ATTLNACT
SCHAUTON
DUM_VILLAGE
TEACHMOM
SCMAT
COGACT
LEADTCH
LEADINST
FUNDMOM
LEADPD
SCORE_PUBLIC

T1b <- DEVCON8a[, c("VIETNAM","DISCLIMA","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07",
                    "TCMORALE","TCHPARTI","BELONG","ATTLNACT","SCHAUTON","SC03Q01","SCMAT","COGACT","LEADTCH","LEADINST", 
                    "LEADPD","SC19Q01","SC25Q10","SC25Q11")]
N1 <- NROW(na.omit(T1b)) 
N1 #26389
N0-N1 #22094 NA's
DEVCON8ab <- DEVCON8a[complete.cases(T1b),]

# Variable prep
SC31OUT.rda <- read.csv("C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/SC31DATOUT.csv")
DEVCON8ab <- merge(DEVCON8ab,SC31OUT.rda,by="NEWID")
DEVCON8ab$TCH_INCENTV <- rescale(DEVCON8ab$WMLE_SC31, mean = 0, sd = 1,df=FALSE)
DEVCON8ab$DUM_VILLAGE <- ifelse(DEVCON8ab$SC03Q01==1,1,0)
DEVCON8ab$SCORE_PUBLIC[DEVCON8ab$SC19Q01==1] <- 1
DEVCON8ab$SCORE_PUBLIC[DEVCON8ab$SC19Q01==2] <- 0
DEVCON8ab$SC25Q08[is.na(DEVCON8ab$SC25Q08)]  <- 0
DEVCON8ab$SC25Q11[is.na(DEVCON8ab$SC25Q11)]  <- 0
DEVCON8ab$FUNDMOM <- DEVCON8ab$SC25Q11
DEVCON8ab$TEACHMOM <- DEVCON8ab$SC25Q08

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8ab,export=FALSE)
R1 # Vietnam: 124.60

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM","DISCLIMA",
                      "TCH_INCENTV",
                      "TCMORALE",
                      "TCHPARTI",
                      "BELONG",
                      "ATTLNACT",
                      "SCHAUTON",
                      "DUM_VILLAGE",
                      "TEACHMOM",
                      "SCMAT",
                      "COGACT",
                      "LEADTCH",
                      "LEADINST",
                      "FUNDMOM",
                      "LEADPD",
                      "SCORE_PUBLIC"),
                  weight="W_FSTUWT",
                  data=DEVCON8ab,export=FALSE)
R2 # Vietnam: 147.29

# this is really all mixed effects (of gap decreasing and gap increasing)

# Just to see how far we could get the Vietnam dummy down with our previous approaches, lets combine two
# rotated parts (incl non-rotated) at a time (we know the sample size will be an issue).

# What about Math, Non rotated, R1 and R2? #

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "MATWKETH","INTMAT","INSTMOT","SUBNORM","MATHEFF","MATINTFC","PERSEV", "ST55Q02",
                    "EXPUREM","FAMCONC")]
N1 <- NROW(na.omit(T1b)) 
N1 #7004
N0-N1 #41479 NA's
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

# Now for the teacher-related variables

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

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

#ST55Q02
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$OUTMATH_NONE <- ifelse(DEVCON8i$ST55Q02==1,1,0)
DEVCON8i$OUTMATH_LESS2   <- ifelse(DEVCON8i$ST55Q02==2,1,0)
DEVCON8i$OUTMATH_2TO4   <- ifelse(DEVCON8i$ST55Q02==3,1,0)
DEVCON8i$OUTMATH_4TO6   <- ifelse(DEVCON8i$ST55Q02==4,1,0)

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R1 # Vietnam 117.44

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                      "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                      "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                      "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INTMAT","INSTMOT",
                      "SUBNORM","MATHEFF","MATINTFC","PERSEV", "OUTMATH_NONE","OUTMATH_LESS2","OUTMATH_2TO4",
                      "OUTMATH_4TO6","EXPUREM","FAMCONC"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R2 # Vietnam: 39.72

# Combining Non-rotated, R1 and R2 we can decrease the dummy to 34% of its original size (-66%)

################### What about Math, Non rotated, R1 and R3? ############################

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "MATWKETH","INTMAT","INSTMOT","SUBNORM","MATHEFF","MATINTFC","PERSEV", 
                    "ST91Q03","ANXMAT","ATSCHL","ATTLNACT","MTSUP",
                    "STUDREL","ST91Q04","TCHBEHTD","TCHBEHSO","TCHBEHFA","DISCLIMA")]
N1 <- NROW(na.omit(T1b)) 
N1 #7502
N0-N1 #40981 NA's
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

# Now for the teacher-related variables

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

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

#ST55Q02
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$OUTMATH_NONE <- ifelse(DEVCON8i$ST55Q02==1,1,0)
DEVCON8i$OUTMATH_LESS2   <- ifelse(DEVCON8i$ST55Q02==2,1,0)
DEVCON8i$OUTMATH_2TO4   <- ifelse(DEVCON8i$ST55Q02==3,1,0)
DEVCON8i$OUTMATH_4TO6   <- ifelse(DEVCON8i$ST55Q02==4,1,0)

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8i$FAMPROB_SA <- ifelse(DEVCON8i$ST91Q03==1,1,0)
DEVCON8i$FAMPROB_A <- ifelse(DEVCON8i$ST91Q03==2,1,0)
DEVCON8i$BKGR_FAMPROB <-DEVCON8i$FAMPROB_SA+DEVCON8i$FAMPROB_A
# DEVCON8i$BKGR_FAMPROB[DEVCON8i$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8i$DIFFTCH_SA <- ifelse(DEVCON8i$ST91Q04==1,1,0)
DEVCON8i$DIFFTCH_A <- ifelse(DEVCON8i$ST91Q04==2,1,0)
DEVCON8i$TCHQUAL_DIFF <- DEVCON8i$DIFFTCH_SA+DEVCON8i$DIFFTCH_A
# DEVCON8i$TCHQUAL_DIFF[DEVCON8i$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R1 # Vietnam 118.71

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                      "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                      "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                      "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","MATWKETH","INTMAT","INSTMOT",
                      "SUBNORM","MATHEFF","MATINTFC","PERSEV","BKGR_FAMPROB","ANXMAT",
                      "ATSCHL","ATTLNACT","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                      "TCHBEHSO","TCHBEHFA","DISCLIMA"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R2 # Vietnam: 53.26

# Vietnam dummy is 45% of original size, we can explain 55%

################### What about Math, Non rotated, R2 and R3? ############################

T1b <- DEVCON8a[, c("VIETNAM","PROPCERT","SMRATIO","TCSHORT","TCFOCST","SC30Q01","SC30Q02","SC31Q01",
                    "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07","ST05Q01","REPEAT",
                    "ST08Q01","ST115Q01","ST28Q01","SC24Q01","PCGIRLS","SC18Q01","SC18Q02","SC18Q05",
                    "SC39Q07","SC40Q01","SC40Q02","SC03Q01","CLSIZE","COMPWEB","SCMATEDU","SCMATBUI",
                    "SC16Q02","SC16Q06","SC16Q10","SC16Q11","SC20Q01","SC19Q01","SC39Q03","SCHSEL",
                    "ST55Q02","EXPUREM","FAMCONC","ST91Q03","ANXMAT","ATSCHL","ATTLNACT","MTSUP",
                    "STUDREL","ST91Q04","TCHBEHTD","TCHBEHSO","TCHBEHFA","DISCLIMA")]
N1 <- NROW(na.omit(T1b)) 
N1 #6779
N0-N1 #41704 NA's
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

# Now for the teacher-related variables

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

# Now for the pedagogical practices-related variables

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

# Now for the schools-related variables

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

#ST55Q02
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8i$OUTMATH_NONE <- ifelse(DEVCON8i$ST55Q02==1,1,0)
DEVCON8i$OUTMATH_LESS2   <- ifelse(DEVCON8i$ST55Q02==2,1,0)
DEVCON8i$OUTMATH_2TO4   <- ifelse(DEVCON8i$ST55Q02==3,1,0)
DEVCON8i$OUTMATH_4TO6   <- ifelse(DEVCON8i$ST55Q02==4,1,0)

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8i$FAMPROB_SA <- ifelse(DEVCON8i$ST91Q03==1,1,0)
DEVCON8i$FAMPROB_A <- ifelse(DEVCON8i$ST91Q03==2,1,0)
DEVCON8i$BKGR_FAMPROB <-DEVCON8i$FAMPROB_SA+DEVCON8i$FAMPROB_A
# DEVCON8i$BKGR_FAMPROB[DEVCON8i$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8i$DIFFTCH_SA <- ifelse(DEVCON8i$ST91Q04==1,1,0)
DEVCON8i$DIFFTCH_A <- ifelse(DEVCON8i$ST91Q04==2,1,0)
DEVCON8i$TCHQUAL_DIFF <- DEVCON8i$DIFFTCH_SA+DEVCON8i$DIFFTCH_A
# DEVCON8i$TCHQUAL_DIFF[DEVCON8i$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

R1 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R1 # Vietnam 122.32

R2 <- pisa.reg.pv(pvlabel="MATH", 
                  x=c("VIETNAM",
                      "PRESCHOOL", "REPEAT", "ST08Q01","ST115Q01","BOOK_N", "PARPRESSURE",
                      "PCGIRLS", "FUNDMOM", "COUNCILMOM","PROPCERT","SMRATIO","TCSHORT",
                      "TCFOCST","TCM_STUASS","TCM_PEER","TCH_INCENTV", "ASS_PROG","ASS_PROM",
                      "ASS_SCH","STU_FEEDB","COMP_USE","TXT_BOOK","TOWN","CLSIZE","COMPWEB",
                      "SCMATEDU","SCMATBUI","EXC2_PLAY","EXC6_MATHCOMP","EXC10_SPORT","EXC11_UNICORN",
                      "SCL_EXTR_CL","SCORE_PUBLIC","QUAL_RECORD","SCHSEL","BKGR_FAMPROB","ANXMAT",
                      "ATSCHL","ATTLNACT","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                      "TCHBEHSO","TCHBEHFA","DISCLIMA","OUTMATH_NONE","OUTMATH_LESS2","OUTMATH_2TO4",
                      "OUTMATH_4TO6","EXPUREM","FAMCONC"),
                  weight="W_FSTUWT",
                  data=DEVCON8i,export=FALSE)
R2 # Vietnam: 41.55

# Vietnam dummy is 34% of original size, we can explain 66%

