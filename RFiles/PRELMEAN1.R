# ALLMEAN1a. R

# Prepared by Suhas Monday, August 10, 2015
# and Elisbeth Sedmik

# Revised by Suhas Thursday, Sept 3, 2015

# Comparison of means (p value, t-stats) to be used in addition to ALLTAB1a.R, to determine which
# variable means of Vietnam and Dev7 are statistically different at teh 5% level 

# Admin packages
library(foreign)# To import and export data to and from R (eg. txt files)
library(xlsx)# To generate MS-Excel output
library(epicalc)# For producing descriptives of data
library(tables) # Computes and displays complex tables of summary statistics
library(reshape2) # For melting and recasting data 
library(psych)# For rescaling variables to given mean and sd

load("DEVCON8a.rda")

# Creating the variables

#ST04Q01
#___________________________________________________________________________________________________________
# We create a dummy variable for Female students
DEVCON8a$FEMALE[DEVCON8a$ST04Q01==1] <- 1
DEVCON8a$FEMALE[DEVCON8a$ST04Q01==2] <- 0

#ST05Q01
#_________________________________________________________________________________________________________
# We change three levels into Yes or No, to create a pre-school dummy variable
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==1] <- 0
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==2] <- 1
DEVCON8a$PRESCHOOL[DEVCON8a$ST05Q01==3] <- 1

#ST08Q01
#_______________________________________________________________________________________________________________
# leave as is

#ST09Q01
#_______________________________________________________________________________________________________________
# leave as is 

# ST115Q01 
#______________________________________________________________________________________________________________
# leave as is

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

#SC24Q01 
#________________________________________________________________________________________________________________
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==1] <- 1
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==2] <- 0
DEVCON8a$PARPRESSURE[DEVCON8a$SC24Q01==3] <- 0

#SC25Q01
#_________________________________________________________________________________________________________________
DEVCON8a$SC25Q01[is.na(DEVCON8a$SC25Q01)]  <- 0
DEVCON8a$SC25Q02[is.na(DEVCON8a$SC25Q02)]  <- 0
DEVCON8a$SC25Q03[is.na(DEVCON8a$SC25Q03)]  <- 0
DEVCON8a$SC25Q04[is.na(DEVCON8a$SC25Q04)]  <- 0
DEVCON8a$SC25Q05[is.na(DEVCON8a$SC25Q05)]  <- 0
DEVCON8a$SC25Q06[is.na(DEVCON8a$SC25Q06)]  <- 0
DEVCON8a$SC25Q07[is.na(DEVCON8a$SC25Q07)]  <- 0
DEVCON8a$SC25Q08[is.na(DEVCON8a$SC25Q08)]  <- 0
DEVCON8a$SC25Q09[is.na(DEVCON8a$SC25Q09)]  <- 0
DEVCON8a$SC25Q10[is.na(DEVCON8a$SC25Q10)]  <- 0
DEVCON8a$SC25Q11[is.na(DEVCON8a$SC25Q11)]  <- 0
DEVCON8a$SC25Q12[is.na(DEVCON8a$SC25Q12)]  <- 0

# SC25Q01 is quite rich in information, so we create sub-variables
#TIGERMOM
DEVCON8a$TIGERMOM  <- DEVCON8a$SC25Q01+DEVCON8a$SC25Q03
DEVCON8a$TIGERMOM[DEVCON8a$TIGERMOM>100] <- 100 

#VOLUMOM
DEVCON8a$VOLUMOM <- DEVCON8a$SC25Q05+DEVCON8a$SC25Q06+DEVCON8a$SC25Q07+DEVCON8a$SC25Q09+DEVCON8a$SC25Q12
DEVCON8a$VOLUMOM[DEVCON8a$VOLUMOM>100] <- 100 # censoring at 100 should look familiar now

#TEACHMOM
DEVCON8a$TEACHMOM <- DEVCON8a$SC25Q08

#FUNDMOM
DEVCON8a$FUNDMOM <-  DEVCON8a$SC25Q11

#COUNCILMOM
DEVCON8a$COUNCILMOM <- DEVCON8a$SC25Q10

#DUTYMOM
DEVCON8a$DUTYMOM  <- DEVCON8a$SC25Q02+DEVCON8a$SC25Q04
DEVCON8a$DUTYMOM[DEVCON8a$DUTYMOM>100] <- 100 

# Teacher variables 

#SC30Q01, SC30Q02, SC30Q03, SC30Q04
#_________________________________________________________________________________________________________________
# Convert into 0 1 variable # Teacher Monitoring (TCM) through Student Assessment (STUASS)
DEVCON8a$TCM_STUASS[DEVCON8a$SC30Q01==1] <- 1
DEVCON8a$TCM_STUASS[DEVCON8a$SC30Q01==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Peer review (PEER)
DEVCON8a$TCM_PEER[DEVCON8a$SC30Q02==1] <- 1
DEVCON8a$TCM_PEER[DEVCON8a$SC30Q02==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Principal or Senior observation (OBSER)
DEVCON8a$TCM_OBSER[DEVCON8a$SC30Q03==1] <- 1
DEVCON8a$TCM_OBSER[DEVCON8a$SC30Q03==2] <- 0

# Convert into 0 1 variable # Teacher Monitoring (TCM) through Inspector/external observer (INSPE)
DEVCON8a$TCM_INSPE[DEVCON8a$SC30Q04==1] <- 1
DEVCON8a$TCM_INSPE[DEVCON8a$SC30Q04==2] <- 0

#SC39Q08
#________________________________________________________________________________________________________________
# Convert into 0 1 variable Quality assurance through teacher mentoring 
DEVCON8a$TCH_MENT[DEVCON8a$SC39Q08==1] <- 1
DEVCON8a$TCH_MENT[DEVCON8a$SC39Q08==2] <- 0

#SC31Q01 - SC31Q07
#________________________________________________________________________________________________________________
SC31OUT.rda <- read.csv("C:/Country/Vietnam/Data/PISA/MS-Excel/SC31DATOUT.csv")
DEVCON8a <- merge(DEVCON8a,SC31OUT.rda,by="NEWID")
DEVCON8a$TCH_INCENTV <- rescale(DEVCON8a$WMLE_SC31, mean = 0, sd = 1,df=FALSE)

# Pedagogical Practices variables 

# SC18Q01-Q08
#________________________________________________________________________________________________________________
DEVCON8a$ASS_PROG[DEVCON8a$SC18Q01==1] <- 1
DEVCON8a$ASS_PROG[DEVCON8a$SC18Q01==2] <- 0

DEVCON8a$ASS_PROM[DEVCON8a$SC18Q02==1] <- 1
DEVCON8a$ASS_PROM[DEVCON8a$SC18Q02==2] <- 0

DEVCON8a$ASS_INSTR[DEVCON8a$SC18Q03==1] <- 1
DEVCON8a$ASS_INSTR[DEVCON8a$SC18Q03==2] <- 0

DEVCON8a$ASS_NAT[DEVCON8a$SC18Q04==1] <- 1
DEVCON8a$ASS_NAT[DEVCON8a$SC18Q04==2] <- 0

DEVCON8a$ASS_SCH[DEVCON8a$SC18Q05==1] <- 1
DEVCON8a$ASS_SCH[DEVCON8a$SC18Q05==2] <- 0

DEVCON8a$ASS_TCH[DEVCON8a$SC18Q06==1] <- 1
DEVCON8a$ASS_TCH[DEVCON8a$SC18Q06==2] <- 0

DEVCON8a$ASS_CUR[DEVCON8a$SC18Q07==1] <- 1
DEVCON8a$ASS_CUR[DEVCON8a$SC18Q07==2] <- 0

DEVCON8a$ASS_OTH[DEVCON8a$SC18Q08==1] <- 1
DEVCON8a$ASS_OTH[DEVCON8a$SC18Q08==2] <- 0

#SC39Q07
#________________________________________________________________________________________________________________
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==1] <- 1
DEVCON8a$STU_FEEDB[DEVCON8a$SC39Q07==2] <- 0

#SC40Q01-SC40Q03
#________________________________________________________________________________________________________________
DEVCON8a$COMP_USE[DEVCON8a$SC40Q01==1] <- 1
DEVCON8a$COMP_USE[DEVCON8a$SC40Q01==2] <- 0

DEVCON8a$TXT_BOOK[DEVCON8a$SC40Q02==1] <- 1
DEVCON8a$TXT_BOOK[DEVCON8a$SC40Q02==2] <- 0

DEVCON8a$STD_CUR[DEVCON8a$SC40Q03==1] <- 1
DEVCON8a$STD_CUR[DEVCON8a$SC40Q03==2] <- 0

# School variables #

# Now for the schools-related variables

#SC01Q01
#_________________________________________________________________________________________________________
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==2] <- 1
DEVCON8a$PRIVATESCL[DEVCON8a$SC01Q01==1] <- 0

#SC02Q02 - leave as is

#SC03Q01/City size
#_________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8a$DUM_VILLAGE <- ifelse(DEVCON8a$SC03Q01==1,1,0)
DEVCON8a$DUM_SMLTOWN <- ifelse(DEVCON8a$SC03Q01==2,1,0)
DEVCON8a$DUM_TOWN    <- ifelse(DEVCON8a$SC03Q01==3,1,0)
DEVCON8a$DUM_CITY    <- ifelse(DEVCON8a$SC03Q01==4,1,0)
DEVCON8a$DUM_LRGCITY <- ifelse(DEVCON8a$SC03Q01==5,1,0)

DEVCON8a$TOWN <- DEVCON8a$DUM_SMLTOWN+DEVCON8a$DUM_TOWN
DEVCON8a$TOWN[DEVCON8a$TOWN>1] <- 1
DEVCON8a$CITY <- DEVCON8a$DUM_CITY+DEVCON8a$DUM_LRGCITY
DEVCON8a$CITY[DEVCON8a$CITY>1] <- 1

# CLSIZE, SCHSIZE, RATCMP15, COMPWEB, SCMATEDU, SCMATBUI

#SC16Q01-Q11
#________________________________________________________________________________________________________
DEVCON8a$EXC1_BAND[DEVCON8a$SC16Q01==1] <- 1
DEVCON8a$EXC1_BAND[DEVCON8a$SC16Q01==2] <- 0

DEVCON8a$EXC2_PLAY[DEVCON8a$SC16Q02==1] <- 1
DEVCON8a$EXC2_PLAY[DEVCON8a$SC16Q02==2] <- 0

DEVCON8a$EXC3_NEWS[DEVCON8a$SC16Q03==1] <- 1
DEVCON8a$EXC3_NEWS[DEVCON8a$SC16Q03==2] <- 0

DEVCON8a$EXC4_VOLU[DEVCON8a$SC16Q04==1] <- 1
DEVCON8a$EXC4_VOLU[DEVCON8a$SC16Q04==2] <- 0

DEVCON8a$EXC5_MCLUB[DEVCON8a$SC16Q05==1] <- 1
DEVCON8a$EXC5_MCLUB[DEVCON8a$SC16Q05==2] <- 0

DEVCON8a$EXC6_MATHCOMP[DEVCON8a$SC16Q06==1] <- 1
DEVCON8a$EXC6_MATHCOMP[DEVCON8a$SC16Q06==2] <- 0

DEVCON8a$EXC7_CHESS[DEVCON8a$SC16Q07==1] <- 1
DEVCON8a$EXC7_CHESS[DEVCON8a$SC16Q07==2] <- 0

DEVCON8a$EXC8_ICTCB[DEVCON8a$SC16Q08==1] <- 1
DEVCON8a$EXC8_ICTCB[DEVCON8a$SC16Q08==2] <- 0

DEVCON8a$EXC9_ARTCB[DEVCON8a$SC16Q09==1] <- 1
DEVCON8a$EXC9_ARTCB[DEVCON8a$SC16Q09==2] <- 0

DEVCON8a$EXC10_SPORT[DEVCON8a$SC16Q10==1] <- 1
DEVCON8a$EXC10_SPORT[DEVCON8a$SC16Q10==2] <- 0

DEVCON8a$EXC11_UNICORN[DEVCON8a$SC16Q11==1] <- 1
DEVCON8a$EXC11_UNICORN[DEVCON8a$SC16Q11==2] <- 0

#SC20Q01
#________________________________________________________________________________________________________
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==1] <- 1
DEVCON8a$SCL_EXTR_CL[DEVCON8a$SC20Q01==2] <- 0

#SC19Q01-Q02
#________________________________________________________________________________________________________
DEVCON8a$SCORE_PUBLIC[DEVCON8a$SC19Q01==1] <- 1
DEVCON8a$SCORE_PUBLIC[DEVCON8a$SC19Q01==2] <- 0

DEVCON8a$SCORE_AUTHRITS[DEVCON8a$SC19Q02==1] <- 1
DEVCON8a$SCORE_AUTHRITS[DEVCON8a$SC19Q02==2] <- 0

# "SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD","LEADTCH" leave as is

#SC39Q03
#_________________________________________________________________________________________________________
DEVCON8a$QUAL_RECORD[DEVCON8a$SC39Q03==1] <- 1
DEVCON8a$QUAL_RECORD[DEVCON8a$SC39Q03==2] <- 0

#"SCHSEL","STUDCLIM","TEACCLIM","TCMORALE" leave as is

# ROTATED PART 1:

# nothing to prepare

# ROTATED PART 2:

#ST55Q02
#________________________________________________________________________________________________________
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==1] <- 0
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==2] <- 1
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==3] <- 3
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==4] <- 5
DEVCON8a$OUTMATH[DEVCON8a$ST55Q02==5] <- 7

#ST55Q01
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==1] <- 0
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==2] <- 1
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==3] <- 3
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==4] <- 5
DEVCON8a$OUTREAD[DEVCON8a$ST55Q01==5] <- 7

#ST55Q03
#________________________________________________________________________________________________________
# First I have to generate a series of dummy variables 
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==1] <- 0
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==2] <- 1
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==3] <- 3
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==4] <- 5
DEVCON8a$OUTSCIE[DEVCON8a$ST55Q03==5] <- 7


# LMINS, MMINS, SMINS
#________________________________________________________________________________________________________
DEVCON8a$SHRS <- (DEVCON8a$SMINS)/60
DEVCON8a$MHRS <- (DEVCON8a$MMINS)/60
DEVCON8a$LHRS <- (DEVCON8a$LMINS)/60

# ROTATED PART 3:

# ST91Q02
#________________________________________________________________________________________________________
DEVCON8a$ATT_SA <- ifelse(DEVCON8a$ST91Q02==1,1,0)
DEVCON8a$ATT_A <- ifelse(DEVCON8a$ST91Q02==2,1,0)
DEVCON8a$ATT_CONTROL <-DEVCON8a$ATT_SA+DEVCON8a$ATT_A
# DEVCON8a$ATT_CONTROL[DEVCON8a$ATT_CONTROL>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q03
#________________________________________________________________________________________________________
DEVCON8a$FAMPROB_SA <- ifelse(DEVCON8a$ST91Q03==1,1,0)
DEVCON8a$FAMPROB_A <- ifelse(DEVCON8a$ST91Q03==2,1,0)
DEVCON8a$BKGR_FAMPROB <-DEVCON8a$FAMPROB_SA+DEVCON8a$FAMPROB_A
# DEVCON8a$BKGR_FAMPROB[DEVCON8a$BKGR_FAMPROB>1]<- 1 # do not need to do since mutually exclusive 

# ST91Q04
#________________________________________________________________________________________________________
DEVCON8a$DIFFTCH_SA <- ifelse(DEVCON8a$ST91Q04==1,1,0)
DEVCON8a$DIFFTCH_A <- ifelse(DEVCON8a$ST91Q04==2,1,0)
DEVCON8a$TCHQUAL_DIFF <- DEVCON8a$DIFFTCH_SA+DEVCON8a$DIFFTCH_A
# DEVCON8a$TCHQUAL_DIFF[DEVCON8a$TCHQUAL_DIFF>1]<- 1 # do not need to do since mutually exclusive 

mean1A <- t(sapply(DEVCON8a[c("FEMALE", "PRESCHOOL", "REPEAT", "ST08Q01", "ST09Q01", "ST115Q01", "HISEI",
                              "MISCED", "WEALTH", "CULTPOS", "HEDRES", "BOOK_N", "PARPRESSURE", "PCGIRLS",
                              "TIGERMOM", "VOLUMOM", "TEACHMOM", "FUNDMOM", "COUNCILMOM","DUTYMOM",
                              "STRATIO", "PROPCERT", "PROPQUAL",
                              "SMRATIO","TCSHORT","TCFOCST","TCM_STUASS","TCM_PEER","TCM_OBSER","TCM_INSPE",
                              "TCH_INCENTV","SC35Q02","TCH_MENT",
                              "ASS_PROG","ASS_PROM",
                              "ASS_INSTR","ASS_NAT","ASS_SCH","ASS_TCH","ASS_CUR","ASS_OTH","STU_FEEDB",
                              "COMP_USE","TXT_BOOK","STD_CUR",
                              "PRIVATESCL","SC02Q02","DUM_VILLAGE","TOWN","CITY",
                              "CLSIZE","SCHSIZE","RATCMP15","COMPWEB","SCMATEDU","SCMATBUI",
                              "EXC1_BAND","EXC2_PLAY","EXC3_NEWS","EXC4_VOLU","EXC5_MCLUB","EXC6_MATHCOMP",
                              "EXC7_CHESS","EXC8_ICTCB","EXC9_ARTCB","EXC10_SPORT","EXC11_UNICORN","SCL_EXTR_CL",
                              "SCORE_PUBLIC","SCORE_AUTHRITS","SCHAUTON","TCHPARTI","LEADCOM","LEADINST","LEADPD",
                              "LEADTCH","QUAL_RECORD","SCHSEL","STUDCLIM","TEACCLIM","TCMORALE",
                              "MATWKETH","INSTMOT","INTMAT",
                              "SUBNORM","MATHEFF","FAILMAT","MATINTFC","MATBEH","PERSEV","OPENPS",
                              "OUTMATH",
                              "OUTREAD",
                              "OUTSCIE","ST57Q01","ST57Q02","ST57Q03","ST57Q04","ST57Q05","ST57Q06",
                              "EXAPPLM","EXPUREM","FAMCON","FAMCONC","LHRS","MHRS","SHRS",
                              "BKGR_FAMPROB","SCMAT","ANXMAT",
                              "BELONG","ATSCHL","ATTLNACT","ATT_CONTROL","MTSUP","STUDREL","TCHQUAL_DIFF","TCHBEHTD",
                              "TCHBEHSO","TCHBEHFA","COGACT","CLSMAN","DISCLIMA","ST72Q01","AGE")], function (x)
  unlist(t.test(x~DEVCON8a$VIETNAM, paired=FALSE,weight="W_FSTUWT")[c("estimate","p.value","statistic")])))
mean1A
                              
write.csv(mean1A, "mean1A.csv")

#              estimate.mean in group 0  estimate.mean in group 1  p.value       statistic.t
#FEMALE                       0.52645311             5.335928e-01  3.443973e-01  -0.9455861
#PRESCHOOL                    0.78880191             9.120427e-01 1.034759e-154 -27.1252511
#REPEAT                       0.19153261             6.790123e-02 8.557329e-189  30.1058308
#ST08Q01                      1.51314463             1.187154e+00  0.000000e+00  42.2886514
#ST09Q01                      1.21916359             1.099897e+00  5.745375e-95  20.9638187
#ST115Q01                     1.25846623             1.076434e+00 2.653508e-239  34.0980332
#HISEI                       40.41957137             2.660231e+01  0.000000e+00  44.4640204
#MISCED                       3.11927580             2.174443e+00 9.057979e-282  37.6506285
#WEALTH                      -1.46057324            -2.134309e+00 9.877621e-284  37.9473519
#CULTPOS                     -0.14241373            -2.360740e-01  1.421461e-09   6.0626884
#HEDRES                      -0.74274623            -1.074292e+00 1.719826e-110  22.7533147
#BOOK_N                      53.63932275             5.078599e+01  1.591587e-02   2.4114388
#PARPRESSURE                  0.26647181             3.836827e-01  1.188862e-56 -16.0325148
#PCGIRLS                      0.49001923             5.282052e-01 4.999730e-101 -21.4474726
#TIGERMOM                    52.44716650             6.241825e+01  8.290658e-57 -16.0551795
#VOLUMOM                     35.21335411             3.836235e+01  1.841681e-07  -5.2207584
#TEACHMOM                    12.17643716             3.828206e+01  0.000000e+00 -43.1136747
#FUNDMOM                     23.07844084             5.960221e+01  0.000000e+00 -55.8809921
#COUNCILMOM                  36.45455138             2.311739e+01 5.383959e-123  24.1308383
#DUTYMOM                     66.97372808             6.855426e+01  5.244857e-03  -2.7926137
#STRATIO                     19.71504749             1.896565e+01  3.894310e-15   7.8717247
#PROPCERT                     0.67570675             7.960587e-01  5.458745e-80 -19.2316892
#PROPQUAL                     0.87556086             8.774751e-01  6.469628e-01  -0.4580108
#SMRATIO                    188.17910333             1.209773e+02  0.000000e+00  62.9817970
#TCSHORT                      0.48458868             4.249337e-01  7.981550e-04   3.3550568
#TCFOCST                      0.49747085             1.402448e-01 2.791486e-158  27.5461022
#TCM_STUASS                   0.87623449             9.817698e-01  0.000000e+00 -42.0299188
#TCM_PEER                     0.79162915             8.381811e-01  1.831729e-16  -8.2548605
#TCM_OBSER                    0.80145737             9.784924e-01  0.000000e+00 -61.9111724
#TCM_INSPE                    0.58824963             8.664482e-01  0.000000e+00 -51.1233264
#TCH_INCENTV                 -0.03169114             2.687061e-01 2.412502e-175 -28.9234648
#SC35Q02                     40.50679343             4.900860e+01  8.001403e-35 -12.3922735
#TCH_MENT                     0.85663082             9.858664e-01  0.000000e+00 -53.3421125
#ASS_PROG                     0.96949003             9.928308e-01  1.538562e-55 -15.7906332
#ASS_PROM                     0.89879703             9.508398e-01  5.489228e-51 -15.1351395
#ASS_INSTR                    0.66479810             7.378124e-01  2.941681e-27 -10.8650496
#ASS_NAT                      0.70081249             8.785334e-01 7.244147e-238 -34.1764662
#ASS_SCH                      0.91113303             9.799263e-01 9.359251e-167 -28.0225291
#ASS_TCH                      0.77643564             9.911921e-01  0.000000e+00 -87.0569619
#ASS_CUR                      0.90171488             9.127407e-01  1.038921e-02  -2.5633728
#ASS_OTH                      0.66102115             8.660385e-01 7.335319e-287 -37.8642132
#STU_FEEDB                    0.71045406             8.418681e-01 6.846065e-114 -23.1164097
#COMP_USE                     0.43446078             6.446521e-01 4.107806e-170 -28.7079023
#TXT_BOOK                     0.79051705             7.855387e-01  4.229834e-01   0.8013100
#STD_CUR                      0.87047666             9.489963e-01 1.622432e-104 -22.0384346
#PRIVATESCL                   0.17136127             8.316264e-02  2.714888e-88  20.1974001
#SC02Q02                     25.72327518             1.661039e+01  4.995235e-99  21.4348693
#DUM_VILLAGE                  0.14030038             4.584187e-01  0.000000e+00 -43.3780567
#TOWN                         0.45079450             3.101188e-01  9.385188e-86  19.9304126
#CITY                         0.40890512             2.314625e-01 2.310371e-155  27.2857090
#CLSIZE                      35.01295038             4.250430e+01  0.000000e+00 -55.9516384
#SCHSIZE                   1057.03319833             1.302901e+03 5.256301e-117 -23.3836395
#RATCMP15                     0.39093520             2.215996e-01 3.700476e-190  30.1915505
#COMPWEB                      0.75559734             7.794846e-01  1.330924e-05  -4.3596680
#SCMATEDU                    -0.81445457            -4.940619e-01  1.077845e-97 -21.3307303
#SCMATBUI                    -0.63221939            -3.988347e-01  3.926068e-50 -15.0198469
#EXC1_BAND                    0.47095695             1.677591e-01  0.000000e+00  51.3775115
#EXC2_PLAY                    0.59281691             8.508808e-01  0.000000e+00 -45.6113799
#EXC3_NEWS                    0.53731984             5.088079e-01  1.709808e-04   3.7607492
#EXC4_VOLU                    0.82703777             8.299877e-01  6.046616e-01  -0.5177352
#EXC5_MCLUB                   0.45303083             2.687423e-01 1.038567e-152  27.0447356
#EXC6_MATHCOMP                0.62678105             8.031544e-01 3.387734e-169 -28.5337774
#EXC7_CHESS                   0.34371638             2.302335e-01  3.437594e-67  17.5205360
#EXC8_ICTCB                   0.48988730             1.749283e-01  0.000000e+00  52.5984614
#EXC9_ARTCB                   0.67736212             4.585396e-01 9.657451e-174  29.0641621
#EXC10_SPORT                  0.93211109             9.920115e-01 2.698412e-239 -33.5747770
#EXC11_UNICORN                0.71523924             9.629250e-01  0.000000e+00 -70.3214961
#SCL_EXTR_CL                  0.65384521             9.584187e-01  0.000000e+00 -82.2784015
#SCORE_PUBLIC                 0.34495301             7.566571e-01  0.000000e+00 -62.6103025
#SCORE_AUTHRITS               0.80026252             8.281708e-01  1.551220e-06  -4.8092737
#SCHAUTON                    -0.25418027            -1.041946e+00  0.000000e+00  54.2133512
#TCHPARTI                    -0.21686623            -1.644545e+00  0.000000e+00 138.9153605
#LEADCOM                      0.23867394             8.937687e-02  7.179392e-41  13.4592530
#LEADINST                     0.08990140            -5.493878e-02  3.138747e-23   9.9665663
#LEADPD                       0.24396850            -5.868257e-02 1.625814e-108  22.5316687
#LEADTCH                      0.32334115            -2.913958e-01  0.000000e+00  43.4552101
#QUAL_RECORD                  0.88649520             9.817698e-01 4.582742e-307 -38.4979068
#SCHSEL                       2.30606017             2.845350e+00  0.000000e+00 -77.0688552
#STUDCLIM                     0.04850410             4.180401e-02  5.557535e-01   0.5891836
#TEACCLIM                    -0.19969168            -8.727031e-02  7.826284e-22  -9.6293768
#TCMORALE                     0.03758320            -2.940526e-01 7.797214e-131  24.8825063
#MATWKETH                     0.45141124            -1.363913e-03 3.394444e-219  33.2648704
#INSTMOT                      0.42531997             3.682795e-01  4.022034e-05   4.1104100
#INTMAT                       0.72118946             6.926561e-01  2.599003e-02   2.2270801
#SUBNORM                      0.71597648            -9.233674e-02  0.000000e+00  49.1855870
#MATHEFF                     -0.22685414            -2.655020e-01  1.808674e-03   3.1217400
#FAILMAT                      0.08295933             8.949602e-02  6.107533e-01  -0.5090272
#MATINTFC                     0.09201733             3.285500e-01  1.565713e-30 -11.5846707
#MATBEH                       0.87640566             6.757492e-01  4.777491e-54  15.6588864
#PERSEV                       0.33865297             4.474748e-01  6.065927e-11  -6.5590662
#OPENPS                       0.19485189            -6.124862e-01  0.000000e+00  48.7874202
#OUTMATH_NONE                 0.40240647             1.744654e-01 3.724595e-190  30.7818962
#OUTMATH_LESS2                0.22196331             1.701271e-01  4.805510e-13   7.2527454
#OUTMATH_2TO4                 0.20412659             2.993492e-01  7.926886e-29 -11.2300116
#OUTMATH_4TO6                 0.10341906             2.150604e-01  1.014471e-48 -14.8846670
#OUTREAD_NONE                 0.55403510             4.731617e-01  8.795369e-18   8.6275803
#OUTREAD_LESS2                0.18855977             2.119144e-01  2.244389e-03  -3.0577699
#OUTREAD_2TO4                 0.14189792             2.022960e-01  5.942073e-16  -8.1249436
#OUTREAD_4TO6                 0.06731546             7.942910e-02  1.620130e-02  -2.4053641
#OUTSCIE_NONE                 0.46793716             3.269891e-01  7.904689e-55  15.8206914
#OUTSCIE_LESS2                0.21100524             2.386895e-01  5.371530e-04  -3.4642842
#OUTSCIE_2TO4                 0.18100266             2.293292e-01  7.939305e-10  -6.1611428
#OUTSCIE_4TO6                 0.08674564             1.344774e-01  4.550201e-14  -7.5728470
#EXAPPLM                      0.11105148            -2.417840e-01 2.014615e-117  23.6692266
#EXPUREM                     -0.13841464             1.586954e-01  1.764117e-79 -19.2555281
#FAMCON                       0.25593149             1.224724e-01  5.672082e-21   9.4320681
#FAMCONC                     -0.54408873             4.297122e-01  0.000000e+00 -57.8182293
#LHRS                         3.59901324             3.220697e+00  3.454297e-49  14.8930959
#MHRS                         3.89597956             3.787778e+00  2.148719e-04   3.7037855
#SHRS                         3.75658265             3.959685e+00  1.722692e-04  -3.7612221
#BKGR_FAMPROB                 0.47048486             2.640050e-01 7.906734e-126  24.6601963
#SCMAT                        0.16732706            -1.896060e-01 2.982803e-193  31.0353008
#ANXMAT                       0.39948316             2.115148e-01  7.350962e-53  15.5036410
#BELONG                       0.05108552            -2.573932e-01 1.576441e-107  22.5895847
#ATSCHL                       0.16161756             1.430037e-01  2.568277e-01   1.1340677
#ATTLNACT                     0.12332565            -5.349843e-01  0.000000e+00  42.1229480
#ATT_CONTROL                  0.85071298             6.607807e-01 5.994881e-101  22.0020963
#MTSUP                        0.47783176             3.685141e-01  2.049235e-13   7.3675379
#STUDREL                      0.37940666             1.863511e-02  3.081736e-97  21.4593623
#TCHQUAL_DIFF                 0.52493396             3.630455e-01  2.804219e-69  17.9242625
#TCHBEHTD                     0.49726706             2.963570e-01  5.264225e-37  12.8173472
#TCHBEHSO                     0.79206942             2.969399e-01 5.542948e-201  31.8889574
#TCHBEHFA                     0.46342883             4.984811e-03 2.127233e-183  30.2367214
#COGACT                       0.29981797            -3.278034e-01  0.000000e+00  47.8235345
#CLSMAN                       0.23943153             2.162578e-01  1.157704e-01   1.5730847
#DISCLIMA                    -0.02425538             3.747449e-01 1.332269e-179 -29.8506654
#ST72Q01                     31.01327988             4.100183e+01  0.000000e+00 -83.5273837



