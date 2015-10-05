

#### DATA CREATION FILE FOR DEVCON9a ####



student.rda <- stu
school.rda <- sch


student.rda <- read.dta("C:/Users/WB484284/Desktop/PISAlatestverstions/RFiles/PISA_2012/stu.dta")
school.rda <- read.dta("C:/Users/WB484284/Desktop/PISAlatestverstions/RFiles/PISA_2012/sch.dta")

# Albania 

ALB_T <- filter(student.rda, cnt == "ALB") # filter by "cnt" and create a new student file just for 'Albania'
ALB_S <- filter(school.rda, cnt == "ALB") # filter by "cnt" and create a new school file just for 'Albania'
ALB_P <- merge(ALB_T, ALB_S, by = "schoolid") # merge both files into one file just for 'Albania'
ALB_P$cnt <- ALB_P$cnt.x # we duplicate "cnt.x" as "cnt" (it is only called "cnt.x" since we merged ALB_S and ALB_T, not that in ALB_S and ALB_T it was called "cnt")
ALB_P$cnt.x <- NULL # we delete the column "cnt.x"
ALB_P$subnatio <- ALB_P$subnatio.x # we duplicate "subnatio.x" as "subnatio" (to have the same nomenclature as in the original data!)
ALB_P$subnatio.x <- NULL # we delete the column "subnatio.x"
ALB_P$stratum <- ALB_P$stratum.x # same as above
ALB_P$stratum.x <- NULL # same as above
ALB_P$oecd <- ALB_P$oecd.x # same as above
ALB_P$oecd.x <- NULL # same as above
ALB_P$nc <- ALB_P$nc.x  # same as above
ALB_P$nc.x <- NULL # same as above

ALB_P$COUNTRY <- 1 # We numerate each country alphabetically, starting with Albania = 1, Colombia = 2, etc.
ALB_P$NEWID <- (ALB_P$COUNTRY*10000000)+((as.numeric(ALB_P$schoolid))*10000)+(as.numeric(ALB_P$stidstd))

# Colombia

COL_T <- filter(student.rda, cnt == "COL")
COL_S <- filter(school.rda, cnt == "COL")
COL_P <- merge(COL_T, COL_S, by = "schoolid")
COL_P$cnt <- COL_P$cnt.x
COL_P$cnt.x <- NULL
COL_P$subnatio <- COL_P$subnatio.x
COL_P$subnatio.x <- NULL
COL_P$stratum <- COL_P$stratum.x
COL_P$stratum.x <- NULL
COL_P$oecd <- COL_P$oecd.x
COL_P$oecd.x <- NULL
COL_P$nc <- COL_P$nc.x
COL_P$nc.x <- NULL
COL_P$COUNTRY <-2
COL_P$NEWID <- (COL_P$COUNTRY*10000000)+((as.numeric(COL_P$schoolid))*10000)+(as.numeric(COL_P$stidstd)) 

# The command 'as.numeric' converts individual columns to numeric variables 
# Useful tip: to test for the class of variables in the column 'schoolid', type in 'class(COL_P$schoolid)'

# Indonesia

IDN_T <- filter(student.rda, cnt == "IDN")
IDN_S <- filter(school.rda, cnt == "IDN")
IDN_P <- merge(IDN_T, IDN_S, by = "schoolid")
IDN_P$cnt <- IDN_P$cnt.x
IDN_P$cnt.x <- NULL
IDN_P$subnatio <- IDN_P$subnatio.x
IDN_P$subnatio.x <- NULL
IDN_P$stratum <- IDN_P$stratum.x
IDN_P$stratum.x <- NULL
IDN_P$oecd <- IDN_P$oecd.x
IDN_P$oecd.x <- NULL
IDN_P$nc <- IDN_P$nc.x
IDN_P$nc.x <- NULL
IDN_P$COUNTRY <-3
IDN_P$NEWID <- (IDN_P$COUNTRY*10000000)+((as.numeric(IDN_P$schoolid))*10000)+(as.numeric(IDN_P$stidstd))

# Jordan

JOR_T <- filter(student.rda, cnt == "JOR")
JOR_S <- filter(school.rda, cnt == "JOR")
JOR_P <- merge(JOR_T, JOR_S, by = "schoolid")
JOR_P$cnt <- JOR_P$cnt.x
JOR_P$cnt.x <- NULL
JOR_P$subnatio <- JOR_P$subnatio.x
JOR_P$subnatio.x <- NULL
JOR_P$stratum <- JOR_P$stratum.x
JOR_P$stratum.x <- NULL
JOR_P$oecd <- JOR_P$oecd.x
JOR_P$oecd.x <- NULL
JOR_P$nc <- JOR_P$nc.x
JOR_P$nc.x <- NULL
JOR_P$COUNTRY <-4
JOR_P$NEWID <- (JOR_P$COUNTRY*10000000)+((as.numeric(JOR_P$schoolid))*10000)+(as.numeric(JOR_P$stidstd))

# Peru

PER_T <- filter(student.rda, cnt == "PER")
PER_S <- filter(school.rda, cnt == "PER")
PER_P <- merge(PER_T, PER_S, by = "schoolid")
PER_P$cnt <- PER_P$cnt.x
PER_P$cnt.x <- NULL
PER_P$subnatio <- PER_P$subnatio.x
PER_P$subnatio.x <- NULL
PER_P$stratum <- PER_P$stratum.x
PER_P$stratum.x <- NULL
PER_P$oecd <- PER_P$oecd.x
PER_P$oecd.x <- NULL
PER_P$nc <- PER_P$nc.x
PER_P$nc.x <- NULL
PER_P$COUNTRY <-5
PER_P$NEWID <- (PER_P$COUNTRY*10000000)+((as.numeric(PER_P$schoolid))*10000)+(as.numeric(PER_P$stidstd))

# Thailand

THA_T <- filter(student.rda, cnt == "THA")
THA_S <- filter(school.rda, cnt == "THA")
THA_P <- merge(THA_T, THA_S, by = "schoolid")
THA_P$cnt <- THA_P$cnt.x
THA_P$cnt.x <- NULL
THA_P$subnatio <- THA_P$subnatio.x
THA_P$subnatio.x <- NULL
THA_P$stratum <- THA_P$stratum.x
THA_P$stratum.x <- NULL
THA_P$oecd <- THA_P$oecd.x
THA_P$oecd.x <- NULL
THA_P$nc <- THA_P$nc.x
THA_P$nc.x <- NULL
THA_P$COUNTRY <-6
THA_P$NEWID <- (THA_P$COUNTRY*10000000)+((as.numeric(THA_P$schoolid))*10000)+(as.numeric(THA_P$stidstd))

# Tunisia

TUN_T <- filter(student.rda, cnt == "TUN")
TUN_S <- filter(school.rda, cnt == "TUN")
TUN_P <- merge(TUN_T, TUN_S, by = "schoolid")
TUN_P$cnt <- TUN_P$cnt.x
TUN_P$cnt.x <- NULL
TUN_P$subnatio <- TUN_P$subnatio.x
TUN_P$subnatio.x <- NULL
TUN_P$stratum <- TUN_P$stratum.x
TUN_P$stratum.x <- NULL
TUN_P$oecd <- TUN_P$oecd.x
TUN_P$oecd.x <- NULL
TUN_P$nc <- TUN_P$nc.x
TUN_P$nc.x <- NULL
TUN_P$COUNTRY <-7
TUN_P$NEWID <- (TUN_P$COUNTRY*10000000)+((as.numeric(TUN_P$schoolid))*10000)+(as.numeric(TUN_P$stidstd))

# Vietnam

VNM_T <- filter(student.rda, cnt == "VNM")
VNM_S <- filter(school.rda, cnt == "VNM")
VNM_P <- merge(VNM_T, VNM_S, by = "schoolid")
VNM_P$cnt <- VNM_P$cnt.x
VNM_P$cnt.x <- NULL
VNM_P$subnatio <- VNM_P$subnatio.x
VNM_P$subnatio.x <- NULL
VNM_P$stratum <- VNM_P$stratum.x
VNM_P$stratum.x <- NULL
VNM_P$oecd <- VNM_P$oecd.x
VNM_P$oecd.x <- NULL
VNM_P$nc <- VNM_P$nc.x
VNM_P$nc.x <- NULL
VNM_P$COUNTRY <-8
VNM_P$NEWID <- (VNM_P$COUNTRY*10000000)+((as.numeric(VNM_P$schoolid))*10000)+(as.numeric(VNM_P$stidstd))

QCN_T <- filter(student.rda, cnt == "QCN") # filter by "cnt" and create a new student file just for 'Albania'
QCN_S <- filter(school.rda, cnt == "QCN") # filter by "cnt" and create a new school file just for 'Albania'
QCN_P <- merge(QCN_T, QCN_S, by = "schoolid") # merge both files into one file just for 'QCNania'
QCN_P$cnt <- QCN_P$cnt.x # we duplicate "cnt.x" as "cnt" (it is only called "cnt.x" since we merged QCN_S and QCN_T, not that in QCN_S and QCN_T it was called "cnt")
QCN_P$cnt.x <- NULL # we delete the column "cnt.x"
QCN_P$subnatio <- QCN_P$subnatio.x # we duplicate "subnatio.x" as "subnatio" (to have the same nomenclature as in the original data!)
QCN_P$subnatio.x <- NULL # we delete the column "subnatio.x"
QCN_P$stratum <- QCN_P$stratum.x # same as above
QCN_P$stratum.x <- NULL # same as above
QCN_P$oecd <- QCN_P$oecd.x # same as above
QCN_P$oecd.x <- NULL # same as above
QCN_P$nc <- QCN_P$nc.x  # same as above
QCN_P$nc.x <- NULL # same as above
QCN_P$COUNTRY <-9
QCN_P$NEWID <- (QCN_P$COUNTRY*10000000)+((as.numeric(QCN_P$schoolid))*10000)+(as.numeric(QCN_P$stidstd))

DEVCON8 <- rbind(ALB_P,COL_P,IDN_P,JOR_P,PER_P,THA_P,TUN_P,VNM_P,QCN_P) # combine all country specific files into the "DEVCON8" file, thanks to "dyplr" package


DEVCON8$VIETNAM[DEVCON8$COUNTRY==8] <- 1 # dummy takes value = 1, if the country is Vietnam
DEVCON8$VIETNAM[DEVCON8$COUNTRY!=8] <- 0 # dummy takes value = 0, if the country is not Vietnam

# The 'intsvy' package (the most important one for the PISA analysis) requires variable names to be in upper case; 
# you might have noticed how we put all newly created variables already in upper case; no we convert all 

names(DEVCON8) <- toupper(names(DEVCON8)) 

DEVCON9a <- DEVCON8

T0 <- DEVCON9a[, c("VIETNAM")] # create a vector only of "VIETNAM" or any other variable you want to look at
N0<- NROW(na.omit(T0)) # tell R to delete any rows with missing variables 
N0

DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==1] <- 0
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==2] <- 1
DEVCON9a$PRESCHOOL[DEVCON9a$ST05Q01==3] <- 1

DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==1] <- 1
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==2] <- 0
DEVCON9a$PARPRESSURE[DEVCON9a$SC24Q01==3] <- 0

DEVCON9a$ASS_PROM[DEVCON9a$SC18Q02==1] <- 1
DEVCON9a$ASS_PROM[DEVCON9a$SC18Q02==2] <- 0

DEVCON9a$ASS_SCH[DEVCON9a$SC18Q05==1] <- 1
DEVCON9a$ASS_SCH[DEVCON9a$SC18Q05==2] <- 0

DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==1] <- 1
DEVCON9a$STU_FEEDB[DEVCON9a$SC39Q07==2] <- 0

DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==1] <- 1
DEVCON9a$SCL_EXTR_CL[DEVCON9a$SC20Q01==2] <- 0

DEVCON9a$FEMALE[DEVCON9a$ST04Q01==1] <- 1
DEVCON9a$FEMALE[DEVCON9a$ST04Q01==2] <- 0

DEVCON9a$FEMALE <- factor(DEVCON9a$FEMALE)

# Endowments need to be positive
DEVCON9a$NOREPEAT <- factor(-(DEVCON9a$REPEAT-1))

DEVCON9a$NOLATE[DEVCON9a$ST08Q01==1] <- 10
DEVCON9a$NOLATE[DEVCON9a$ST08Q01==2] <- 8.5
DEVCON9a$NOLATE[DEVCON9a$ST08Q01==3] <- 6.5
DEVCON9a$NOLATE[DEVCON9a$ST08Q01==4] <- 4

DEVCON9a$NOMISS[DEVCON9a$ST09Q01==1] <- 10
DEVCON9a$NOMISS[DEVCON9a$ST09Q01==2] <- 8.5
DEVCON9a$NOMISS[DEVCON9a$ST09Q01==3] <- 6.5
DEVCON9a$NOMISS[DEVCON9a$ST09Q01==4] <- 4

DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==1] <- 10
DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==2] <- 8.5
DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==3] <- 6.5
DEVCON9a$NOSKIP[DEVCON9a$ST115Q01==4] <- 4

DEVCON9a$MSRATIO <- 100/DEVCON9a$SMRATIO

# Additionally, need to create variables OUTMATH, OUTREAD, OUTSCIE, TIGERMOM, 
# TCH_INCENTV, TCM_INSPE, COMP_USE

DEVCON9a$TIGERMOM  <- DEVCON9a$SC25Q01+DEVCON9a$SC25Q03
DEVCON9a$TIGERMOM[DEVCON9a$TIGERMOM>100] <- 100

DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==1] <- 1
DEVCON9a$TCM_INSPE[DEVCON9a$SC30Q04==2] <- 0

DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==1] <- 1
DEVCON9a$COMP_USE[DEVCON9a$SC40Q01==2] <- 0

DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==1] <- 0
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==2] <- 1
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==3] <- 3
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==4] <- 5
DEVCON9a$OUTMATH[DEVCON9a$ST55Q02==5] <- 7

DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==1] <- 0
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==2] <- 1
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==3] <- 3
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==4] <- 5
DEVCON9a$OUTREAD[DEVCON9a$ST55Q01==5] <- 7

DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==1] <- 0
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==2] <- 1
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==3] <- 3
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==4] <- 5
DEVCON9a$OUTSCIE[DEVCON9a$ST55Q03==5] <- 7

# S31  teacher incentives
# __________________________________________________________________________________________
# How many non-missing values ?
T1b <- DEVCON9a[, c("SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
N1 <- NROW(na.omit(T1b)) 
N1 # 51,315
N0-N1 # 2345
# Data set with non NAs 
DEVCON9b <- DEVCON9a[complete.cases(T1b),]

# I will generate an OECD style rasch index that measures incentives - High incentives means high value on this WMLE measure
SC31DAT9 <- DEVCON9b[,c("NEWID","W_FSCHWT","W_FSTUWT","SC31Q01", "SC31Q02","SC31Q03","SC31Q04","SC31Q05","SC31Q06","SC31Q07")]
write.csv(SC31DAT9, "SC31DAT9.csv")
# Generated Winsteps output using Winsteps control+data file SC31a9.txt
# Person file Output read back into R
SC31OUTN.rda <- read.csv("C:/Users/WB484284/Desktop/PISA_PAPER/Excel/SC31DAT9OUT2.csv")
# I merge back to the PISA data, except now I have to give it a c suffix.
# merge school and student datasets 
DEVCON9c <- merge(DEVCON9b,SC31OUTN.rda,by="NEWID")
DEVCON9c$TCH_INCENTV <- rescale(DEVCON9c$WMLE, mean = 0, sd = 1,df=FALSE)

DEVCON9a <- DEVCON9c

save(DEVCON9a, file="C:/Users/WB484284/Desktop/PISAlatestversions/RFiles/PISA_2012/DEVCON9a.rda")