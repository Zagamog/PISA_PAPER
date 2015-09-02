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
# be 48483 - 4959 (Vietnam) = 43524 observations, so lets do the countries individually and then 'rbind'

PISA_VN <- subset(DEVCON8,CNT==c("VNM")) # 4959 observations

PISA_AL <- subset(DEVCON8,CNT==c("ALB")) # 4743 observations

PISA_CO <- subset(DEVCON8,CNT==c("COL")) # 9073 observations

PISA_ID <- subset(DEVCON8,CNT==c("IDN")) # 5622 observations

PISA_JO <- subset(DEVCON8,CNT==c("JOR")) # 7038 observations

PISA_PE <- subset(DEVCON8,CNT==c("PER")) # 6035 observations

PISA_TH <- subset(DEVCON8,CNT==c("THA")) # 6606 observations

PISA_TU <- subset(DEVCON8,CNT==c("TUN")) # 4407 observations

PISA_DEV7 <- rbind(PISA_AL,PISA_CO,PISA_ID,PISA_JO,PISA_PE,PISA_TH,PISA_TU) # 43524 observations q.e.e.d.

# Start to create our Oaxaca-Blinder Output:

# For SCIENCE

#Science Regression
R_VS <- pisa.reg.pv(pvlabel="SCIE", 
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
                    data=PISA_VN,
                    export=FALSE)
R_VS # works for PISA_VN

R_DEV7S <- pisa.reg.pv(pvlabel="SCIE", 
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
                    data=PISA_DEV7,
                    export=FALSE)
R_DEV7S # works for PISA_DEV7

# Now to the Oaxaca Package ...






















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


