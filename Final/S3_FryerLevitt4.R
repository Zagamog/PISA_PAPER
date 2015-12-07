#### S3_FryerLevitt4.R (Figure 4)

#### Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 ####

# Prepared by Suhas D. Parandekar and Elisabeth K. Sedmik (The World Bank Group)
# Accompanying code to research paper
# Date of this version: 12/02/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam.

##################################################################################
# Outline:
# S0_Prelims      Generating data sets (merging, cleaning) 
# S1_Figures      SECTION 1: Introduction (Descriptive statistics, plots, etc)
# S2_Endowments   SECTION 2: Endowments tables
#### S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
# S4_OaxacaBlin   SECTION 4: Regressions following Oaxaca-Blinder approach
# SAppx_FryerLev  SECTION 5: Regressions following Fryer & Levitt (2004) on the rotated questionnaire
##################################################################################

# We create Figure 4: PISA 2012 results compared with School Infrastructure Quality

# Loading R packages to process PISA data:

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
library(data.table) # For enhanced dataframe faster processing
library(dplyr) # For faster data crunching

# 1. DATA PREPARATION: A. PISA Math Score and GDP Per capita

GDP_MATH_SCATTER.rda <- read.xlsx("S3_GDP_MATH.xls",
                                 sheetName="Table_1")
CTS <- data.table(GDP_MATH_SCATTER.rda) # Create data table
CTS <- filter(CTS, !is.na(cnt)) # # modify to delet OECD average row, now has 64 records

# 1. DATA PREPARATION: B. School Infrstructure Quality Index (SCMATBUI)

stu.rda <- read.dta("stu.dta")
sch.rda <- read.dta("sch.dta")
SCHDT <- data.table(sch.rda)
STUDT <- data.table(stu.rda)

setkey(STUDT,cnt) # Need to delete US states 
STUDT <- STUDT[cnt!="QUA" & cnt!="QUB" & cnt!="QUC"] # I select schoolid and cnt and make a new variable.

SCHDT$CNTNUM <- group_indices(SCHDT,cnt) 
SCHDT <- SCHDT[, schoolno := seq(.N), by = list(cnt)]
SCHDT$NEWSCHID <- SCHDT$CNTNUM*1000 + SCHDT$schoolno
tail(SCHDT)

STUDT$CNTNUM <- group_indices(STUDT,cnt) # drop qua qub quc
STUDT$schoolno <- group_indices(STUDT,schoolid)
STUDT <- STUDT[, stuno := seq(.N), by = list(cnt,schoolid)]
STUDT$NEWSCHID <- STUDT$CNTNUM*1000 + STUDT$schoolno
STUDT$NEWSTUID <- STUDT$NEWSCHID*1000 + STUDT$stuno
tail(STUDT)

setkey(SCHDT,NEWSCHID) # we eliminate schools with repeated schoolids
SCHDT <- unique(SCHDT)

setkey(STUDT,NEWSTUID) # we do the same with students
STUDT <- unique(STUDT) 

a <- select(SCHDT,cnt,NEWSCHID,CNTNUM,scmatbui,w_fschwt)  
b <- select(STUDT,NEWSCHID,NEWSTUID,w_fstuwt)  
c <- right_join(b,a, by="NEWSCHID")

MTS <- select(m1,cnt,scmatbui) # Finally we just keep the variables we need

### 2. CREATING Figure 4

Fig4 <- full_join(CTS,MTS,by="cnt") %>%
  filter(cnt!="CYP" & cnt!="MNE" &cnt!="LIE" &cnt!="QRS")

write.xlsx(Fig4,file = "Fig4.xlsx",
           col.names=TRUE,row.names = FALSE)

Fig4 <- data.table(Fig4)

Dev8 <- c("ALB","COL","IDN","JOR","PER","THA","TUN","VNM")
Fig4 <-  Fig4[, Dev8:=as.numeric(cnt %in% Dev8)] 

j <- order(Fig4$scmatbui)

plot(pisa_m~scmatbui, 
     pch=19,col="indianred4",
     xlim=c(-1.5,0.6), ylim=c(300,700), xaxs="i", yaxs="i",  
     xlab='School Infrastructure Quality Index', ylab= 'PISA Math Average Score 2012', font.lab=1,
     data=Fig4)
abline((lm(pisa_m~scmatbui, data=Fig4)),lty=3, untf=T, lwd=3,col = "purple")

# I want Vietnam to stand out in the plot
points(-0.40401,511,col="blue",bg="red",pch=24)
points(-0.40401,511,col="blue",bg="red",pch=25)

# Other countries
points(-1.25086,388,col="blue",bg="blue",pch=19)
points(-0.87318,427,col="blue",bg="blue",pch=19)
points(-0.77556,376,col="blue",bg="blue",pch=19)
points(-0.71298,407,col="blue",bg="blue",pch=19)

text(-0.40401,511,labels="Vietnam",pos=2,cex=0.75,col="red")

arrows(-1.2, 511, -0.67,511)


#### End of S3_FryerLevitt4.R
