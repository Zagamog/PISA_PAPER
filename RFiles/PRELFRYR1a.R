# PISA2012_FL_part1
# Unraveling a secret: Vietnam's outstanding performance on the PISA test 2012 following the Fryer & Levitt (2004) approach

# Prepared by Elisabeth Sedmik on Wednesday, June 24 2015
# Based on code by Suhas D. Parandekar

# Revised on 07/21/2015

# The following code tries to unravel the secret of Vietnam's outstanding performance on the PISA 2012 assessment. 
# It presents an analytical comparison of possible explanatory factors (as assessed within the PISA 2012 test) 
# of Vietnam's high test score in MATH, comparing 7 other developing countries with Vietnam. The statistical 
# approach taken is a modified dummy variable approach following Fryer and Levitt (2004).

##################################################################################
# Outline:
# 1. GENERATING DATA SET (MERGING, CLEANING) 
# 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES
# 3. PISA SCORES
# 4. REGRESSION ANALYSIS FOR MATH OF A MODIFIED FRYER & LEVITT (2004) APPROACH (in part 2)
##################################################################################

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

# Please be aware that many packages (eg. tables, intsvy) require additional packages to run. When trying to load
# the package, R will tell you which ones are missing. Overall you may need to download around 40 packages.


################################ 1. GENERATING DATA SET (MERGING, CLEANING) #################################

# We generate new, merged data sets based on the student data set and school data set for the 8 countries
# that have a Per Capita GDP below $10,000 in PPP for 2010, based on OECD-PISA Table Table IV.3.2 
# OECD-PISA Report: "PISA 2012 Results: What Makes a School Successful?" (Volume IV, Chapter 3, Table IV.3.2)
# Original data downloaded from: http://pisa2012.acer.edu.au/downloads.php (txt files, spss or sas)

student.rda <- read.dta("C:/Country/Vietnam/Data/PISA/STATADATA/stu.dta")
school.rda <- read.dta("C:/Country/Vietnam/Data/PISA/STATADATA/sch.dta")

# Please refer to the questionnaires, codebooks and the technical manual (pages 396-398) about the specific
# variables included in the data files and their abbreviations  
# for an overview of included variables see: http://www.oecd.org/pisa/pisaproducts/pisa2012technicalreport.htm
# and for abbreviations see the student and school codebooks: http://pisa2012.acer.edu.au/downloads.php

# We will filter for the 8 developing countries per country by school id and merge into one file ("DEVCON8"):

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

# We want to have a unique identifier number per student per school per country. Note, that for all countries, numbering of school and student id's
# starts at 1; so we introduce "NEWID" to create for a cumulative list spanning all 8 countries:

ALB_P$COUNTRY <- 1 # We numerate each country alphabetically, starting with Albania = 1, Colombia = 2, etc.
ALB_P$NEWID <- (ALB_P$COUNTRY*10000000)+((as.numeric(ALB_P$schoolid))*10000)+(as.numeric(ALB_P$stidstd))

# Useful tip: these are very large data files. to double check if the new columns have been created, do not load 
# the new file ALB_P into the editor window, as it will only show up until a certain column or take very long to load. 
# Instead, type in "ALB_P" into the Console to see all columns (and the first 10 rows per column). 

# Now we repeat the same process for the remaining 7 countries:

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

DEVCON8 <- rbind(ALB_P,COL_P,IDN_P,JOR_P,PER_P,THA_P,TUN_P,VNM_P) # combine all country specific files into the "DEVCON8" file, thanks to "dyplr" package

# Finally, we add a Vietnam dummy variable to "DEVCON8", which we will need to produce descriptive statistics 
# and for the modified Fryer-Levitt analysis

DEVCON8$VIETNAM[DEVCON8$COUNTRY==8] <- 1 # dummy takes value = 1, if the country is Vietnam
DEVCON8$VIETNAM[DEVCON8$COUNTRY!=8] <- 0 # dummy takes value = 0, if the country is not Vietnam

# The 'intsvy' package (the most important one for the PISA analysis) requires variable names to be in upper case; 
# you might have noticed how we put all newly created variables already in upper case; no we convert all 

names(DEVCON8) <- toupper(names(DEVCON8)) 

# Let's save it for now

save(DEVCON8, file = "C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/DEVCON8.rda") 

# Get summary of the DEVCON file (variables, mean, sd, etc.) in LATEX via use of the 'stargazer' package
# This will give you a neat ouptut of all variables and you can use it as an overview of all variables in the data set

stargazer(DEVCON8,
          type="latex", out="C:/Country/Vietnam/Data/PISA/PISA_PAPER/Latex/DEVCON8.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          summary=TRUE)

# Useful tip: read up on the specifics of the stargazer command, especially since you might need to load additional packages in Latex


###################### 2. DESCRIPTIVE STATISTICS WITH VIETNAM + 7 DEVELOPING COUNTRIES ######################

# We generate Kernel plots (comparison of non-parametric univariate density estimates) for Math, Science and Reading scores 
# comparing Vietnam to the group of 7 developing countries as identified above and the OECD Average. 

# The 'sm' package (used for locally smoothed density estimates) requires that country names are converted into factors, 
# (ie categorical variables) so we create a new column (VNM.f), which contains either 'Vietnam' or 'Group of 7' as a factor. 
# Useful resource on the use of factors in R: http://www.stat.berkeley.edu/~s133/factors.html

DEVCON8$VNM.f <- factor(DEVCON8$VIETNAM, levels=c(0,1),labels = c("GROUP OF 7", "Vietnam"))

# For the Kernel plots we are using the 'sm.density.compare' function of the 'sm' package, because it allows
# to superimpose the kernel density plots of two or more groups; in our case 'Vietnam' and 'Group of 7'

# Kernel density plots for MATH (Vietnam and Group of 7)

sm.density.compare(DEVCON8$PV3MATH,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("darkgreen","red"),
                   nbins=0,h=35,xlab="Mathematics Score",weights=DEVCON8$W_FSTUWT)
title(main="Kernel Density PISA 2012 Mathematics Scores")

# Two important points:

# 1. You will notice that from now on, that we will frequently use 'W_FSTUWT'. It stands for the
# final student weight (ie. size of the student body in the respective country), that needs to be applied 
# to the PVs (Plausible Values) to correctly compute the PISA score.
# The methodology can be found here: http://www.oecd.org/pisa/pisaproducts/pisa2012technicalreport.htm (Chapter 8)

# 2. For the numeric vector (PV3MATH) we randomly pick one of the 5 PVs for the Math Score,
# since we are 'smoothing' the values anyway (through Kernel). Plausible values are intermediate values
# provided to obtain consistent estimates of population parameters. Ideally, we would analyse all five PV's, 
# average the result and significance tests adjusting for variation between the five sets of results computed. 
# However, for the purpose of producing descriptive statistics, we deem our approach sufficient. 
# You can double check with different PV's and will notice only slight differences.
# Please refer to the PISA 2012 technical report (cited above) page 147. 

# Note: if 'weights' is used, the number of bins must be set to 0 ('nbins=0') 
# Note: we select bandwith as h=35. There are different approaches to selecting bandwith and you can adjust 'h' to see
# which bin size you prefer to best visualize the density plots. 

# We draw a reference line for the OECD average

abline(v=494, lty=5, col="grey",lwd=2)
axis(1,at=494,labels=494)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("darkgreen","red"),bty="n")
text(-50,0.0050, labels="OECD Average",pos=4)
arrows(400, 0.0050, 494, 0.0050)

# Note: The PISA scores have been standardized with an international mean of 500 and a standard deviation of 100.
# The 'OECD Average' scores can be found here: http://www.oecd.org/pisa/keyfindings/pisa-2012-results-overview.pdf (page 5)
# I took the OECD Average not the international (overall PISA) average ... do you agree?

# Kernel density plots for SCIENCE (Vietnam and Group of 7)

sm.density.compare(DEVCON8$PV1SCIE,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("blue","red"),
                   nbins=0,h=35,xlab="Science Score",weights=DEVCON8$W_FSTUWT)
title(main="Kernel Density PISA 2012 Science Scores")

abline(v=501, lty=5, col="grey",lwd=2)
axis(1,at=501,labels=501)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("blue","red"),bty="n")
text(-50,0.0051, labels="OECD Average",pos=4)
arrows(400, 0.0051, 501, 0.0051)

# Kernel density plots for READING (Vietnam and Group of 7)

sm.density.compare(DEVCON8$PV5READ,lwd=2,DEVCON8$VNM.f,lty=c(2,1),col=c("purple","red"),
                   nbins=0,h=35,xlab="Reading Score",weights=DEVCON8$W_FSTUWT)
title(main="Kernel Density PISA 2012 Reading Scores")

# Draw reference line for OECD average
abline(v=496, lty=5, col="grey",lwd=2)
axis(1,at=496,labels=496)
legend(-150,0.003, levels(DEVCON8$VNM.f),lty=c(2,1), lwd=2, col=c("purple","red"),bty="n")
text(-50,0.0052, labels="OECD Average",pos=4)
arrows(400, 0.0052, 496, 0.0052)

# Finally note that there are other ways to create Kernel Density Plots; eg for Kernel Density of only
# one variable/group (above we compared two groups: group of 7 & Vietnam) you do not need the sm package and can simply
# code 'plot(density(DEVCON8$PV3MATH))' which will give you the Kernel density for the Math score for the group of
# all 8 countries. 
# For a very basic overview we recommend: http://www.statmethods.net/graphs/density.html 

# We will keep DEVCON8 as a master file, so we create a new version for the subsequent steps

save(DEVCON8, file = "C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/DEVCON8a.rda")

############################################## 3. PISA SCORES  ##############################################

# The most important tip from now on: work closely with the PISA 2012 technical manual AND get familiar with
# functions of the 'intsvy' package (which does all the rigorous calculations of PISA PV's etc. for you!)

# To highlight this, look how the intsvy packages helps you find the Mean PISA Scores for Vietnam from the PVs:

meanMATH <- pisa.mean.pv(pvlabel="MATH",by="VIETNAM", data=DEVCON8, weight="W_FSTUWT")
meanMATH
meanSCIE <- pisa.mean.pv(pvlabel="SCIE",by="VIETNAM", data=DEVCON8, weight="W_FSTUWT")
meanSCIE
meanREAD <- pisa.mean.pv(pvlabel="READ",by="VIETNAM", data=DEVCON8, weight="W_FSTUWT")
meanREAD

# Similarly, you can find the mean Math scores for all 8 countries through sorting by "COUNTRY" 
# (remember we assigned countries numerical values from 1 to 8 alphabetically)

meanCNT <- pisa.mean.pv(pvlabel="MATH", by="CNT", data=DEVCON8, weight="W_FSTUWT")
meanCNT

# So lets do the baseline regressions with help of the 'intsvy' package. The command 'pisa.reg.pv' performs linear 
# regression analysis (OLS) with plausible values and replicate weights.

# MATH

MATH0 <- pisa.reg.pv(pvlabel="MATH", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
MATH0

# Estimate Std. Error t value
# (Intercept)   383.29       2.50  153.26
# VIETNAM       128.05       5.65   22.68
# R-squared      27.21       2.25   12.0

# SCIENCE

SCIE0 <- pisa.reg.pv(pvlabel="SCIE", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
SCIE0

# Estimate Std. Error t value
# (Intercept)   393.86       2.25  175.00
# VIETNAM       134.56       4.91   27.41
# R-squared      30.75       1.96   15.66

# READING

READ0 <- pisa.reg.pv(pvlabel="READ", 
                     x=c("VIETNAM"),
                     weight="W_FSTUWT",
                     data=DEVCON8,export=FALSE)
READ0

# Estimate Std. Error t value
# (Intercept)   403.06       2.46  163.78
# VIETNAM       105.16       5.03   20.89
# R-squared      19.61       1.81   10.85

# The difference between mean score values is the coefficient value on the dummy. For example, the intercept (383.29) +
# VIETNAM (128.05) = 511.34: the mean Math Score for Vietnam.

# Since we will now alter most of the initial file (delete missing cases, etc.) we create a new file (DEVON8a) and 
# to have the masterfile (DEVCON8) as a back-up. 

save(DEVCON8, file = "C:/Country/Vietnam/Data/PISA/PISA_PAPER/RFiles/DEVCON8a.rda") 


#############################################################################################################
############################################### END OF PART 1 ###############################################
#############################################################################################################

