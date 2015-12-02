#### S1_Figures.R

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
#### S1_Figures   SECTION 1: Introduction (Descriptive statistics, plots, etc)
# S2_Endowments   SECTION 2: Mean tables
# S3_FryerLevitt  SECTION 3: Regressions following Fryer & Levitt (2004)
# S4_OaxacaBlin   SECTION 4: Regressions following Oaxaca-Blinder approach
##################################################################################

# We create initital descriptive statistics, the GDP plot (Figure 1) and Kernel density plot (Figure 2).
# For the tikz plot (Figure 3), please see the accompanying S1_TIKZ.tex file

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

load("DEVCON8.rda")

# This is not included as a table in the research paper, but to get summary statistics of all variables in the DEVCON8 file 
# (variables, mean, sd, etc.) in LATEX, we recommend to use the 'stargazer' package and the command below.
# This will give you a neat ouptut of all variables and you can use it as an overview of all variables in the data set

stargazer(DEVCON8,
          type="latex", out="DEVCON8.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          summary=TRUE)

# Useful tip: read up on the specifics of the stargazer command, especially since you might need to load additional packages

# The most important tip from now on: work closely with the PISA 2012 technical manual AND get familiar with
# functions of the 'intsvy' package (which does all the rigorous calculations of PISA PV's etc. for you)

# To highlight this, look how the intsvy packages helps you find the Mean PISA Scores for Vietnam from the PV's:

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
# VIETNAM (128.05) = 511.34, which is the mean Math Score for Vietnam.

#### Figure 1: PISA 2012 results compared with GDP per capita

# We created a simple txt file with a list of countries, their GDP per capita (2010 in PPP) and PISA math score in 2012
# The data is sorted in order of increaseing GDP per capita

# Loading the .txt file (S1_GDP_MATH.txt) into R:
gdp_math_scatter <- read.table("S1_GDP_MATH.txt",
                                     header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

attach(gdp_math_scatter) # This is just so that we can avoid referring to the datframe name each time that we invoke a variable
plot(pisa_m~gdp, 
     xlim=c(0,65000), ylim=c(300,700), xaxs="i", yaxs="i",  #xaxs = i instead of r to eliminate offset
     pch=19,col="indianred4",
     xlab='GDP per Capita in PPP 2010', ylab= 'PISA Math Average Score 2012', font.lab=1,
     data=gdp_math_scatter)

# We want Vietnam to stand out in the plot
points(4098,511,col="blue",bg="red",pch=24)
points(4098,511,col="blue",bg="red",pch=25)

text(4098,511,labels="Vietnam (511)",pos=4,cex=0.75,col="red") # pos=4 means to the side

# Key other countries
points(9555,376,col="blue",bg="blue",pch=19)
text(9555,376,labels="Colombia (376)",pos=4,cex=0.75,col="blue") # pos=4 means to the side
text(4700,375,labels="Indonesia",pos=1,cex=0.75,col="blue") # pos=1 means to the bottom
text(9400,360,labels="Peru",pos=4,cex=0.75,col="blue") # pos=4 means to the right side
text(18805,613,labels="Shanghai-China",pos=1,cex=0.75) # pos=1 means to the bottom
text(48962,531,labels="Switzerland",pos=1,cex=0.75,col="red") # pos=1 means to the bottom
text(36030,519,labels="Finland",pos=2,cex=0.75,col="red") # pos=2 means to the left side

# Additional countries/points
points(48962,531,col="red",bg="red",pch=19)
points(36030,519,col="red",bg="red",pch=19)
points(4638,375,col="blue",bg="blue",pch=19)
points(9350,368,col="blue",bg="blue",pch=19)

# Lastly, we add the loess fit line
temp1 <- filter(gdp_math_scatter, gdp_math_scatter$country != "vietnam" & gdp_math_scatter$country != "Shanghai_China"
                & gdp_math_scatter$country != "luxembourg" & gdp_math_scatter$country != "Qatar")

loess_fit <- loess(pisa_m ~ gdp, data=temp1, span=2)
lines(temp1$gdp, predict(loess_fit), col = "purple",lwd=2, lty=2)

# We save the data for later use
save(gdp_math_scatter, file ="gdp_math_scatter.rda") 

#### Figure 2: Kernel Density comparison between Vietnam and other Developing Countries

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

# 1. You will notice that from now on, we will frequently use 'W_FSTUWT'. It stands for the
# final student weight (ie. size of the student body in the respective country), that needs to be applied 
# to the PVs (Plausible Values) to correctly compute the PISA score.
# The methodology can be found here: http://www.oecd.org/pisa/pisaproducts/pisa2012technicalreport.htm (Chapter 8)

# 2. For the numeric vector (PV3MATH) we randomly pick one of the 5 PVs for the Math Score,
# since we are 'smoothing' the values anyway (through Kernel). Plausible values are intermediate values
# provided to obtain consistent estimates of population parameters. Ideally, we would analyse all five PV's, 
# average the result and significance tests adjusting for variation between the five sets of results computed. 
# However, for the purpose of producing descriptive statistics, we deem our approach sufficient. 
# You can double check with different PV's and will notice only very slight differences.
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

# Since we will now alter most of the initial file (delete missing cases, etc.) we create a new file (DEVON8a) and 
# to have the masterfile (DEVCON8) as a back-up. 

save(DEVCON8, file = "DEVCON8a.rda")

#### Figure 3: Conceptual scheme based on available comparative variables from PISA 2012
# Please see accompanying S1_TIKZ.tex file

#### End of S1_Figures.R

