# Figure4.R

# Written by Suhas Monday, Sept 21, 2015

# To produce Figure 4 in paper maybe


library(foreign)# To import and export data to and from R (eg. txt files)
library(xlsx)# To generate MS-Excel output
library(dplyr) # For data manipulation
library(intsvy) # For PISA Means
library(data.table) # to make data.tables
library(gmodels) # For PROC FREQ like tables
library(ggplot2) # For the plot

# %%%%%%%%%%%%%%%%%%%%%%%%%%
# Read MS-Excel data with CNT PISA Math Score and GDP Per capita
GDP_MATH_SCATTER.rda <- read.xlsx("C:/Country/Vietnam/Data/PISA/MS-Excel/GDP_MATH_SCATTER.xls",
                                  sheetName="Table_1")

GDP_MATH_SCATTER.rda$cnt=GDP_MATH_SCATTER.rda$CNT
GDP_MATH_SCATTER.rda$CNT <- NULL
# Create data table
CTS <- data.table(GDP_MATH_SCATTER.rda )
# modify to delet OECD average row
CTS <- filter(CTS, !is.na(cnt)) # now has 64 records
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create local dt for sch.rdata
SCHDT <- data.table(sch)
STUDT <- data.table(stu)
# Need to delete US states 
setkey(STUDT,cnt)
STUDT <- STUDT[cnt!="QUA" & cnt!="QUB" & cnt!="QUC"]
# I select schoolid and cnt and make a new variable.

SCHDT$CNTNUM <- group_indices(SCHDT,cnt) # could not figure out how to do through sequencing
SCHDT <- SCHDT[, schoolno := seq(.N), by = list(cnt)]
SCHDT$NEWSCHID <- SCHDT$CNTNUM*1000 + SCHDT$schoolno
tail(SCHDT)


# drop qua qub quc
STUDT$CNTNUM <- group_indices(STUDT,cnt)
STUDT$schoolno <- group_indices(STUDT,schoolid)
STUDT <- STUDT[, stuno := seq(.N), by = list(cnt,schoolid)]
STUDT$NEWSCHID <- STUDT$CNTNUM*1000 + STUDT$schoolno
STUDT$NEWSTUID <- STUDT$NEWSCHID*1000 + STUDT$stuno
tail(STUDT)


# I cannot figure out merging problem - why more rows are added 
a0 <- select(SCHDT,cnt, NEWSCHID) %>%
         group_by(cnt) %>%
           summarise(num=n_distinct(NEWSCHID)) %>%
a1 <- select(STUDT,cnt, NEWSCHID) %>%
         group_by(cnt) %>%
          summarise(num=n_distinct(NEWSCHID)) %>%
a0 <- as.data.frame(a0)
a1 <- as.data.frame(a1)
write.xlsx(a0,"C:/Country/Vietnam/Data/PISA/MS-Excel/temp1.xlsx",
                                    col.names=TRUE)
write.xlsx(a0,"C:/Country/Vietnam/Data/PISA/MS-Excel/temp2.xlsx",
                        col.names=TRUE)

# I checked manually and found that the number of schools is identical
# in the school and student datasets
# I eliminate schools with repeated schoolids
setkey(SCHDT,NEWSCHID)
SCHDT <- unique(SCHDT)

# I do the same with students
setkey(STUDT,NEWSTUID)
STUDT <- unique(STUDT) # 474756



a <- select(SCHDT,cnt,NEWSCHID,CNTNUM,scmatbui,scmatedu,w_fschwt)  
b <- select(STUDT,NEWSCHID,NEWSTUID,w_fstuwt)  

c <- right_join(b,a, by="NEWSCHID")

# Now I want to get the mean of scmatbui and scmatedu by country


m1 <- weighted.mean(c$scmatedu,c$w_fstuwt,na.rm=T)
m1

m1 <- c %>% 
  group_by(cnt) %>% 
   summarise_each(funs(weighted.mean(., w_fstuwt,na.rm=T)),
                 -w_fstuwt)

# The vietnam mean -0.40401340 -0.48316602 are a bit off
#            from  -0.3988 and -0.4941  in the paper
# proably because
                   
m2 <- c %>% 
  group_by(cnt) %>% 
  summarise(nstu=n_distinct(NEWSTUID))

# we have 4959 observations here and 4882 observations in the paper 
# Ideally they should have been identical .

# I just keep the variables I need

# I call the data I need MTS
MTS <- select(m1,cnt,scmatbui,scmatedu)

# Now merge for Fig4

Fig4 <- full_join(CTS,MTS,by="cnt") %>%
          filter(cnt!="CYP" & cnt!="MNE" &cnt!="LIE" &cnt!="QRS")

write.xlsx(Fig4,"C:/Country/Vietnam/Data/PISA/MS-Excel/Fig4.xlsx",
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

# Plot for scmatedu - not used. 

plot(pisa_m~scmatedu, 
     pch=19,col="indianred4",
     xlim=c(-1.5,1.25), ylim=c(300,700), xaxs="i", yaxs="i",  
     xlab='School Education Materials Index', ylab= 'PISA Math Average Score 2012', font.lab=1,
     data=Fig4)
abline((lm(pisa_m~scmatedu, data=Fig4)),lty=3, untf=T, lwd=3,col = "orange")
