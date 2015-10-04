# s31_tch_incentv.R

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
SC31OUTN.rda <- read.csv("C:/Country/Vietnam/Data/PISA/PISA_PAPER/Excel/SC31DAT9OUT.csv")
# I merge back to the PISA data, except now I have to give it a c suffix.
# merge school and student datasets 
DEVCON9c <- merge(DEVCON9b,SC31OUTN.rda,by="NEWID")
DEVCON9c$TCH_INCENTV <- rescale(DEVCON9c$WMLE, mean = 0, sd = 1,df=FALSE)


