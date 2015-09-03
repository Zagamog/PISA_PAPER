# Quicktabs. R

# Written by Suhas Parandekar, Tuesday, Sept. 1, 2015

# Using data.table and dplyr for fast look at any key variables - caution
# make sure the dataframe also accrues the data.table properties before using

# Will substantially reduce time rather than use base R

library(data.table)
library(dplyr)
library(devtools) # to install new packages not yet on CRAN

# Add modify to dplyr
# install_github(repo = "modify", username = "skranz")
# library(modify)

# Exploring the determinants of MATWKETH

a <- select(DEVCON8aDT.rda,ST46Q01,ST46Q02,ST46Q03,ST46Q04,ST46Q05,ST46Q05,ST46Q06,ST46Q07,ST46Q08,ST46Q09,VIETNAM) %>%
      mutate(ST46Q01=abs(ST46Q01-4),ST46Q02=abs(ST46Q02-4),ST46Q03=abs(ST46Q03-4),ST46Q04=abs(ST46Q04-4),ST46Q05=abs(ST46Q05-4),ST46Q06=abs(ST46Q06-4),
      ST46Q07=abs(ST46Q07-4),ST46Q08=abs(ST46Q08-4),ST46Q09=abs(ST46Q09-4)) %>%
       group_by(VIETNAM) %>%
        summarise(n_students=n(),Q01=mean(ST46Q01,na.rm=T),nQ01=sum(is.na(ST46Q01)),
                  Q02=mean(ST46Q02,na.rm=T),nQ02=sum(is.na(ST46Q02)),
                  Q03=mean(ST46Q03,na.rm=T),nQ03=sum(is.na(ST46Q03)),
                  Q04=mean(ST46Q04,na.rm=T),nQ04=sum(is.na(ST46Q04)),
                  Q05=mean(ST46Q05,na.rm=T),nQ05=sum(is.na(ST46Q05)),
                  Q06=mean(ST46Q06,na.rm=T),nQ06=sum(is.na(ST46Q06)),
                  Q07=mean(ST46Q07,na.rm=T),nQ07=sum(is.na(ST46Q07)),
                  Q08=mean(ST46Q08,na.rm=T),nQ08=sum(is.na(ST46Q08)),
                  Q09=mean(ST46Q09,na.rm=T), Q09=sum(is.na(ST46Q09)))
a


# I want to see if the OECD mean is really 0 - looks like 0.06, but I guess that is because I
# have not used any weights.

# The method is to calculate the country mean using student weight
# then calculate the simple mean for OECD

a <- select(stuDT.rda,matwketh,oecd,w_fstuwt,cnt) %>%
       group_by(cnt) %>%
        summarise(n_students=n(),
                  MTWKBAR=weighted.mean(matwketh,w_fstuwt,na.rm=T), 
                  oecd=mean(oecd),
                  nrel=sum(!is.na(matwketh)))
a



b <-  filter(a,oecd==1) %>%
            summarise(oe_mtwk=mean(MTWKBAR))
b

# Indeed oe_mtwk is zero




