

################################################################
##################Pollutant correlation matrix##################
###############################################################

# Source the bioshare environnment
source('utils/utils_analysis.R',echo=F,print.eval=F)

#load libraries
library(datashieldclient)
library(dsBetaTestClient)
library(dsStatsClient)

#pollutant variables
myvar<-list('PM25_ESCAPE','PM10_ESC','PMcoarse_ESCAPE','NO2_ESCAPE')

#load loggin information
load('aprh/login-aprh.rda')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

#or.....only lifelines/ukb
study<-c('lifelines')
study<-c('ukb')
ld<-subset(logindata, server %in% study)
opals <- datashield.login(logins=ld,assign=TRUE,variables=myvar)


#build correlation matrix
ds.cor(x='D')
