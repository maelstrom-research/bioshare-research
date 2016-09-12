

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


#Study-specific quantiles and means
ds.summary(x='D$PM25_ESCAPE')
ds.summary(x='D$PM10_ESC')
ds.summary(x='D$PMcoarse_ESCAPE')
ds.summary(x='D$NO2_ESCAPE')

#Study-specific standard deviation
ds.var(x='D$PM25_ESCAPE', type='split')
sqrt(0.1245475)
sqrt(1.117355)
ds.var(x='D$PM10_ESC', type='split')
sqrt(0.4286679)
sqrt(3.606458)
ds.var(x='D$PMcoarse_ESCAPE', type='split')
sqrt(0.19376)
sqrt(0.8111445)
ds.var(x='D$NO2_ESCAPE', type='split')
sqrt(13.70513)
sqrt(57.3971)

#Pooled quantiles and mean
ds.quantileMean(x='D$PM25_ESCAPE')
ds.quantileMean(x='D$PM10_ESC')
ds.quantileMean(x='D$PMcoarse_ESCAPE')
ds.quantileMean(x='D$NO2_ESCAPE')

#Pooled standard deviation
ds.var(x='D$PM25_ESCAPE')
sqrt(0.987823)
ds.var(x='D$PM10_ESC')
sqrt(3.19185)
ds.var(x='D$PMcoarse_ESCAPE')
sqrt(0.730594)
ds.var(x='D$NO2_ESCAPE')
sqrt(52.03927)

####CORRELATION MATRICES########
#Study-specific correlation matrix
ds.cor(x='D')

#Pooled correlation matrix




