#loading important (or required) packages

library(opal)                         
library(dsBaseClient)                 
library(dsStatsClient)  	     
library(dsModellingClient)
library(datashieldclient)

#the variables needed for the HOP 
myvar <- list('DIET_VEGETARIAN', 'DIET_VEGETARIAN_VERIFIED', 'AGE_YRS', 'AGE_YRS_CATEGORICAL', 'GENDER', 'EDU_HIGHEST_1', 'WORK_STATUS_CURRENT', 'SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL', 'PM_WAIST_SIZE', 'PM_SYSTOLIC_MEASURE', 'PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING', 'LAB_HDL', 'LAB_TRIG', 'LAB_hsCRP', 'METABSYNDR_NBR_STRICT', 'METABSYNDR_STRICT', 'METABSYNDR_NBR_MODERATE', 'METABSYNDR_MODERATE')

#load loggin information
load("~datashield/hop/logindata.hop.rda")

#only studies participating in MetS and Vegetarian diet:  finrisk, kora, lifelines, mitchelstown, cartagene
ld<-subset(logindata,(server=="!finrisk")|(server=="kora")|(server=="lifelines")|(server=="mitchelstown")|(server=="!ncds")|(server=="!ship")|(server=="!prevend")|(server=="!chris")|(server=="!micros"))


opals <- datashield.login(logins=ld,assign=TRUE,variables=myvar)


ds.table2D(x='D$GENDER', y='D$AGE_YRS_CATEGORICAL', type="split")

ds.isValid(x='D$AGE_YRS')
ds.isValid(x='D$DIET_VEGETARIAN')
?ds.isValid

ds.summary(x='D$AGE_YRS')

ds.table2D(x='D$DIET_VEGETARIAN', y='D$GENDER', type='split')

ds.table2D(x='D$DIET_VEGETARIAN', y='D$WORK_STATUS_CURRENT')

ds.table2D(x='D$DIET_VEGETARIAN', y='D$PM_BMI_CATEGORIAL')
ds.table2D(x='D$DIET_VEGETARIAN', y='D$PM_BMI_CATEGORIAL', type='split')

ds.meanByClass(x='D', outvar='PM_WAIST_SIZE', covar='DIET_VEGETARIAN', type='combine')
ds.meanByClass(x='D', outvar='PM_WAIST_SIZE', covar='DIET_VEGETARIAN', type='split')


ds.meanByClass(x='D', outvar='LAB_GLUC_FASTING', covar='DIET_VEGETARIAN')
ds.meanByClass(x='D', outvar='LAB_HDL', covar='DIET_VEGETARIAN')
ds.meanByClass(x='D', outvar='LAB_TRIG', covar='DIET_VEGETARIAN')
ds.meanByClass(x='D', outvar='LAB_hsCRP', covar='DIET_VEGETARIAN')

ds.glm(formula=D$DIET_VEGETARIAN~D$GENDER,family='binomial')
ds.glm(formula=D$PM_WAIST_SIZE~D$DIET_VEGETARIAN,family='gaussian')
ds.glm(formula=D$LAB_GLUC_FASTING~D$DIET_VEGETARIAN,family='gaussian')


datashield.logout(opals)