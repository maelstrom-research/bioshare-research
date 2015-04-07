rm(list=ls());gc()
#loading important (or required) packages

library(opal)                         
library(dsBaseClient)                 
library(dsStatsClient)         
library(dsModellingClient)
library(datashieldclient)

#the variables needed for the HOP 
myvar <- list('DIET_VEGETARIAN', 'DIET_VEGETARIAN_VERIFIED', 'AGE_YRS', 'AGE_YRS_CATEGORICAL', 'GENDER', 'EDU_HIGHEST_2', 
              'WORK_STATUS_CURRENT', 'SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL', 'PM_WAIST_SIZE', 
              'PM_SYSTOLIC_MEASURE', 'PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING', 'LAB_HDL', 'LAB_TRIG', 'LAB_hsCRP', 
              'METABSYNDR_NBR_STRICT', 'METABSYNDR_STRICT', 'METABSYNDR_NBR_MODERATE', 'METABSYNDR_MODERATE')

#load loggin information
load("~datashield/hop/logindata.hop.rda")

#only studies participating in MetS and Vegetarian diet:  finrisk, kora, lifelines, mitchelstown, cartagene
#cartagene are not ready yet
study<-c('mitchelstown','lifelines','kora','finrisk')
ld<-subset(logindata, server %in% study)

#login to datashield and assign data to 'D'
opals <- datashield.login(logins=ld,assign=TRUE,variables=myvar)

#verify the validity of the whole dataframe 'D'
message('\n\nTesting Data validity for all studies...')
suppressWarnings(all(ds.isValid('D')))
message('VALIDITY TESTING DONE')


#Define variable to subset by
sub_by<-'DIET_VEGETARIAN'                       #<=== may vary often: ALWAYS CALL THIS VARIABLE 


##########################################################################
#
#         ******CONTINUOUS VARIABLES STATISTICS **** 
#
##########################################################################
var_cont<-list( 'AGE_YRS','PM_WAIST_SIZE','PM_SYSTOLIC_MEASURE','PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING','LAB_HDL', 'LAB_TRIG','LAB_hsCRP')


#################
#STATS WITH SPLIT
#################
source('continuous_var_stats_split.R',echo=F,print.eval=T)

####################
#STATS WITH COMBINED
####################
source('continuous_var_stats_combined.R',echo=F,print.eval=T)

#################################################
# STATS WITH DOUBLE SUBSET 

sub_by2<-'PM_BMI_CATEGORIAL'                 #<=== update first subset by: ALWAYS CALL THIS VARIABLE 
sub_double_by<-sub_by             #<----VARIABLE USED AS DOUBLE SUBSET SOULD BE ONLY TWO CATEGORIES BECAUSE OF ttest

var_cont_double<-list('PM_SYSTOLIC_MEASURE','PM_DIASTOLIC_MEASURE','LAB_GLUC_FASTING','LAB_HDL','LAB_TRIG','LAB_hsCRP')



################################
#STATS WITH DOUBLE SUBSET SPLIT
################################
source('cont_double_sub_stats_split.R',echo=F,print.eval=T)

##################################
#STATS WITH DOUBLE SUBSET COMBINED
###################################
source('cont_double_sub_stats_combined.R',echo=F,print.eval=T)  


######################################################################################
#
#         ******CATEGORICAL VARIABLES STATISTICS **** 
#
#####################################################################################

var_cat<-list('AGE_YRS_CATEGORICAL','GENDER','SMK_CIG_CURRENT','ALC_CURRENT',
              'PM_BMI_CATEGORIAL','METABSYNDR_NBR_STRICT','METABSYNDR_STRICT',
              'METABSYNDR_NBR_MODERATE','METABSYNDR_MODERATE','EDU_HIGHEST_2') #,'WORK_STATUS_CURRENT'

#################
#STATS WITH SPLIT
#################
source('categorical_var_stats_split.R',echo=F,print.eval=T)

####################
#STATS WITH COMBINED
####################
source('categorical_var_stats_combined.R',echo=F,print.eval=T)

#datashield.logout(opals)
# clean workspace 
to_rm<-ls()[-grep('^opals$',ls())]
rm(to_rm,list=to_rm)