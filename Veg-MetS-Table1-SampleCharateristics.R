#loading important (or required) packages

library(opal)                         
library(dsBaseClient)                 
library(dsStatsClient)  	     
library(dsModellingClient)
library(datashieldclient)

#the variables needed for the HOP 
myvar <- list('DIET_VEGETARIAN', 'DIET_VEGETARIAN_VERIFIED', 'AGE_YRS', 'AGE_YRS_CATEGORICAL', 'GENDER', 'EDU_HIGHEST_1', 
              'WORK_STATUS_CURRENT', 'SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL', 'PM_WAIST_SIZE', 
              'PM_SYSTOLIC_MEASURE', 'PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING', 'LAB_HDL', 'LAB_TRIG', 'LAB_hsCRP', 
              'METABSYNDR_NBR_STRICT', 'METABSYNDR_STRICT', 'METABSYNDR_NBR_MODERATE', 'METABSYNDR_MODERATE')

#load loggin information
load("~datashield/hop/logindata.hop.rda")

#only studies participating in MetS and Vegetarian diet:  finrisk, kora, lifelines, mitchelstown, cartagene
#finrisk and cartagene are not ready yet
study<-c('kora','mitchelstown','lifelines')
ld<-subset(logindata, server %in% study)

#login to datashield and assign data to 'D'
opals <- datashield.login(logins=ld,assign=TRUE,variables=myvar)

#verify the validity of the whole dataframe 'D'
suppressWarnings(all(ds.isValid('D')))

############################################
#   REORGANIZE DATA BY VEGETARIAN (yes/no)
# SUBCLASS AND ASSIGN TO D_VEG_0 AND D_VEG_1
ds.subclass('D','D_VEG','DIET_VEGETARIAN')
ds.assign('D_VEG$DIET_VEGETARIAN.level_1','D_VEG_1')
ds.assign('D_VEG$DIET_VEGETARIAN.level_0','D_VEG_0')



############age mean by vegetarian for each study##################
ds.isValid('D$AGE_YRS')
age_by_veg_split<-ds.meanByClass('D','AGE_YRS','DIET_VEGETARIAN',type='split')
##compute the corresponding t-test using ds.glm for AGE_YRS~DIET_VEGETARIAN
glm_age_vs_veget_split<-lapply(opals,function(op){ds.glm(D$AGE_YRS~D$DIET_VEGETARIAN,'gaussian',datasources=list(op))})
##compute the t.test using data from D_VEG_XXX
ttest_age_by_Veg_split<-ds.tTest('D_VEG_1$AGE_YRS','D_VEG_0$AGE_YRS',paired=F,type='split')
pvalue_age_by_veg_split<-lapply(ttest_age_by_Veg_split,function(x){x$p.value})


#####age mean by vegetarian for the combined valid studies############
age_by_veg_comb<-ds.meanByClass('D','AGE_YRS','DIET_VEGETARIAN')
age_by_veg_comb<-data.frame(age_by_veg_comb,stringsAsFactors=F)
##compute the corresponding t-test using ds.glm for AGE_YRS~DIET_VEGETARIAN
glm_age_vs_veget_comb<-ds.glm(D$AGE_YRS~D$DIET_VEGETARIAN,'gaussian')
##compute the t.test using data from D_VEG_XXX
ttest_age_by_Veg_comb<-ds.tTest('D_VEG_1$AGE_YRS','D_VEG_0$AGE_YRS',paired=F)
pvalue_age_by_veg_comb<-as.numeric(ttest_age_by_Veg_comb$p.value)

file_name<-paste0('RESULTS_ver::',Sys.Date(),'.Rdata')
save(age_by_veg_comb,age_by_veg_split,pvalue_age_by_veg_comb,pvalue_age_by_veg_split,ttest_age_by_Veg_comb,ttest_age_by_Veg_split,file=file_name)






datashield.logout(opals)