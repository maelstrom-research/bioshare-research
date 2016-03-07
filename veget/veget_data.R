###########################################
#     DATA ASSIGNMENT IN EACH OPALS SERVER
##########################################

#load libraries
library(datashieldclient)
library(dsBetaTestClient)

#the variables needed for the HOP 
myvar <- list('DIET_VEGETARIAN', 'DIET_VEGETARIAN_VERIFIED', 'AGE_YRS', 'AGE_YRS_CATEGORICAL', 'GENDER', 'EDU_HIGHEST_1', 'EDU_HIGHEST_2',
              'WORK_STATUS_CURRENT', 'SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL', 'PM_WAIST_SIZE', 
              'PM_SYSTOLIC_MEASURE', 'PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING', 'LAB_HDL', 'LAB_TRIG', 'LAB_hsCRP', 
              'METABSYNDR_NBR_STRICT', 'METABSYNDR_STRICT', 'METABSYNDR_NBR_MODERATE', 'METABSYNDR_MODERATE', 'HLTH_OBESE_STRICT', 'HLTH_OBESE_MODERATE')


#load loggin information
load('login-veget.rda')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)