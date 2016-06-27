###########################################
#     DATA ASSIGNMENT IN EACH OPALS SERVER
##########################################

#load libraries
library(datashieldclient)
library(dsBetaTestClient)

#variables need of aprh
myvar<-list('AGE_YRS','AGE_YRS_CATEGORICAL','AGE_YRS_65','GENDER','EDU_HIGHEST_2','WORK_STATUS_CURRENT','SMK_STATUS','SMK_TBC_CURRENT','INCOME',
            'SMK_PASSIVE_ALL','SMK_PASSIVE_TIME','PM_BMI_CONTINUOUS','PM_BMI_CATEGORIAL','MEDI_ASTHMA_COPD','NO2_ESCAPE','DIS_ASTHMA',
            'PM25_ESCAPE','PM10_ESC','PMcoarse_ESCAPE','SYM_WHEEZ','SYM_WHEEZ_NOCOLD','SYM_SBREATH','SYM_SBREATH_WALK','SYM_BREATH_PRB',
            'SYM_PHLEGM_UP','SYM_PHLEGM_UP_FREQ','SYM_PHLEGM_DAY','SYM_PHLEGM_DAY_FREQ','SYM_COUGH_UP','SYM_COUGH_UP_FREQ','SYM_COUGH_DAY','SYM_COUGH_DAY_FREQ',
            'SYM_SBREATH_FLAT','SYM_SBREATH_EXERT','SYM_SBREATH_NIGHT','SYM_SBREATH_WOKEN','SYM_WHEEZ_SBREATH','SYM_SBREATH_NUMB'
)

#load loggin information
load('aprh/login-aprh.rda')
#study<-c('lifelines','ukb')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)