###########################################
######    SYMPTOM VALIDATION   ############
###########################################


# Source the bioshare environnment
source('utils/utils_analysis.R',echo=F,print.eval=F)

#load libraries
library(datashieldclient)
library(dsBetaTestClient)

#variables need of aprh
myvar<-list('AGE_YRS','AGE_YRS_CATEGORICAL','AGE_YRS_65', 'GENDER','EDU_HIGHEST_2','WORK_STATUS_CURRENT','SMK_STATUS','SMK_TBC_CURRENT','INCOME',
            'SMK_PASSIVE_ALL','SMK_PASSIVE_TIME','PM_BMI_CONTINUOUS','PM_BMI_CATEGORIAL','DIS_ASTHMA', 'MEDI_ASTHMA_COPD','NO2_ESCAPE',
            'PM25_ESCAPE','PM10_ESC','PMcoarse_ESCAPE','SYM_WHEEZ','SYM_WHEEZ_NOCOLD','SYM_SBREATH','SYM_SBREATH_WALK','SYM_BREATH_PRB',
            'SYM_PHLEGM_UP','SYM_PHLEGM_UP_FREQ','SYM_PHLEGM_DAY','SYM_PHLEGM_DAY_FREQ','SYM_COUGH_UP','SYM_COUGH_UP_FREQ','SYM_COUGH_DAY','SYM_COUGH_DAY_FREQ'
)

#load loggin information
load('aprh/login-aprh.rda')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

#or.....only lifelines/ukb
study<-c('lifelines')
study<-c('ukb')
ld<-subset(logindata, server %in% study)
opals <- datashield.login(logins=ld,assign=TRUE,variables=myvar)



#HARMONIZE SBREATH_EXT
ds.assign('D$SYM_SBREATH','SYM_SBREATH_EXT',datasources=opals[1])
ds.assign('D$SYM_SBREATH_WALK','SYM_SBREATH_EXT',datasources=opals[2])
ds.cbind(c('SYM_SBREATH_EXT','D'),newobj='D')
run.rm('SYM_SBREATH_EXT')

#Recode smoking into two categories
ds.recodeLevels(x='D$SMK_STATUS', newCategories=c('1','2','2'), newobj='SMK_STATUS_NEW')
ds.cbind(c('SMK_STATUS_NEW','D'),newobj='D')
ds.levels(x='SMK_STATUS_NEW')
ds.table1D(x='SMK_STATUS_NEW', type='split')
ds.table1D(x='D$SMK_STATUS', type='split')


##### POOLED symptoms #########

#3-category smoking satus
ds.glm(formula='D$SYM_WHEEZ~D$ukb+D$SMK_STATUS', family='binomial')
ds.glm(formula='D$SYM_SBREATH_EXT~D$ukb+D$SMK_STATUS', family='binomial')

#2-cat smoking status
ds.glm(formula='D$SYM_WHEEZ~D$ukb+D$SMK_STATUS_NEW', family='binomial')
ds.glm(formula='D$SYM_SBREATH_EXT~D$ukb+D$SMK_STATUS_NEW', family='binomial')

#passive smoke exposure
ds.glm(formula='D$SYM_WHEEZ~D$ukb+D$SMK_PASSIVE_ALL', family='binomial')
ds.glm(formula='D$SYM_SBREATH_EXT~D$ukb+D$SMK_PASSIVE_ALL', family='binomial')



### LifeLines symptoms ########

#3-category smoking satus
ds.glm(formula='D$SYM_WHEEZ~D$SMK_STATUS', family='binomial')
ds.glm(formula='D$SYM_SBREATH~D$SMK_STATUS', family='binomial')
ds.glm(formula='D$SYM_WHEEZ_NOCOLD~D$SMK_STATUS', family='binomial')
ds.glm(formula='D$SYM_PHLEGM_UP~D$SMK_STATUS', family='binomial')
ds.glm(formula='D$SYM_PHLEGM_DAY~D$SMK_STATUS', family='binomial')

#2-cat smoking status
ds.glm(formula='D$SYM_WHEEZ~D$SMK_STATUS_NEW', family='binomial')
ds.glm(formula='D$SYM_SBREATH~D$SMK_STATUS_NEW', family='binomial')
ds.glm(formula='D$SYM_WHEEZ_NOCOLD~D$SMK_STATUS_NEW', family='binomial')
ds.glm(formula='D$SYM_PHLEGM_UP~D$SMK_STATUS_NEW', family='binomial')
ds.glm(formula='D$SYM_PHLEGM_DAY~D$SMK_STATUS_NEW', family='binomial')


#passive smoke exposure
ds.glm(formula='D$SYM_WHEEZ~D$SMK_PASSIVE_ALL', family='binomial')
ds.glm(formula='D$SYM_SBREATH~D$SMK_PASSIVE_ALL', family='binomial')
ds.glm(formula='D$SYM_WHEEZ_NOCOLD~D$SMK_PASSIVE_ALL', family='binomial')
ds.glm(formula='D$SYM_PHLEGM_UP~D$SMK_PASSIVE_ALL', family='binomial')
ds.glm(formula='D$SYM_PHLEGM_DAY~D$SMK_PASSIVE_ALL', family='binomial')



##### UK Biobank symptoms #########

#3-category smoking satus
ds.glm(formula='D$SYM_WHEEZ~D$SMK_STATUS', family='binomial')
ds.glm(formula='D$SYM_SBREATH_WALK~D$SMK_STATUS', family='binomial')

#2-cat smoking status
ds.glm(formula='D$SYM_WHEEZ~D$SMK_STATUS_NEW', family='binomial')
ds.glm(formula='D$SYM_SBREATH_WALK~D$SMK_STATUS_NEW', family='binomial')

#passive smoke exposure
ds.glm(formula='D$SYM_WHEEZ~D$SMK_PASSIVE_ALL', family='binomial')
ds.glm(formula='D$SYM_SBREATH_WALK~D$SMK_PASSIVE_ALL', family='binomial')




####################
outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_WHEEZ_NOCOLD'
outcome <- 'SYM_SBREATH'
outcome <- 'SYM_SBREATH_WALK'
#outcome <- 'SYM_SBREATH_EXT'
#outcome <- 'SYM_BREATH_PRB'
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
outcome <- 'SYM_PHLEGM_DAY_FREQ'
outcome <- 'SYM_SBREATH_EXT'


#pooled model to run [first create dummy study variables in server side by running run.dummy() once]
run.dummy.study(data) #run once 
datashield.symbols(opals) # check objects on server side


#generate formula based on previously called expo, outcome, model and data.
formula <- run.make.formula(outcome,expo,data) #[ data = 'D' main analysis]

glm.res<-run.meta.glm(formula,family='binomial',ref='lifelines')
run.extract.glm.stats(glm.res, pval=T)
print(glm.res)

