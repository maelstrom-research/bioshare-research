################################################################
#
#           MULTIPLE GLM RUNS FOR MAIN ANALYSIS
#
###############################################################

# Source the bioshare environnment
source('utils_analysis.R',echo=F,print.eval=F)

#load libraries
library(datashieldclient)
library(dsBetaTestClient)

#variables need of aprh
myvar<-list('AGE_YRS','AGE_YRS_CATEGORICAL','GENDER','EDU_HIGHEST_2','WORK_STATUS_CURRENT','SMK_STATUS','SMK_TBC_CURRENT','INCOME',
            'SMK_PASSIVE_ALL','SMK_PASSIVE_TIME','PM_BMI_CONTINUOUS','PM_BMI_CATEGORIAL','MEDI_ASTHMA_COPD','NO2_ESCAPE','DIS_ASTHMA',
            'PM25_ESCAPE','PM10_ESC','PMcoarse_ESCAPE','SYM_WHEEZ','SYM_WHEEZ_NOCOLD','SYM_SBREATH','SYM_SBREATH_WALK','SYM_BREATH_PRB',
            'SYM_PHLEGM_UP','SYM_PHLEGM_UP_FREQ','SYM_PHLEGM_DAY','SYM_PHLEGM_DAY_FREQ','SYM_COUGH_UP','SYM_COUGH_UP_FREQ','SYM_COUGH_DAY','SYM_COUGH_DAY_FREQ'
)

#load loggin information
load('login-aprh.rda')
#study<-c('lifelines','ukb')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)


########################     DATA THAT WILL BE USED FOR THE MAIN ANALYSIS      ############
#specify the name (character) of the dataframe that was assigned in datashield.login ('D' by default)
data <- 'D'

#################################################################################
##            running some regressions :multivariate                           ##
#################################################################################

####################### define models [model0,model1, model2a,model2b model3a,model3b]
### model0 (unajusted)
model <- NULL

###model_1(ajusted for age,sex )                                                         <-
model<-'AGE_YRS+GENDER'

#Model 2a (adjusted for age, sex, bmi, highest level of education)                        <-
model<-'AGE_YRS+GENDER+EDU_HIGHEST_2+PM_BMI_CATEGORIAL'

#Model 2b (adjusted for age, sex, bmi, highest level of education and household income)
model <- 'AGE_YRS+GENDER+EDU_HIGHEST_2+PM_BMI_CATEGORIAL+INCOME'

# Model 3a: adjusted for age, sex, bmi, highest level of education, smoking status, and exposure to second-hand tobacco smoke    			
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL'

#Model 3b: adjusted for age, sex, bmi, highest level of education, household income, smoking status, and exposure to second-hand tobacco smoke 
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'



####----------- MULTIPLE GLMS RUN AT A TIME-----------------


# define sets of outcomes and expos: ALWAYS LOAD THESES 2 SET OF VARIABLES 
expos.c <- c('PM25_ESCAPE','PM10_ESC','NO2_ESCAPE','PMcoarse_ESCAPE')

outcomes.c <- c(
  'SYM_WHEEZ', 
  'SYM_WHEEZ_NOCOLD',
  'SYM_SBREATH',
  #'SYM_SBREATH_WALK',
  'SYM_BREATH_PRB',
  'SYM_PHLEGM_UP',
  'SYM_PHLEGM_UP_FREQ',
  'SYM_PHLEGM_DAY',
  'SYM_PHLEGM_DAY_FREQ'
  #'SYM_SBREATH_EXT'
)



#by 
#####################
expo <- 'PM25_ESCAPE'
expo <- 'PM10_ESC'
expo <- 'NO2_ESCAPE'
expo <- 'PMcoarse_ESCAPE'

####################
outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_WHEEZ_NOCOLD'
#outcome <- 'SYM_SBREATH'
outcome <- 'SYM_SBREATH_WALK'
#outcome <- 'SYM_BREATH_PRB'
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'
outcome <- 'SYM_SBREATH_EXT'



##### many glms either by expo or by outcomes (by model not yet ready) and stack them 
#####single
#lifelines: run by outcome
glm.stack <- run.stack.glm.by(expo=expo,outcome=outcomes.c,model,data,fam='binomial',by='outcome',datasources=opals[1])
#ukb: run by expo
glm.stack <- run.stack.glm.by(expo=expos.c,outcome=outcome,model,data,fam='binomial',by='expo',datasources=opals[2]) #ukb


#pooled: run by expo
glm.stack <- run.stack.glm.by(expo=expos.c,outcome=outcome,model,data,fam='binomial',ref = 'lifelines',by='expo',datasources=opals)

print(glm.stack)




#####CLOSE EVERYTHING
run.close(all = TRUE)
