################################################################
#
#                 APRH DESCRIPTIVE STATISTICS
#
###############################################################

# Source the bioshare environnment
source('utils_analysis.R',echo=F,print.eval=F)

#load libraries
library(datashieldclient)
library(dsBetaTestClient)

#variables need of aprh
myvar<-list('AGE_YRS','AGE_YRS_CATEGORICAL','GENDER','EDU_HIGHEST_2','WORK_STATUS_CURRENT','SMK_STATUS','SMK_TBC_CURRENT','INCOME',
            'SMK_PASSIVE_ALL','SMK_PASSIVE_TIME','PM_BMI_CONTINUOUS','PM_BMI_CATEGORIAL','MEDI_ASTHMA_COPD','NO2_ESCAPE',
            'PM25_ESCAPE','PM10_ESC','PMcoarse_ESCAPE','SYM_WHEEZ','SYM_WHEEZ_NOCOLD','SYM_SBREATH','SYM_SBREATH_WALK','SYM_BREATH_PRB',
            'SYM_PHLEGM_UP','SYM_PHLEGM_UP_FREQ','SYM_PHLEGM_DAY','SYM_PHLEGM_DAY_FREQ','SYM_COUGH_UP','SYM_COUGH_UP_FREQ','SYM_COUGH_DAY','SYM_COUGH_DAY_FREQ'
            )

#load loggin information
load('login-aprh.rda')
#study<-c('lifelines','ukb')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
###########################################################################################

########################################################
#           univariate: outcome~confounding
#######################################################

########### PART 1 #########################
# glm to compute ttest
confounding1 <- list('AGE_YRS','SMK_PACKYRS','RES_LENGTH')

####################
outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_WHEEZ_NOCOLD'
#outcome <- 'SYM_SBREATH'
#outcome <- 'SYM_SBREATH_WALK'
#outcome <- 'SYM_BREATH_PRB'
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'
#outcome <- 'SYM_SBREATH_EXT'

result.part1<-list()
for (conf in confounding1){
  formula.update <- paste0('D$',conf,'~','D$',outcome)
  
  glm.res<-ds.glm(formula.update,family='gaussian') 
  glm.stat<-run.extract.glm.stats(glm.res)
  glm.stat<-structure(list(glm.stat),.Names=conf)
  result.part1<-c(result.part1,glm.stat)
}

#print part1 stats per outcome
print(result.part1) 

################## PART 2 ####################
### CHI-SQUARE 
confounding2 <- list('GENDER','PM_BMI_CATEGORIAL','EDU_HIGHEST_2','SMK_STATUS','DIS_ALLERG','SMK_PASSIVE_ALL')
result.part2 <- run.cat(outcome,confounding2) #type = 'split'
#print part2 p.value(s) per outcome
lapply(result.part2,function(x){ format(x$chi2Test$pooled$p.value,digits=5)})

