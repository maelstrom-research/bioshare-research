source('utils_analysis.R',echo=F,print.eval=F)

#Adjusted for age, sex, BMI, highest level of education, and smoking status,
#pack-years smoked, length at baseline residence, exposure to second-hand smoke, and self-declared allergies

library(datashieldclient)

#variables need of aprh
myvar<-list('ADM_YRINT','AGE_YRS','AGE_YRS_CATEGORICAL','GENDER','EDU_HIGHEST_2','WORK_STATUS_CURRENT','SMK_STATUS','SMK_TBC_CURRENT','SMK_PACKYRS',
            'SMK_PASSIVE_HOME','SMK_PASSIVE_WORK','SMK_PASSIVE_ALL','SMK_PASSIVE_TIME','PM_BMI_CONTINUOUS','PM_BMI_CATEGORIAL','DIS_ASTHMA','DIS_ALLERG',
            'MEDI_ASTHMA_COPD','DIS_ASTHMA_MEDS','DIS_COPD','DIS_COPD_MEDS','DIS_ARRHY','DIS_AMI','DIS_CVA','DIS_ANGINA','DIS_DIAB','MEDI_DIAB','DIS_HBP',
            'PM_FEV1','PM_FEVC','PM_PEF','SYM_WHEEZ','SYM_SBREATH','NO2_ESCAPE','PM25_ESCAPE','PM25abs_ESC','PM10_ESC','NO2bg_ESCAPE','PMcoarse_ESCAPE',
            'RES_LENGTH','PM10_07_EU','NO2_06_EU','NO2_07_EU','NO2_05_EU','SYM_PHLEGM_UP','SYM_PHLEGM_DAY','SYM_PHLEGM_UP_FREQ','SYM_PHLEGM_DAY_FREQ')

#load loggin information
load('/home/datashield/aprh/logindata.aprh.rda')

study<-c('lifelines','ukb')

#login to datashield and assign data to 'D'
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)


##############################################################################################################
#################################################################################
##                         running some regressions                            ##
#################################################################################

expo <- 'PMcoarse_ESCAPE'
#expo <- 'PM25_ESCAPE'
#expo <- 'PM10_ESC'
#expo <- 'NO2_ESCAPE'

####################
#outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_SBREATH'
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'
#outcome <- 'MEDI_ASTHMA_COPD'


#####################
###model_1(age ajusted)
formula<-'~D$AGE_YRS'

#model_2(Ajusted for age, sex, BMI, highest level of education, and smoking status)
#formula<-'~D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_STATUS'

# model_3(Adjusted for age, sex, BMI, highest level of education, and smoking status, pack-years smoked,... 
#...length at baseline residence, exposure to second-hand smoke, and self-declared allergies)  					
#formula <- '~D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_STATUS+D$SMK_PACKYRS+D$RES_LENGTH+D$SMK_PASSIVE_ALL'

#update formula
formula <- paste0('D$',outcome,formula,'+D$',expo)

##########################
#run glm 
glm.res <- run.meta.glm(formula,'binomial',opals,print=T)

#extract glm stats and process result
glm.stats<-run.extract.glm.stats(glm.res)

#print glm stats
print(glm.stats)


####################################################################################
#           univariate: outcome~confounding
####################################################################################

########### PART 1 #########################
# glm for to compute ttest
confounding1 <- list('AGE_YRS','SMK_PACKYRS','RES_LENGTH')#,GENDER,'EDU_HIGHEST_2','SMK_STATUS','DIS_ALLERG','SMK_PASSIVE_ALL'

####################
#outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_SBREATH'
#outcome <- 'SYM_PHLEGM_UP'
outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'
#outcome <- 'MEDI_ASTHMA_COPD'

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
result.part2 <- run.cat(outcome,confounding2)
#print part2 p.value(s) per outcome
lapply(result.part2,function(x){ format(x$chi2Test$pooled$p.value,digits=5)})

