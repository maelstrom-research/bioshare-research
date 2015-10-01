################################################################
#
#                         SUBGROUP ANALYSIS
#  
###############################################################

# Source the bioshare environnment
source('utils_analysis.R',echo=F,print.eval=F)

#load libraries
library(datashieldclient)
library(dsBetaTestClient)

#variables need of aprh
myvar<-list('ADM_YRINT','AGE_YRS','AGE_YRS_CATEGORICAL','GENDER','EDU_HIGHEST_2','WORK_STATUS_CURRENT','SMK_STATUS','SMK_TBC_CURRENT','SMK_PACKYRS',
            'SMK_PASSIVE_HOME','SMK_PASSIVE_WORK','SMK_PASSIVE_ALL','SMK_PASSIVE_TIME','PM_BMI_CONTINUOUS','PM_BMI_CATEGORIAL','DIS_ASTHMA','DIS_ALLERG',
            'MEDI_ASTHMA_COPD','DIS_ASTHMA_MEDS','DIS_COPD','DIS_COPD_MEDS','DIS_ARRHY','DIS_AMI','DIS_CVA','DIS_ANGINA','DIS_DIAB','MEDI_DIAB','DIS_HBP',
            'PM_FEV1','PM_FEVC','PM_PEF','SYM_WHEEZ','SYM_SBREATH','NO2_ESCAPE','PM25_ESCAPE','PM25abs_ESC','PM10_ESC','NO2bg_ESCAPE','PMcoarse_ESCAPE',
            'RES_LENGTH','PM10_07_EU','NO2_06_EU','NO2_07_EU','NO2_05_EU','SYM_PHLEGM_UP','SYM_PHLEGM_DAY','SYM_PHLEGM_UP_FREQ','SYM_PHLEGM_DAY_FREQ')

#load loggin information
#load('/home/datashield/aprh/logindata.aprh.rda')
load('login-aprh.rda')
study<-c('lifelines','ukb')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

#here main data is 'D'
main.data <- 'D'

##################################################################################################################

############ GENERATE THE SUBGROUP DATAFRAMES #####################
# ex: here we are computing subset of all vars in myvar from D by GENDER. This function will return the names of the subgroup dataframes stored on server side 
subgroup.names.gender <- run.get.subset(subvar='GENDER',vars.list=myvar,data=main.data)             #run once

# here we are computing subset of vars in myvar from D by PM_BMI_CATEGORIAL. This function will return the names of the subgroup dataframes stored on server side  
subgroup.names.bmi <- run.get.subset(subvar='PM_BMI_CATEGORIAL',vars.list=myvar,data=main.data)     #run once


###################    DATA THAT WILL BE USED FOR THE SUBGROUP  ANALYSIS      ################################ 
#specify the name (character) of the dataframe that result from run.get.subset ex: D.GENDER0 or D.GENDER1
#data name depend on the subgroup we want to study so his value change frequently. Here the sugroup is GENDER0(male)

#data <- 'D.GENDER0' # 0 OR 1
data <- 'D.PM_BMI_CATEGORIAL2' # 1, 2 OR 3 

###################################################################################################################
#################################################################################
##            running some regressions :multivariate                           ##
#################################################################################

####################### define models [model1, model2, model3]
###model_1(age ajusted)                                                                                     <-
model<-'AGE_YRS'

#model_2(Ajusted for age, sex, BMI, highest level of education, and smoking status)                         <-
model<-'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS'

# model_3(Adjusted for age, sex, BMI, highest level of education, and smoking status, pack-years smoked,>>..  <-
#>>...length at baseline residence, exposure to second-hand smoke, and self-declared allergies)      			
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+RES_LENGTH+SMK_PASSIVE_ALL' #+SMK_PACKYRS


#######################
expo <- 'PM25_ESCAPE'
#expo <- 'PM10_ESC'
#expo <- 'NO2_ESCAPE'
#expo <- 'PMcoarse_ESCAPE'

####################
outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_SBREATH'
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'
#outcome <- 'MEDI_ASTHMA_COPD'


##########################################################
# remove all subgroup terms in the model: we can use "data" because this variable has some information on the subgroup 
#run for each specific model
model.sgrp <- run.adjust.subgroup.model(model,data)

#pooled model to run [first create dummy study variables in server side by running run.dummy() once]
run.dummy.study(data) #run once for each subgroup

#exposure effect statistics  [see data = 'D' for main analysis]
run.model(outcome,expo,model.sgrp,family = 'binomial',data,Ncases=T,ref ='lifelines') #ref is specified here

#split model run [ no need to create dummy study variable, but you need to specify the datasource] 
run.model(outcome,expo,model.sgrp,family = 'binomial',data,Ncases=T,datasources = opals[2]) #ukbiobank opals[2]


#generate formula based on previously called expo, outcome, model.srgp and data.
formula <- run.update.formula(outcome,expo,model.sgrp,data) #[ data = 'D' main analysis]

glm.res<-run.meta.glm(formula,family='binomial',ref='lifelines')
run.extract.glm.stats(glm.res)


################# NA STATS ##########################

#### ex: create NA subset = 'NA.D.GENDER0' if sugroup is GENDER0 (mâle)
#this function assign NA.D.xxxx on server side and return the name of the assign NA dataframe.
nadata <- run.NA.glm.subset(formula=formula)

#check number of missing cases
ds.dim(nadata)

#### run NA stats 
run.NA.stats('AGE_YRS',iscat=F, na.data = nadata)
#run.NA.stats('GENDER',na.data= nadata)
run.NA.stats('PM_BMI_CATEGORIAL',na.data= nadata)
run.NA.stats('EDU_HIGHEST_2',na.data= nadata)
