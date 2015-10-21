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

#load loggin information: 'lifelines','ukb'
load('login-aprh.rda')

#login to datashield and assign data to 'D' as default
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

#here main data is 'D': select it once at start
main.data <- 'D'


################################################################################################################################################################################

################## -------------GENERATE THE SUBGROUP DATAFRAMES ----------------------------------------

#---------------subgroup of D by GENDER on server side
subgroup.names.gender <- run.get.subset(subvar='GENDER',vars.list=myvar,data=main.data)           #run once

# ------------- subroup of D by DIS_ASTHMA on server side
subgroup.asthma <- run.get.subset(subvar='DIS_ASTHMA',vars.list=myvar,data=main.data)             #run once

##------------#BINARIZE BMI CAT--------------------
# binarize the BMIcat_1_2_3 into BMIcat_0_1 (BMI>=30): 
run.changelevel('PM_BMI_CATEGORIAL',new.levels=c(0,0,1),new.varname='OBESE_STATUS',data=main.data) #here OBESE_STATUS
# now compute subgroup on server side 
subgroup.obese <- run.get.subset(subvar='OBESE_STATUS',vars.list=myvar,data=main.data)    #run once


##-----------BINARIZE SMOKE STATUS (history) ------------------
run.changelevel('SMK_STATUS',new.levels=c(0,1,1),new.varname='SMOKE_EVER',data=main.data)
#compute subgroup on server side
subgroup.smoke <- run.get.subset(subvar='SMOKE_EVER',vars.list=myvar,data=main.data)      #run once


##-----------subgroup of D by MEDI_ASTHMA_COPD
subgroup.astm.curr <- run.get.subset('DIS_ASTHMA_MEDS',myvar,main.data)                   #run once

##############---------- SELECT THE DATA THAT WILL BE USED FOR THE CORRESPONDING SUBGROUP  ANALYSIS ----------------------
#specify the name (character) of the dataframe that resulted from run.get.subset ex: D.GENDER0 or D.GENDER1

##------ by SEX ---------
data <- 'D.GENDER0'                                 # MÂLE
data <- 'D.GENDER1'                                 # FEMALE

## ------by OBESITY ----------
data <- 'D.OBESE_STATUS0'                           # BMI_<30
data <- 'D.OBESE_STATUS1'                           # BMI_>=30

## ------ by ASTHMA HISTORY ----
data <- 'D.DIS_ASTHMA0'                             # never
data <- 'D.DIS_ASTHMA1'                             # ever

## ----- by SMOKE HISTORY ------
data <- 'D.SMOKE_EVER0'                             # never
data <- 'D.SMOKE_EVER1'                             # previous + current



## ------ by ASTHMA CURRENT: DIS_ASTHMA_MEDS
data <- 'D.DIS_ASTHMA_MEDS0'                        #no current asthma 
data <- 'D.DIS_ASTHMA_MEDS1'                        #current asthma




################################################################################################################################################
                  ################################################################################
                  ##            running some regressions :multivariate                           ##
                  #################################################################################

## ---------- define models [model1, model2, model3] --------------------------------
# -----model_1(age ajusted)                                                                                     <-
model<-'AGE_YRS'

#-----model_2(Ajusted for age, sex, BMI, highest level of education, and smoking status)                         <-
model<-'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS'

# ----model_3(Adjusted for age, sex, BMI, highest level of education, and smoking status, pack-years smoked,>>..  <-
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

###############################################################################
# ------ pooled model to run [first create dummy study variables in server side by running run.dummy() once] ---------
run.dummy.study(data) #run once for each subgroup

########--------- RE-ADJUST THE MODEL HERE FOR EACH SUBGROUP TERMS -----------------------
### -----remove all subgroup terms in the model: we can use a keyword stem of the subgroup terms to remove 
### -----run it for each specific model

#------ by SEX ---------
model.sgrp <- run.adjust.subgroup.model(model,'gender')                        #ex: will remove all terms spelled like 'gender'

# ------by OBESITY ----------
model.sgrp <- run.adjust.subgroup.model(model,'bmi')                           #ex: will remove all terms spelled like 'bmi'

# ------ by ASTHMA HISTORY ----
model.sgrp <- run.adjust.subgroup.model(model,'asthma')                        #ex: will remove all terms spelled like 'asthma'

# ------ by SMOKE HISTORY ----
model.sgrp <- run.adjust.subgroup.model(model,'smk_status')                     #ex: will remove all terms spelled like 'SMK_STATUS' but not smk_passive

# ------ by ASTHMA CURRENT ----
model.sgrp <- run.adjust.subgroup.model(model,'asthma')                        #ex: will remove all terms spelled like 'asthma'


## ------------- RUN --------------------------------------------------------------------------------------------------
# --- exposure effect statistics:   MAKE SURE THAT STUDY DUMMY VARS ARE ALREADY CREATED
run.model(outcome,expo,model.sgrp,family = 'binomial',data,Ncases=T,ref ='lifelines') #ref is specified here

#split model run [ no need to create dummy study variable, but you need to specify the datasource] 
run.model(outcome,expo,model.sgrp,family = 'binomial',data,Ncases=T,datasources = opals[2]) #ukbiobank opals[2]

## -------- OPTIONAL ---------------------------------------------------------------------
#----- generate formula based on previously called expo, outcome, model.srgp and data.
#formula <- run.update.formula(outcome,expo,model.sgrp,data) #[ data = subgroup analysis data]
#glm.res<-run.meta.glm(formula,family='binomial',ref='lifelines')
#run.extract.glm.stats(glm.res)


## -------- -DESCRIPTIVE STATS OF THE OUTCOME VARS:  -------------------------------------
# ---run it for each outcome in a particular subgroup (data)
run.desc.stats(outcome,data)   


##-------------- NA STATS OF THE MODEL----------------------------------------------------
#this function assign NA.D.xxxx on server side and return the name of the assigned NA dataframe.
formula <- run.update.formula(outcome,expo,model.sgrp,data)
nadata <- run.NA.glm.subset(formula=formula)

#----- run NA stats 
run.desc.stats('AGE_YRS', data = nadata)
run.desc.stats('GENDER',data= nadata)
run.desc.stats('PM_BMI_CATEGORIAL',data= nadata)
run.desc.stats('EDU_HIGHEST_2',data= nadata)