################################################################
#
#                         MAIN ANALYSIS
#
###############################################################

# Attach the DS UTILS environnment
source('utils/utils_analysis.R',echo=F,print.eval=F)

# LOAD DATAs IN EACH OPAL SERVER
source('aprh/aprh_data.R',echo=F,print.eval=F)


########################     DATA THAT WILL BE USED FOR THE MAIN ANALYSIS      ############
#specify the name (character) of the dataframe that was assigned in datashield.login ('D' by default)
data <- 'D'

#################################################################################
##            running some regressions :multivariate                           ##
#################################################################################

####################### define models [model0,model1, model2a,model2b model3a,model3b]
### model0 (unajusted)
model <- NULL

###model_1(ajusted for age,sex )                                                                                     <-
model<-'AGE_YRS+GENDER'

#Model 2a (adjusted for age, sex, bmi, highest level of education)                        <-
model<-'AGE_YRS+GENDER+EDU_HIGHEST_2+PM_BMI_CATEGORIAL'

#Model 2b (adjusted for age, sex, bmi, highest level of education and household income)
model <- 'AGE_YRS+GENDER+EDU_HIGHEST_2+PM_BMI_CATEGORIAL+INCOME'

# Model 3a: adjusted for age, sex, bmi, highest level of education, smoking status, and exposure to second-hand tobacco smoke  				
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL'

#Model 3b: adjusted for age, sex, bmi, highest level of education, household income, smoking status, and exposure to second-hand tobacco smoke 
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'



#######################
expo <- 'PM25_ESCAPE'
#expo <- 'PM10_ESC'
#expo <- 'NO2_ESCAPE'
#expo <- 'PMcoarse_ESCAPE'

####################
#Pooled outcomes
#outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_SBREATH_EXT'

###UKBB only variables (asthma symptom)
#outcome <- 'SYM_SBREATH_WALK'

###Lifelines only variables
# Asthma symptoms
#outcome <- 'SYM_SBREATH'
#outcome <- 'SYM_WHEEZ_NOCOLD'
#outcome <- 'SYM_WHEEZ_SBREATH'
#outcome <- 'SYM_SBREATH_FLAT'
#outcome <- 'SYM_SBREATH_EXERT'
#outcome <- 'SYM_SBREATH_NIGHT'
#outcome <- 'SYM_SBREATH_WOKEN'
#outcome <- 'SYM_SBREATH_NUMB'


###COPD symptoms
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'

#Other symptoms
#outcome <- 'SYM_BREATH_PRB'


#pooled model to run [first create dummy study variables in server side by running run.dummy() once]
run.dummy.study(data) #run once 
datashield.symbols(opals) # check objects on server side

#exposure effect statistics  [see data = 'D' for main analysis]
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T,pval = F, ref ='lifelines',check=F) #ref is specified here

#UKBIOBANK REGRESSION 
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T, pval = F, datasources = opals[2],check=F)


#LIFELINES REGRESSION 
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T, pval = F, datasources = opals[1],check=F)




