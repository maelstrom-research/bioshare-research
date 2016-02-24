################################################################
#
#                         MAIN ANALYSIS
#
###############################################################

# Attach the DS UTILS environnment
source('utils_analysis.R',echo=F,print.eval=F)

# LOAD DATAs IN EACH OPAL SERVER
source('aprh_data.R',echo=F,print.eval=F)


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

#pooled model to run [first create dummy study variables in server side by running run.dummy() once]
run.dummy.study(data) #run once 
datashield.symbols(opals) # check objects on server side

#exposure effect statistics  [see data = 'D' for main analysis]
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T,pval = F, ref ='lifelines',check=F) #ref is specified here

#split model run [ no need to create dummy study variable, but you need to specify the datasource] 
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T, pval = F, datasources = opals[2],check=F) #ukbiobank opals[2]



### ------generate formula based on previously called expo, outcome, model and data.
#formula <- run.update.formula(outcome,expo,model,data) #[ data = 'D' main analysis]
#glm.res<-run.meta.glm(formula,family='binomial',ref='lifelines')
#run.extract.glm.stats(glm.res)


################# NA STATS ##########################

#### create NA subset ex: NA.D (default for main analysis and for a specific model) on server side
#this function assign NA.D.xxxx on server side and return the name of the assign NA dataframe.
nadata <- run.NA.glm.subset(formula=formula)

#check number of missing cases
ds.dim(nadata)

#### run NA stats 
run.desc.stats('AGE_YRS',data = nadata)
run.desc.stats('GENDER',data= nadata)
run.desc.stats('PM_BMI_CATEGORIAL',data= nadata)
run.desc.stats('EDU_HIGHEST_2',data= nadata)



