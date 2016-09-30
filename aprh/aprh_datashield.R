################################################################
#
#                         MAIN ANALYSIS
#
###############################################################

# Attach the DS UTILS environnment
source('utils/utils_analysis.R',echo=F,print.eval=F)

# LOAD DATAs IN EACH OPAL SERVER
source('aprh/aprh_data.R',echo=F,print.eval=F)


#HARMONIZE SBREATH_EXT
ds.assign('D$SYM_SBREATH','SYM_SBREATH_EXT',datasources=opals[1])
ds.assign('D$SYM_SBREATH_WALK','SYM_SBREATH_EXT',datasources=opals[2])
ds.cbind(c('SYM_SBREATH_EXT','D'),newobj='D')
run.rm('SYM_SBREATH_EXT')


########################     DATA THAT WILL BE USED FOR THE MAIN ANALYSIS      ############
#specify the name (character) of the dataframe that was assigned in datashield.login ('D' by default)
data <- 'D'

#################################################################################
##            running some regressions :multivariate                           ##
#################################################################################

####################### define models [model0,model1, model2a,model2b model3a,model3b]
### model0 (unajusted)
model <- NULL

#Model 1(ajusted for age,sex )                                                                                     <-
model<-'AGE_YRS+GENDER'

#Model 2 (adjusted for age, sex, bmi, highest level of education and household income)
model <- 'AGE_YRS+GENDER+EDU_HIGHEST_2+PM_BMI_CATEGORIAL+INCOME'

#Model 3 adjusted for age, sex, bmi, highest level of education, household income, smoking status, and exposure to second-hand tobacco smoke 
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'

#Model 3a aditionnaly adjusted for NO2 (for PM metrics)
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME+NO2_ESCAPE'

#Model 3b aditionnaly adjusted for PM25 (for NO2)
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME+PM25_ESCAPE'

#######################
expo <- 'PM25_ESCAPE'
expo <- 'PM10_ESC'
expo <- 'NO2_ESCAPE'
expo <- 'PMcoarse_ESCAPE'

####################
#Pooled outcomes
outcome <- 'SYM_WHEEZ' 
outcome <- 'SYM_SBREATH_EXT'

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



################# NA and CC STATS ##########################

#### Build formula base on ouctom, exposure and model#####
formula <- run.make.formula(outcome,expo,model,data)


############ MISSING DATA ####################

#### create NA subset ex: NA.D (default for main analysis and for a specific model) on server side
#this function assign NA.D.xxxx on server side and return the name of the assign NA dataframe.
##run.NA.glm.subset<-function(formula,glm.result,NAsubset=NULL,datasources=NULL)
nadata <- run.NA.glm.subset(formula=formula)
ds.colnames('NA.D')

#check number of missing cases
ds.dim(nadata)

#### run NA stats 
run.meanSd('AGE_YRS',data = nadata)
run.table1d('GENDER',data= nadata)
run.table1d('PM_BMI_CATEGORIAL',data= nadata)
run.table1d('EDU_HIGHEST_2',data= nadata)


########## COMPLETE CASES #################

#### run.CC.glm.subset <- function(formula,glm.result,CCsubset=NULL,datasources=NULL)
ccdata <- run.CC.glm.subset (formula=formula)
ds.colnames('CC.D')

#check number of complete cases
ds.dim(ccdata)

########Run Analyses for only complete cases in Model 3 #######
run.dummy.study(data=ccdata)
run.model(outcome,expo,model,family = 'binomial',data=ccdata,Ncases=T,pval = F, ref ='lifelines',check=F)



############## COMPARISON OF MISSING CASES VS COMPLETE CASES IN GLM: EX FOR VARIABLES SYM_WHEEZE + PM25 + MODEL3
formula <- run.make.formula(outcome,expo,model,data)
# compute the complete case ID variables (ccID) FROM SYM_WHEEZ  in the model
run.ID.glm.subset(formula,IDsubset = 'ID_Data',fromVars = 'SYM_WHEEZ')

#then compare SYM_WHEEZE for Missing cases vs complete cases status for all the glm variables
run.table2d(x='ccID',y='SYM_WHEEZ',data = 'ID_Data',col.percent = TRUE)
