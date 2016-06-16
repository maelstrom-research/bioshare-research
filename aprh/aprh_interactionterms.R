################################################################
#
#                         MAIN ANALYSIS
#
###############################################################

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

#Recode BMI into two categories
ds.recodeLevels(x='D$PM_BMI_CATEGORIAL', newCategories=c('1','1','2'), newobj='BMI_CAT_NEW')
ds.cbind(c('BMI_CAT_NEW','D'),newobj='D')
ds.levels(x='BMI_CAT_NEW')
ds.table1D(x='BMI_CAT_NEW', type='split')
ds.table1D(x='D$PM_BMI_CATEGORIAL', type='split')

#Recode smoking into two categories
ds.recodeLevels(x='D$SMK_STATUS', newCategories=c('1','2','2'), newobj='SMK_STATUS_NEW')
ds.cbind(c('SMK_STATUS_NEW','D'),newobj='D')
ds.levels(x='SMK_STATUS_NEW')
ds.table1D(x='SMK_STATUS_NEW', type='split')
ds.table1D(x='D$SMK_STATUS', type='split')



########################     DATA THAT WILL BE USED FOR THE MAIN ANALYSIS      ############
#specify the name (character) of the dataframe that was assigned in datashield.login ('D' by default)
data <- 'D'

#################################################################################
##            running some regressions :multivariate                           ##
#################################################################################

####################### define models: ##########################################
###### first line for study-specific analyses, second for pooled analyses #######

#Model for interaction with age 
model <- 'GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'
model <- 'GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME+AGE_YRS_65*ukb'


#Model for interaction with gender
model <- 'AGE_YRS+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'
model <- 'AGE_YRS+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME+GENDER*ukb'


#Model for interaction with household income 
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL'
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME*ukb'


#Model for interaction with obesity
model <- 'AGE_YRS+GENDER+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'
model <- 'AGE_YRS+GENDER+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME+BMI_CAT_NEW*ukb'


#Model for interaction with smoking status
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_PASSIVE_ALL+INCOME'
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_PASSIVE_ALL+INCOME+SMK_STATUS_NEW*ukb'


#Model for interaction with asthma status
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME+DIS_ASTHMA*ukb'



#######################
expo <- 'PM25_ESCAPE*AGE_YRS_65'
expo <- 'PMcoarse_ESCAPE*AGE_YRS_65'
          
expo <- 'PM25_ESCAPE*GENDER'
expo <- 'PMcoarse_ESCAPE*GENDER'
          
expo <- 'PM25_ESCAPE*INCOME'
expo <- 'PMcoarse_ESCAPE*INCOME'

expo <- 'PM25_ESCAPE*BMI_CAT_NEW'
expo <- 'PMcoarse_ESCAPE*BMI_CAT_NEW'
          
expo <- 'PM25_ESCAPE*SMK_STATUS_NEW'
expo <- 'PMcoarse_ESCAPE*SMK_STATUS_NEW'

expo <- 'PM25_ESCAPE*DIS_ASTHMA'
expo <- 'PMcoarse_ESCAPE*DIS_ASTHMA'
          
#expo <- 'PM10_ESC'
#expo <- 'NO2_ESCAPE'
#expo <- 'PMcoarse_ESCAPE'

####################
#Pooled outcomes
outcome <- 'SYM_WHEEZ' 
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

####################### Run modelss ##################################

#pooled model to run [first create dummy study variables in server side by running run.dummy() once]
run.dummy.study(data) #run once 
datashield.symbols(opals) # check objects on server side

#exposure effect statistics  [see data = 'D' for main analysis]
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T,pval = F, ref ='lifelines',check=F) #ref is specified here

#split model run [ no need to create dummy study variable, but you need to specify the datasource] 
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T, pval = T, datasources = opals[1],check=F) #ukbiobank opals[2]


#generate formula based on previously called expo, outcome, model and data.
formula <- run.make.formula(outcome,expo,model,data) #[ data = 'D' main analysis]

glm.res<-run.meta.glm(formula,family='binomial',ref='lifelines')
run.extract.glm.stats(glm.res, pval=T)
print(glm.res)





#################################################################
##################EXTRA STUFF####################################
#################################################################


#################################################################
##### Centre PM25, PM10, PMcoarse and NO2 around their mean #####
#################################################################

##Obtain mean for PM25
meanPM25 <- ds.mean("D$PM25_ESCAPE", datasources=opals)

datashield.aggregate(opals,as.name(meanPM25))

##Generate centered varaible
ds.assign(toAssign='D$PM25_ESCAPE-10.71563', newobj='PM25cent', datasources=opals
          #Verify
          ds.mean("PM25cent", datasources=opals)
          
          
          call <- 'meanDS(D$PM25_ESCAPE)'
          meanRes <- datashield.aggregate(opals,as.name(call))
          meanRes
          datashield.assign(opals[1],'PMcent25',as.name('D$PM25_ESCAPE-15.53826'))
          datashield.assign(opals[2],'PMcent25',as.name('D$PM25_ESCAPE-9.992007'))
          ds.cbind(c('PMcent25','D'),newobj='D')          
          
          
          
          ##Obtain mean for PM10
          ds.mean("D$PM10_ESC", datasources=opals)
          ##Generate centered varaible
          ds.assign(toAssign='D$PM10_ESC-17.277823', newobj='PM10cent', datasources=opals)
          #Verify
          ds.mean("PM10cent", datasources=opals)
          
          ##Obtain mean for PMcoarse
          ds.mean("D$PMcoarse_ESCAPE", datasources=opals)
          ##Generate centered varaible
          ds.assign(toAssign='D$PMcoarse_ESCAPE-6.720976', newobj='PMcoarcent', datasources=opals)
          #Verify
          ds.mean("PMcoarcent", datasources=opals)
          
          ##Obtain mean for NO2
          ds.mean("D$NO2_ESCAPE", datasources=opals)
          ##Generate centered varaible
          ds.assign(toAssign='D$NO2_ESCAPE-25.44937', newobj='NO2cent', datasources=opals)
          #Verify
          ds.mean("NO2cent", datasources=opals)

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



################# VALID STATS ##########################

sbreath_form <- run.make.formula(outcome,expo,model,data)

run.CC.glm.subset(formula=sbreath_form, datasources=data, CCsubset='sbreathPM25')
run.isAssigned("sbreathPM25")

run.desc.stats('GENDER',data = 'sbreathPM25')




