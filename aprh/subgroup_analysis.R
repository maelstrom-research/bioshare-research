#####Example for subgroup analysis: GENDER######

#subset by class first
ds.subsetByClass('D',subsets='gender',variables='GENDER')

#check the names of subset in gender object
ds.names('gender')

#assign the subcategory to an object correctly named: MALES
ds.assign('gender$GENDER.level_0',newobj='GENDER0')

#assign the subcategory to an object correctly named: FEMALES
ds.assign('gender$GENDER.level_1',newobj='GENDER1')

#use the main R script file and change 
#data <- 'name_of _subset_object'

#Assign the subgroup dataframe to data: MALES
data <- 'GENDER0'

#Assign the subgroup dataframe to data: FEMALES
data <- 'GENDER1'


#Model 3b: sex-specifc subgroup analyses
model <- 'AGE_YRS+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


#####Example for subgroup analysis: AGE######

#subset by class first
ds.subsetByClass('D',subsets='age',variables='AGE_YRS_65')

#check the names of subset in age object
ds.names('age')

#assign the subcategory to an object correctly named: under 65
ds.assign('age$AGE_YRS_65.level_0',newobj='AGE0')

#assign the subcategory to an object correctly named: 65 and older
ds.assign('age$AGE_YRS_65.level_1',newobj='AGE1')

#use the main R script file and change 
#data <- 'name_of _subset_object'

#Assign the subgroup dataframe to data: under 65
data <- 'AGE0'

#Assign the subgroup dataframe to data: 65 and older
data <- 'AGE1'

#Model 3b: age-specifc subgroup analyses
model <- 'GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


############Income ################


#subset by class first
ds.subsetByClass('D',subsets='income',variables='INCOME')

#check the names of subset in age object
ds.names('income')

#assign the subcategory to an object correctly named: under mean income
ds.assign('income$INCOME.level_0',newobj='INCOME0')

#assign the subcategory to an object correctly named: mean income and over
ds.assign('income$INCOME.level_1',newobj='INCOME1')

#Assign the subgroup dataframe to data: under mean income
data <- 'INCOME0'

#Assign the subgroup dataframe to data: mean income and over
data <- 'INCOME1'

#Model 3b: income-specifc subgroup analyses
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL'

#### Obesity ######

#Recode obesity into two categories
ds.recodeLevels(x='D$PM_BMI_CATEGORIAL', newCategories=c('1','1','2'), newobj='PM_BMI_2')
ds.cbind(c('PM_BMI_2','D'),newobj='D')
ds.levels(x='PM_BMI_2')
ds.table1D(x='PM_BMI_2', type='split')
ds.table1D(x='D$PM_BMI_2', type='split')

#subset by class first
ds.subsetByClass('D',subsets='bmi',variables='PM_BMI_2')

#check the names of subset in age object
ds.names('bmi')

#assign the subcategory to an object correctly named: normal weight
ds.assign('bmi$PM_BMI_2.level_1',newobj='BMI1')

#assign the subcategory to an object correctly named: obese
ds.assign('bmi$PM_BMI_2.level_2',newobj='BMI2')

#Assign the subgroup dataframe to data: under mean income
data <- 'BMI1'

#Assign the subgroup dataframe to data: mean income and over
data <- 'BMI2'

#Model 3b: income-specifc subgroup analyses
model <- 'AGE_YRS+GENDER+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


#### Smoking status #####

#Recode smoking into two categories
ds.recodeLevels(x='D$SMK_STATUS', newCategories=c('1','2','2'), newobj='SMK_STATUS_NEW')
ds.cbind(c('SMK_STATUS_NEW','D'),newobj='D')
ds.levels(x='SMK_STATUS_NEW')
ds.table1D(x='SMK_STATUS_NEW', type='split')
ds.table1D(x='D$SMK_STATUS', type='split')


#subset by class first
ds.subsetByClass('D',subsets='smoke',variables='SMK_STATUS_NEW')

#check the names of subset in age object
ds.names('smoke')

#assign the subcategory to an object correctly named: non-smokers
ds.assign('smoke$SMK_STATUS_NEW.level_1',newobj='SMOKE1')

#assign the subcategory to an object correctly named: current and past smokers
ds.assign('smoke$SMK_STATUS_NEW.level_2',newobj='SMOKE2')

#Assign the subgroup dataframe to data: under mean income
data <- 'SMOKE1'

#Assign the subgroup dataframe to data: mean income and over
data <- 'SMOKE2'

#Model 3b: income-specifc subgroup analyses
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_PASSIVE_ALL+INCOME'

##### Asthma status ######

#subset by class first
ds.subsetByClass('D',subsets='asthma',variables='DIS_ASTHMA')

#check the names of subset in asthma object
ds.names('asthma')

#assign the subcategory to an object correctly named: non-asthmatics
ds.assign('asthma$DIS_ASTHMA.level_0',newobj='ASTHMA0')

#assign the subcategory to an object correctly named: asthmatics
ds.assign('asthma$DIS_ASTHMA.level_1',newobj='ASTHMA1')


#Assign the subgroup dataframe to data: non-asthmatics
data <- 'ASTHMA0'

#Assign the subgroup dataframe to data: asthmatics
data <- 'ASTHMA1'


#Model 3b: sex-specifc subgroup analyses
model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


#######################
expo <- 'PM25_ESCAPE'
expo <- 'PM10_ESC'
expo <- 'NO2_ESCAPE'
expo <- 'PMcoarse_ESCAPE'

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
outcome <- 'SYM_SBREATH_EXT'

#pooled model to run [first create dummy study variables in server side by running run.dummy() once]
run.dummy.study(data) #run once 
datashield.symbols(opals) # check objects on server side


#exposure effect statistics  [see data = 'D' for main analysis]
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T,pval = F, ref ='lifelines',check=F) #ref is specified here

#split model run [ no need to create dummy study variable, but you need to specify the datasource] 
run.model(outcome,expo,model,family = 'binomial',data,Ncases=T, pval = F, datasources = opals[2],check=F) #ukbiobank opals[2]



#generate formula based on previously called expo, outcome, model and data.
formula <- run.make.formula(outcome,expo,model,data) #[ data = 'D' main analysis]

glm.res<-run.meta.glm(formula,family='binomial',ref='lifelines')
run.extract.glm.stats(glm.res, pval=T)





#####Sensitivity analyses: 5 years living at residence######

#subset by class first
ds.subsetByClass('D',subsets='res5yrs',variables='RES_5YRS')

#check the names of subset in gender object
ds.names('res5yrs')

#assign the subcategory to an object correctly named: under 5 years
ds.assign('res5yrs$RES_5YRS.level_0',newobj='RES_5YRS0')

#assign the subcategory to an object correctly named: 5 years or more
ds.assign('res5yrs$RES_5YRS.level_1',newobj='RES_5YRS1')

#use the main R script file and change 
#data <- 'name_of _subset_object'

#Assign the subgroup dataframe to data: under 5 years
data <- 'RES_5YRS0'

#Assign the subgroup dataframe to data: 5 years or more
data <- 'RES_5YRS1'


#Model 3b: sex-specifc subgroup analyses
model <- 'AGE_YRS+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


#####Sensitivity analyses: 10 years living at residence######

#subset by class first
ds.subsetByClass('D',subsets='res10yrs',variables='RES_10YRS')

#check the names of subset in gender object
ds.names('res10yrs')

#assign the subcategory to an object correctly named: under 5 years
ds.assign('res10yrs$RES_10YRS.level_0',newobj='RES_10YRS0')

#assign the subcategory to an object correctly named: 5 years or more
ds.assign('res10yrs$RES_10YRS.level_1',newobj='RES_10YRS1')

#use the main R script file and change 
#data <- 'name_of _subset_object'

#Assign the subgroup dataframe to data: under 5 years
data <- 'RES_10YRS0'

#Assign the subgroup dataframe to data: 5 years or more
data <- 'RES_10YRS1'


#Model 3b: sex-specifc subgroup analyses
model <- 'AGE_YRS+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'



#####Sensitivity analyses: 50 years and older######

#subset by class first
ds.subsetByClass('D',subsets='age50',variables='AGE_YRS_50')

#check the names of subset in gender object
ds.names('age50')

#assign the subcategory to an object correctly named: under 5 years
ds.assign('age50$AGE_YRS_50.level_0',newobj='AGE_YRS_50_0')

#assign the subcategory to an object correctly named: 5 years or more
ds.assign('age50$AGE_YRS_50.level_1',newobj='AGE_YRS_50_1')

#use the main R script file and change 
#data <- 'name_of _subset_object'

#Assign the subgroup dataframe to data: under 5 years
data <- 'AGE_YRS_50_0'

#Assign the subgroup dataframe to data: 5 years or more
data <- 'AGE_YRS_50_1'


#Model 3b: sex-specifc subgroup analyses
model <- 'AGE_YRS+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'
