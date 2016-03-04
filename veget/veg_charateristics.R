# Attach the DS UTILS environnment
source('utils_analysis.R',echo=F,print.eval=F)

library(datashieldclient)

#the variables needed for the HOP 
myvar <- list('DIET_VEGETARIAN', 'DIET_VEGETARIAN_VERIFIED', 'AGE_YRS', 'AGE_YRS_CATEGORICAL', 'GENDER', 'EDU_HIGHEST_1', 'EDU_HIGHEST_2',
              'WORK_STATUS_CURRENT', 'SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL', 'PM_WAIST_SIZE', 
              'PM_SYSTOLIC_MEASURE', 'PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING', 'LAB_HDL', 'LAB_TRIG', 'LAB_hsCRP', 
              'METABSYNDR_NBR_STRICT', 'METABSYNDR_STRICT', 'METABSYNDR_NBR_MODERATE', 'METABSYNDR_MODERATE', 'HLTH_OBESE_STRICT', 'HLTH_OBESE_MODERATE')

#load loggin information
load("../login/login-veget.rda")

#only studies participating in MetS and Vegetarian diet:  finrisk, kora, lifelines, mitchelstown, cartagene


#login to datashield and assign data to 'D'
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)


#Define variable to subset by
sub_by<-'DIET_VEGETARIAN_VERIFIED'                       #<=== may vary often: ALWAYS CALL THIS VARIABLE 


##########################################################################
#
#         ******CONTINUOUS VARIABLES STATISTICS **** 
#
##########################################################################
var_cont<-list( 'AGE_YRS','PM_WAIST_SIZE','PM_SYSTOLIC_MEASURE','PM_DIASTOLIC_MEASURE', 'LAB_GLUC_FASTING','LAB_HDL', 'LAB_TRIG','LAB_hsCRP')




#################################################
# STATS WITH DOUBLE SUBSET - BMI and MetS components

sub_by2<-'PM_BMI_CATEGORIAL'                 #<=== update first subset by: ALWAYS CALL THIS VARIABLE 
sub_double_by<-sub_by             #<----VARIABLE USED AS DOUBLE SUBSET SOULD BE ONLY TWO CATEGORIES BECAUSE OF ttest

var_cont_double<-list('PM_SYSTOLIC_MEASURE','PM_DIASTOLIC_MEASURE','LAB_GLUC_FASTING','LAB_HDL','LAB_TRIG','LAB_hsCRP')


# STATS WITH DOUBLE SUBSET - Gender and MetS components
####sub_by2<-'GENDER'                 #<=== update first subset by: ALWAYS CALL THIS VARIABLE 
####sub_double_by<-sub_by             #<----VARIABLE USED AS DOUBLE SUBSET SOULD BE ONLY TWO CATEGORIES BECAUSE OF ttest

####var_cont_double<-list('PM_SYSTOLIC_MEASURE','PM_DIASTOLIC_MEASURE','LAB_GLUC_FASTING','LAB_HDL','LAB_TRIG','LAB_hsCRP')


################################
#STATS WITH DOUBLE SUBSET SPLIT
################################
source('cont_double_sub_stats_split.R',echo=F,print.eval=T)

##################################
#STATS WITH DOUBLE SUBSET COMBINED
###################################
source('cont_double_sub_stats_combined.R',echo=F,print.eval=T)  


######################################################################################
#
#         ******CATEGORICAL VARIABLES STATISTICS **** 
#
#####################################################################################


var_cat<-list('AGE_YRS_CATEGORICAL','GENDER','SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL',
              'METABSYNDR_NBR_STRICT','METABSYNDR_STRICT','METABSYNDR_NBR_MODERATE','METABSYNDR_MODERATE', 'EDU_HIGHEST_1', 'EDU_HIGHEST_2') #,'WORK_STATUS_CURRENT'


#################
#STATS WITH SPLIT
#################
source('categorical_var_stats_split.R',echo=F,print.eval=T)

####################
#STATS WITH COMBINED
####################
source('categorical_var_stats_combined.R',echo=F,print.eval=T)


###################################
#Vegetarian by BMI contigency table
###################################
Veg_by_BMI_table <- ds.table2D(x='D$DIET_VEGETARIAN_VERIFIED', y='D$PM_BMI_CATEGORIAL', type='split')
save(Veg_by_BMI_table,file='Veg_byBMI.RData')

######################################
#Vegetarian by GENDER contigency table
######################################
Veg_by_GENDER_table <- ds.table2D(x='D$DIET_VEGETARIAN_VERIFIED', y='D$GENDER', type='split')
save(Veg_by_GENDER_table,file='Veg_byGender.RData')

######################################
#Vegetarian by HO Strict contigency table
######################################
Veg_by_HOstr_table <- ds.table2D(x='D$DIET_VEGETARIAN_VERIFIED', y='D$HLTH_OBESE_STRICT', type='split')
save(Veg_by_HOstr_table,file='Veg_byHOstr.RData')

######################################
#Vegetarian by HO Moderate contigency table
######################################
Veg_by_HOmod_table <- ds.table2D(x='D$DIET_VEGETARIAN_VERIFIED', y='D$HLTH_OBESE_MODERATE', type='split')
save(Veg_by_HOmod_table,file='Veg_byHOmod.RData')


#########################################################
#meta_strict_syn by Age_Cat  contingency table
#########################################################
Met_str_by_Age<-ds.table2D(x='D$METABSYNDR_STRICT',y='D$AGE_YRS_CATEGORICAL')
save(Met_str_by_Age,file='Met_str_by_Age.rda')

#########################################################
#meta_strict_syn by GENDER contingency table
#########################################################
Met_str_by_Gender<-ds.table2D(x='D$METABSYNDR_STRICT',y='D$GENDER')
save(Met_str_by_Gender,file='Met_str_by_Gender.rda')

########################################################
#meta_mod_syn by Age_Cat  contingency table
#########################################################
Met_mod_by_Age<-ds.table2D(x='D$METABSYNDR_MODERATE',y='D$AGE_YRS_CATEGORICAL')
save(Met_mod_by_Age,file='Met_mod_by_Age.rda')

#########################################################
#meta_mod_syn by GENDER contingency table
#########################################################
Met_mod_by_Gender<-ds.table2D(x='D$METABSYNDR_MODERATE',y='D$GENDER')
save(Met_mod_by_Gender,file='Met_mod_by_Gender.rda')



######################################################################################
#
#         ******GLM**** 
#       TODO: ADD HETEROGENEITY EFFECT IN GLM
#####################################################################################

# add specific formula, family for each regression you run
#meta_strict regression
formula<-'D$METABSYNDR_STRICT~D$GENDER+D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_CIG_CURRENT+D$ALC_CURRENT+D$DIET_VEGETARIAN_VERIFIED'
family<-'binomial'
ref<-'lifelines'
save_to<-'metstrict_glm'
source('meta_glm.R',echo=F,print.eval=T)

#meta_moderate
formula<-'D$METABSYNDR_MODERATE~D$GENDER+D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_CIG_CURRENT+D$ALC_CURRENT+D$DIET_VEGETARIAN_VERIFIED'
family<-'binomial'
ref<-'lifelines'
save_to<-'metmoderate_glm'
source('meta_glm.R',echo=F,print.eval=T)

#metstrict_glm <- ds.glm(formula='D$METABSYNDR_STRICT~D$GENDER+D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_CIG_CURRENT+D$ALC_CURRENT+D$DIET_VEGETARIAN_VERIFIED', family='binomial')
#metmoderate_glm <- ds.glm(formula='D$METABSYNDR_MODERATE~D$GENDER+D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_CIG_CURRENT+D$ALC_CURRENT+D$DIET_VEGETARIAN_VERIFIED', family='binomial')



#datashield.logout(opals)
# clean workspace 
to_rm<-ls()[-grep('^opals$',ls())]
rm(to_rm,list=to_rm)