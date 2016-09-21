############################################
####     Testing missing data    ###########
############################################

# Source the bioshare environnment
source('utils/utils_analysis.R',echo=F,print.eval=F)

#load libraries
library(datashieldclient)
library(dsBetaTestClient)
library(dsStatsClient)

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


###############################################################
#######        Harmonize SOB variable        ##################
###############################################################

#HARMONIZE SBREATH_EXT
ds.assign('D$SYM_SBREATH','SYM_SBREATH_EXT',datasources=opals[1])
ds.assign('D$SYM_SBREATH_WALK','SYM_SBREATH_EXT',datasources=opals[2])
ds.cbind(c('SYM_SBREATH_EXT','D'),newobj='D')
run.rm('SYM_SBREATH_EXT')


########## Subset missing values for SOB for UKBB  #####################

# replace missing values in variable 'SYM_SBREATH_WALK': replace by some crazy value you know would never exits in the vector le's say 999999
# you have to replace missing values in each study so the length of the parameter 'forNA' below is equal to the number of studies (in the below case I assume you have 2 studies)
# please note that, in datashield, the NAs in the original column are not replace instead a new column here named 'monOutcome.noNA' is created and appended to the table 'D'
# please read the header (help) of the below function for more details if the above is not clear
ds.asNumeric(x='D$SYM_SBREATH_WALK')
ds.replaceNA(x='D$SYM_SBREATH_WALK', forNA=list('9'), newobj='SYM_SBREATH_WALK.noNA')
ds.recodeLevels(x='D$SYM_SBREATH_WALK.noNA', newCategories=c('0','0','9'), newobj='S_SB_W.noNA')
ds.cbind(x=c('D','S_SB_W.noNA'), newobj="newD")

ds.summary(x='newD$S_SB_W.noNA')

# now verify the levels of the new vector
ds.levels('newD$S_SB_W.noNA')
ds.summary(x='newD$S_SB_W.noNA')


# Table 'D' now has an additional column ('S_SB_W.noNA') where missing values have been replaced by 999999, in each study
# Subset 'D' to have two sub-tables one without the missing values and one with the missing values, we run the 2 commands below to have those sub-tables

ds.subsetByClass(x='newD', subsets='subtables', variables='S_SB_W.noNA')
ds.names('subtables')
ds.table2D(x='subtables$S_SB_W.noNA.level_0$EDU_HIGHEST_2', y='subtables$S_SB_W.noNA.level_1$EDU_HIGHEST_2')

ds.table1D(x='subtables$S_SB_W.noNA.level_0$GENDER')
ds.table1D(x='subtables$S_SB_W.noNA.level_1$GENDER')
ds.table1D(x='subtables$S_SB_W.noNA.level_9_EMPTY$GENDER')


ds.subset(x='newD', subset='subD_noNA', logicalOperator='S_SB_W.noNA<', threshold=9)
ds.subset(x='newD', subset='subD_NA', logicalOperator='S_SB_W.noNA>=', threshold=9)


#############################################

ds.numNA(x='subD_NA$SYM_SBREATH_WALK')
ds.numNA(x='subD_noNA$SYM_SBREATH_WALK')

ds.table2D(x='subtables$S_SB_W.noNA.level_0$EDU_HIGHEST_2', y='subtables$S_SB_W.noNA.level_1$EDU_HIGHEST_2')

ds.colnames(x='newD')
ds.colnames(x='subD_NA')
ds.colnames(x='subD_noNA')
ds.length(x='subD_NA$GENDER')
ds.length(x='subD_noNA$GENDER')
ds.length(x='D$GENDER')
ds.length(x='newD$GENDER')

ds.summary(x='D$SYM_SBREATH_WALK.noNA')




#############################


# For some reasons when you as R to change factor into numeric it can go funny 
# so to freeze the initial stuff you change first into character
ds.asCharacter(x='D$SYM_SBREATH_WALK', newobj='SYM_SBREATH_char')
ds.asNumeric(x='SYM_SBREATH_char', newobj='SYM_SBREATH_num')

# replace NA by ‘9’ and check
ds.replaceNA(x='SYM_SBREATH_num', forNA=list(9), newobj='S_SB_W.noNA')
ds.summary('S_SB_W.noNA')

# append new variable to D if everything fine
ds.cbind(x=c('D','S_SB_W.noNA'), newobj="newD")

# now split newD
ds.subset(x='newD', subset='subD_noNA', logicalOperator='S_SB_W.noNA<', threshold=9)
ds.subset(x='newD', subset='subD_NA', logicalOperator='S_SB_W.noNA>=', threshold=9)


ds.table2D(x='subD_noNA$EDU_HIGHEST_2', y='subD_NA$EDU_HIGHEST_2')



ds.length(x='subD_noNA$EDU_HIGHEST_2')
ds.length(x='subD_NA$EDU_HIGHEST_2')







-#### create NA subset ex: NA.D (default for main analysis and for a specific model) on server side
  -#this function assign NA.D.xxxx on server side and return the name of the assign NA dataframe.
  -nadata <- run.NA.glm.subset(formula=formula)
-
  -#check number of missing cases
  -ds.dim(nadata)
-
  -#### run NA stats 
  -run.desc.stats('AGE_YRS',data = nadata)
-run.desc.stats('GENDER',data= nadata)
-run.desc.stats('PM_BMI_CATEGORIAL',data= nadata)
-run.desc.stats('EDU_HIGHEST_2',data= nadata)


