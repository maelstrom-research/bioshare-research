################################################################
#
#                 APRH DESCRIPTIVE STATISTICS
#
###############################################################

# Attach the DS UTILS environnment
source('../bioshare-research/utils/utils_analysis.R',echo=F,print.eval=F)

# LOAD DATAs IN EACH OPAL SERVER
source('aprh_data.R',echo=F,print.eval=F)




########################################################
#           univariate: outcome~confounding
#######################################################

########### PART 1 #########################
# glm to compute ttest
confounding1 <- list('AGE_YRS','SMK_PACKYRS','RES_LENGTH')

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
result.part2 <- run.cat(outcome,confounding2) #type = 'split'
#print part2 p.value(s) per outcome
lapply(result.part2,function(x){ format(x$chi2Test$pooled$p.value,digits=5)})

