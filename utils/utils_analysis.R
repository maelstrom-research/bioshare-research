if('ds_utils.env' %in% search())detach(ds_utils.env)

#load or install dependency
.dsloaded <- suppressWarnings(library('datashieldclient',logical.return=T,quietly=T))
if(!.dsloaded) {
  install.packages(c('dsModellingClient','dsStatsClient'), repos=c(getOption('repos'), 'http://cran.obiba.org'), dependencies=TRUE)
}
.dsloaded <- suppressWarnings(require('datashieldclient'))
if(!.dsloaded) stop('datashieldclient is not properly installed.')


#########################################################
# ENV INITIALISATION
ds_utils.env <- new.env()




#################################################
# utils functions
#################################################

source('utils/ds_utils_internal.R')
source('utils/ds_utils_var_assign.R')
source('utils/ds_utils_run_rm.R')

############################################################
#    analytics functions here below 
############################################################
source('utils/ds_utils_run_subset.R')
source('utils/ds_utils_run_subclass.R')
source('utils/ds_utils_run_changelevel.R')


#############################################################
########################## MODELING UTILS ###################

source('utils/ds_utils_run_adjustmodel.R')
source('utils/ds_utils_run_dummystudy.R')
source('utils/ds_utils_run_makeformula.R')


######  GLM   #####

source('utils/ds_utils_run_extractglmstats.R')
source('utils/ds_utils_run_metaglm.R')
source('utils/ds_utils_run_model.R')
source('utils/ds_utils_run_stackglm.R')
source('utils/ds_utils_run_NAglmsubset.R')
source('utils/ds_utils_run_CCglmsubset.R')



#############----- BASE STATS --#############################
source('utils/ds_utils_run_table1d.R')
source('utils/ds_utils_run_meansd.R')
source('utils/ds_utils_run_table2d.R')





#close everything
ds_utils.env$run.close<-function(all=F)
{
  #detect opals 
  objs <- ls(name=.GlobalEnv)
  if(length(objs)){
    for(obj in objs){
      obj.val<- eval(parse(text=obj))
      if (is.list(obj.val) && (class(obj.val[[1]]) == 'opal')){
        message(cat('Closing opal(s) server connection(s)'))
        obj.opal <- obj.val
        datashield.logout(obj.opal)
        rm(list= obj,pos=search())
        cat(paste0( names(obj.opal),' server is disconnected...'),sep='\n')
      }
    }
  }

 if(as.logical(all)) rm(list=ls(name=.GlobalEnv,all.names=T),envir=.GlobalEnv)
 if('ds_utils.env' %in% search()){
   detach(ds_utils.env,pos=search())
   cat('utils_analysis environnment is now detached from memory...') 
 }
  
  
}


# attach ds_utils env
attach(ds_utils.env)
if('ds_utils.env' %in% search()) {message(cat('ds_utils.env is correctly loaded...'))}
rm(ds_utils.env)





