## ----- CREATE A DATAFRAME OF complete cases  SUBSET FROM A REGRESSION MODEL -----



############################################################################
#This function takes either a formula for a glm or a result from a run glm,
#then computes a dataframe for COMPLETE CASES in the glm. this is similar to completecases in R glm
#the dataframe comprises only the variables defined in the glm
#param <formula>: a glm formula (in character) to compute from
#param <glm.result> (optional if formula is given): a result from a run glm. This is optional when formula is given
#param <CCsubset> (optional): the name of the newly created dataframe.
#param <datasources> (optional): the datasources (study opal infos) where to do carry the computation. If not specified it will use all server(s)




ds_utils.env$run.CC.glm.subset <- function(formula,glm.result,CCsubset=NULL,datasources=NULL)
{
  mf <- match.call(expand.dots = FALSE)
  arg.names <- names(mf)
  
  if ((! 'glm.result' %in% arg.names) && (!'formula' %in% arg.names)) {stop('Either a glm result or a formula is required ...',call.=F)}
  if(!'formula' %in% arg.names){
    glm.formula <- gsub('\\s+','',glm.result$formula)
  }else {
    glm.formula <- formula
  }
  
  #parsing var.list from glm formula
  vars.list<-strsplit(glm.formula,'\\+|~|\\*|\\|+')
  
  DATA <- unique(extract(vars.list)$holders)
  
  if(is.null(CCsubset)){ 
    CCsubset <- paste0('CC.',DATA)
    warning(paste0("CCsubset is not specified ...the subset will be saved in ", CCsubset, " dataframe on server side"),call.=F,immediate.=T)
  }
  
  if(is.null(datasources)) { datasources = findLoginObjects()}
  ds <- datasources
  
  #define vars.2df and vars.names from var.list 
  vars.names<-extract(vars.list)$elements
  
  #do the subset and completecases
  suppressMessages(run.subset(data=DATA,cols=vars.names,completeCases=T,subsetName=CCsubset))  #<- dependency to run.subset
  
  #return the name of the assigned df
  return(invisible(CCsubset))
}