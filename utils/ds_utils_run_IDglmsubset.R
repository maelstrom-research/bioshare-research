## ----- CREATE A DATAFRAME OF SUBSET FROM A REGRESSION MODEL PLUS THE STATUS OF COMPLETECASES -----

#################################################################
#This function takes either a formula for a glm or a result from a run glm,
#then computes a dataframe with the variables in the glm plus a ccID variables indicating the status of completecases: 1:complete, 0:not-complete.
#the dataframe comprises only the variables defined in the glm
#param <formula>: a glm formula (in character) to compute from
#param <glm.result> (optional if formula is given): a result from a run glm. This is optional when formula is given
#param <IDsubset> (optional): the name of the newly created dataframe.
#param <datasources> (optional): the datasources (study opal infos) where to do carry the computation. If not specified it will use all server(s)


ds_utils.env$run.ID.glm.subset<-function(formula,glm.result,fromVars= NULL,IDsubset=NULL,datasources=NULL)
{
  mf <- match.call(expand.dots = FALSE)
  arg.names <- names(mf)
  
  if ((! 'glm.result' %in% arg.names) && (!'formula' %in% arg.names)) {stop('Either a glm run or a formula is required ...',call.=F)}
  if(!'formula' %in% arg.names){
    glm.formula <- gsub('\\s+','',glm.result$formula)
  }else {
    glm.formula <- formula
  }
  
  #parsing var.list from glm formula
  vars.list<-strsplit(glm.formula,'\\+|~|\\*|\\|+')
  
  DATA <- unique(extract(vars.list)$holders)
  
  if(is.null(IDsubset)){ 
    
    IDsubset <- paste0('ID.',DATA)
    warning(paste0("IDsubset is not specified ...the subset will be saved in ", IDsubset, " dataframe on server side"),call.=F,immediate.=T)
  }
  
  if(is.null(datasources)) { datasources = findLoginObjects()}
  ds <- datasources
  
  #define vars.2df and vars.names from var.list 
  vars.2df<-unlist(vars.list)
  vars.names<-extract(vars.list)$elements
  
  # create a df for the selected variables  ---> TO CHANGE HERE WILL run.subset instead
  cally<-call('subsetDS', dt=DATA, complt=FALSE, rs=NULL, cs=vars.names)
  datashield.assign(ds,'RD',cally)
  
  
  # ----------define complete cases boolean var------------------
  
  if (is.null(fromVars)){ # use all the variables
    cat('\nall variables of the model will be used to create complete.cases:"ccID"\n')
    callcc<-'asFactorDS(complete.cases(RD)*1)'
   
  }else{ #use the designed variable
    #first check that the designed var was in the model
    fromVars <- unlist(fromVars)
    checkfromvars <- all(idx <- fromVars %in% vars.names)
    if(!checkfromvars) {
      stop(paste0('Variable(s) "',paste0(fromVars[!idx],collapse = ', '), '" is(are) not in the model'),call. = FALSE)
      callcc<-'asFactorDS(complete.cases(RD)*1)'
    }else{
      cat(paste0('\nVariable(s) "', paste0(fromVars,collapse = ' , '),'" of the model will be used to created complete.cases:"ccID"\n'))
      cbindtxt <- paste0('cbind(',paste0(paste0('RD$',fromVars),collapse = ','),')')
      callcc<- paste0('asFactorDS(complete.cases(',cbindtxt,')*1)')
    }
  }
  
  #then compute ccID
  datashield.assign(ds,'ccID',as.symbol(callcc))
  
  
  
  
  
  #define new df RD with cc variable
  callcbind<-'cbind(RD,ccID)'
  datashield.assign(ds,IDsubset,as.symbol(callcbind))
  
  
  
  #clean server workspace
  #to_rm <- c("ccID","complt","cs","dt","lg" , "rs", "th","varname","RD")
  #run.rm(to_rm,datasources=ds)
  
  #info to user
  cat(paste0("\nYou may check the assigned ",IDsubset," dataframe with the following datashield commands: 
   run.isAssigned('",IDsubset,"'), ds.colnames('",IDsubset,"') AND ds.dim('",IDsubset,"')")) 
  
  return(invisible(IDsubset))
}
