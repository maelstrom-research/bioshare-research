## ----- CREATE A DATAFRAME OF NA SUBSET FROM A REGRESSION MODEL -----

#################################################################
#This function takes either a formula for a glm or a result from a run glm,
#then computes a dataframe for NA cases in the glm. this is similar to na.action = T in R glm
#the dataframe comprises only the variables defined in the glm
#param <formula>: a glm formula (in character) to compute from
#param <glm.result> (optional if formula is given): a result from a run glm. This is optional when formula is given
#param <NAsubset> (optional): the name of the newly created dataframe.
#param <datasources> (optional): the datasources (study opal infos) where to do carry the computation. If not specified it will use all server(s)


ds_utils.env$run.NA.glm.subset<-function(formula,glm.result,NAsubset=NULL,datasources=NULL)
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
  
  if(is.null(NAsubset)){ 
    
    NAsubset <- paste0('NA.',DATA)
    warning(paste0("NAsubset is not specified ...the subset will be saved in ", NAsubset, " dataframe on server side"),call.=F,immediate.=T)
  }
  
  if(is.null(datasources)) { datasources = findLoginObjects()}
  ds <- datasources
  
  #define vars.2df and vars.names from var.list 
  vars.2df<-unlist(vars.list)
  vars.names<-extract(vars.list)$elements
  
  # create a df for the selected variables  ---> TO CHANGE HERE WILL run.subset instead
  cally<-call('subsetDS', dt=DATA, complt=FALSE, rs=NULL, cs=vars.names)
  datashield.assign(ds,'RD',cally)
  
  # define complete cases boolean var
  callcc<-'complete.cases(RD)'
  datashield.assign(ds,'cc',as.symbol(callcc))
  
  #define new df RD with cc variable
  callcbind<-'cbind(RD,cc)'
  datashield.assign(ds,'RD',as.symbol(callcbind))
  
  #define subset of RD --> RDF with values == NA in cc
  callSUBSET <- call('subsetDS', dt='RD', complt=FALSE, rs=NULL, cs=NULL, lg=5, th=FALSE, varname='cc')
  datashield.assign(ds, NAsubset, callSUBSET)
  
  #clean server workspace
  to_rm <- c("cc","complt","cs","dt","lg" , "rs", "th","varname","RD")
  run.rm(to_rm,datasources=ds)
  
  #info to user
  cat(paste0("You may check the assigned ",NAsubset," dataframe with the following datashield commands: 
   run.isAssigned('",NAsubset,"'), ds.colnames('",NAsubset,"') AND ds.dim('",NAsubset,"')")) 
  
  return(invisible(NAsubset))
}
