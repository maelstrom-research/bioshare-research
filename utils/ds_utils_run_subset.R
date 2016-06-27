# ---- FUNCTION TO SUBSET (SIMILAR TO R) A DATABASE ON SERVER SIDE ----# 

##############################################
#   SUBSET  is can be done by logical or/and complete cases or/and cols or/and rows
#   param : TODO
#   ex: run.subset(data = 'D', logic = 'AGE_YRS<50',subsetName = 'ageUnder50')
#########################


ds_utils.env$run.subset<-function(data = NULL,logic = NULL, cols=NULL,rows =NULL,completeCases=FALSE, subsetName = NULL,  datasources=NULL){
  
  
  if(is.null(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  
  #verify data
  if(is.null(data)) {stop('Please specify the data(dataframe) to subset',call.=F)}
  
  cols <- unlist(cols)
  
  has.col.row <- !(is.null(cols) && is.null(rows))
  has.default <- has.col.row || !is.null(logic)
  
  if((!has.default) && (!completeCases)) stop("Nothing to do! Specify param <logic (character, EX: 'AGE >10')> OR/AND <completeCases (TRUE)> ...",call.=F)
  
  #   COMPUTATION
  
  #validate and retrieve correct logic: the process will stop if logic is wrongly specified
  logicds <- .logical2ds(logic)
  #get ready for execution
  lg <- logicds$lg
  th <- logicds$th
  varsub <- logicds$varsub
  indic <- logicds$indic
  
  
  X <- data
  name.X <- data
  X.info <- data
  
  is.a.vector <- FALSE
  if(length(cols) == 1) {
    one.col <- T #indicator that one column (a variable) should be returned
    X.info <- paste0(data,'$',cols)
    name.X <- paste0(data,'.',cols)
    check.X <- do.call(all,run.isAssigned(cols,data,ds))
    
    # vector not dataframe
    if(cols == varsub){
      X <- paste0(X,'$',cols) 
      is.a.vector <- TRUE
    }
    
  }else{
    one.col <- F
    check.X <- do.call(all,run.isAssigned(X,datasources=ds))
  }
  
  #sanity check
  if(!check.X) stop (paste0('"',X.info, '" is not found in some server(s)...Process is halted!'),call.=F)
  
  
  #define subsetName
  if(is.null(subsetName)) { 
    subsetName <- if (!is.null(logicds)) {
      paste0(name.X,'.',varsub,'.',indic,'.',th)  
    }else if(completeCases){
      paste0(name.X,'.cmplt')
    }
    message(paste0('subsetName is not specified. Resulting subset will be assigned to ',subsetName))
  }
  
  #get basic infos
  info <- if(!is.null(logicds)){ 
    paste0('---Subsetting ',X.info,' by ',logic)
  }else if (!is.null(cols) && (!is.null(rows))){
    paste0('---Subsetting ',X.info,' by ',paste0(cols,collapse=','),'AND by rows')
  }else if(!is.null(rows)){
    paste0('--Subsetting ',X.info,' by rows')
  }else if (!is.null(cols)){
    paste0('---Subsetting ',X.info,' by ',paste0(cols,collapse=','))
  }else if (completeCases) {
    paste0('--Subsetting ',X.info,' by complete cases')
  }
  
  
  #####excute server side
  
  message(info) 
  
  #1- default
  
  if(has.col.row) {
    
    cols.uniq <- if (one.col) {unique(c(varsub,cols))} else{cols } #the key is here
    
    cally.r.c  <-  call('subsetDS', dt=X, complt=FALSE, rs=rows,cols.uniq)
    datashield.assign(ds, subsetName, cally.r.c)
    
    #update X for the next steps
    X <- subsetName
  }
  
  
  #do logic if any
  if ( !is.null(logicds)){ 
    
    cally.logic <- call('subsetDS', dt=X, complt=FALSE, rs=NULL, cs=NULL, lg=lg, th=th, varname=varsub)
    datashield.assign(ds, subsetName, cally.logic)
  }
  
  #special case to process before any complete cases
  if (one.col && !is.a.vector) {
    cally.vect <- paste0(subsetName,'$',cols)
    datashield.assign(ds,subsetName,as.name(cally.vect)) 
  }
  
  #completecases if any 
  if(completeCases ){
    cally.cc <- call('subsetDS', dt=X, complt=completeCases)
    datashield.assign(ds, subsetName, cally.cc)
  }
  
  #clean server workspace
  run.rm(c( "complt","cs","dt","lg","rs","th","varname"),datasources=ds)
  
  #message to the user
  cat(paste0('You may check the assigned subset with this command: run.isAssigned("',subsetName,'")\n'))
  
  return (invisible(subsetName))
  
}
