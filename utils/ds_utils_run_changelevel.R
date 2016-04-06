# ----- RECODE THE LEVELS OF A FACTORIAL VARIABLE ----- #


################################################################################################
# this function recode the levels of a factor variable ex: bin a bmi 1,2,3 to bmi 0, 1 
ds_utils.env$run.changelevel <- function(var,new.levels,new.varname=NULL,data=NULL,datasources) {
  
  if(missing(var)) stop('var is required')
  if(missing(new.levels)) stop('new.levels is required')
  if(missing(datasources)) datasources <- findLoginObjects()
  
  ds <- datasources
  
  # if no name was provided for the new variable give it a default name
  if(is.null(new.varname)) { 
    new.varname <- paste0(var,'.new')
    warning(paste0('new.varname is not specified ... the new var will be assigned to ',new.varname),immediate.=T,call.=F)
  }
  new.var <- new.varname
  
  if(!is.null(data)) var.dollar <- paste0(data,'$',var)
  else var.dollar <- var 
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(ds, var.dollar)
  
  # if input vector is not a factor stop
  if(typ != 'factor') stop("The input var must be a factor!", call.=FALSE)
  
  # get the current number of levels
  cally <- paste0("levels(", var.dollar, ")")
  xx <- datashield.aggregate(ds[1], as.symbol(cally))  ###check info only for one server reduce time
  if((ll<-length(unique(unlist(xx)))) != length(new.levels)){
    stop(gettextf("Please new.levels must be a vector of size %d",ll), call.=FALSE)
  }
  message(paste0('->recoding levels of ',var,' ...'))
  new.levels.char <- paste0('c(',paste0(new.levels,collapse=','),')') #ex:  "c(0,0,1)"
  cally <- paste0('recodeLevelsDS(',var.dollar,',',new.levels.char,')')
  datashield.assign(ds,new.var,as.name(cally))
  
  if(!is.null(data)) {
    # put in the dataframe data
    callycbind <- paste0('cbind(',data,',',new.var,')')
    datashield.assign(ds,data,as.name(callycbind))
    #clean server side
    datashield.rm(ds,new.var)   
    cat(paste0("You may check the level-recoded variable with the following commands: run.desc.stats('",new.var,"',data='",data,"')"))  
  }else{
    cat(paste0("You may check the level-recoded variable with the following commands: run.desc.stats('",new.var,"')"))
  }
  
}


