## --------internal functions------------ #

#get opals login object(s) in the env
ds_utils.env$findLoginObjects <- dsBaseClient:::findLoginObjects
#check the class of an object
ds_utils.env$checkClass <- dsBaseClient:::checkClass

#extract elements and object form server side vector (e.g. D$AGE_YRS)
ds_utils.env$extract <- dsBaseClient:::extract



# -------------------------------------------- #

#check if an object is assigned
ds_utils.env$run.isAssigned <- function(obj ,where = NULL,datasources = NULL)
{
  if(is.null(datasources)) datasources = findLoginObjects()
  ds <- datasources
  
  #define cally expression 
  cally <- if(is.null(where)) {
    call('exists',obj)
  }else{
    #see if where is present in all ds
    #check.where <- sum(unlist(datashield.symbols(ds)) %in% where)  testing
    check.where <- do.call(all,run.isAssigned(where,datasources=ds))
    if(check.where) {  ############<---------------------
                       call('exists',obj,as.name(where)) 
    }else {
      stop(paste0('"',where,'" is not available in some server(s). \nCheck availability in server(s):-> run.isAssigned("',where,'")'),call.=F)
    }
  }
  
  datashield.aggregate(ds,cally)
}



# -------------------------------------------- #

#@internal: process logical string within run.subset
ds_utils.env$.logical2ds <- function(logicexpr=NULL){
  
  if(is.null(logicexpr)) {return (NULL)}
  
  error <- F
  
  lg.char <- unlist(strsplit(logicexpr,'>|<|\\={2}|>\\=|<\\=|!\\='))
  lg.op <- sub('\\w+(>|<|\\={2}|>\\=|<\\=|!\\=)\\d+(\\.\\d+)?','\\1',logicexpr)
  lg.var <- lg.char[1]
  lg.th <- lg.char[2]
  
  if((length(lg.char) !=2) || is.logical(lg.th) || is.na(as.numeric(lg.th)) ) error <- T  
  all.lg <- c('>' = 1, 
              '>=' = 2,
              '<' = 3,
              '<=' = 4,
              '==' = 5,
              '!='= 6
  )
  
  indic <- c('>' = 'gt', 
             '>=' = 'ge',
             '<' = 'lt',
             '<=' = 'le',
             '==' = 'eq',
             '!='= 'diff'
  )
  
  if(! lg.op %in% names(all.lg)) error <- T
  
  if(error)  stop(paste0(' "',logicexpr,  '" is not a valid logical expression...'),call.=F)
  
  return (  list(lg = all.lg[[lg.op]],varsub= lg.var,th =lg.th,indic =indic[[lg.op]]) )  
  
}



# -------------------------------------------- #


#vectorize list like structure
ds_utils.env$.vectorize <- function(x,subscript,simplify = T) 
{ 
  if(simplify) sapply(x,'[[',subscript)
  else lapply(x,'[[',subscript)
}



