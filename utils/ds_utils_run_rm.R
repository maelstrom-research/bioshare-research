##########################################

#clean workspace server side


ds_utils.env$run.rm <- function(x,all=F,datasources=NULL)
{
  if(all) { 
    call <- 'list = ls()'
  }else{
    if (missing(x)) {stop('arg x is required',call.=F)}
    quote.x <- paste0("'",x,"'",collapse=',')
    call <- paste0('list =c(',quote.x,')')
  }
  if(is.null(datasources)) datasources = findLoginObjects()
  ds <- datasources
  
  datashield.rm(ds,as.symbol(call))
}
