#this function compute desc statistic: table1d for a factor variable
#across all studies and pool the results
#


ds_utils.env$run.table1d<-function(var,data = NULL,datasources = NULL)
{
  if(is.null(datasources)) { datasources = findLoginObjects()}
  ds <- datasources
  
  if(missing(var)) stop('var is required.',call.=F)
  
  ##### cases of a list of variables : recursive
  if(length(var)>1) {
    var <- unlist(var)
    res <- sapply(var,run.table1d,data,ds,simplify=F,USE.NAMES=T)
    print(res)
    return(invisible(res))
  }
  
  #check data and update var
  if(!is.null(data)) {
    var<- paste0(data,'$',var)
  }else {
    DATA <- extract(var)$holders
    if(is.na(DATA)) {
      var.exist <- do.call(all,run.isAssigned(var,datasources=ds))
      if(!var.exist) stop (paste0('"',var,'"',' is not present in some server(s)'),call.=F)
    }
    
  }
  
  message('Running. Please wait...')
  
  tocall <- paste0('table1dDS(',var,')')
  rs <- datashield.aggregate(ds,as.name(tocall))
  
  #XXXXXXXX 
  rs.tab<-.vectorize(rs,'table')
  rs.mess <- .vectorize(rs,'message')
  #get the message validity
  rs.ok <- !any(grepl('invalid',rs.mess,ignore.case=T))
  #compute pooled tab
  Pooled <- apply(rs.tab,1,function(x) sum(as.numeric(x)))
  
  #put everything together
  rs.tab.with.pooled <- cbind(rs.tab,Pooled)
  k <- apply(rs.tab.with.pooled,2,function(x) {x<-as.numeric(x); pct <- round(x/x[length(x)]*100,2); paste0(pct,'(n = ',x,')')})
  res <- data.frame(k,row.names=row.names(rs.tab),stringsAsFactors=F)
  
  return (res)
}
