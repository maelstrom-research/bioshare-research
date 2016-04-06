#this function computes desc statistic:  meansd for a continuous variable
#across all studies and pool the results
#



ds_utils.env$run.meanSd<-function(var,data = NULL,datasources = NULL)
{
  if(is.null(datasources)) { datasources = findLoginObjects()}
  ds <- datasources
  
  if(missing(var)) stop('var is required.',call.=F)
  
  ##### cases of a list of variables : recursive
  if(length(var)>1) {
    var <- unlist(var)
    res <- sapply(var,run.meanSd,data,ds,simplify=F,USE.NAMES=T)
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
  
  #tocall <- paste0('quantileMeanDS(',var,')')
  tocallmean <- paste0('meanDS(',var,')')
  tocallength <- paste0('length(',var,')')
  tocallnumna<-paste0('numNaDS(',var,')')
  tocallvar <- paste0('varDS(',var,')')
  rs.mean <- .vectorize(datashield.aggregate(ds,as.name(tocallmean)),1); 
  rs.numna <- .vectorize(datashield.aggregate(ds,as.name(tocallnumna)),1)
  rs.length <- .vectorize(datashield.aggregate(ds,as.name(tocallength)),1)
  rs.var <- .vectorize(datashield.aggregate(ds,as.name(tocallvar)),1)
  
  validN <- rs.length - rs.numna 
  
  .var.pooled <- function(variances,means,weights )
  {
    v<- variances
    m <- means
    l <- weights
    l.minus1 <- l-1
    m.total <- weighted.mean(m,l)
    err.ss <- sum(l.minus1*v) # overall error sum of squares
    tg.ss <- sum(l*(m-m.total)^2) # total (overall) group sum of squares
    l.total <- sum(l)
    v.total <- (err.ss + tg.ss)/(l.total-1)
    #s.ag <- sqrt(v.ag)
    return (v.total)
  }
  
  
  rs.mean.agg <- weighted.mean(rs.mean,validN,na.rm=T)
  rs.var.agg <- .var.pooled(rs.var, rs.mean, validN)
  validN.agg <- sum(validN)
  names.w.p <- c(names(ds),'Pooled')
  
  rs.mean.with.pooled <- c(rs.mean,rs.mean.agg)
  names(rs.mean.with.pooled) <- names.w.p
  rs.var.with.pooled <- c(rs.var,rs.var.agg)
  names(rs.var.with.pooled) <- names.w.p
  validN.w.p <- c(validN,validN.agg) 
  names(validN.w.p) <- names.w.p
  
  sd.w.p<-round(sqrt(rs.var.with.pooled),2)
  m.w.p <- round(rs.mean.with.pooled,2) 
  meanSd <- paste0(m.w.p,'(',sd.w.p,') (n = ',validN.w.p,')')
  
  res <- data.frame(meanSd,row.names=names.w.p,stringsAsFactors=F)
  
  res
}