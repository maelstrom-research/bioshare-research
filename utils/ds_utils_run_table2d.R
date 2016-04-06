#this function computes 2 x 2 table and chi2 

ds_utils.env$run.table2d <- function(x,y, data = NULL, col.percent = F,row.percent = F, chisq.test = T,split = F,datasources = NULL,...) 
{
  if(missing(x)) stop ('x variable (character) is required ...',call.=F)
  if(missing(y)) stop ('y variable (character) is required ...',call.=F)
  if(is.null(datasources)) datasources = findLoginObjects()
  ds <- datasources
  
  #ALL PARAM ARE CHECKED #then run the list version if x is a list
  ### <------------  recursive 
  if(length(x)>1) {
    res.list <- lapply(x,run.table2d,y,data, col.percent,row.percent, chisq.test,split,ds,...)
    names(res.list) <- unlist(x)
    return (invisible(res.list))
  }
  #---end of recursive----
  
  
  
  #get dot dot dot info
  dots <- list(...)
  correct <- if (any(grepl('correct',names(dots)))){ dots$correct}else{F}
  
  
  #start server side computation
  if(is.null(data)) {
    check.x <- do.call(all,run.isAssigned(x))
    if(check.x) {
      check.y <- do.call(all,run.isAssigned(y))
      if(!check.y) stop (paste0('"',y,'" is not found in some server(s). Did you forget [data] argument ?'))  
    }else {
      stop (paste0('"',x,'" is not assigned in some server(s). Did you forget [data] argument ?')) 
    }
    callt2 <- paste0('table2dDS(',x,',',y,')')
  }else {
    callt2 <- paste0('table2dDS(',data,'$',x,',',data,'$',y,')')
  }
  
  t2.res <-  datashield.aggregate(ds,as.symbol(callt2))
  
  # client side
  process.result <- function (result)
  {
    with.pooled <- length(result) > 1
    t2.res <- result  #start here
    
    t2.mess <- .vectorize(t2.res,'message')
    t2.bad.idx <- which(grepl('invalid',t2.mess,ignore.case=T))
    
    
    t2d.i <- .vectorize(t2.res,subscript='table',simplify=F)
    
    null.study <- names(which(sapply(t2d.i,function(x)all(x==0))))
    
    t2.ok <- !length(t2.bad.idx)
    
    #get basic infos
    cnames <- colnames(t2d.i[[1]])
    rnames <- row.names(t2d.i[[1]])
    #combine table
    if(with.pooled){
      Pooled <- Reduce('+',t2d.i)
      t2d.i.and.pool <- c(t2d.i,list(Pooled = Pooled))
      res <- t2d.i.and.pool
    }else{
      res <- t2d.i
    }
    
    # rm margin
    .rm.margin <- function(x) { as.matrix(x[-nrow(x),-ncol(x)])}
    t2.nomarg <- lapply(res,.rm.margin)
    
    if(row.percent && !col.percent){
      res <- lapply(t2.nomarg, function(x){
        p <- prop.table(x,1)*100
        p.marg<- margin.table(p,1)
        x.marg <- margin.table(x,1)
        p <- round(cbind(p,p.marg),2) #prop
        x <- round(cbind(x,x.marg),2) #count
        p.x <- paste0(p,'(n = ',x,')')
        p.x <- data.frame(matrix(p.x,nrow(p),ncol(p)))
        colnames(p.x) <- cnames
        row.names(p.x) <- row.names(x)
        p.x
      } )
    }
    if(col.percent){
      res <- lapply(t2.nomarg, function(x){
        p <- prop.table(x,2)*100
        p.marg <- margin.table(p,2)
        x.marg <- margin.table(x,2)
        p <- round(rbind(p,p.marg),2) #prop
        x <- round(rbind(x,x.marg),2) #count
        p.x <- paste0(p,'(n = ',x,')')
        p.x <- data.frame(matrix(p.x,nrow(p),ncol(p)))
        row.names(p.x) <- rnames
        colnames(p.x) <- colnames(x)
        p.x
      } )
    }
    
    #correct result: invalid result display total margin only
    if(!t2.ok) {
      res[t2.bad.idx] <- t2d.i[t2.bad.idx]
      if(with.pooled) res$Pooled <- t2d.i.and.pool$Pooled
    }
    
    #now compute chisquare test 
    if(chisq.test) {
      
      chi2 <- if((!t2.ok)) {
        'Invalid table(s): all entries of the table must be nonnegative and finite (>5)'
      }else{
        if(with.pooled) {
          Pooled.data <- t2.nomarg$Pooled
          check.cell <- all(Pooled.data > 5)
          if(check.cell){try(chisq.test(Pooled.data,correct = correct),silent=T)}else{ 'Invalid table(s): some cell value are less (<) than 5 '}
        }else{
          Study.data <- t2.nomarg[[1]]
          check.cell <- all(Study.data > 5)
          if(check.cell){try(chisq.test(Study.data,correct = correct),silent=T)}else{ 'Invalid table(s): some cell value are less (<) than 5 '}
        }
      }
      
      name.chi <- if(with.pooled) { 'Pooled'}else{ names(t2.nomarg[1])}
      chi2list <- structure(list(chi2),.Names = name.chi)
      
      if(with.pooled && length(null.study) ) {
        warning(paste0('[INDICATION]: ',paste0(null.study,collapse=','),' is(are) excluded from pooled analysis for ',x,' variable'),call.=F)
      }
      
      res <- c(stat = res, chi2 = chi2list)
    }
    
    return (res)
  } #END PROCESS.RESULT
  
  if(split) {
    final <- c()
    for(i in 1:length(t2.res)){final <- c(final,process.result(t2.res[i])) }
    
  }else {
    final <- process.result(t2.res) 
  }
  
  # now get final results
  
  #computation info
  info <- paste0('\n-----------  ',x,'(row) X ',y,'(col)  ------------\n\n')
  
  #names(final) <- info
  
  cat(info)
  print (final)
  
  invisible(final)
  
}