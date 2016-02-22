if(exists('bioshare.env'))detach(bioshare.env)
source('utils_analysis.R',echo=F,print.eval=F)



.t.extract<- function(x) 
{ 
  pval <- format(x$p.value,digits = 4); if (as.numeric(pval)<2.2e-16 ) {pval <-'<2.2e-16'}
  est <- x$estimate; diff<-format((est[1] - est[2])[[1]],digits = 3); 
  paste0(diff,'(',pval,')') 
}


##################continous simple subset#################
.run.cont<-function(var.cont,var.cat, vars.list, type = NULL, save = F, print = F)
{
  
  if(missing(var.cat) && missing(var.cont)){
    stop('Either (not both) var.cat or var.cont are required\nvar.cat(factor or categorical) 
         is used when t.test(vars.list~var.cat)...\nvar.cont is used for t.test(var.cont,vars.list) ...',
         call.=F)
  }
  if(missing(varslist)){
    stop('vars.list (a list of variables(cont) to test for ) is(are) required ...',call.=F)
  }
  vars.list<-as.list(vars.list)
  type <- match.arg(type,c('combined','split'))
  
  #define analysis
  if(missing(var.cont) && !missing(var.cat)) analysis <- 1 #with formula
  else if(missing(var.cat) && !missing(var.cont)) analysis <- 2 ##normal ttest
  
  #starting message
  message(paste0('--',toupper(type),' analysis'))
  message('--Continuous variables list: \n',paste0(vars,collapse='; '))
  
  ###preparing progress bar
  total <- length(vars.list)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  i<-1
  ####start looping now
  result<-NULL
  for (var in vars.list){
    
    if(analysis == 1){
      #define formula
      regformula<-paste0('D$',var,'~D$',subset)
      #compute mean by class
      message(paste0('\n==> Computing mean ',var,'~',subset))
      mean<-ds.meanByClass(regformula,type=type)               
      
      #compute the t.test 
      message(paste0('\n==> Computing t-test pvalue ',var,'~',subset,'\nDo not interrupt!...'))
      ttest<-ds.tTest(regformula,paired=F,type=type)
      
    }else if (analysis ==2){
      #compute the t.test 
      message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var1,'vs',var,'\nDo not interrupt!...'))
      x<-paste0('D$',var1)
      y<-paste0('D$',var)
      ttest<-ds.tTest(x=x,y=y,paired=F,type=type)
    }
    
    
    
    #ajust depending on type
    if(type == 'split'){
      pvalue<-lapply(ttest,function(x){x$p.value})
    }else{
      pvalue<-ttest$p.value
    }
    message(paste0('==>P_VALUE FOR ',var,'~',subset,' IS OK'))
    
    #arranging final result
    z<-list(pvalue = pvalue, mean_stats = mean)
    w<-structure(list(z),.Names=var)
    result<-c(result,w)
    setTxtProgressBar(pb, i)
    i<-i+1
  }
  #close progress bar
  close(pb)
  
  ##Saving Result object in file versioned
  if(as.logical(save)){
    date<-format(Sys.Date(),'%d%b%y')
    resultname<-paste0('Cont_split_',subset,'_',date,'.rda')
    message(paste0('\n\n***\tSaving Results for Continuous Variables in <',resultname,'> file.'))
    save(result,file=resultname)
  }
  
  #print result
  if(as.logical(print)){
    return(result)
  }else{
    return(invisible(result))
  }
}


####################double subset ####################
.run.dbl.cont<-function(subset1,subset2,vars.list,type = 'combine',save = F,print = F)
{
  
  if(missing(subset1))stop('subset1 variable is required ...',call.=F)
  if(missing(subset2))stop('subset2 variable is required ...',call.=F)
  if(missing(vars.list)) stop('vars.list is required ...',call.=F)
  
  vars.list<-as.list(vars.list)
  
  
  #adapt starting message
  message(paste0('\n\t\t*** Double subset analysis', toupper(type) ,' ANALYSIS ****\n'))
  message('--variables list: \n',paste0(vars.list,collapse='; '))
  
  #get first subset object names and save them on server side
  subinfobj<-run.get.subset(subset2,c(vars.list,subset1))
  
  #######
  ###preparing progress bar
  total <- length(vars.list)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  i<-1
  
  ####start looping now
  result<-NULL
  
  for(var in vars){
    
    #define formula set
    formulaset<-sapply(subinfobj,function(x){ paste0(x,'$',var,'~',x,'$',subset1)})
    
    ###compute mean by class doule
    message(paste0('\n==> COMPUTING MEAN_VALUE DOUBLE SUBSET FOR ',var,'~',subset2,'|',subset1,'\nDo not interrupt!...'))
    #resultset
    mean<-lapply(formulaset,ds.meanByClass,type=type)
    message(paste0('==> MEAN_VALUE FOR ',var,'~',subset2,'|',subset1,' IS OK'))
    
    #compute the t.test 
    message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var,'~',subset2,'|',subset1,'\nDo not interrupt!...'))
    ttest<-lapply(formulaset,ds.tTest,type=type)
    
    #ajust depending on type
    if(type == 'split'){
      pvalue<-lapply(ttest,function(x){lapply(x,function(y){ y$p.value})})
    }else{
      pvalue<-lapply(ttest,function(x){x$p.value})
    }
    
    message(paste0('==>P_VALUE FOR ',var,'~',subset2,'|',subset1,' IS OK'))
    
    #arranging final result
    z<-list(pvalue = pvalue, mean_stats = mean)
    w<-structure(list(z),.Names=var)
    result<-c(result,w)
    setTxtProgressBar(pb, i)
    i<-i+1
  }
  
  #close progress bar
  close(pb)
  
  ##Saving Result object in file versioned
  if(as.logical(save)){ 
    date<-format(Sys.Date(),'%d%b%y')
    resultname<-paste0('Cont_dblcomb_',subset2,'-',subset1,'_',date,'.rda')
    message(paste0('\n\n***\tSaving Results for Continuous Variables DOUBLE in <',resultname,'> file.'))
    save(result,file=resultname)
  }
  
  #print result
  if(as.logical(print)){
    return(result)
  }else{
    return(invisible(result))
  }
  
}



############################################################################
.run.CC.glm.subset <- function(formula,glm.result,CCsubset=NULL,datasources=NULL)
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
  
  if(is.null(CCsubset)){ 
    data <- unique(extract(vars.list)$holders)
    CCsubset <- paste0('CC.',data)
    warning(paste0("CCsubset is not specified ...the subset will be saved in ", CCsubset, " dataframe on server side"),call.=F,immediate.=T)
  }
  
  if(is.null(datasources)) { datasources = findLoginObjects()}
  ds <- datasources
  
  #define vars.2df and vars.names from var.list 
  vars.2df<-unlist(vars.list)
  vars.names<-extract(vars.list)$elements
  
  #do the subset and completecases
  cally<-paste0('dataframeDS(cbind(',paste0(vars.2df,collapse=','),')',',NULL,FALSE,TRUE,','c(',paste0("'",vars.names,"'",collapse=','),')',',TRUE,TRUE)')
  datashield.assign(ds, CCsubset, as.symbol(cally))
  
  #clean server workspace
  to_rm <- c("complt","cs","dt","rs")
  run.clean(to_rm,datasources=ds)
  
  #info to user
  cat(paste0("You may check the assigned ",CCsubset," dataframe with the following datashield commands: 
   datashield.symbols(opals), ds.colnames('",CCsubset,"') AND ds.dim('",CCsubset,"')")) 
  
  return(invisible(CCsubset))
}




.run.check.obj <- function(obj ,where = NULL,datasources = NULL)
{
  if(is.null(datasources)) datasources = findLoginObjects()
  ds <- datasources
  
  cally <- if(is.null(where)) {call('exists',obj)}else{call('exists',obj,as.name(where))}
  datashield.aggregate(ds,cally)
}



.dummy.var <- function (data,datasources)
{
  if(missing(data)) stop('data is mandatory ...\nplease add data name as character!')
  if(missing(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  # call study length
  call.len <- call('NROW',as.name(data))
  ln <- datashield.aggregate(ds,call.len)
 
  
  message(paste0(paste0(names(ds),collapse=', '), ' dummy variable(s) will be stored in dataframe ',data))
  
  # define levels categories (is the same for all study(ies))
  ctgries <- seq(length(ds))
  lvl.str <- paste0("c(",paste(ctgries,collapse=','),")")
  
  for (i in 1:length(ds)){
    
    effect_name<-names(ds[i])
    rep.str <- paste0("rep(",i,",",ln[[i]],')')
    callfactor <- paste0("asFactorDS(",rep.str,",",lvl.str,")")
    
    # assign cat var to each server 
    #thiscall <- paste0("as.character(",i,")")
    message(paste0('---processing ',effect_name,' dummy ...'))
    datashield.assign(ds[i],"study_effect",as.name(callfactor))
  }
  
  message( '---assigning to server(s)...')
  callcbind<-paste0('cbind(',data,',study_effect)')
  datashield.assign(ds,data,as.name(callcbind))
    
  #clean server side
  #run.rm(names(ds),datasources=ds)
  
  cat(paste0("You may check the stored dummy variables with the following datashield command: ds.colnames('",data,"')"))
}



##############################################
#   SUBSET  by logical or/and complete cases or/and cols or/and rows
#########################
.run.subset<-function(data = NULL,logic = NULL, cols=NULL,rows =NULL,completeCases=FALSE, subsetName = NULL,  datasources=NULL){
  
  
  if(is.null(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  
  #verify data
  if(is.null(data)) {stop('Please specify the data(dataframe) to subset',call.=F)}
  
  has.col.row <- !(is.null(cols) && is.null(rows))
  has.default <- has.col.row || !is.null(logic)
  
  if((!has.default) && (!completeCases)) stop("Nothing to do! Specify param <logic (character, EX: 'AGE >10')> OR/AND <completeCases (TRUE)> ...",call.=F)
  
  
  
  ################
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
    check.X <- Reduce(all,run.isAssigned(cols,data,ds))
    
    # vector not dataframe
    if(cols == varsub){
      X <- paste0(X,'$',cols) 
      is.a.vector <- TRUE
    }
    
  }else{
    one.col <- F
    check.X <- Reduce(all,run.isAssigned(X,datasources=ds))
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
  

 

  #excute server side
  
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
  
  
  message(info) 
  
  #1- default
  
  if(has.col.row) {
    
    cols.uniq <- if (one.col) {unique(c(varsub,cols))} else{cols } #the key is here
    
    cally.r.c  <-  call('subsetDS', dt=X, complt=FALSE, rs=rows,cols.uniq)
    datashield.assign(ds, subsetName, cally.r.c)
    
    #TRACKING
    track <- cally.r.c
    
    #update X for the next steps
    X <- subsetName
  }
  
  
  #do logic if any
  if ( !is.null(logicds)){ 
    
    cally.logic <- call('subsetDS', dt=X, complt=FALSE, rs=NULL, cs=NULL, lg=lg, th=th, varname=varsub)
    datashield.assign(ds, subsetName, cally.logic)
    
    #TRACKING  
    track <- cally.logic
    
  }
  
  #special case to process before any complete cases
  if (one.col && !is.a.vector) {
    cally.vect <- paste0(subsetName,'$',cols)
    datashield.assign(ds,subsetName,as.name(cally.vect))
    
    #TRACKING  
    track <- cally.vect
      
  }
    
  #completecases if any 
  if(completeCases ){
    cally.cc <- call('subsetDS', dt=X, complt=completeCases)
    datashield.assign(ds, subsetName, cally.cc)
    
    #TRACKING  
    track <- cally.cc
  }
  
  #clean server workspace
  run.rm(c( "complt","cs","dt","lg","rs","th","varname"),datasources=ds)
  
  cat(paste0('You may check the assigned subset with this command: run.isAssigned("',subsetName,'")\n'))
  print(track)
  return (invisible(subsetName))

}



.logical2ds <- function(logicexpr=NULL){
  
  if(is.null(logicexpr)) {return (NULL)}
 
  error <- F
  
  lg.char <- unlist(strsplit(logicexpr,'>|<|\\={2}|>\\=|<\\=|!\\='))
  lg.op <- sub('\\w+(>|<|\\={1,2}|>\\=|<\\=|!\\=)\\d+','\\1',logicexpr)
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

