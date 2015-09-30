bioshare.env <- new.env()

############################################################################
# utils functions
###########################################################################
# check if an object is defined
bioshare.env$isDefined <- dsBaseClient:::isDefined
#get opals login object(s) in the env
bioshare.env$findLoginObjects <- dsBaseClient:::findLoginObjects
#check the class of an object
bioshare.env$checkClass <- dsBaseClient:::checkClass

#check if an object is assigned
bioshare.env$isAssigned <- dsBaseClient:::isAssigned
#extract elements and object form server side vector (e.g. D$AGE_YRS)
bioshare.env$extract <- dsBaseClient:::extract
#pooled mean
bioshare.env$run.pooled.mean<- dsBaseClient:::getPooledMean



#########################################################################################
#    analytics functions here below 
########################################################################################

################ categorical (table2d) for multiple variables ######################
bioshare.env$run.cat<-function(subset,vars.list,type = NULL,save=F, print= F)
{
  if(missing(subset)) stop('subset variable is required ...',call.=F)
  if(missing(vars.list)) stop('var(s) is(are) required ...',call.=F)
  type <- match.arg(type,c('combine','split'))
  vars.list <-as.list(vars.list)
    
  #starting message
  message(paste0('--',toupper(type),' analysis'))
  message('--Categorical variables list: \n',paste0(vars.list,collapse='; '))
  
  ###preparing progress bar
  total <- length(vars.list)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  i<-1
  
  ####start looping now
  result<-NULL
  for(var in vars.list){
    #compute table2D
    message(paste0('\n==> Computing chi-square ',var,' X ',subset,'\nDo not interrupt!...'))
    chi_square<-ds.table2D(x=paste0('D$',var), y=paste0('D$',subset), type=type,warningMessage=F)
    message(paste0('==> chi-square ',var,' X ',subset,' is completed'))
    
    #arranging final result
    w<-structure(list(chi_square),.Names=var)
    result<-c(result,w)
    setTxtProgressBar(pb, i)
    i<-i+1
  }
  #close progress bar
  close(pb)
  
  if(as.logical(save)){
    ##Saving Result object in file versioned
    date<-format(Sys.Date(),'%d%b%y')
    resultname<-paste0('Cat_split_',subset,'_',date,'.rda')
    message(paste0('\n\n***\tSaving Results for Categorical Variables in <',resultname,'> file.'))
    save(result,file=resultname)
  }
  
  #print result
  if(as.logical(print)){
    return(result)
  }else{
    return(invisible(result))
  }
}


########INTERNAL FUNCTION
bioshare.env$run.get.subset<-function(subvar = NULL,vars.list=NULL,dt = NULL, datasources=NULL){
  
  #subset by sub_by first to generate the first subset
  if(is.null(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  
  #verify dt (datatable)
  if(is.null(dt)) {message('dt(data table) is not specified ... default table "D" (if available on server side) will be used '); dt <- 'D'}

  #Sanity check : subvar should always be a factor
  subset.class <- checkClass(ds,paste0(dt,'$',subvar))
  if(subset.class != 'factor') stop('subvar must be a categorical variable ...',call.=F)
  
  message(paste0('\n===> Subsetting ',dt, ' by ',subvar,'\nWait please do not interrupt!...'))
  vars<-unique(c(subvar,unlist(vars.list)))
  cally <- call('subsetDS',dt = dt, complt=F, rs=NULL, cs=vars)
  datashield.assign(ds,'newdt',cally)   #new df with only the variables needed (avoid time consuming)
  
  cally <- call('subsetByClassDS','newdt', subvar)
  datashield.assign(ds,'subobj',cally)
  message(paste0('===> Subset of ',dt, ' by ',subvar,' is created'))
  
  #define infoname
  message('\n===>Assigning subsetted objects on server side...\nWait please do not interrupt!...')
  cally <- as.name('namesDS(subobj)')
  subinfo<-datashield.aggregate(ds,cally)[[1]] #get names of subsetted object
  
  #define name of object to be assigned
  subinfobj<-sapply(subinfo,function(x){  #assign new subsetted object in server and return their name
    newname<-paste0(dt,'.',sub('\\.level_(\\d+)$','\\1',x))  #transform names
    toassign<-paste0('subobj$',x)
    datashield.assign(ds,newname,as.name(toassign))    ###assigned objs are in server
    return(newname)
  })
  
  #adjust name according to new objects
  names(subinfobj)<-subinfobj
  
  to.rm <- c("complt","cs","dt","newdt","rs","subobj")
  for(x in to.rm) {datashield.rm(ds,x)} #Clean space
  
  message(paste0('-- Object ',subinfobj ,' is assigned',collapse='\n'))
  return(invisible(subinfobj))
}


#########################################################################################################
########################################### MODELING UTILS ##############################################

####################GLM
bioshare.env$run.meta.glm<-function(formula, family, ref, datasources,save = F, print = T,...)
{
  if(missing(formula)){
    stop('formula is required ...',call.=F)
  }
  if(missing(family)){
    stop('family is required ...',call.=F)
  }
  if(missing(datasources)){
    datasources<-findLoginObjects()
  }
  ds <- datasources
  
  if(length(ds) > 1){
    if(missing(ref) || !ref %in% names(ds)) stop('ref study is required when running a glm with more than one studies!\nPlease check that reference study is correctly spelled...',call.=F)
    
    formulasplit<-unlist(strsplit(formula,'~'))
    outcomevar<-formulasplit[1]
    explanvars<-formulasplit[2]
    
    effect_name <- paste0(names(opals),'_effect')
    effect_name <- effect_name[-(which(grepl(ref,effect_name)))]
    effect.vars.in.formula <- paste(effect_name,collapse='+')
    
    if(!is.na(explanvars)) explanvars <- paste(effect.vars.in.formula,explanvars,sep = '+')
    else explanvars<-effect.vars.in.formula
    
    #update formula with study effect dummies vars
    formula<-paste0(outcomevar,'~',explanvars)  
  }
  
  message('->running glm...\n wait do not interrupt!\n')
  message(paste0('formula for glm: ',formula))  
  message(paste0('family for glm: ',family))
  
  if(missing(...)){
    glm.result<-ds.glm(formula=formula,family=family,datasources=ds)
  }else{
    glm.result<-ds.glm(formula=formula,family=family,datasources=ds,...)
  }
  
  
  ##Saving Result object in file versioned
  if(as.logical(save)){
    
    date<-format(Sys.Date(),'%d%b%y')
    save_file<-paste0('glm_result','_',date,'.rda')
    message(paste0('saving result in ',save_file,'...'))
    save(glm.result,file=save_file)
  }
  
  #print result
  if(as.logical(print)){
    return(glm.result)
  }else{
    return(invisible(glm.result))
  }
  
}





####################################################################################
#this function create a formula according to the model, outcome and exposition vars

bioshare.env$run.update.formula<-function(outcome,expo,model,dt)
{
  mf <- match.call(expand.dots = FALSE)
  arg.call <- names(mf)[-1]
  arg.names<-c('outcome','expo','model','dt')
  missing.arg<-which(!arg.names %in% arg.call)
  missing.call <- paste(arg.names[missing.arg],collapse=' and ')
  if(length(missing.arg)>1) stop(paste0(missing.call,' are required'),call.=F)
  else if (length(missing.arg)==1) stop(paste0(missing.call,' is required'),call.=F)
  
  fm <-  paste0(dt,'$',outcome,'~',dt,'$',model,'+',expo)
  fm <- gsub('+',paste0('+',dt,'$'),fm,fixed=T)
  fm <- gsub('*',paste0('*',dt,'$'),fm,fixed=T)  
  fm
}



################################################################################################
#this function run a glm based on the model
#param outcome = the outcome variable in character (ex: 'SYM_SBREATH')
#param expo =  the exposition variable in character (ex: 'NO2_ESCAPE')
#param model = the model to apply in glm, it's a truncated formula made of -->
#-->counfounding variables (ex: '~D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_STATUS') without outcome and expo
#param family = glm family (ex: 'gaussian' or 'binomial',...) 
#param ...= any params that go to glm (except formula) (i.e: offset, data, weights,wiewIter, datasource)
#param Ncases = a boolean (TRUE or FALSE(default)) wether to compute N cases or not in the final result  

bioshare.env$run.model<-function(outcome,expo,model,family,dt,Ncases=FALSE,...)
{
  if(missing(outcome)) stop('outcome is required...',call.=F)
  if(missing(expo)) stop('exposition variable is required...',call.=F)
  if(missing(model)) stop('model formula is required...',call.=F)
  #verify dt (datatable)
  if(missing(dt)) stop('dt(datatable) is mandatory',call.=F)
  
  #update formula
  formula <- run.update.formula(outcome,expo,model,dt)
  
  #run glm 
  glm.res <- run.meta.glm(formula,family,print=T,...)
  
  #extract glm stats and process result
  glm.stats<-run.extract.glm.stats(glm.res)
  
  #compute N valid (complete cases)
  if(Ncases) {
    Nvalid <- glm.res$nsubs
    glm.stats$Ncases = Nvalid
  }
  
  glm.stats$results <- paste0(glm.stats$stats[expo,],' [N = ',glm.stats$Ncases,']')
  
  #print glm stats
  cat(glm.stats$formula,'\n')
  cat(glm.stats$results)
}


##############################################
#function to extract glm result :P_OR(p.value) 
bioshare.env$run.extract.glm.stats <- function(glm.result)
{
  if(missing(glm.result)) stop('Please provide a valid glm result...',call.=F)
  glm.family <- glm.result$family$family
  glm.coef <- coef(glm.result)
  if(grepl("poisson|binomial", glm.family)){
    stats <- data.frame(OR_with_pvalue = apply(glm.coef,1,function(x) {
      OR <- round(x['P_OR'],3)
      pvalue<- format(x['p-value'],digits=4) ; pvalue <- if(as.numeric(pvalue)< 2.2e-16) {"<2.2e-16"}else{pvalue}
      low <- round(x[length(x)-1],3)
      high <- round(x[length(x)],3)
      
      paste0(OR,' [',low,' - ',high,'] (',pvalue,')')
    }),stringsAsFactors = F
    )
  }else if (glm.family == 'gaussian'){
    stats <- data.frame(Estimate_with_pvalue = apply(glm.coef,1,function(x) {
      estimate <- round(x['Estimate'],3)
      pvalue<- format(x['p-value'],digits=4) ; pvalue <- if(as.numeric(pvalue)< 2.2e-16) {"<2.2e-16"}else{pvalue}
      low <- round(x[length(x)-1],3)
      high <- round(x[length(x)],3)
      
      paste0(estimate,' [',low,' - ',high,'] (',pvalue,')')
    }),stringsAsFactors = F
    )
  }
  glm.coef[2,dim(glm.coef)[2]-1]
  formula <-  gsub('\\s+',' ',formula(glm.result)) #fix spaces in formula
  list(formula = formula, stats = stats)
}


#################################################################
#DOC TO DO 
#
bioshare.env$run.NA.glm.subset<-function(glm.result,formula ,NAsubset=NULL,datasources=NULL)
{
  mf <- match.call(expand.dots = FALSE)
  arg.names <- names(mf)
  
  if ((! 'glm.result' %in% arg.names) && (!'formula' %in% arg.names)) {stop('Either a glm run or a formula is required ...',call.=F)}
  if(is.null(datasources)) { datasources = findLoginObjects()}
  if(is.null(NAsubset)){ 
    warning("NAsubset is not specified ...the subset will be saved in 'NAdf' object on server side",call.=F,immediate.=T)
    NAsubset <- 'NAdf'
  }
  ds <- datasources
  #parsing var names from glm formula
  if(!'formula' %in% arg.names){
    glm.formula <- gsub('\\s+','',glm.result$formula)
  }else {
    glm.formula <- formula
  }
  vars.list<-strsplit(glm.formula,'\\+|~|\\*|\\|+')
  vars.2df<-unlist(vars.list)
  vars.names<-extract(vars.list)$elements
  
  # create a df for the selected variables
  cally<-paste0('dataframeDS(list(',paste0(vars.2df,collapse=','),')',',NULL,FALSE,TRUE,','c(',paste0("'",vars.names,"'",collapse=','),')',',TRUE,FALSE)')
  datashield.assign(ds,'RD',as.symbol(cally))
  
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
  to_rm <- c("cc","complt","cs","dt","lg" , "rs", "th","varname"  )
  invisible(sapply(to_rm,function(x) datashield.rm(ds,x)))
}


bioshare.env$run.NA.stats<-function(var,iscat=F,datasource = NULL)
{
  if(is.null(datasource)) { datasource = findLoginObjects()}
  ds <- datasource
  if(as.logical(iscat)) {
    tocall <- paste0('table1dDS(',var,')')
    rs <- datashield.aggregate(ds,as.name(tocall))
    
    res <- lapply(rs,function(x){
      rsi<-t(as.matrix(x$table))    
      rni<-paste0(rownames(rsi),':')
      stats<-as.matrix(paste0(rsi,"(",round((rsi/rsi[nrow(rsi)])*100,2),') [N = ',rsi[nrow(rsi)],']'))
      data.frame(stats,row.names=rni)
    })
     
   
  }else{
    
    .vectorize <- function(x,subscript) {sapply(x,'[[',subscript)}
    
    #tocall <- paste0('quantileMeanDS(',var,')')
    tocallmean <- paste0('meanDS(',var,')')
    tocallength <- paste0('length(',var,')')
    tocallnumna<-paste0('numNaDS(',var,')')
    tocallvar <- paste0('varDS(',var,')')
    rs.mean <- .vectorize(datashield.aggregate(ds,as.name(tocallmean)),1); 
    rs.numna <- .vectorize(datashield.aggregate(ds,as.name(tocallnumna)),1)
    rs.length <- .vectorize(datashield.aggregate(ds,as.name(tocallength)),1)
    rs.var <- .vectorize(datashield.aggregate(ds,as.name(tocallvar)),1)
      
    validN<-rs.length - rs.numna  
    sd<-round(sqrt(rs.var),2)
    rs.mean <- round(rs.mean,2)
    res <- paste0(rs.mean,'(',sd,') [N = ',validN,']')
    meanSd.stats <- as.matrix(res)
    res <- data.frame(meanSd.stats,row.names=names(ds))
    
  }  
  
  res
}


#################### create dummy study effect vars #####

bioshare.env$run.dummy.study <- function (datasources = NULL)
{
  if(is.null(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  message('->Assigning Zeros vector(s) with appropriate size to the respective servers\n')
  # call length
  ln <- datashield.aggregate(ds,as.symbol('NROW(D)'))
  #make zeros vector in each server 
  for (i in 1:length(ds)) { datashield.assign(ds[i],'zeros.dummy',call('rep',0,ln[[i]])) }
  
  message('->Creating dummies effect size variables for each study...')
  
  for (i in 1:length(ds)){
    
    effect_name<-paste0(names(ds[i]),'_effect')
    #assign 1 to study and 0 to others
    message(paste0('---processing ',effect_name,'...'))
    datashield.assign(ds[i],effect_name,as.name('zeros.dummy+1'))
    datashield.assign(ds[-i],effect_name,as.name('zeros.dummy'))
    
    
    cat(capture.output(datashield.aggregate(ds,as.name(paste0('meanDS(',effect_name,')')))))
    
  }  
}

##########################################

#close everything
bioshare.env$run.close<-function(all=F)
{
  #detect opals 
  objs <- ls(name=.GlobalEnv)
  if(length(objs)){
    for(obj in objs){
      obj<- eval(parse(text=obj))
      if (is.list(obj) && (class(obj[[1]]) == 'opal')){
        message('Closing opal(s) server connection(s)')
        obj.opal <- obj
        datashield.logout(obj.opal)
        cat(paste0( names(obj.opal),' server is disconnected...'),sep='\n')
      }
    }
  }

 if(as.logical(all)) rm(list=objs,envir=.GlobalEnv)
 else rm(bioshare.env,pos=search())  
 detach(bioshare.env,pos=search())
 cat('bioshare environnment is now detached from memory...')  
}


# attach bioshare env
attach(bioshare.env)






