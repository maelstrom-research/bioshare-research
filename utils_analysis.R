bioshare.env <- new.env()

############################################################################
# utils functions
###########################################################################
# check if an object is defined
bioshare.env$isDefined <- dsBaseClient:::isDefined
#get opals login object(s) in the env
bioshare.env$findLoginObjects <- dsBaseClient:::findLoginObjects
#check if an object is assigned
bioshare.env$isAssigned <- dsBaseClient:::isAssigned

#########################################################################################
#    analytics functions here below 
########################################################################################

################ categorical (table2d) for multiple variables ######################
bioshare.env$run.cat<-function(subset=NULL,vars=NULL,type='combine',save=F, print= F)
{
  if(missing(subset)){
    stop('subset variable is required ...',call.=F)
  }
  if(missing(vars)){
    stop('var(s) is(are) required ...',call.=F)
  }else if (class(vars) != 'list'){
    vars<-as.list(vars)
  }
    
  #adapt starting message
  message(paste0('\n\t\t*** STARTING CATEGORICAL VARIABLES ', toupper(type) ,' ANALYSIS ****\n'))
  message('--CATEGORICAL VARIABLES SELECTED: \n',paste0(vars,collapse='; '))
  
  
  ###preparing progress bar
  total <- length(vars)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  i<-1
  
  ####start looping now
  result<-NULL
  for(var in vars){
    #compute table2D
    message(paste0('\n==> COMPUTING CHI-SQUARE FOR ',var,' X ',subset,'\nDo not interrupt!...'))
    chi_square<-ds.table2D(x=paste0('D$',var), y=paste0('D$',subset), type=type,warningMessage=F)
    message(paste0('==> COMPUTATION OF CHI-SQUARE FOR ',var,' X ',subset,' IS OK'))
    
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

##################continous simple subset#################
bioshare.env$run.cont<-function(subset = NULL,var1=NULL, vars = NULL, type = 'combine', save = F, print = F)
{
  
  if(missing(subset) && missing(var1)){
    stop('subset or var1 are required ...',call.=F)
  }
  if(missing(vars)){
    stop('var(s) is(are) required ...',call.=F)
  }
  vars<-as.list(vars)
  
  #define analysis
  if(missing(var1) && !missing(subset)) analysis <- 1 #with formula
  else if(missing(subset) && !missing(var1)) analysis <- 2 ##normal ttest
  
  #adapt starting message
  message(paste0('\n\t\t*** STARTING CONTINUOUS VARIABLES ', toupper(type) ,' ANALYSIS ****\n'))
  message('--CONTINUOUS VARIABLES SELECTED: \n',paste0(vars,collapse='; '))
  
  ###preparing progress bar
  total <- length(vars)
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  i<-1
  ####start looping now
  result<-NULL
  for (var in vars){
    
    if(analysis == 1){
      #define formula
      regformula<-paste0('D$',var,'~D$',subset)
      #compute mean by class
      message(paste0('\n==> COMPUTING MEAN_VALUE FOR ',var,'~',subset))
      mean<-ds.meanByClass(regformula,type=type)               
      message(paste0('==> MEAN_VALUE FOR ',var,'~',subset,' IS OK'))
      
      #compute the t.test 
      message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var,'~',subset,'\nDo not interrupt!...'))
      ttest<-ds.tTest(regformula,paired=F,type=type)
      
    }else if (analysis ==2){
      #compute the t.test 
      message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var1,'~',var,'\nDo not interrupt!...'))
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



####################double subset ##############dsBaseClient:::findLoginObjects()######
bioshare.env$run.dbl.cont<-function(subset1=NULL, vars = NULL, subset2 = NULL, type = 'combine', save = F, print = F)
{
  
  if(missing(subset1)){
    stop('subset1 variable is required ...',call.=F)
  }
  if(missing(subset2)){
    stop('subset2 variable is required ...',call.=F)
  }
  if(missing(vars)){
    stop('var(s) is(are) required ...',call.=F)
  }else if (class(vars) != 'list'){
    vars<-as.list(vars)
  }
  
  #adapt starting message
  message(paste0('\n\t\t*** STARTING CONTINUOUS DOUBLE SUBSET ', toupper(type) ,' ANALYSIS ****\n'))
  message('--CONTINUOUS VARIABLES SELECTED: \n',paste0(vars,collapse='; '))
  
  #get first subset object names and save them in server side
  subinfobj<-run.get.subset(subset2,c(vars,subset1))
  
  #######
  ###preparing progress bar
  total <- length(vars)
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



########INTERNAL FUNCTION
bioshare.env$run.get.subset<-function(subvar = NULL,vars=NULL){
  #subset by sub_by first to generate the first subset
  message(paste0('\n===> FIRST SUBSET BY ',subvar,'\nWait please do not interrupt!...'))
  vars<-c(subvar,unlist(vars))
  ds.subset(x='D', subset='newD', cols=vars)  #new df with only the variables needed (avoid time consuming)
  ds.subsetByClass('newD',subsets='subobj',variables= subvar)
  message(paste0('===> FIRST SUBSET BY ',subvar,' IS CREATED'))
  
  #define infoname
  message('\n===>ASSIGNING FRIST SUBSET OBJECTS IN SERVER SIDE...\nWait please do not interrupt!...')
  subinfo<-ds.names('subobj')[[1]]
  
  #####define formula sets
  #define name of object to be assigned
  subinfobj<-sapply(subinfo,function(x){  #assign new objects of first subset in server and return their name
    newname<-sub('\\.level_(\\d+)$','\\1',x)
    toassign<-paste0('subobj$',x)
    ds.assign(toassign,newname)    ###assigned objs are in server
    return(newname)
  })
  
  #adjust name according to new objects
  names(subinfobj)<-subinfobj
  message('===>FIRST SUBSET OBJECTS ASSIGNED IN SERVER SIDE OK')
  return(invisible(subinfobj))
}


####################GLM
bioshare.env$run.meta.glm<-function(formula, family,opals, ref, type = 'combine', save = F, print = F,...)
{
  if(missing(formula)){
    stop('formula is required ...',call.=F)
  }
  if(missing(family)){
    stop('family is required ...',call.=F)
  }
  if(missing(opals)){
    stop('opal(s) environment is required ...',call.=F)
  }
  
  message('starting metaGLM...')
  message(paste0('formula for glm: ',formula))
  message(paste0('family for glm: ',family))
  
  message('getting data ready ....\nwait do not interrupt!')
  formulasplit<-unlist(strsplit(formula,'~'))
  outcomevar<-formulasplit[1]
  explanvars<-formulasplit[2]
  
  if(length(opals) > 1){
    
    zero.flag<-ds.exists('ZEROS',datasources=opals[1])
    if(!as.logical(zero.flag)){
      
      var_class<-unlist(unique(ds.class(outcomevar)))
      zero_to_assign<-numeric(length(opals))  
      
      if(var_class == 'factor'){
        ds.replaceNA(paste0(outcomevar,'-',outcomevar),zero_to_assign,'ZEROS')    
      }else if (var_class %in% c('integer','numeric')){
        ds.replaceNA(outcomevar,zero_to_assign,'TEMP')
        ds.assign('TEMP-TEMP','ZEROS')
      }
      
      message('->Creating dummies effect size variables for each study...')
      for (i in 1:length(opals)){
        opal<-opals[i]
        effect_name<-paste0(names(opal),'_effect')
        if(!grepl(ref,effect_name)){ #only for non-ref studies
          message(paste0('---processing ',effect_name,'...'))
          ds.assign('ZEROS+1',effect_name,opal)
          ds.assign('ZEROS',effect_name,opals[-i])
          print(ds.mean(effect_name,'split',opals))
          explanvars<-paste0(effect_name,'+',explanvars)
        }
        rm(opal)
      }
    }
  
      
    
    formula<-paste0(outcomevar,'~',explanvars)    
    message('->running glm...\n wait do not interrupt!\n')
    message(paste0('new formula: ',formula))
  }
  
  if(missing(...)){
    glm.result<-ds.glm(formula=formula,family=family)
  }else{
    glm.result<-ds.glm(formula=formula,family=family,...)
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



################################################################################################
#this function run a glm based on the model
#param outcome = the outcome variable in character (ex: 'SYM_SBREATH')
#param expo =  the exposition variable in character (ex: 'NO2_ESCAPE')
#param model = the model to apply in glm, it's a truncated formula made of -->
#-->counfounding variables (ex: '~D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_STATUS') without outcome and expo
#param family = glm family (ex: 'gaussian' or 'binomial',...) 
#param ...= any params that go to glm (except formula) (i.e: offset, data, weights,wiewIter, datasource)
#param Ncases = a boolean (TRUE or FALSE(default)) wether to compute N cases or not in the final result  

bioshare.env$run.model<-function(outcome = NULL,expo= NULL ,model= NULL,family,Ncases=FALSE,...)
{
  if(is.null(outcome)) stop('outcome is required...',call.=F)
  if(is.null(expo)) stop('exposition variable is required...',call.=F)
  if(is.null(model)) stop('model formula is required...',call.=F)
  
  #update formula
  formula <- paste0('D$',outcome,'~',model,'+D$',expo)
  
  #run glm 
  glm.res <- run.meta.glm(formula,family,opals,print=T,...)
  
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




#################################################################
# this can be used as an internal private function
#function to compute the number of complete participants in a glm
#glm.result = result of a glm run


#bioshare.env$run.num.completecases<-function(glm.result){
#  formula <-  gsub('\\s+','',formula(glm.result))
#  z <- strsplit(formula,'\\+|~')
#  df.name<-unlist(strsplit(unlist(z)[1],'$',fixed=T))[1]
#  vars.in.formula <- sapply(unlist(z),function(x) unlist(strsplit(x,'$',fixed=T))[2],USE.NAMES=F)
#  # get first new cols data subset
#  ds.subset(df.name,cols=vars.in.formula,subset='dfglm')
#  #now get completecases
#  ds.subset('dfglm',completeCases=T,subset='dfglmcomplete')
#  nbr.cases <- ds.dim('dfglmcomplete')
  
#  data.frame(nbr.cases)[1,]
  #nbr.cases
#}


##############################################
#function to extract glm result :P_OR(p.value) 
bioshare.env$run.extract.glm.stats <- function(glm.result)
{
  if(missing(glm.result)) stop('Please provide a valid glm result...',call.=F)
  
  glm.coef <- coef(glm.result)
  if('P_OR' %in% colnames(glm.coef)){
    stats <- data.frame(OR_with_pvalue = apply(glm.coef,1,function(x) {
      OR <- round(x['P_OR'],3)
      pvalue <-  pvalue<- format(x['p-value'],digits=4)
      low <- round(x[length(x)-1],3)
      high <- round(x[length(x)],3)
      
      paste0(OR,' [',low,' - ',high,'] (',pvalue,')')
     }),stringsAsFactors = F
    )
  }else{
    stats <- data.frame(Estimate_with_pvalue = apply(glm.coef,1,function(x) {
      estimate <- round(x['Estimate'],3)
      pvalue<- format(x['p-value'],digits=4)
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

##########################################

#close everything
bioshare.env$run.close<-function(all=F)
{
  #detect opals 
  objs <- ls(name=.GlobalEnv)
  if(length(objs)){
    for(obj in objs){
      if (is.list(obj) && (class(obj[[1]] == 'opal'))){
        datashield.logout(obj)
      }
    }
  }

  if(as.logical(all)) rm(list=objs,envir=.GlobalEnv) 
  detach(bioshare.env,pos=search())
}


# attach bioshare env
attach(bioshare.env)


