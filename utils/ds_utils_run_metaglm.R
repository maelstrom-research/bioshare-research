# ---- ADD STUDY EFFECT IF EXIST AND RUN A GLM WITH STUDY EFFECT INCLUDED IN THE MODEL ----


ds_utils.env$run.meta.glm<-function(formula, family, ref, datasources,save = F, print = T,...)
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
    
    data <- extract(outcomevar)$holders
    effect_name <- paste0(data,'$',names(ds))
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
  
  #run glm now
  glm.result<-ds.glm(formula=formula,family=family,datasources=ds,...)
  
  
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