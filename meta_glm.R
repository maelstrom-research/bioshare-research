#################################################
#metaregression
#################################################

message('starting metaGLM...')
message(paste0('formula for glm: ',formula))
message(paste0('family for glm: ',family))
message('getting data ready ....\nwait do not interrupt!')
formulasplit<-unlist(strsplit(formula,'~'))
outcomevar<-formulasplit[1]
explanvars<-formulasplit[2]
var_class<-unlist(unique(ds.class(outcomevar)))
zero_to_assign<-numeric(length(opals))
if(var_class == 'factor'){
  ds.replaceNA(paste0(outcomevar,'-',outcomevar),zero_to_assign,'ZEROS')
  
}else if (var_class == 'numeric'){
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
formula<-paste0(outcomevar,'~',explanvars)

message('->running glm...\n wait do not interrupt!\n')
message(paste0('new formula: ',formula))
glm_result<-ds.glm(formula=formula,family=family)

##Saving Result object in file versioned
date<-format(Sys.Date(),'%d%b%y')
save_file<-paste0(save_to,'_',date,'.rda')
message(paste0('saving result in ',save_file,'...'))
save(glm_result,file=save_file)


#clean workspace
rm(outcomevar,save_file,date,zero_to_assign,var_class,save_to,i,formula,effect_name,formulasplit,explanvars)