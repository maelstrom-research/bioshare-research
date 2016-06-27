###########################################
#this function run a glm based on the model
#param outcome = the outcome variable in character (ex: 'SYM_SBREATH')
#param expo =  the exposition variable in character (ex: 'NO2_ESCAPE')
#param model = the model to apply in glm, it's a truncated formula made of -->
#-->counfounding variables (ex: '~D$AGE_YRS+D$EDU_HIGHEST_2+D$SMK_STATUS') without outcome and expo
#param family = glm family (ex: 'gaussian' or 'binomial',...) 
#param ...= any params that go to glm (except formula) (i.e: offset, data, weights,wiewIter, datasource)
#param Ncases = a boolean (TRUE or FALSE(default)) wether to compute N cases or not in the final result  



ds_utils.env$run.model<-function(outcome,expo,model,family,data,Ncases=FALSE,pval=FALSE, ...)
{
  if(missing(outcome)) stop('outcome is required...',call.=F)
  if(missing(expo)) stop('exposition variable is required...',call.=F)
  if(missing(model)) stop('model formula is required...',call.=F)
  #verify data
  if(missing(data)) stop('data is mandatory',call.=F)
  
  #update formula
  formula <- run.make.formula(outcome,expo,model,data)
  
  #run glm 
  glm.res <- try(run.meta.glm(formula,family,print=T,...),silent=T)
  glm.err <- inherits(glm.res,'try-error')
  glm.ok <-  !( glm.err || is.null(glm.res)) 
  
  #extract glm stats and process result
  if(glm.ok) glm.stats<-run.extract.glm.stats(glm.res,Ncases=Ncases,pval=pval)
  else if (glm.err) return(message(glm.res))
  else return(glm.res)
  
  # rewrite if any interaction term
  expo <- gsub('*',':',expo,fixed=T)
  glm.stats.estim <- glm.stats$stats
  coef.names <- row.names(glm.stats.estim)
  expo <- coef.names[grepl(expo,coef.names,fixed=T)]
  
  
  glm.stats$results <- glm.stats.estim[expo,,F]   #<--display variable names and statistics
  result <- data.frame(glm.stats$results,row.names=gsub('\\s+','',paste(outcome,'~...',expo)))
  
  #print glm stats
  cat(glm.stats$formula,'\n\n')
  print(result)
  return(invisible(result))
}
