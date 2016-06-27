################################################################
#run.stack.glm.by function will run many glms and return a stack of all the glm compute based on the <by> params
#ex: if by = 'expo', the function will compute a list of glm by 'expo'
#param <X> = the list of names (charcter) to compute by
#param <expo> = the expo var (in character) 
#param <outcome> = the outcome var (in character)
#param <model> = the model object
#param <data> = the data where the variables the reside
#param <fam> = family of the glm
#param <ref> = the study selected as reference in a heterogeneity model
#param <by> = the name in charactr of the option to compute the list. 
#ex: <by> can be either 'expo' to compute the list of glm by expo var 
#,this implies that <X> is a vector or a list of expo var names.
#<by> can be either 'expo','outcome', or 'model'
# EX: 
#expo.c <- c('expo1','expo2','expo3')
#glm.stack <- run.stack.glm.by(expo.c,outcome=outcome,model=model,data='D',fam='binomial',by='expo',datasources=opals[1])


ds_utils.env$run.stack.glm.by <- function(expo,outcome,model,data,fam,ref,by,...)
{
  if(missing(by)) {stop('[by] is mandatory',call.=F)}
  else{info <- by}
  
  .ml <- function(x) {
    if(grepl('expo',info,T)) {expo <- x}
    else if(grepl('outcome',info,T)) {outcome <- x}
    else if (grepl('model',info,T)) {model <- x}
    #formul <- run.make.formula(outcome,expo,model,data)
    #run.meta.glm(formul,family=fam,ref=ref,datasources = datasources,...) 
    run.model(outcome,expo,model,family = fam,data,Ncases=T,pval = F, ref =ref,...)
  }
  
  if(grepl('expo',info,T)) {
    result <- do.call(rbind,lapply(expo,.ml))
  }else if(grepl('outcome',info,T)) {
    result <- do.call(rbind,lapply(outcome,.ml))
  }
  else if (grepl('model',info,T)) {
    result <- do.call(rbind,lapply(model,.ml))
  }else {
    stop('[by] should be either "expo","outcome" or "model"',call.=F)
  }
  return (result)
}