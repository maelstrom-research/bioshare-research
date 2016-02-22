#.glist function will run many glms and return a list of all the glm compute based on the by params
#ex: if by = 'expo', .glist will compute a list of glm by 'expo'
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
#gstack <- .glist.by(expo.c,outcome=outcome,model=model,data='D',fam='binomial',by='expo',datasources=opals[1])

.glist.by <- function(X,expo,outcome,model,data,fam,ref,by,datasources,...)
{
  if(missing(by)) {stop('[by] is mandatory',call.=F)}
  else{info <- by}
  
  .gl <- function(x) {
    if(grepl('expo',info,T)) {expo <- x}
    else if(grepl('outcome',info,T)) {outcome <- x}
    else if (grepl('model',info,T)) {model <- x}
    else {stop('[by] should be either "expo","outcome" or "model"',call.=F)}
    formul <- run.update.formula(outcome,expo,model,data)
    run.meta.glm(formul,family=fam,ref=ref,datasources = datasources,...)  
  }
  lapply(X,.gl)
}

#.fp.info function take one ds glm result and return a dataframe with mean lower upper of the 
# estimated of expo variable. 
#This dataframe will be used by forestplot function
#By default the function assumes that the expo var is the last term in the glm formula.
#Otherwise you can specify the term (variable name in character) of interest

.fp.info <- function(glm.result,term=NULL)
{
  if(missing(glm.result)) stop('Please provide a valid glm result...',call.=F)
  glm.family <- glm.result$family$family
  glm.coef <- coef(glm.result)
  if(grepl("poisson|binomial", glm.family)){
    num.cols <- dim(glm.coef)[2]
    m.ci <- as.data.frame(glm.coef[,(num.cols-2):num.cols],stringsAsFactors=F)
  }else if (glm.family == 'gaussian'){
    m.ci <- as.data.frame(glm.coef[,-c(2:4)],stringsAsFactors=F)
  } 
  # return the mean estimate or OR or RR and the confidence interval (ci)
  names(m.ci) <- c('mean','lower','upper')
  if(is.null(term)) return(m.ci[nrow(m.ci),])
  return(m.ci[term,])
  
}

#.stack.fp.info use a list of glm returned by .glist.by, process and stack them using .fp.info
# It will return a dataframe ready for forestplot

.stack.fp.info <- function(glm.res.list,names = NULL){                      # <- TODO TODO TODO  ADD NAMES INFOS IN FUNCTION: FIND BEST WAY TO DO THAT 
  if(missing(glm.res.list)) stop('Please provide a valid glm result(s) list...',call.=F)
  glist <- glm.res.list
  k.list <- lapply(glist,.fp.info)
  rbind(c(NA,NA),Reduce(rbind,k.list))
}

.labelfptext <- function(glm.res.list,digit=NULL){
  if(missing(glm.res.list)) stop('Please provide a valid glm result(s) list...',call.=F)
  if(is.null(digit)){digit <- 3}
  glist <- glm.res.list
  cnames <- names(run.extract.glm.stats(glist[[1]])$stats)
  f<-function(x){
    res.all <- run.extract.glm.stats(x,rdigit=digit)$stats
    expo <- row.names(res.all)[nrow(res.all)]
    cbind(expo,res.all[nrow(res.all),])
  }
  s.list <- lapply(glist,f)
  rbind(c('Exposure',cnames),Reduce(rbind,s.list))
}








