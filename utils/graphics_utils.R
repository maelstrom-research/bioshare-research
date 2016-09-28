# Source the DS UTILS environnment
source('utils/utils_analysis.R',echo=F,print.eval=F)
#forestplot library
library('forestplot',quietly=T)

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

.glist.by <- function(expo,outcome,model,data,fam,ref,by,datasources,...)
{
  if(missing(by)) {stop('[by] is mandatory',call.=F)}
  else{info <- by}
  
  if(missing(datasources)){
    datasources<-findLoginObjects()
  }
  ds <- datasources
  
  .gl <- function(x) {
    if(grepl('expo',info,T)) {expo <- x}
    else if(grepl('outcome',info,T)) {outcome <- x}
    else if (grepl('model',info,T)) {model <- x}
    else {stop('[by] should be either "expo","outcome" or "model"',call.=F)}
    formul <- run.make.formula(outcome,expo,model,data)
    run.meta.glm(formul,family=fam,ref=ref,datasources = ds,...) 
  }
  
  if(grepl('expo',info,T)) {
    result <- sapply(expo,.gl,simplify = FALSE, USE.NAMES = TRUE)
  }else if(grepl('outcome',info,T)) {
    result <- sapply(outcome,.gl,simplify = FALSE, USE.NAMES = TRUE)
  }
  else if (grepl('model',info,T)) {
    result <- sapply(model,.gl,simplify = FALSE, USE.NAMES = TRUE)
  }else {
    stop('[by] should be either "expo","outcome" or "model"',call.=F)
  }
  return (result)
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
  glm.coef <- glm.result$coef
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

.stack.fp <- function(glm.list,fix,...){                      # <- TODO TODO TODO  ADD NAMES INFOS IN FUNCTION: FIND BEST WAY TO DO THAT 
  if(missing(glm.list)) stop('Please provide a valid glm result(s) list...',call.=F)
  glist <- glm.list
  
  if(missing(fix)) stop('Provide a fix name ...',call.=F)
  label <- paste0('__',fix,'__')
  k.list <- lapply(glist,.fp.info,...)
  
  k.stack <- rbind(c(NA,NA),Reduce(rbind,k.list))
  row.names(k.stack) <-c(fix, names(glist))
  
  k.stack
}

.label.fp <- function(glm.list,digit=NULL,fix,term = NULL,statdesc='',labeldesc=NULL){
  if(missing(glm.list)) stop('Please provide a valid glm result(s) list...',call.=F)
  if(is.null(labeldesc)) stop('Please provide a text description for each odd ratio value to displayed. Ex. " SYM_BREATH_PRB = General breathing problems " ',call.=F)
  glist <- glm.list
  
  space <-'           '
  
  if(missing(fix)) stop('Provide a fix name ...',call.=F)
  fix <- paste0(fix,space)
  
  
  if(is.null(digit)){digit <- 3}
  
  
  #cnames <- names(run.extract.glm.stats(glist[[1]])$stats)
  f<-function(x){
    res.all <- run.extract.glm.stats(x,rdigit=digit)$stats
    #expo <- row.names(res.all)[nrow(res.all)]
    #cbind(expo,res.all[nrow(res.all),])
    term <- if(is.null(term)){nrow(res.all)}else{term}
    res.all[term,]
  }
  s.list <- lapply(glist,f)
  estim <-  do.call(rbind,s.list)
  labels<- paste0(space,.checklbl(names(glist),literal= labeldesc))
  s.stack <- cbind(labels, estim)
  rbind(c(fix,statdesc),s.stack)
}


.checklbl <- function(lbl,literal){
  f <- function(x,l){l[[x]]}
  sapply(lbl,f,literal)
}



.outcomes.lit <- c(
   'SYM_BREATH_PRB' = 'General breathing problems' ,
   'SYM_WHEEZ' = 'Wheeze', 
   'SYM_WHEEZ_NOCOLD' = 'Wheeze without a cold' ,
   'SYM_SBREATH' = 'Shortness of breath at rest',
   'SYM_SBREATH_WALK' = 'Shortness of breath walking',
   'SYM_PHLEGM_UP' = 'Phlegm when waking up',
   'SYM_PHLEGM_UP_FREQ' = 'Phlegm when waking up (almost daily)' ,
   'SYM_PHLEGM_DAY' = 'Phlegm during the day',
   'SYM_PHLEGM_DAY_FREQ' = 'Phlegm during the day (almost daily)' ,
   'SYM_SBREATH_EXT' = 'Shortness of breath at rest or walking'
)


#-----------------------------------
#Read a text formatted csv file and extract mean lower upper for 
#forest plot
.mean_lower_upper<- function (CSV=NULL) 
{
  if(is.null(CSV)) stop ('Add the csv file ',call.=FALSE)
  CSV[,3] <- gsub(',','\\.',CSV[,3])
  or_list <- strsplit(CSV[,3],'\\[|-|\\]')
  f <- function(z) 
  {
    if(is.na(z) || length(z)==0 || any(grepl('odds|ratios',z,TRUE))) return (NA)
    mean <- as.numeric(unlist(z)[1])
    lower <- as.numeric(unlist(z)[2])
    upper <- as.numeric(unlist(z)[3])
    return (cbind(mean,lower,upper))
  }
  do.call(rbind,lapply(or_list,f))
}



.csv_to_text <- function(CSV)
{
  csvtext <- CSV[1:19,]
  csv1 <- CSV[1:19,]
  csv2 <- CSV[20:38,]
  csvtext[-c(1,2),2] <- paste0(csv1[-c(1,2),2],'\n',csv2[-c(1,2),2] )
  csvtext[-c(1,2),3] <- paste0(csv1[-c(1,2),3],'\n',csv2[-c(1,2),3] )
  csvtext[-c(1,2),4] <- paste0(csv1[-c(1,2),4],'\n',csv2[-c(1,2),4] )
  return(csvtext)
}


message('All required graphics functions are loaded...')
