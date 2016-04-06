##############################################
#function to extract glm result :P_OR(p.value) 


ds_utils.env$run.extract.glm.stats <- function(glm.result,pval=FALSE,Ncases=FALSE,rdigit =3)
{
  if(missing(glm.result)) stop('Please provide a valid glm result...',call.=F)
  glm.family <- glm.result$family$family
  glm.coef <- (glm.result$coef)
  if(grepl("poisson|binomial", glm.family)){
    stats <- data.frame(OR_CI = apply(glm.coef,1,function(x) {
      OR <- round(x['P_OR'],rdigit)
      pvalue<- format(x['p-value'],digits=4) ; pvalue <- if(as.numeric(pvalue)< 2.2e-16) {"<2.2e-16"}else{pvalue}
      low <- round(x[length(x)-1],rdigit)
      high <- round(x[length(x)],rdigit)
      
      res.extract <- paste0(OR,' [',low,' - ',high,']')
      #add N valid (complete cases)
      if(Ncases) {res.extract <- paste0(res.extract,' (n = ',glm.result$nsubs,')')}
      #add pvalue in result
      if(pval) {res.extract <- paste0(res.extract, '(p=',pvalue,')')}
      return (res.extract)
      
    }),stringsAsFactors = F
    )
  }else if (glm.family == 'gaussian'){
    stats <- data.frame(Estimate_CI = apply(glm.coef,1,function(x) {
      estimate <- round(x['Estimate'],rdigit)
      pvalue<- format(x['p-value'],digits=4) ; pvalue <- if(as.numeric(pvalue)< 2.2e-16) {"<2.2e-16"}else{pvalue}
      low <- round(x[length(x)-1],rdigit)
      high <- round(x[length(x)],rdigit)
      
      res.extract <- paste0(estimate,' [',low,' - ',high,']')
      #add N valid (complete cases)
      if(Ncases) {res.extract <- paste0(res.extract,' (n = ',glm.result$nsubs,')')}
      #add pvalue in result
      if(pval) {res.extract <- paste0(res.extract, '(p=',pvalue,')')}
      return (res.extract)
    }),stringsAsFactors = F
    )
  }
  glm.coef[2,dim(glm.coef)[2]-1]
  formula <-  gsub('\\s+',' ',formula(glm.result)) #fix spaces in formula
  list(formula = formula, stats = stats)
}

