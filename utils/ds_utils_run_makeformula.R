####################################################################################
#this function create a formula according to the model, outcome and exposition vars
# RETURN A FORMULA READY FOR REGRESSION

ds_utils.env$run.make.formula<-function(outcome,expo,model,data)
{
  mf <- match.call(expand.dots = FALSE)
  arg.call <- names(mf)[-1]
  arg.names<-c('outcome','expo','model','data')
  missing.arg<-which(!arg.names %in% arg.call)
  missing.call <- paste(arg.names[missing.arg],collapse=' and ')
  if(length(missing.arg)>1) stop(paste0(missing.call,' are required'),call.=F)
  else if (length(missing.arg)==1) stop(paste0(missing.call,' is required'),call.=F)
  
  fm <-  ifelse(!(is.null(model) || model ==''),paste0(data,'$',outcome,'~',data,'$',model,'+',expo),paste0(data,'$',outcome,'~',data,'$',expo))
  fm <- gsub('+',paste0('+',data,'$'),fm,fixed=T)
  fm <- gsub('*',paste0('*',data,'$'),fm,fixed=T)  
  fm
}
