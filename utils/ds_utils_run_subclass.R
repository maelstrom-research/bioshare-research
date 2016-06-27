# ---- FUNCTION TO SPLIT (SIMILAR TO R) A DATABASE ON SERVER SIDE ----# 

##############################################
#   SPLIT A DATASET BY A CATEGORICAL VARIABLE
#   param : TODO
#   ex: run.subclass(data = 'D', subclass = 'SMK_STATUS')
#########################

#
ds_utils.env$run.subclass<-function(subclass = NULL,vars.list=NULL,data = NULL, datasources=NULL){
  
  
  if(is.null(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  
  #verify data
  if(is.null(data)) {stop('Please specify the data(dataframe) to subset',call.=F)}
  
  if(is.null(subclass)) {
    stop('Nothing to do, sublass var is required...',call.=F)
  }else {
    #Sanity check : subclass should always be a factor
    subset.class <- checkClass(ds,paste0(data,'$',subclass))
    if(subset.class != 'factor') stop('subclass must be a categorical or factor type variable ...',call.=F)
    
    if (!is.null(vars.list)){
      vars<-unique(c(subclass,unlist(vars.list)))
      message(paste0('=> Creating sub classes of  selected vars.list in ',data, ' by ',subclass,'\nWait please do not interrupt!...'))
      
      cally <- call('subsetDS',dt = data, complt=F, rs=NULL, cs=vars)
      datashield.assign(ds,'newdt',cally)   
      #sublclassing 
      cally <- call('subsetByClassDS','newdt', subclass)
      datashield.assign(ds,'subobj',cally)
      to.rm <- c('newdt','subobj')
      
    } else { #sublcass only
      message(paste0('=> Creating sub classes of all vars in ',data, ' by ',subclass,'\nWait please do not interrupt!...'))
      
      cally <- call('subsetByClassDS',data, subclass)
      datashield.assign(ds,'subobj',cally)
      to.rm <- 'subobj'
    }
  }
  
  #### arranging subclass result in server side
  
  #define infoname
  message('=> Assigning the subsetted dataframe(s) on server side...\nWait please do not interrupt!...')
  cally <- as.name('namesDS(subobj)')
  subinfo<-datashield.aggregate(ds,cally)[[1]] #get names of subsetted object
  
  #define name of object to be assigned
  subinfobj<-sapply(subinfo,function(x){  #assign new subsetted object in server and return their name
    newname<-paste0(data,'.',sub('\\.level_(\\d+)$','\\1',x))  #transform names
    toassign<-paste0('subobj$',x)
    datashield.assign(ds,newname,as.name(toassign))    ###assigned objs are in server
    return(newname)
  },USE.NAMES = F)
  
  to.rm <- c("complt","cs","dt","rs",to.rm)
  #Clean space
  run.rm(to.rm,datasources=ds)
  
  message(paste0('-- dataframe ',subinfobj ,' is assigned',collapse='\n'))
  info <- paste0("run.isAssigned('",subinfobj,"')",collapse=' OR ')
  cat(paste0("You may check the assigned subsetted dataframes with the following datashield command: ",info,""))
  return(invisible(subinfobj))
}