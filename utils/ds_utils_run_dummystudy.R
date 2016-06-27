#################### create dummy study effect vars #####
# RETURN THE DUMMY BINARY VARIABLE FOR EACH STUDY IN SERVER SIDE

ds_utils.env$run.dummy.study <- function (data,datasources)
{
  if(missing(data)) stop('data is mandatory ...\nplease add data name as character!')
  if(missing(datasources)) datasources <- findLoginObjects()
  ds <- datasources
  
  message(paste0(paste0(names(ds),collapse=', '), ' dummy variable(s) will be stored in dataframe ',data))
  
  for (i in 1:length(ds)){
    
    effect_name<-names(ds[i])
    #assign 1 to study and 0 to others
    message(paste0('---processing ',effect_name,' dummy ...'))
    
    datashield.assign(ds[i],effect_name,as.name('1'))
    datashield.assign(ds[-i],effect_name,as.name('0'))
    
    callcbind<-paste0('cbind(',data,',',effect_name,')')
    datashield.assign(ds[i],data,as.symbol(callcbind))
    datashield.assign(ds[-i],data,as.symbol(callcbind))
    
  }
  #clean server side
  run.rm(names(ds),datasources=ds)
  info <- paste0("run.isAssigned('",names(ds),"','",data,"')",collapse=" OR ")
  cat(paste0("You may check the stored dummy variables with the following datashield command: ", info ))
}
