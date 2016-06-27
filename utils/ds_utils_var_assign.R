
## ---OPAL FUNCTION ----#
# ASSIGN A DATASET TO OPAL # 

#function get vars from opal
ds_utils.env$var.assign<-function(opal,datasource,table,variables=NULL)
{
  datafile<-paste0(datasource, ".", table)
  if(length(variables)==1){
    variable<-paste0(datafile,':',variables)
    data.err <- try(opal.assign(opal,'D', variable,missings=T),silent = T)
  }else{
    data.err <- try(opal.assign(opal,'D', datafile,variables,missings=T),silent = T)
  }
  
  if(inherits(data.err,'try-error')) { 
    message(paste0('-- The required data is not assigned\n'),(data.err),'-- Check the error message and try again!')
  }else { cat(paste0('The required data from ', datafile,' was correctly assigned.'))}
  
  var.value<-opal.execute(opal,'D')
  return (var.value)
}