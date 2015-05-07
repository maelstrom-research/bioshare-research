##########################################################
#
#******CONTINUOUS VARIABLES DOUBLE SUBSET STATISTICS **** 
#
##########################################################

message('\n\t\t*** STARTING CONTINUOUS VARIABLES DOUBLE SUBSET COMBINED ANALYSIS ****\n')
message('\n\t\t===> ****PREPARING DATA FOR DOUBLE SUBSET ANALYSIS**** <====')
message('--CONTINUOUS VARIABLES SELECTED: ')
cat(unlist(var_cont_double))

#subset by sub_by first to generate the first subset
message(paste0('\n===> CREATING FIRST SUBSET BY ',sub_by2,'\nWait please do not interrupt!...'))
ds.subsetByClass('D',subsets='subobj',variables= sub_by2)
message(paste0('===> FIRST SUBSET BY ',sub_by2,' IS CREATED'))

#define infoname
message('\n===>CREATING FRIST SUBSET OBJECTS IN SERVER SIDE...\nWait please do not interrupt!...')
subinfo<-ds.names('subobj')[[length(opals)]]

#####define formula sets
#define name of object to be assigned
subinfobj<-sapply(subinfo,function(x){  #assigned new objects of first subset in server and return their name
  m<-gregexpr('\\d+',x)
  catnum<-regmatches(x,m)
  newname<-sub('\\.level_\\d+$',catnum,x)
  toassign<-paste0('subobj$',x)
  ds.assign(toassign,newname)    ###assigned objs are in server
  return(newname)
})

#adjust name according to new objects
names(subinfobj)<-subinfobj
message('===>FIRST SUBSET OBJECTS ASSIGNED IN SERVER SIDE OK')

#####################################################
###preparing progress bar
total <- length(var_cont_double)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1

####start looping now
result_cont_double_comb<-NULL

for(var in var_cont_double){
  
  #define formula set
  formulaset<-sapply(subinfobj,function(x){ paste0(x,'$',var,'~',x,'$',sub_double_by)})
  
  ###compute mean by class doule
  message(paste0('\n==> COMPUTING MEAN_VALUE DOUBLE SUBSET FOR ',var,'~',sub_by2,'|',sub_double_by,'\nDo not interrupt!...'))
  #resultset
  mean_double_comb<-lapply(formulaset,ds.meanByClass)
  message(paste0('==> MEAN_VALUE FOR ',var,'~',sub_by2,'|',sub_double_by,' IS OK'))
  
  #compute the t.test 
  message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var,'~',sub_by2,'|',sub_double_by,'\nDo not interrupt!...'))
  ttest_double_comb<-lapply(formulaset,ds.tTest)
  pvalue_double_comb<-lapply(ttest_double_comb,function(x){x$p.value})
  message(paste0('==>P_VALUE FOR ',var,'~',sub_by2,'|',sub_double_by,' IS OK'))
  
  #arranging final result
  z<-list(pvalue = pvalue_double_comb, mean_stats = mean_double_comb)
  w<-structure(list(z),.Names=var)
  result_cont_double_comb<-c(result_cont_double_comb,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}

#close progress bar
close(pb)

##Saving Result object in file versioned
date<-format(Sys.Date(),'%d%b%y')
result_cont_name<-paste0('Cont_dblcomb_',sub_by2,'-',sub_double_by,'_',date,'.rda')
message(paste0('\n\n***\tSaving Results for Continuous Variables DOUBLE COMBINED in <',result_cont_name,'> file.'))
save(result_cont_double_comb,file=result_cont_name)

