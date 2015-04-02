##########################################################
#
#******CONTINUOUS VARIABLES DOUBLE SUBSET STATISTICS **** 
#
##########################################################

message('\n\t\t*** STARTING CONTINUOUS VARIABLES DOUBLE SUBSET SPLIT ANALYSIS ****\n')
message('--CONTINUOUS VARIABLES SELECTED: ')
cat(unlist(var_cont_double))


#subset by sub_by first to generate the first subset
message(paste0('\n===> CREATING FIRST SUBSET BY ',sub_by,'\nWait please do not interrupt!...'))
ds.subsetByClass('D',subsets='subobj',variables= sub_by)
message(paste0('\===> FIRST SUBSET BY ',sub_by,' IS CREATED'))

#define infoname
message('===>CREATING FRIST SUBSET OBJECTS IN SERVER SIDE...\nWait please do not interrupt!...')
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
message('===>FIRST SUBSET OBJECTS CREATED OK')

#####################################################################################
###preparing progress bar
total <- length(var_cont)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1

####start looping now
result_cont_double_split<-NULL

for(var in var_cont_double){
  
  #define formula set
  formulaset<-sapply(subinfobj,function(x){ paste0(x,'$','PM_SYSTOLIC_MEASURE','~',x,'$',sub_double_by)})
  
  ###compute mean by class doule
  message(paste0('\n==> COMPUTING MEAN_VALUE DOUBLE SUBSET FOR ',var,'~',sub_by,'/',sub_double_by,'\nDo not interrupt!...'))
  #resultset
  mean_double_split<-lapply(formulaset,ds.meanByClass,type='split')
  message(paste0('==> MEAN_VALUE FOR ',var,'~',sub_by,'_',sub_double_by,' IS OK'))
  
  #compute the t.test using data from D_VEG_XXX
  message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var,'~',sub_by,'/',sub_double_by,'\nDo not interrupt!...'))
  ttest_double_split<-lapply(formulaset,ds.tTest,type='split')
  pvalue_double_split<-lapply(ttest_double_split,function(x){lapply(x,function(y){ y$p.value})})
  message(paste0('==>P_VALUE FOR ',var,'~',sub_by,'/',sub_double_by,' IS OK'))
  
  #arranging final result
  z<-list(pvalue = pvalue_double_split, mean_stats = mean_double_split)
  w<-structure(list(z),.Names=var)
  result_cont_double_split<-c(result_cont_double_split,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}

#close progress bar
close(pb)

##Saving Result object in file versioned
result_cont_name<-paste0('RESULTS_Cont_DOUBLE_SPLIT_',sub_by,'-',sub_double_by,'_',Sys.Date(),'.Rdata')
message(paste0('\n\n******\t\tSaving Results for Continous Variables DOUBLE SPLIT in <',result_cont_name,'> file.'))
save(result_cont_double_split,file=result_cont_name)

