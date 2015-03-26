######################################################
#
#         ******CONTINUOUS VARIABLES STATISTICS **** 
#
######################################################

message('\t\t*** STARTING CONTINUOUS VARIABLES SPLIT ANALYSIS ****\n')
message('--CONTINUOUS VARIABLES SELECTED: ')
cat(unlist(var_cont))


############################################
#   REORGANIZE DATA BY VEGETARIAN (yes/no)
# SUBCLASS AND ASSIGN TO D_VEG_0 AND D_VEG_1

message(paste0('\n\n==> SUBSETTING DATA BY '),sub_by,'\nWait do not interrupt!...')
ds.subclass('D','D_VEG',sub_by)
D_sub_name_1<-paste0('D_VEG$',sub_by,'.level_1')
D_sub_name_2<-paste0('D_VEG$',sub_by,'.level_0')
ds.assign(D_sub_name_1,'D_VEG_1')
ds.assign(D_sub_name_2,'D_VEG_0')
message(paste0('==> SUBSETTING DATA BY '),sub_by,' IS OK\n')



####******BY STUDY: SPLIT
#mean by veget.

###preparing progress bar
total <- length(var_cont)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1
####start looping now
result_cont<-NULL
for (var in var_cont){
  
  #compute mean by class
  message(paste0('\n==> COMPUTING MEAN_VALUE FOR ',var,'~',sub_by))
  mean_split<-ds.meanByClass('D',var,sub_by,type='split')
  message(paste0('==> MEAN_VALUE FOR ',var,'~',sub_by,' IS OK'))
  
  #compute the t.test using data from D_VEG_XXX
  message(paste0('\n==> COMPUTING Ttest P_VALUE FOR ',var,'~',sub_by,'\nDo not interrupt!...'))
  var1<-paste0('D_VEG_1$',var)
  var2<-paste0('D_VEG_0$',var)
  ttest_split<-ds.tTest(var1,var2,paired=F,type='split')
  pvalue_split<-lapply(ttest_split,function(x){x$p.value})
  message(paste0('==>P_VALUE FOR ',var,'~',sub_by,' IS OK'))
  
  #arranging final result
  z<-list(pvalue = pvalue_split, mean_stats = mean_split)
  w<-structure(list(z),.Names=var)
  result_cont<-c(result_cont,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}
#close progress bar
close(pb)

##Saving Result object in file versioned
result_cont_name<-paste0('RESULTS_Cont_',sub_by,'_::',Sys.Date(),'.Rdata')
message(paste0('\n\n******\t\tSaving Results for Continous Variables in <',result_cont_name,'> file.'))
save(result_cont,file=result_cont_name)

