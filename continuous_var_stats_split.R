######################################################
#
#         ******CONTINUOUS VARIABLES STATISTICS **** 
#
######################################################

message('\n\t\t*** STARTING CONTINUOUS VARIABLES SPLIT ANALYSIS ****\n')
message('--CONTINUOUS VARIABLES SELECTED: ')
cat(unlist(var_cont))



####******BY STUDY: SPLIT

###preparing progress bar
total <- length(var_cont)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1
####start looping now
result_cont_split<-NULL
for (var in var_cont){
  
  #define formula
  regformula<-paste0('D$',var,'~D$',sub_by)
  #compute mean by class
  message(paste0('\n==> COMPUTING MEAN_VALUE FOR ',var,'~',sub_by))
  mean_split<-ds.meanByClass(regformula,type='split')
  message(paste0('==> MEAN_VALUE FOR ',var,'~',sub_by,' IS OK'))
  
  #compute the t.test using data from D_VEG_XXX
  message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var,'~',sub_by,'\nDo not interrupt!...'))
  ttest_split<-ds.tTest(regformula,paired=F,type='split')
  pvalue_split<-lapply(ttest_split,function(x){x$p.value})
  message(paste0('==>P_VALUE FOR ',var,'~',sub_by,' IS OK'))
  
  #arranging final result
  z<-list(pvalue = pvalue_split, mean_stats = mean_split)
  w<-structure(list(z),.Names=var)
  result_cont_split<-c(result_cont_split,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}
#close progress bar
close(pb)

##Saving Result object in file versioned
result_cont_name<-paste0('RESULTS_Cont_SPLIT_',sub_by,'_',Sys.Date(),'.Rdata')
message(paste0('\n\n******\t\tSaving Results for Continous Variables SPLIT in <',result_cont_name,'> file.'))
save(result_cont_split,file=result_cont_name)

