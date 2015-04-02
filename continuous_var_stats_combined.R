######################################################
#
#         ******CONTINUOUS VARIABLES STATISTICS **** 
#
######################################################

message('\n\t\t*** STARTING CONTINUOUS VARIABLES COMBINED ANALYSIS ****\n')
message('--CONTINUOUS VARIABLES SELECTED: ')
cat(unlist(var_cont))


############################################

###preparing progress bar
total <- length(var_cont)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1
####start looping now
result_cont_comb<-NULL
for (var in var_cont){
  
  #define formula
  regformula<-paste0('D$',var,'~D$',sub_by)
  #compute mean by class
  message(paste0('\n==> COMPUTING MEAN_VALUE FOR ',var,'~',sub_by))
  res_mean<-ds.meanByClass(regformula)  #mean of var combined result
  message(paste0('==> MEAN_VALUE FOR ',var,'~',sub_by,' IS OK'))
  
  #compute the t.test using data from D_VEG_XXX
  message(paste0('\n==> COMPUTING ttest P_VALUE FOR ',var,'~',sub_by,'\nDo not interrupt!...'))
  ttest_cont<-ds.tTest(regformula,paired=F)
  message(paste0('==>P_VALUE FOR ',var,'~',sub_by,' IS OK'))
  
  #arranging final result
  z<-list(pvalue = ttest_cont$p.value, mean_stats = res_mean)
  w<-structure(list(z),.Names=var)
  result_cont_comb<-c(result_cont_comb,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}
#close progress bar
close(pb)

##Saving Result object in file versioned
result_cont_name<-paste0('RESULTS_Cont_COMBINED_',sub_by,'_',Sys.Date(),'.Rdata')
message(paste0('\n\n******\t\tSaving Results for Continous Variables COMBINED  in <',result_cont_name,'> file.'))
save(result_cont_comb,file=result_cont_name)

