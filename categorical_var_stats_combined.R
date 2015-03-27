
######################################################
#
#         ******CATEGORICAL VARIABLES STATISTICS **** 
#
######################################################

#####################################
message('\n\t\t*** STARTING CATEGORICAL VARIABLES COMBINED ANALYSIS ****\n')
message('--CATEGORICAL VARIABLES SELECTED: ')
cat(unlist(var_cat))


###preparing progress bar
total <- length(var_cat)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1

####start looping now
result_cat_comb<-NULL
for(var in var_cat){
  #compute table2D
  message(paste0('\n==> COMPUTING CHI-SQUARE FOR ',var,' X ',sub_by,'\nDo not interrupt!...'))
  chi_square_comb<-suppressWarnings(ds.table2D(x=paste0('D$',var), y=paste0('D$',sub_by)))
  message(paste0('==> COMPUTATION OF CHI-SQUARE FOR ',var,' X ',sub_by,' IS OK'))
  
  #arranging final result
  w<-structure(list(chi_square_comb),.Names=var)
  result_cat_comb<-c(result_cat_comb,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}
#close progress bar
close(pb)


##Saving Result object in file versioned
result_cat_name<-paste0('RESULTS_Cat_COMBINED_',sub_by,'_',Sys.Date(),'.Rdata')
message(paste0('\n\n******\t\tSaving Results for Categorical Variables COMBINED in <',result_cat_name,'> file.'))
save(result_cat_comb,file=result_cat_name)




