
######################################################
#
#         ******CATEGORICAL VARIABLES STATISTICS **** 
#
######################################################

var_cat<-list('AGE_YRS_CATEGORICAL','GENDER','SMK_CIG_CURRENT', 'ALC_CURRENT', 'PM_BMI_CATEGORIAL',
              'METABSYNDR_NBR_STRICT','METABSYNDR_STRICT','METABSYNDR_NBR_MODERATE','METABSYNDR_MODERATE','EDU_HIGHEST_2') #,'WORK_STATUS_CURRENT'

#####################################
message('\t\t*** STARTING CATEGORICAL VARIABLES SPLIT ANALYSIS ****\n')
message('--CATEGORICAL VARIABLES SELECTED: ')
cat(unlist(var_cat))

#define the variable to subset by
sub_by<-'DIET_VEGETARIAN'                ####<========MANDATORY


###preparing progress bar
total <- length(var_cat)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
i<-1

####start looping now
result_cat<-NULL
for(var in var_cat){
  #compute table2D
  message(paste0('\n==> COMPUTING CHI-SQUARE FOR ',var,' X ',sub_by,'\nDo not interrupt!...'))
  chi_square<-suppressWarnings(ds.table2D(x=paste0('D$',var), y=paste0('D$',sub_by), type="split"))
  message(paste0('==> COMPUTATION OF CHI-SQUARE FOR ',var,' X ',sub_by,' IS OK'))
  
  #arranging final result
  w<-structure(list(chi_square),.Names=var)
  result_cat<-c(result_cat,w)
  setTxtProgressBar(pb, i)
  i<-i+1
}
#close progress bar
close(pb)


##Saving Result object in file versioned
result_cat_name<-paste0('RESULTS_Cat_',sub_by,'_::',Sys.Date(),'.Rdata')
message(paste0('\n\n******\t\tSaving Results for Categorical Variables in <',result_cat_name,'> file.'))
save(result_cat,file=result_cat_name)




