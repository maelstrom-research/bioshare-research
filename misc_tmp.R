#HARMONIZE SBREATH_EXT
ds.assign('D$SYM_SBREATH','SYM_SBREATH_EXT',datasources=opals[1])
ds.assign('D$SYM_SBREATH_WALK','SYM_SBREATH_EXT',datasources=opals[2])
ds.cbind(c('SYM_SBREATH_EXT','D'),newobj='D')
run.rm('SYM_SBREATH_EXT')




#invisible(sapply(outcomes.c,run.table2d,'OBESE_STATUS',col= T,chisq = F,data = main.data,dataso = opals[1]))