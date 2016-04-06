# ----- ADJUST THE FORMULA OF A MODEL -----#

#######################################################
# This function remove the term in original model
# is particulary useful for subgroup analysis
# when we need to automatically remove all subgroup related 
# terms in the model
# RETURN the model without the subgroup related terms


ds_utils.env$run.adjust.model <- function(model,term){
  x <- unlist(strsplit(model,'\\+'))
  x.ind <- which(sapply(x,function(k) grepl(term,k,ignore.case=T)))
  if(length(x.ind)) paste0(x[-x.ind],collapse='+') 
  else paste0(x,collapse='+')
} 