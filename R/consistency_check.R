#'Check the consistency on input parameters
#'
#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param ic_type information criterion type: string "aic"(=default) or "bayes"
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#
#'@return Boolean
consistency_check <- function(exp_data,ic_type,segment){
  x = empty.graph(colnames(exp_data))
  e = try(score(x,exp_data,ic_type),silent=FALSE)
  if(class(e)=="try-error") {
    return(FALSE)
  }
  
  #segment check
  if(length(segment) == 1 && is.na(segment)[1]) segment <- nrow(exp_data)
  if(sum(segment) != nrow(exp_data)) {
    warning(message="Illegal segment. sum of segment is not equal to #samples in exp_data.")    
    return(FALSE)
  } 
  
  return(TRUE)
}