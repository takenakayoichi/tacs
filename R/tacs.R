#'Calculate Control Strength toward every nodes on directed graph based on information criterion
#'
#'@name tacs
#'@docType package
#'@import bnlearn
NULL
#'Calculate Control Strength toward every nodes on directed graph based on information criterion
#'
#'@param network directed graph: format used in library bnlearn is used.
#'
#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param ic_type information criterion type: string "aic"(=default) or "bayes"
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'
#'@return strength Control strength toward each node for every time point: data frame
#'@examples
#'data(dream4)
#'tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'@export
tacs <- function(network,exp_data,ic_type="aic",is_markov=FALSE,segment=NA)
{
  #ic_type and segment check
  if (consistency_check(exp_data,ic_type,segment) == FALSE) return() 

  
  res <- c()
  res$ic <- calc_ic(network,exp_data,ic_type,is_markov,segment)
  return("yes")
}
