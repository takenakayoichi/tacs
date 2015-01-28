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
#'@param tc_data time course data on nodes in the network: data frame
#'
#'@param ic_type information criterion type: string "aic"(=default) or "bayes"
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@return strength Control strength toward each node for every time point: data frame
#'
#'@export
tacs <- function(network,tc_data,ic_type="aic",is_markov=FALSE)
{
  return("yes")
}
