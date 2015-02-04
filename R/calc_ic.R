#'Calculate information criterion of each node of the network without edge,
#' and ic of every innode for every time point
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
#'@return two dataframes that include information criterion of nodes in the network without edge and 
#' and ic of every innode for every time point
#' 
calc_ic <- function(network,exp_data,ic_type,is_markov,segment){
  res_ic <- c()
  res_ic$wo_edge <- calc_ic_without_edge(exp_data,ic_type,is_markov,segment)
  res_ic$with_edge <- calc_ic_with_edges(network,exp_data,ic_type,is_markov,segment,res_ic$wo_edge)
  return(res_ic)
}

