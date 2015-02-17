#'Calculate Control Strength toward every nodes on directed graph based on information criterion
#'
#'@name tacs
#'@docType package
#'@import bnlearn
#'@import ggplot2
#'@import grid
#'@import qgraph
#'@import animation
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
#'@param segment_name name list of segments. default (seg1,seg2,...)
#'
#'@return strength Control strength toward each node for every time point: data frame
#'@examples
#'data(dream4)
#'tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment,NA)
#'@export
tacs <- function(network,exp_data,ic_type="aic-g",is_markov=FALSE,segment=NA,segment_name=NA)
{
  if(length(segment) == 1 && is.na(segment)) segment <- nrow(exp_data)
  seg_name_col <- set_segname_col(is_markov,segment,segment_name)
  if(length(seg_name_col) == 1 && is.na(seg_name_col)) return()

#ic_type and segment check
  if (consistency_check(exp_data,ic_type,segment) == FALSE) return() 

  info <-list(is_markov=is_markov,segment=segment,segment_col=seg_name_col,network=network)
  res <- calc_ic(exp_data,info)

  class(res) <- "tacs"
  return(res)
}
