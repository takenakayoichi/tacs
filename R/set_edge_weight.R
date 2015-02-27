#'generate edgeweight from a row of dataframe 
#'
#'@param elist edge list
#'
#'@param node_weight node weight
#'
#'@return a vector of edge weights
set_edge_weight <- function(elist,node_weight){
  weight <- rep(0,nrow(elist))
  for(i in 1:length(weight)) {  
    innode <- elist[i,2]
    weight[i] <- as.numeric(node_weight[innode])
  }
  
  return(weight)
}

#'set edges weights to zero under n*standard deviations
#'
#'@param elist edge list with weights. It have 3 columns [from, to, weight].
#'
#'@param sd standared deviations of edges
#'
#'@param n deviation
#'
#'@return a vector of new edge weights
cutoff_edge_weight <-function(elist,sd,n){
  weight <- rep(0,nrow(elist))
  for(i in 1: nrow(elist)){
    innode <- elist[i,2]
    if( elist$weight[i] >= n*sd[[innode]]) weight[i] <- elist$weight[i]
  }
  return(weight)
}