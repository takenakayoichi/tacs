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