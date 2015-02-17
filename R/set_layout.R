#'returns layout of graph nodes
#'
#'@param obj  an object of class tacs
#'@export
#'@return matrix with two columns. each row shows a coordinates of a node.
set_layout <- function(obj){
  edgelist <- obj$info$network
  
  #Layout of edge IC
  edgelist$weight <- set_edge_weight(edgelist,obj$edge_ic)
  q <- qgraph(edgelist,DoNotPlot=F)
  
  #Layout of score_sd
  edgelist$weight <- set_edge_weight(edgelist,obj$info$score_sd)
  p <- qgraph(edgelist,DoNotPlot=FALSE)
  
  loc <- averageLayout(p,q)
  return(loc)
}