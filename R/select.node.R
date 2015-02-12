#'Select nodes by name list 
#'
#'@param obj  an object of class tacs
#'
#'@param nodes nodes names to be selected.
#'
#'@param delete if TRUE designated nodes are deleted 
#'
#'@examples
#'data(dream4)
#'obj <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#' nodes <- c("mtlR","gadW","marR")
#'select1 <- select.nodes(obj,nodes)
#'select2 <- select.nodes(obj,nodes,delete=TRUE)
#'@export
select.nodes <- function(obj,nodes,delete=FALSE){
  if(delete==TRUE) {
    new_nodes <- setdiff(names(obj$score) ,nodes)
    tacs <- select.nodes(obj,new_nodes,delete=FALSE)
    return(tacs)
  }
  
  tacs <- c()
  tacs$score <- obj$score[nodes]
  tacs$edge_ic <- obj$edge_ic[nodes]
  tacs$info <- c()
  tacs$info$is_markov <- obj$info$is_markov
  tacs$info$segment <- obj$info$segment
  tacs$info$segment_col <- obj$info$segment_col
  to  <- obj$info$network[,2] %in% nodes
  
  tacs$info$network <- obj$info$network[to,]
  tacs$info$score_sd <-  obj$info$score_sd[nodes]
  
  if(length(tacs$info$segment) >1 ) 
    tacs$info$seg_mean <- obj$info$seg_mean[nodes]
  class(tacs) <- "tacs"
  return(tacs)
}