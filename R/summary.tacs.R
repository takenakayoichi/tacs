#'summary function for tacs class
#'
#'@param obj  an object of class tacs
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'summary(res)
#'@export
summary.tacs <- function(obj,...){
  cat("Time-course of control strength toward target nodes")
  if(obj$info$is_markov) {
    cat("(on Markov process)\n\n")
  } 
  cat("#nodes  ",ncol(obj$score))
  cat("\t",colnames(obj$score))
  cat("\n#samples",nrow(obj$score))
  
  if(length(obj$info$segment) != 1) { 
    cat("\n#segments", length(obj$info$segment))
    cat("\t", rownames(obj$info$seg_mean))
  }
  
  cat("\n#edges", nrow(obj$info$network))
  cat("\n")
  
#  if(length(obj$info$segment) != 1) { 
#    cat("\n[segment summary]\n\n")    
#    df <- rbind(edge_ic=obj$edge_ic,sd=obj$info$score_sd,obj$info$seg_mean)
#    print(df)
#    cat("\n[node summary]\n\n")
#  }
  
#  summary(obj$score,...)  

}