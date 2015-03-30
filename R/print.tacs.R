#'print function for tacs class
#'
#'@param obj  an object of class tacs
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'print(res)
#'@export
print.tacs <- function(obj,...){
  cat("Time-course of control strength toward target nodes")
  if(obj$info$is_markov) {
    cat("(on Markov process)\n\n")
  } 
  
  if(length(obj$info$segment) != 1) { 
    cat("\n[segment summary]\n\n")    
    df <- rbind(ave_InfoGain=obj$ave_InfoGain,sd=obj$info$score_sd,obj$info$seg_mean)
    print(df)
  }
  
  cat("\n[node summary]\n")
  tmp <- summary(obj$score,...)  
  print(tmp)
#  cat("Time-course of control strength toward target nodes")
#  if(obj$info$is_markov) {
#    cat("(on Markov process\n\n")
#  } 
#  cat("\n[network]\n\n")    
#  print(obj$info$network)
  
#  if(length(obj$info$segment) != 1) { 
#    cat("\n[segment summary]\n\n")    
#    df <- rbind(ave_InfoGain=obj$ave_InfoGain,sd=obj$info$score_sd,obj$info$seg_mean)
#    print(df)
#  }
#  cat("\n[time-course of control strength toward nodes] \n\n")
  
#  print(obj$score,...)
}