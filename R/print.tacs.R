#'print function for tacs class
#'
#'@param obj  an object of class tacs
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'print(res)
#'@export
print.tacs <- function(obj){
  if(obj$input$is_markov) {
    print("Network on simple Markov process")
  } else {
    print("Network")
  }
  print(obj$input$network)
  
  print("Information Gain")
  print(obj$information_gain)
  
#  summary(c(1,2,3,4,5))  
}