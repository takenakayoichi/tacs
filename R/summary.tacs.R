#'summary function for tacs class
#'
#'@param obj  an object of class tacs
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'summary(res)
#'@export
summary.tacs <- function(obj){
  pos_valid <- as.vector(!is.na(obj$information_gain[1,]))
  
  info <- obj$info
  
  gain <- obj$information_gain[,pos_valid]
  if(length(info$segment)!=1){ #show summary info on each segment
    gain_mean = aggregate(gain,by=list(seg=info$segment_col),FUN=mean)
    rownames(gain_mean) <- gain_mean$seg
    gain_mean$seg <- NULL
    gain_mean_sum = apply(gain_mean,2,sum)
    gain_order <- order(gain_mean_sum,decreasing=TRUE)
    gain_mean[gain_order]
  }
  
  
  by(obj$information_gain[,pos_valid],seg_name,summary)
  by(obj$information_gain[,pos_int],seg_name,summary)
  by(obj$information_gain[,c(1)],seg_name,mean)
  

}