#'Select segment by name
#'
#'@param obj  an object of class tacs
#'
#'@param segs segment name list to be selected.
#'
#'@param delete if TRUE designated segments are deleted 
#'
#'@examples
#'data(dream4)
#'obj <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'segs <- c("seg1","seg4")
#'select1 <- select.segments(obj,segs)
#'select2 <- select.segments(obj,segs,delete=TRUE)
#'@export
select.segments <- function(obj,segs,delete=FALSE){
  if(length(obj$info$segment)==1) {
    warning("There is only one segment")
    return
  }
  if(delete==TRUE) {
    new_segs <- setdiff(rownames(obj$info$seg_mean) ,segs)
    tacs <- select.segments(obj,new_segs,delete=FALSE)
    return(tacs)
  }  
  
  segcols <- obj$info$segment_col %in% segs
  segpos <- rownames(obj$info$seg_mean) %in% segs
  
  tacs <- c()
  tacs$score <- obj$score[segcols,]
  tacs$ave_InfoGain <- obj$ave_InfoGain
  tacs$info <- c()
  tacs$info$is_markov <- obj$info$is_markov
  tacs$info$segment <- obj$info$segment[segpos]
  tacs$info$segment_col <- obj$info$segment_col[segcols]

  tacs$info$network <- obj$info$network
  tacs$info$score_sd <-  obj$info$score_sd
  
  tacs$info$seg_mean <- obj$info$seg_mean[segpos,]

  class(tacs) <- "tacs"
  return(tacs)
  
  
  
  
}