#'Plot function for tacs class
#'
#'@param obj  an object of class tacs
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'plot(res)
#'@export
plot.tacs <- function(obj,segment=FALSE,...){
  if(segment==TRUE){
    seg_df <- obj$info$seg_mean
    seg_df$seg <- as.factor(rownames(obj$info$seg_mean))
    seg_df2 <- reshape2::melt(seg_df,id.vars=c("seg"),variable.name="gene")
    #ggplot(seg_df2,aes(x=seg,y=value,fill=gene)) + geom_bar(position="dodge",stat="identity")
    #ggplot(seg_df2,aes(x=gene,y=value,fill=seg)) + geom_bar(position="dodge",stat="identity")
    return()
  }


}