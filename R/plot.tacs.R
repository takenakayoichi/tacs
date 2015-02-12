#'Plot function for tacs class
#'
#'@param obj  an object of class tacs
#'
#'@param segment logical if TRUE, plot control strength of each segment, FALSE plot each gene
#
#'
#'@param max_node maximum number of nodes to display
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'plot(res)
#'@export
plot.tacs <- function(obj,segment=FALSE,max_node=0,...){
  grid.newpage()
  num_nodes <- ncol(obj$score)  
  if(max_node==0 || max_node > num_nodes) max_node <- num_nodes

  if(segment==TRUE && length(obj$info$segment)!=1){
    seg_df <- obj$info$seg_mean[1:max_node]
    seg_df$seg <- as.factor(rownames(obj$info$seg_mean))
    seg_df2 <- reshape2::melt(seg_df,id.vars=c("seg"),variable.name="gene")
    num_seg = nrow(seg_df)
    #num_gene = ncol(seg_df)
    byseg  <- ggplot(seg_df2,aes(x=gene,y=value,fill=gene)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~seg,ncol=num_seg)
    bygene <- ggplot(seg_df2,aes(x=seg,y=value,fill=seg)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~gene,ncol=max_node)
    
    pushViewport(viewport(layout=grid.layout(2,1)))
    print(byseg,vp=viewport(layout.pos.row=1,layout.pos.col=1))    
    print(bygene,vp=viewport(layout.pos.row=2,layout.pos.col=1))
  } else if(segment==TRUE && length(obj$info$segment)== 1){
    warning("One segment, No plot")  
  } else 
  {
    df <- obj$score[1:max_node]
    df$timing <- as.factor(rownames(obj$score))
    
    df2 <- reshape2::melt(df,id.vars=c("timing"),variable.name="gene")
    p  <- ggplot(df2,aes(x=timing,y=value,fill=timing)) + geom_bar(position="dodge",stat="identity") +facet_wrap(~gene,nrow=max_node)
    print(p)
  }
}
