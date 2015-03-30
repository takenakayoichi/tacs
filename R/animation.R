#'Display Animation of Control Strength of each time-point or segment
#'
#'@param obj  an object of class tacs
#'
#'@param segment logical if TRUE, plot control strength of each segment, FALSE plot each gene
#'@param filename filename of html that includes the animation.
#'@param width width of animation 
#'@param height height of animation
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'animation(res)
#'@export
animation <- function(obj,segment=FALSE,filename="index.html",width=1000,height=1000,cutoff=0){
  if( is.null(obj$info$plot_info$layout)) {
    obj$info$plot_info$layout <- set_layout(obj)
  }
  
  elist <- obj$info$network
  groups <- obj$info$plot_info$groups
  layout <- obj$info$plot_info$layout
  color  <- obj$info$plot_info$color
  legend <- obj$info$plot_info$legend
  sd     <- obj$info$score_sd
  
  
  if(segment){
    nmax <- 1 + length(obj$info$segment)
    
    oopt <- ani.options(interval=1,ani.width=width,ani.height=height,
                        title="TACS animation",description="TACS animation on segments",
                        img.name="tacs_seg",nmax=nmax)
    saveHTML({
#      ani.options(oopt)
      elist$weight <- set_edge_weight(elist,obj$ave_InfoGain)
      elist$weight <- cutoff_edge_weight(elist,sd,cutoff)
#      p <- qgraph(elist,layout=obj$info$plot_info$layout,groups=obj$info$groups,title="Infomation Content of inNode")
      p <- qgraph(elist,layout=layout,groups=groups,color=color,legend=legend,title="Infomation Content of inNode")
      ani.pause()
      for(i in 1:length(obj$info$segment)){
        elist$weight <- set_edge_weight(elist,obj$info$seg_mean[i,])
        elist$weight <- cutoff_edge_weight(elist,sd,cutoff)
        p <- qgraph(elist,layout=layout,groups=groups,color=color,legend=legend,title=paste0("Average control strength at ",rownames(obj$info$seg_mean)[i]))
        ani.pause()
      }
    }, )
    
  }else {
    nmax <- 1 + nrow(obj$score)
    oopt <- ani.options(interval=1,ani.width=width,ani.height=height,
                        title="TACS animation",description="TACS animation on segments",
                        img.name="tacs_seg",nmax=nmax)
    
    saveHTML({
      #      ani.options(oopt)
      elist$weight <- set_edge_weight(elist,obj$ave_InfoGain)
      elist$weight <- cutoff_edge_weight(elist,sd,cutoff)
      p <- qgraph(elist,layout=layout,groups=groups,color=color,legend=legend,title="Infomation Content of inNode")
      ani.pause()
      for(i in 1:nrow(obj$score)){
        elist$weight <- set_edge_weight(elist,obj$score[i,])
        elist$weight <- cutoff_edge_weight(elist,sd,cutoff)
        p <- qgraph(elist,layout=layout,groups=groups,color=color,legend=legend,title=paste0("Average control strength at ",rownames(obj$score)[i]))
        ani.pause()
      }
    }, )
  }
  
  
  
}