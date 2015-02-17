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
#'animation(res,dream4$exp)
#'@export
animation <- function(obj,segment=FALSE,filename="index.html",width=1000,height=1000){
  if( is.null(obj$info$plot_info$layout)) {
    obj$info$plot_info$layout <- set_layout(obj)
  }
  
  elist <- obj$info$network
  
  if(segment){
    nmax <- 1 + length(obj$info$segment)
    
    oopt <- ani.options(interval=1,ani.width=width,ani.height=height,
                        title="TACS animation",description="TACS animation on segments",
                        img.name="tacs_seg",nmax=nmax)
    saveHTML({
#      ani.options(oopt)
      elist$weight <- set_edge_weight(elist,obj$edge_ic)
      p <- qgraph(elist,layout=obj$info$plot_info$layout,title="Infomation Content of inNode")
      ani.pause()
      for(i in 1:length(obj$info$segment)){
        elist$weight <- set_edge_weight(elist,obj$info$seg_mean[i,])
        p <- qgraph(elist,layout=obj$info$plot_info$layout,title=paste0("Average control strength at ",rownames(obj$info$seg_mean)[i]))
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
      elist$weight <- set_edge_weight(elist,obj$edge_ic)
      p <- qgraph(elist,layout=obj$info$plot_info$layout,title="Infomation Content of inNode")
      ani.pause()
      for(i in 1:nrow(obj$score)){
        elist$weight <- set_edge_weight(elist,obj$score[i,])
        p <- qgraph(elist,layout=obj$info$plot_info$layout,title=paste0("Average control strength at ",rownames(obj$score)[i]))
        ani.pause()
      }
    }, )
  }
  
  
  
}