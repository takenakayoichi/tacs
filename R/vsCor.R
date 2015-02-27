#'Calc coeffcient between Controll strength of nodes and experimental data
#'
#'@param obj  an object of class tacs
#'
#'@param exp_data experimental data use to calculate tacs: data frame
#'
#'@examples
#'data(dream4)
#'res <- tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment)
#'vsCor(res,dream4$exp)
#'@export
vsCor <- function(obj,exp_data){
  if(obj$info$is_markov == TRUE){
    exp <- set_upper_exp(exp_data,obj$info$segment) 
  } else {
    exp <- exp_data
  }
  #check dimension
  if( nrow(exp) != nrow(obj$score)) {
    warning(message="Numbers of rows of tacs score and experimental data are inequal.")
    return
  }
  #copy edge list and add corr and p-value
  elist <- obj$info$network
  names(elist) <- c("exp.","tacs")
  elist$cor <- rep(NA,nrow(elist))
  elist$pvalue <- rep(NA,nrow(elist))
  for(i in 1: nrow(elist)) {
    from <- elist[i,1]
    to <- elist[i,2]
    from_exp <- exp[[from]]
    to_tacs  <- obj$score[[to]]
    c <- cor.test(from_exp,to_tacs)
    elist$cor[i] <- c$estimate
    elist$pvalue[i] <- c$p.value
  }
  #print(elist)
  #plot elist
  #set.seed(2)
  #g <- graph.edgelist(as.matrix(elist[,1:2]))
  #plot(g)
  #q <- qgraph(elist,edge.labels=TRUE,maximum=1)
  
  #自分自身の発現プロファイルと比較する．
  ingenes <- unique(elist[,2])
  gene <- c()
  cor_value <- c()
  p.value <- c()
  for(g in ingenes){
    self_exp <- exp[[g]]
    self_tacs <- obj$score[[g]]
    c <- cor.test(self_exp,self_tacs)
    gene <- c(gene,g)
    cor_value <- c(cor_value,c$estimate)
    p.value <- c(p.value,c$p.value)
  }
  elist2 <- data.frame(exp.=gene,tacs=gene,cor=cor_value,pvalue = p.value,row.names=NULL)
  
  #elist2 <- elist
  #elist2[,2] <- paste0(elist2[,2],"_tacs")  
  #tacs_nodes <- unique(elist2[,2])
  #exp_nodes  <- unique(elist2[,1])
  
  #g <- graph.edgelist(as.matrix(elist2[,1:2]))
  #bip <- bipartite.mapping(g)
  #plot(g,layout=layout.bipartite(g,types=bip$type))
  
  
  elist_res <- rbind(elist,elist2)
  
  #groups <- obj$info$plot_info$groups
  #layout <- obj$info$plot_info$layout
  #color  <- obj$info$plot_info$color
  #legend <- obj$info$plot_info$legend
  #p <- qgraph(elist_res,layout=layout,groups=groups,color=color,legend=legend,title="Correlation between expression and tacs",edge.labels=TRUE,maximum=1)
  #plot(q)
  
  return(elist_res)
}
