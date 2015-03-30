#'Calculate Control Strength toward every nodes on directed graph based on information criterion
#'
#'@name tacs
#'@docType package
#'@import bnlearn
#'@import ggplot2
#'@import grid
#'@import qgraph
#'@import animation
NULL
#'Calculate Control Strength toward every nodes on directed graph based on information criterion
#'
#'@param network directed graph: format used in library bnlearn is used.
#'
#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param ic_type information criterion type: string "aic"(=default) or "bayes"
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'
#'@param segment_name name list of segments. default (seg1,seg2,...)
#'
#'@return strength Control strength toward each node for every time point: data frame
#'@examples
#'data(diauxie)#an example in GIW2015 paper
#'res <- tacs(diauxie$net,diauxie$exp,"aic-g",is_markov=TRUE,segment=diauxie$segment,segment_name=diauxie$segment_name)
#'gene_selected <- apply(res$score,2,max) + res$ave_InfoGain >0
#'score <- res$score[1:16,gene_selected] # 1:16: Wild Type.
#'score[score<0] <- 0  #to show the results in barplot, remove negatives
#'barplot(t(as.matrix(score)),width=nrow(score),beside=TRUE,
#'col=topo.colors(ncol(score)),legend.text=names(score))
#'WT <-  select.segments(res,"WT") #Plot score of each gene
#'WT_genes <- select.nodes(WT,names(gene_selected[gene_selected]))
#'plot(WT_genes)
#'data(dream4) #Anther example
#'tacs(dream4$net,dream4$exp,"aic-g",TRUE,dream4$segment,NA) -> res
#'@export
tacs <- function(network,exp_data,ic_type="aic-g",is_markov=FALSE,segment=NA,segment_name=NA)
{
  if(length(segment) == 1 && is.na(segment)) segment <- nrow(exp_data)
  seg_name_col <- set_segname_col(is_markov,segment,segment_name)
  if(length(seg_name_col) == 1 && is.na(seg_name_col)) return()

#ic_type and segment check
  if (consistency_check(exp_data,ic_type,segment) == FALSE) return() 

  info <-list(is_markov=is_markov,segment=segment,segment_col=seg_name_col,network=network)
  res <- calc_ic(exp_data,info)

  class(res) <- "tacs"
  return(res)
}
