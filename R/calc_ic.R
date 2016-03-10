#'Calculate information criterion of each node of the network without edge,
#' and ic of every innode for every time point
#'
#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param info input parameters of tacs execution except experimental data
#'@return Scores of node controll strength and supplementary information
#'
calc_ic <- function(exp_data,info){
#calc_ic <- function(network,exp_data,ic_type,is_markov,segment){
  res_ic <- c()
  res_ic$info <- info
  middle <- c()

  middle$wo_edge <- calc_ic_without_edge(exp_data,info$ic_type,info$is_markov,info$segment)
  middle$with_edge <- calc_ic_with_edges(info$network,exp_data,info$ic_type,info$is_markov,info$segment,middle$wo_edge)

  middle$information_gain <- middle$with_edge -middle$wo_edge

  gain_ave <- apply(middle$information_gain,2,ave)
  middle$gain_diff <- -(middle$information_gain - gain_ave)

  
#  res_ic$middle <- middle

  #remove nodes without inedge and store 
  col_valid <- as.vector(!is.na(middle$information_gain[1,]))  
  res_ic$score <- middle$gain_diff[,col_valid]
  #res_ic$edge_ic <- gain_ave[1,col_valid]
  res_ic$ave_InfoGain <- gain_ave[1,col_valid]
  is.null(dim(res_ic$score)){
    score_sd <- sd(res_ic$score)
  }else {
    score_sd <- apply(res_ic$score,2,sd)  
  }
  res_ic$info$score_sd <- score_sd

  #Prosess when experiment has segments
  if(length(info$segment)!=1){ #generate segment info
    seg_mean = aggregate(res_ic$score,by=list(seg=info$segment_col),FUN=mean)
    rownames(seg_mean) <- seg_mean$seg
    seg_mean$seg <- NULL
#    seg_mean_sum = apply(seg_mean,2,sum)
#    col_order <- order(seg_mean_sum,decreasing=TRUE)
#    seg_mean[col_order]
    res_ic$info$seg_mean <- seg_mean
  }
  #set col order by ave_InfoGain
  #res_ic$info$col_order <- order(res_ic$ave_InfoGain,decreasing=TRUE)
  col_order <- order(res_ic$ave_InfoGain,decreasing=TRUE)
  res_ic$score <- res_ic$score[,col_order]
  res_ic$ave_InfoGain <- res_ic$ave_InfoGain[col_order]
  res_ic$info$seg_mean <- res_ic$info$seg_mean[,col_order]
  res_ic$info$score_sd <- res_ic$info$score_sd[col_order]

  return(res_ic)
}

