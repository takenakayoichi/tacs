#'Calculate information criterion of each node of the network with edges
#'
#'@param network directed graph: format used in library bnlearn is used.

#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param ic_type information criterion type: string "aic"(=default) or "bayes"
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'@param icwoedge information criterion of nodes without edge
#'@return a dataframe of nodes x samples 
calc_ic_with_edges <-function(network,exp_data,ic_type,is_markov,segment,icwoedge){
  #make data frame to store ic
  gene_name <- names(exp_data)
  sample_name <- rownames(exp_data)
  ic <- make_data_frame_to_store_ic(gene_name,sample_name,is_markov,segment)
  
  if (is_markov == TRUE) {
    upper_exp <- set_upper_exp(exp_data,segment)
    lower_exp <- set_lower_exp(exp_data,segment)
  } 
  #ネットをとってくる＆上流ノード
  #マルコフ FALSE/TRUEの順で実装
  for(i in 1:length(gene_name)){
    #gene_name[i]が下流のノード集合をとってくる
    logic <- (network[,2] == gene_name[i])
    subnet <- network[logic,]
    if(nrow(subnet)==0) {
      ic[,i] <- NA
      next
    }
    #ネット形成
    nodes <- c(gene_name[i],subnet[[1]])
    e = empty.graph(nodes)
    arcs(e) = subnet
    
    #観測データ加工
    if(is_markov ==TRUE) {
      in_lower_exp <- as.data.frame(lower_exp[,i])
      colnames(in_lower_exp) <- gene_name[i]
    
      out_upper_exp <- lower_exp[, is.element(colnames(lower_exp),subnet[[1]])]
      if( class(out_upper_exp) == "numeric") {
        out_upper_exp <- as.data.frame(out_upper_exp)
        colnames(out_upper_exp) <- subnet[[1]][1]
      }
      subexp <- cbind(in_lower_exp,out_upper_exp)
    } else {
      subexp <- exp_data[,is.element(colnames(exp_data),nodes)]
    }
    #ネットスコア計算
    for(time_point in 1:nrow(subexp)){
      subnet_score <- score(e,subexp,type=ic_type)
      #ICに代入する
      ic[time_point,i] <- subnet_score
    }
    #上流ノードスコアを引く
    for(upper_node in subnet[[1]]) {
      ic[,i] <- ic[,i] - icwoedge[,colnames(icwoedge) == upper_node ]
      
    }
      #node_score <- subnet_score - icwoedge[time_point,i]
    
  }
  return(ic)

}
