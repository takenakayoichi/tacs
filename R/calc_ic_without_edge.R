#'Calculate information criterion of each node of the network without edge
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
#'@return a dataframe of nodes x samples 
calc_ic_without_edge <-function(exp_data,ic_type,is_markov,segment){
  #make data frame to store ic
  gene_name <- names(exp_data)
  sample_name <- rownames(exp_data)
  ic <- make_data_frame_to_store_ic(gene_name,sample_name,is_markov,segment)

  if (is_markov == TRUE) {
    exp_data <- set_upper_exp(exp_data,segment)
    #lower_exp <- set_lower_exp(exp_data,segment)
  }   
  for(i in 1:length(gene_name)) {
    e = empty.graph(gene_name[i])
    #print(gene_name[i])
      for(time_point in 1:nrow(exp_data)){
        one_gene_data <- as.data.frame(exp_data[-time_point,i])
        colnames(one_gene_data) <- gene_name[i]
        ic[time_point,i] <- score(e,one_gene_data,type = ic_type)        
      }
  }
  
  return(ic)
}
