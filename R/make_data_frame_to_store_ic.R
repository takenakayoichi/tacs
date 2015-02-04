#'make a data_frame object to store ic
#'
#'@param col_name  : colname of data_frame (ex. gene name) 
#'
#'@param sample_name : rowname of data_frame
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'
#'@return a dataframe of nodes x samples (or samples-length(segment) in is_markov)
make_data_frame_to_store_ic <-function(col_name,sample_name,is_markov,segment){
  num_col <-  length(col_name)
  if(is_markov)   num_row <- length(sample_name) -length(segment) 
  else num_row <- length(sample_name)

  row_name <- set_row_name(sample_name,is_markov,segment)    
  
  
  df <- data.frame( matrix(data=0.0,nrow=num_row,ncol=num_col),row.names = row_name)
  colnames(df) <- col_name
  
  return(df)
}