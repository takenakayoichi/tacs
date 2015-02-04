#' Generate list of row names of data frame, which to store ic of each node
#' When is_markov is TRUE, the simple markov model is adopted.
#'
#'@param sample_name : rowname of data_frame
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'@return
#' row name (sample name) of data frame to store ic of each node 

set_row_name <- function(sample_name,is_markov,segment){
  if(is_markov == FALSE) return(sample_name)
  row_name <- c()
  row <-1;
  for( seg in 1: length(segment)) {
    for(i in 1:(segment[seg]-1)){
      row_name <- c(row_name,paste0(sample_name[row]," -> ",sample_name[row+1]))
      row = row +1
    }
    row = row +1
  }
  return(row_name)
}