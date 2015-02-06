#'Generate list of segment name to categorize
#'
#'@param is_markov edges are drown under markov property :boolean TRUE or FALSE(=default)
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'
#'@param segment_name name list of segments. default (seg1,seg2,...)
#'
#'@return list of segment name
set_segname_col <- function(is_markov,segment,segment_name){
  if(length(segment_name) == 1 && is.na(segment_name)) {
    segment_name <- c()
    for(i in 1:length(segment)){
      segment_name <- c(segment_name,paste0("seg",i))
    }
  }
  if(length(segment) != length(segment_name)) {
    warning(message="Nuber of segment and num. of segment name is inequal.")
    warning(message="They must be the same numbers.")
    return(NA)
  }
  
  name_col <- c()
  if(is_markov == TRUE) segment = segment -1
  for(i in 1:length(segment)){
    name_col <- c(name_col,rep(segment_name[i],segment[i]))
  }
  return(name_col)
  
}