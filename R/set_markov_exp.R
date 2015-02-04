#' Convert data set for simple markov property set data for up stream node
#' 
#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'@return
#' data set without segment end
set_upper_exp<- function(exp_data,segment){
  end_point <- c()
  tmp_end <- 0
  for(seg in 1:length(segment)){
    tmp_end = tmp_end + segment[seg]
    end_point <- c(end_point, tmp_end)
  }
  return(exp_data[-end_point,])
}

#' Convert data set for simple markov property, set data for down stream node
#' 
#'@param exp_data experimental data on nodes in the network: data frame
#'
#'@param segment This paramer assigns the segments of exp_data. 
#' It is used when exp_data is composed of plural time-course experiment and the edges are drown under markov property
#'@return
#' data set without segment start
set_lower_exp<- function(exp_data,segment){
  start_point <- c()
  tmp_start <- 1
  for(seg in 1:length(segment)){
    start_point <- c(start_point,tmp_start)
    tmp_start = tmp_start + segment[seg]
  }
  return(exp_data[-start_point,])
}