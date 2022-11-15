#' Emission density function for specific 
#' discrete emission distribution (emission/observation matrix)
#'  
#' @description A emission density function for "mhsmm" package. 
#' The emission/observation matrix is used to mock the distribution 
#' For example, an 3 X 2 emission/observation matrix of 2 obs and 3 states is as below:
#' Obs:        o1 o2
#'  States: s1
#'          s2
#'          s3 

#' @author Yihuang Kang
#' @param obsNum The observation number/symbol/label
#' @param stateNum The state number
#' @param model HMM/HSMM Model of the package "mhsmm" 
#' @return Return the probability from the observation matrix
#' 
dspec.hsmm = function(obsNum, stateNum, model) 
{
  return(model$parms.emission$om[stateNum, obsNum] );
  
}