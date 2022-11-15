#' Random observation class geneation from an emission/observation matrix of
#' an HMM/HSMM
#'  
#' @description A random observation class geneation from an
#' emission/observation matrix of an HMM/HSMM
#' For example, an 3 X 2 emission/observation matrix of 2 obs and 3 states is as below:
#' Obs:        o1 o2
#'  States: s1
#'          s2
#'          s3 

#' @author Yihuang Kang
#' @param stateNum The state number
#' @param model HMM/HSMM Model of the package "mhsmm" 
#' @return Return a random generated number given the probabilties in the 
#' emission/observation matrix
#' 
rspec.hsmm = function(stateNum, model) 
{
  # Discrete class/observation probablity distribution
  obsClassProb = model$parms.emission$om[stateNum,];
  
  # Return class number
  obsClassNumber = 
    rdiscrete(n=1,numOfClass=length(obsClassProb),classProbs=obsClassProb ); 
  # Return observation
  # obsClass = names(obsClassProb)[obsClassNumber];
  
  return(obsClassNumber);
  
}
