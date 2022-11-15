#' Re-estimate the parameters of density function on each iteration
#' 
#' @param x is a vector of observed values
#' @param wt is a TxJ matrix of weights, column entries are the weights for respective state
#' T = levels of observed values
#' J = number of hidden states
#' @return a probability matrix
#' 

mstep.multinom <- function(x, wt) {
  print(wt)
  wt[, x-1] = wt[, x-1] / sum(wt[, x-1])
  list(wt)
}

# emis_mat <- matrix(c(0.3, 0.5, 0.4, 
#                         0.7, 0.5, 0.6), nrow=3)
# mstep.multinom(c(1, 2), emis_mat)
# 
# mstep.norm <- function(x,wt) {
#   k = ncol(wt)
#   mu = numeric(k)
#   sigma = numeric(k)
#   for(i in 1:k) {
#     tmp = cov.wt(data.frame(x[!is.na(x)]),wt[!is.na(x),i])
#     mu[i] = tmp$center
#     sigma[i] = tmp$cov
#   }
#   list(mu=mu,sigma=sigma)
# }