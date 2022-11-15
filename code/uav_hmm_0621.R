library(mhsmm)

## customized emission function for multinomial emission
source("code/utils/dspec.hsmm.R")

## execute modify_dataframe_value.R to get processed data before building HSMM model
head(payload_df)


N <- as.numeric(table(payload_df$file_name)) # get length of sequence of each subject
train <- list(x = payload_df$payload_act, N = N)
class(train) <- "hmm.data"


J <- 3
P <- matrix(1/J, nrow = J, ncol = J)

hmm.start.np <- hmmspec(
  init = rep(1/J, J), trans = P, 
  parms.emission = list(om=matrix(c(1/2, 1/2, 0,
                                    1/2, 0, 1/2, 
                                    0, 1/2, 1/2), nrow = J)),
  dens.emission = dspec.hsmm)

hmm.activity <- hmmfit(train, hmm.start.np, mstep = mstep.norm)
hmm.activity$loglik
plot(hmm.activity$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
summary(hmm.activity)