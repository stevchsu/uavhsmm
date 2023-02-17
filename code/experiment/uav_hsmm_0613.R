library(mhsmm)

## execute modify_dataframe_value.R to get processed data before building HSMM model
head(uav_df)


N <- as.numeric(table(uav_df$file_name)) # get length of sequence of each subject
train <- list(x = uav_df$payload_act, N = N)
class(train) <- "hsmm.data"

M <- max(N) # maximum of length of sequences

durations <- list()
counter <- 0
subject <- uav_df[1, "file_name"]

for (idx in seq(nrow(uav_df))) {
  if(uav_df[idx, "action"] == "payload") {
    counter <- counter + 1
    
    if (!is.na(uav_df[idx, "payload_act"]) | uav_df[idx, "payload_act"] %in% c(0, 1, 2) | subject != uav_df[idx, "file_name"]) {
      durations <- append(durations, counter)
      counter <- 0
    }
    if (subject != uav_df[idx, "file_name"]) {
      subject <- uav_df[idx, "file_name"]
    }
  }
}

durations <- unlist(durations)
plot(density(durations))


## Assume there are 3 hidden states, and start from the first hidden state.
## Also, assume the transition probability and emission probability

##### HMM #####
J <- 3
P <- matrix(1/J, nrow = J, ncol = J)

hmm.start.np <- hmmspec(
  init = rep(1/J, J), trans = P, parms.emission = list(mu = c(0, 0, 0), sigma = c(1, 1, 1)), 
  dens.emission = dnorm.hsmm)

hmm.activity <- hmmfit(train, hmm.start.np, mstep = mstep.norm)
hmm.activity$loglik
plot(hmm.activity$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
summary(hmm.activity)

##### HSMM #####
## Estimate distribution of duration
library(fitdistrplus)
descdist(durations, boot=1000)  # Cullen and Frey-plot


tmp <- gammafit(durations)

hist(rgamma(M, tmp$shape, tmp$scale), breaks = 100)

d <- cbind(dgamma(1:M, shape = tmp$shape, scale = tmp$scale), 
           dgamma(1:M, shape = tmp$shape, scale = tmp$scale),
           dgamma(1:M, shape = tmp$shape, scale = tmp$scale))

## Define HSMM parameters
J <- 3
P <- matrix(c(0, 0.5, 0.5,
              0.5, 0, 0.5,
              0.5, 0.5, 0), nrow = J, ncol = J)
# P <- matrix(c(0, 1,
#               1, 0), nrow = J, ncol = J)

hsmm.start.np <- hsmmspec(
  init = rep(1/J, J),
  transition = P,
  parms.emission = list(mu = c(0, 0, 0), sigma = c(1, 1, 1)), #會是不一樣的值
  sojourn = list(d = d, type = "gamma"),
  dens.emission = dnorm.hsmm
)

hsmm.activity <- hsmmfit(train, hsmm.start.np, mstep = mstep.norm, graphical = T, maxit = 50)
hsmm.activity$model$transition
hsmm.activity$loglik
summary(hsmm.activity)



##### ignore #####
## Calculate duration of each event
duration_list <- list()
duration <- 0

for (i in seq(nrow(payload_df))) {
  if (payload_df[i, "payload_act"] %in% c(0, 1, 2)) {
    duration_list <- append(duration_list, duration)
    duration <- 0
  } else {
    duration <- duration + 1
  }
}

duration_list <- unlist(duration_list[-1])
plot(density(duration_list))
nortest::lillie.test(duration_list)

##### end of section #####
