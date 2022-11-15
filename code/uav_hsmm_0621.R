library(mhsmm)

## customized emission function for multinomial emission
source("code/utils/dspec.hsmm.R")
source("code/utils/rspec.hsmm.R")
source("code/utils/mstep.multinom.R")

## execute modify_dataframe_value.R to get processed data before building HSMM model
head(payload_df)


N <- as.numeric(table(payload_df$file_name)) # get length of sequence of each subject
train <- list(x = payload_df$payload_act, N = N)
class(train) <- "hsmm.data"

M <- max(N) # maximum of length of sequences

##### Calculate duration of observation sequence #####
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

## Estimate distribution of duration
library(fitdistrplus)
descdist(durations, boot=1000)  # Cullen and Frey-plot

fitdist(durations, distr = "normal", method = "mle")
mean(log(durations))
sd(log(durations))
# Fitting of the distribution ' gamma ' by maximum likelihood 

# Parameters:
#   estimate Std. Error
# shape 3.6265731 0.35814997
# rate  0.2261341 0.02395207

##### end of calculation #####

## Assume there are 3 hidden states, and start from the first hidden state.
## Also, assume the transition probability and emission probability

##### HSMM #####
tmp <- gammafit(durations)
hist(rgamma(M, tmp$shape, tmp$scale), breaks = 100)

d <- cbind(dnorm(1:M, mean = mean(durations), sd = sd(durations)),
           dunif(1:M, 1, 8985), 
           dunif(1:M, 1, 8985))

## Define HSMM parameters
J <- 3
P <- matrix(c(0, 0, 1,
              1, 0, 0,
              0, 1, 0), nrow = J)
table(uav_df$payload_act)

# a 3x3 matrix
emis.uav <- list(om = matrix(c(0, 0,   1,
                               0, 0.5, 0,
                               0, 0.5, 0), nrow = J))

# emis <- list(mu = c(0, 2.5, 0), sigma = c(1, 1, 1))


hsmm.start.np <- hsmmspec(
  init = rep(1/J, J),
  transition = P,
  parms.emission = emis.uav, #會是不一樣的值
  sojourn = list(d = d, type = "gamma"),
  dens.emission = dspec.hsmm)

hsmm.activity <- hsmmfit(train, hsmm.start.np, mstep = mstep.multinom, maxit = 5)
hsmm.activity$model$transition
hsmm.activity$loglik
summary(hsmm.activity)
##### end of HSMM #####


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
