library(mhsmm)

data("reproai")
data("reprocows")
data("reproppa")

J <- 3
init <- c(1, 0, 0)

trans <- matrix(c(0, 0, 0,
                  1, 0, 1, 
                  0, 1, 0), nrow = J)
emis <- list(mu = c(0, 2.5, 0), sigma = c(1, 1, 1))

N <- as.numeric(table(reprocows$id))
train <- list(x = reprocows$activity, N = N)
class(train) <- "hsmm.data"

tmp <- gammafit(reproppa * 24)

M <- max(N)
d <- cbind(dgamma(1:M, shape = tmp$shape, scale = tmp$scale),
           dunif(1:M, 4, 30), dunif(1:M, 15 * 24, 40 * 24))


startval <- hsmmspec(init, trans, emis, list(d = d, type = "gamma"),
                     dens.emis = dnorm.hsmm)
h.activity <- hsmmfit(train, startval, mstep = mstep.norm, maxit = 10,
                      M = M, lock.transition = TRUE)

h.activity$model$transition
h.activity$model$parms.emission
h.activity$model$sojourn

startval$parms.emission
tmp$shape
tmp$scale



