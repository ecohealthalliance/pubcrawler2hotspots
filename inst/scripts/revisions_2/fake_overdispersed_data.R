# Fake Overdispersed Data
# From https://stat.ethz.ch/pipermail/r-help/2002-June/022425.html

library(ggplot2)

rpois.od<-function (n, lambda,d=1) {
  if (d==1)
    rpois(n, lambda)
  else
    rnbinom(n, size=(lambda/(d-1)), mu=lambda)
}

rnbinom.od<-function (n, size, prob, mu, d=1) {
  if (!missing(prob)) {
    if (!missing(mu))
      stop("prob and mu both specified")
    mu<-size*(1-prob)/prob
  }
  size2 <- mu/(d-1+(d*mu/size))
  prob2 <- size2/(size2 + mu)
  .Internal(rnbinom(n, size2, prob2))
}

poisson <- rpois(16000, lambda = 0.1)
qplot(poisson)
qplot(log(poisson))

negative_binomial <- rnbinom(16000, mu = 1, size = 0.1)
qplot(negative_binomial)
qplot(log(poisson))

poisson <- rpois(16000, lambda = 0.1)
qplot(poisson)
qplot(log(poisson))

negative_binomial <- rnbinom(16000, mu = 1, size = 0.1)
qplot(negative_binomial)
qplot(log(poisson))

poisson.od <- rpois.od(16000, lambda = 0.1, d = 4)
qplot(poisson.od)
qplot(log(poisson.od))

negative_binomial.od <- rnbinom.od(16000, mu = 1, size = 0.1)
qplot(negative_binomial.od)
qplot(log(negative_binomial.od))
