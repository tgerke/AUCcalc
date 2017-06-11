##############################################################################
# compare various AUC-calculating functions + a custom script

library(pROC)
library(ROCR)
library(PerfMeas)
library(glmnet) #note that this masks auc() from pROC

# log which versions are installed
sessionInfo()

##############################################################################
# simulate binomial y with p=.2 and standard normal x

iter <- 1e4

# test ROCR package
ROCRest <- rep(0, iter)
set.seed(8675309)
ROCRtime <- system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    ROCRest[i] <- performance(prediction(dat$x, dat$y), "auc")@y.values[[1]]
  }
)[3]

# test pROC package
pROCest <- rep(0, iter)
set.seed(8675309)
pROCtime <- system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    pROCest[i] <- pROC:::auc(response=dat$y, predictor=dat$x, direction="<")
  }
)[3]

# test PerfMeas package
PerfMeasest <- rep(0, iter)
set.seed(8675309)
PerfMeastime <- system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    PerfMeasest[i] <- AUC.single(dat$x, dat$y)
  }
)[3]

# test glmnet package
glmnetest <- rep(0, iter)
set.seed(8675309)
glmnettime <- system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    glmnetest[i] <- glmnet:::auc(y=dat$y, prob=dat$x)
  }
)[3]

# test a simpler function
Simpleest <- rep(0, iter)
set.seed(8675309)
Simpleesttime <- system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    x1 <- dat$x[dat$y==1]
    x0 <- dat$x[dat$y==0]
    n1 <- length(x1)
    n0 <- length(x0)
    Simpleest[i] <- sum(vapply(x1, function(z) sum(z>x0), 0))/n1/n0
  }
)[3]

# do they all give equivalent estimates?
summary(ROCRest - pROCest)
summary(ROCRest - PerfMeasest)
summary(ROCRest - glmnetest)
summary(ROCRest - Simpleest)

timings <- data.frame(ROCR=ROCRtime, pROC=pROCtime, 
                      PerfMeas=PerfMeastime, glmnet=glmnettime,
                      Simpleest=Simpleesttime)
timings

# # precision recall
# precrec <- precision.at.all.recall.levels(dat$x, dat$y)
# trap.rule.integral(precrec$recall, precrec$precision)
# plot(precrec$recall, precrec$precision)
# 
# # here's a way to do it based on ordering (slower, but could be useful for pAUC)
# dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
# ordered <- dat[order(dat$x, decreasing=TRUE),]
# 
# auc(response=dat$y, predictor=dat$x)
# auc(response=ordered$y, predictor=ordered$x)
# 
# aucsum <- 0
# n1 <- sum(ordered$y)
# n0 <- sum(ordered$y==0)
# sensterm <- specterm <- rep(0, nrow(ordered)-1)
# for (i in 2:nrow(ordered)) {
#   sensterm[i] <- sum(ordered$y[1:(i-1)])/n1
#   specterm[i] <- (ordered$y[i]==0)/n0
#   aucsum <- aucsum + sensterm[i]*specterm[i]
# }
# aucsum
# 
# obj <- roc(response=ordered$y, predictor=ordered$x)
# obj
# obj$sens
# obj$spec

plot(obj)