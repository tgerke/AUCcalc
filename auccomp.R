##############################################################################
# compare various AUC-calculating functions + a custom script

library(pROC)
library(ROCR)
library(PerfMeas)

# log which versions are installed
sessionInfo()

##############################################################################
# simulate binomial y with p=.2 and standard normal x

iter <- 1e4

# test ROCR package
ROCRest <- rep(0, iter)
set.seed(8675309)
system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    ROCRest[i] <- performance(prediction(dat$x, dat$y), "auc")@y.values[[1]]
  }
)

# test pROC package
pROCest <- rep(0, iter)
set.seed(8675309)
system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    pROCest[i] <- auc(response=dat$y, predictor=dat$x, direction="<")
  }
)

# test PerfMeas package
PerfMeasest <- rep(0, iter)
set.seed(8675309)
system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    PerfMeasest[i] <- AUC.single(dat$x, dat$y)
  }
)

# test a simple one-liner
Simpleest <- rep(0, iter)
set.seed(8675309)
system.time(
  for (i in 1:iter) {
    dat <- data.frame(y=rbinom(100, 1, .2), x=rnorm(100))
    Simpleest[i] <- sum(vapply(dat$x[which(dat$y==1)], 
                               function(z) sum(z>dat$x[which(dat$y==0)]), 0))/sum(dat$y)/sum(dat$y==0)
  }
)

# do they all give equivalent estimates?
summary(ROCRest - pROCest)
summary(ROCRest - PerfMeasest)
summary(ROCRest - Simpleest)

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