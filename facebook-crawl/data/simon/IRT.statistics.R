# Author: Simon Guo.
# This script is to use the IRT model to analyse the dataset.
# we are using the binary IRT model ltm for the analysis, so 
# the input data of this script should have binary response. 
#
require(ltm)

args <- commandArgs()

print(args)

data <- read.csv(args[6], head=TRUE, sep=",")
data0 <- 1 - data # the opposite of the current response data. 

###########################
# IRT modeling functions. #
###########################
ltm.model <- function (dat) {
  # model.rasch <- rasch(data, constraint = cbind(ncol(data) + 1, 1))
  model <- ltm(data ~ z1)
  model$dffclt <- coef(model)[,1]
  model$dscrmn <- coef(model)[,2]
  model$fitted <- fitted(model, data, type="conditional-probabilities")
  model$scores <- factor.scores(model, data) # factor scores. 
  model$theta  <- scores$score.data$z1 # ability level / theta.
  model$sump   <- apply(model$fitted, 1, sum) # summed probabilities. 
  model$sumu   <- apply(data, 1, sum)    # summed binary settings.
  return(model)
}

# histgram of the hidden variables.
ltm.hist <- function (model, model0) {
  hist(model$theta, 30, freq=FALSE, main="Histgram of theta for privacy data(0).", 
       xlab=NULL, ylab=NULL); 
  lines(density(model$theta), col="red")
  rug(jitter(model$theta), col="brown")
  
  hist(model0$theta, 30, freq=FALSE, main="Histgram of theta for privacy data(1).", 
       xlab=NULL, ylab=NULL); 
  lines(density(model0$theta), col="red")
  rug(jitter(model0$theta), col="brown")
}

# estimated theta vs. estimated score and expected scores.
ltm.scores <- function (model, model0) {
  plot(model$theta, model$sump, col="blue", main="Theta vs. Scores(0).", xlab=NULL, ylab=NULL); 
  points(model$theta, model$sumu, col="red");
  
  plot(model0$theta, model0$sump, col="blue", main="Theta vs. Scores(1).", xlab=NULL, ylab=NULL); 
  points(model0$theta, model0$sumu, col="red");
  
  # trade off plot of users.
  plot(model$sump, model0$sump, col="blue", main="Tradeoff Between Utility and Privacy",
       xlab="Weighted Utility Rating", ylab="Original Privacy Rating");
}

# model fitting analysis, by using cross-validation log-likelihood.(CVLL)
ltm.gof <- function (model, model0) {
  
}

# model consistency analysis, to test if the 
# model is consistent with data from different users. 
ltm.consistency <- function (dat1, dat2) {
  model1 <- ltm.model(dat1)
  model2 <- ltm.model(dat2)
  consist <- anova(model1, model2, B=1000, verbose=TRUE, seed=NULL)
  return (consist)
}

# weighted model fitting. 
# TODO. 
ltm.wgted <- function (data, wgt) {
  # wgt <- spos.ltm.dscrmn; 
  # change the weight to reflect the importance. 
  spos.ltm.score2 <- data.matrix(spos) %*% wgt
  data1 <- data.matrix(sneg) %*% wgt;
  model <- ltm.model(data1)
  return (model)
}

################
# Do the work. #
################
model <- ltm.model(data)
model0 <- ltm.model(data0)
ltm.hist(model, model0)
ltm.scores(model, model0)
