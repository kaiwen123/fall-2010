# Author: Simon Guo.
# This script is to use the IRT model to analyse the dataset.
# we are using the binary IRT model ltm for the analysis, so 
# the input data of this script should have binary response. 
#
library(ltm)

###########################
# IRT modeling functions. #
###########################
ltm.model <- function (data, tdata=NULL) {
  if (is.null(tdata)) {
    tdata <- data
  }
  # model.rasch <- rasch(data, constraint = cbind(ncol(data) + 1, 1))
  model <- ltm(data ~ z1)
  model$dffclt <- coef(model)[,1]
  model$dscrmn <- coef(model)[,2]

  model$fitted <- fitted(model, tdata, type="conditional-probabilities")
  model$scores <- factor.scores(model, tdata)          # factor scores. 
  model$theta  <- model$scores$score.dat$z1           # ability level / theta.
  model$sump   <- apply(model$fitted, 1, sum)          # summed probabilities. 
  model$sumu   <- apply(tdata, 1, sum)                 # summed binary settings.
  model$residu <- residuals(model, tdata, order=FALSE) # residuals.
  model$residu <- model$residu[,"Resid"]
  model$info   <- information(model, c(-3, 3))         # information in range.
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
# function to draw the figure for utility and privacy.
ltm.scores <- function (model, model0) {
  plot(model$theta, model$sump, col="blue", main="Theta vs. Scores(0).", xlab=NULL, ylab=NULL); 
  points(model$theta, model$sumu, col="red");
  
  plot(model0$theta, model0$sump, col="blue", main="Theta vs. Scores(1).", xlab=NULL, ylab=NULL); 
  points(model0$theta, model0$sumu, col="red");
  
  # trade off plot of users.
  plot(model$sump, model0$sump, col="blue", main="Tradeoff Between Utility and Privacy",
       xlab="Weighted Utility Rating", ylab="Original Privacy Rating");
}