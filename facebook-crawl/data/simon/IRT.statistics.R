# Author: Simon Guo.
# This script is to use the IRT model to analyse the dataset.
# we are using the binary IRT model ltm for the analysis, so 
# the input data of this script should have binary response. 
#
###########################
# IRT modeling functions. #
###########################
ltm.model <- function (data, tdata=NULL, weight = NULL) {
  require('ltm')
  
  print('Building 2-parametric IRT models.')
  if (is.null(tdata)) {
    tdata <- data
  }
  
  if (is.null(weight)) {
    weight <- rep(1, 27)
  }
  
  # model.rasch <- rasch(data, constraint = cbind(ncol(data) + 1, 1))
  model <- ltm(data ~ z1, weight = weight)
  
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
  print('---> done')
  
  return(model)
}

# histgram of the hidden variables.
ltm.hist.plot <- function (model0, model1) {
  print('Plotting histgram of the irt models...')
  hist(model0$theta, 30, freq=FALSE, main="Histgram of theta for privacy data(0).", 
       xlab=NULL, ylab=NULL); 
  lines(density(model0$theta), col="red")
  rug(jitter(model0$theta), col="brown")
  
  hist(model1$theta, 30, freq=FALSE, main="Histgram of theta for privacy data(1).", 
       xlab=NULL, ylab=NULL); 
  lines(density(model1$theta), col="red")
  rug(jitter(model1$theta), col="brown")
  print('---> done.')
}

# estimated theta vs. estimated score and expected scores.
# function to draw the figure for utility and privacy.
ltm.scores.plot <- function (model0, model1) {
  print('plotting scores for the IRT models...')
  plot(model0$theta, model0$sump, col="blue", main="Theta vs. Scores(0).", xlab="", ylab="", pch=1); 
  points(model0$theta, model0$sumu, col="red", xlab="", ylab="", pch=4);
  
  plot(model1$theta, model1$sump, col="blue", main="Theta vs. Scores(1).", xlab="", ylab="", pch=1); 
  points(model1$theta, model1$sumu, col="red", xlab="", ylab="", pch=4);
  print('---> done.')
}