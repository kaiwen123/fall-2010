# model fitting analysis, by using cross-validation log-likelihood.(CVLL)
ltm.cv <- function (data, folds) {
  print('doing cross validation on the ltm model...')
  n <- nrow(data)
  idx <- sample(n, n)
  data <- data[idx,] # random shuffle of the dataset. 
  part <- as.integer(n / folds) # sample size for each fold. 
  
  models.logLik <- vector()
  test.residus  <- vector()
  
  for(i in 1:folds) {
    cat('Fold', i, '\n')
    out.idx  <- ((i - 1) * part + 1):(i * part)
    out.fold <- data[is.element(idx, out.idx),]
    in.fold  <- data[!is.element(idx, out.idx),]
    
    # model.
    model.fold <- ltm.model(in.fold, out.fold)
    
    # Fitting statistics.
    models.logLik[i] <- model.fold$log.Lik
    test.residus[i]  <- sum(abs(model.fold$residu))
  }
  # mean statistics.
  cv <- NULL
  cv$Log.Lik <- mean(models.logLik)
  cv$residual<- mean(test.residus)
  print(paste('mean log likelihood', cv$Log.Lik))
  print(paste('mean residual', cv$residual))
  print('---> done.')
  return (cv)
}

# model consistency analysis, to test if the 
# model is consistent with data from different users. 
ltm.consistency <- function (dat1, dat2) {
  print('doing anova analysis on two data sets ....')
  model1 <- ltm.model(dat1)
  model2 <- ltm.model(dat2)
  consist <- anova(model1, model2, B=1000, verbose=TRUE, seed=NULL)
  print('---> done.')
  return (consist)
}