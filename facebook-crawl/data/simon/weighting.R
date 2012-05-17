# This is the weighting script for the IRT model.

# generate random samples while keeping the distribution unchanged. 
# size is an integer for the number of random sample .
# and probvector is a vector for the probability that item was set to 1. 
# data is the original dataset. 
# wgt is the weight to add, it determines how many random samples to generate.
# idx is the index of profile items to add to.
ltm.wgt.sample <- function (data, wgt, idx=NULL, probs=NULL) {
  res         <- NULL;
  
  if (!is.null(wgt)) {
    
  }
  res$pvector <- colMeans(data); 
  res$size    <- as.integer(wgt * nrow(data));
  res$priv.samples <- as.data.frame(matrix(nrow=res$size, ncol=ncol(data)));
  res$util.samples <- as.data.frame(matrix(nrow=res$size, ncol=ncol(data)));
  colnames(res$priv.samples) <- colnames(data)
  colnames(res$util.samples) <- colnames(data)

  # generate util data first, and keep the distribution not changed. 
  for (i in 1:ncol(data)) {
    sample.data <- runif(res$size);
    res$priv.samples[,i] <- as.numeric(sample.data < res$pvector[i]); 
  }
  
  res$util.samples <- rbind(data, 1 - res$priv.samples);

  # emphsize on the privacy. 
  if(!is.null(idx)) {
    if(!is.null(probs)) {
      res$pvector[idx] <- probs; 
    } else {
      res$pvector[idx] <- 0.01;
    }
    for (i in idx) {
      sample.data <- runif(res$size);
      res$priv.samples[,i] <- as.numeric(sample.data < 0.05);
    }
  }
  res$priv.samples <- rbind(data, res$priv.samples);

  return(res);
}