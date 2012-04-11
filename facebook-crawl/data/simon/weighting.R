# This is the weighting script for the IRT model.

# generate random samples while keeping the distribution unchanged. 
# size is an integer for the number of random sample .
# and probvector is a vector for the probability that item was set to 1. 
# data is the original dataset. 
# wgt is the weight to add. 
# idx is the index of profile items to add to.
ltm.wgt.sample <- function (data, wgt, idx=NULL) {
  res         <- NULL;
  res$size    <- as.integer(wgt * nrow(data));
  res$pvector <- colMeans(data); 
  res$samples <- as.data.frame(matrix(nrow=res$size, ncol=ncol(data)));
  colnames(res$samples) <- colnames(data)
  
  for (i in 1:ncol(data)) {
    sample.data <- runif(res$size);
    res$samples[,i] <- as.numeric(sample.data < res$pvector[i]); 
  }
  res$samples <- rbind(data, res$samples);
  return(res);
}