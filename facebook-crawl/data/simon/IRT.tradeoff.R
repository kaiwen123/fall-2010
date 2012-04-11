# weighted model fitting. 
ltm.wgted <- function (data, wgt) {
  # wgt <- spos.ltm.dscrmn; 
  # change the weight to reflect the importance. 
  spos.ltm.score2 <- data.matrix(spos) %*% wgt
  data1 <- data.matrix(sneg) %*% wgt;
  model <- ltm.model(data1)
  return (model)
}

# trade off between utility and privacy. 
ltm.tradeoff.plot <- function(model0, model1) {
  # tradeoff plot. 
  print('ploting trade off graph for privacy and utility settings.')
  plot(model0$sumu, model1$sumu, col="blue", main="Trade off between utility and privacy.",
       pch=1, xlab="", ylab=""); 
  points(model0$sump, model1$sump, col="red", pch=4, xlab="", ylab=""); 
}