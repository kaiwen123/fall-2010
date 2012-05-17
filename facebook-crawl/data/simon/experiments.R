# function files. 
source('explore.statistics.R')
source('IRT.statistics.R')
source('IRT.tradeoff.R')
source('IRT.validation.R')
source('weighting.R')
#install.packages("ltm")
library(ltm)

# arguments. 
args <- commandArgs()

################
# Do the work. #
################
# How to use it: 
# Rscript [this-file].R args
#
args <- commandArgs()

print('input arguments:')
print(args)

# file name for the multi level data, in csv format.
# print(paste('loading multiple level from data file', args[7]));
# data.multi <- read.csv(args[7], header=TRUE, sep=',')

# basic analysis for multi-level data set.
# print(paste('doing basic statistical analysis with data from file', args[7]));
# basic <- basic.stat(args[7]);
# 
# # build model. 
print(paste('loading binary data from file', args[6]));
data <- read.csv(args[6], header=TRUE, sep=",")
# 
# print('builing model 0.')
# model0 <- ltm.model(data)
# plot(model0, main="ICC of model(0)", xlab="", ylab="")
# 
# print('building model 1.')
# model1 <- ltm.model(1-data)
# plot(model1, main="ICC of model(1)", xlab="", ylab="")
# 
# # 
# # plot the IRT model. 
# ltm.hist.plot(model0, model1)
# ltm.scores.plot(model0, model1)
#  
# # tradeoff between utility and privacy. 
# ltm.tradeoff.plot(model0, model1)
# 
# # validation of the model, including cross validation for one data set. 
# # and anova validation on the consistency of the model.
# ltm.cv.res <- ltm.cv(data, 10)

# if(!is.null(args[8])) {
#   data0 <- data
#   data1 <- read.csv(args[8])
#   ltm.anova.res <- ltm.consistency(data0, data1)
# } else {
#   print('skipping the anova analysis, because there is no second dataset to compare with.')
# }

############################################
# building weighted model.
# generate random sample, and then combine the random sample with orginal data for the modeling. 
if(!is.null(args[8])) {
  wgt.samples <- ltm.wgt.sample(data, as.double(args[8]), idx=c(13,15,17), probs=c(0.2, 0.8, 0.9));
  wgt.model0 <- ltm.model(wgt.samples$priv.samples, data); 
  wgt.model1 <- ltm.model(wgt.samples$util.samples, 1-data);
  
  ltm.hist.plot(wgt.model0, wgt.model1)
  ltm.scores.plot(wgt.model0, wgt.model1)
  # tradeoff between utility and privacy. 
  ltm.tradeoff.plot(wgt.model0, wgt.model1)
} else {
  print('skipping the weighting analysis, because there is no weight provided.')
}
