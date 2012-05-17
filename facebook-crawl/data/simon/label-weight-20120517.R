# load the dataset. 
source('explore.statistics.R')
source('IRT.statistics.R')
source('IRT.tradeoff.R')
source('IRT.validation.R')
source('weighting.R')
library(ltm)

# load data set.
print('Loading data......')
data <- read.csv('simon.combine.fof.priv-bin-setting.csv', header=TRUE, sep=",")

# create labeled weighted IRT model. 
wgt <- t(as.data.frame(rep(1, 27)))
colnames(wgt) <- colnames(data)
wgt <- as.data.frame(wgt) # <--- change back to data frame. 
wgt[c("employers", "phone", "email")] <- 1.5
wgt <- as.numeric(wgt)

# build the weighted model. 
print('Builing utility model and privacy model......')
model0 <- ltm.model(data = data)
model1 <- ltm.model(data = 1-data, weight = wgt)

# draw the tradeoff graph. 
ltm.tradeoff.plot(model0, model1)

# use different color for users who have set items 'employers', 'phone' and 'email' to be visible. 
