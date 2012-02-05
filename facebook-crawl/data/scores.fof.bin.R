# script to compute score of facebook privacy settings. 
# for comparison purposes, i will use both positive and negative
# settings. 
require(ltm);

####################
# Read in files.   #
####################
# Read in data from files. 
fofpos <- read.csv('simon.fof.pos.csv');
fofneg <- read.csv('simon.fof.neg.csv');

# remove the error data. 
# fofpos <- fofpos[,2:27]; 
# fofneg <- fofneg[,2:27]; 

####################
# buiding models.  #
####################
# rasch model, with discriminate parameter setting as one. 
fofpos.rasch <- rasch(fofpos, constraint = cbind(ncol(fofpos) + 1, 1)); 
fofneg.rasch <- rasch(fofneg, constraint = cbind(ncol(fofneg) + 1, 1)); 

# ltm model, using one latent variable z1. 
# fofpos.ltm <- ltm(fofpos ~ z1); 
# fofneg.ltm <- ltm(fofneg ~ z1); 

# plot of ICC curve. 
# plot(fofpos.ltm, main="ICC for Friends of Friends Binary Data.", 
#      xlab="Perceived Benefit Level", 
#      ylab="Visible Probability"); 
# savePlot("icc.fof.bin.pos"); 

####################
# get parameters.  #
####################
# rasch parameters.
fofpos.rasch.params <- coef(fofpos.rasch);  
fofpos.rasch.dffclt <- fofpos.rasch.params[,1];
fofpos.rasch.dscrmn <- fofpos.rasch.params[,2]; 

fofneg.rasch.params <- coef(fofneg.rasch);  
fofneg.rasch.dffclt <- fofneg.rasch.params[,1];
fofneg.rasch.dscrmn <- fofneg.rasch.params[,2]; 

# ltm parameters. 
# fofpos.ltm.params <- coef(fofpos.ltm); 
# fofpos.ltm.dffclt <- fofpos.ltm.params[,1]; 
# fofpos.ltm.dscrmn <- fofpos.ltm.params[,2]; 

# fofneg.ltm.params <- coef(fofneg.ltm); 
# fofneg.ltm.dffclt <- fofneg.ltm.params[,1]; 
# fofneg.ltm.dscrmn <- fofneg.ltm.params[,2]; 

# select items that are interesting for example [-3,3]; 
# a is the lower bound and b is the upper bound. 
a <- -3; 
b <- 3; 
lower <- fofpos.rasch.dffclt > a; 
upper <- fofpos.rasch.dffclt < b; 

# manually get subset of data. 
fof <- subset(fofpos, select=c(1:13, 15:22, 26)); 
fofr <- rasch(fof, constraint = cbind(ncol(fof) + 1, 1)); 

# plot the graph with item selected data. 

####################
# obtaining scores.# 
####################
# scores given by the model. 
fofpos.rasch.score <- factor.scores(fofpos.rasch, fofpos); 
fofneg.rasch.score <- factor.scores(fofneg.rasch, fofneg); 

# fofpos.ltm.score <- factor.scores(fofpos.ltm, fofpos); 
# fofneg.ltm.score <- factor.scores(fofneg.ltm, fofneg); 

# expected score for each user. 
# plot(spos.ltm.score$score.dat$Exp, 
#      main="Expected Score from IRT Model for Friends of Friends Data", 
#      xlab="User Index", 
#      ylab="Expected Scores");
# savePlot("expscore.fof.bin.pos");

####################
# my own score.    #
####################
# sum of discrim * reprent value.
fofpos.rasch.score1 <- t(t(fofpos)) %*% t(t(fofpos.rasch.dscrmn)); 
# fofpos.ltm.score1 <- t(t(fofpos)) %*% t(t(fofpos.ltm.dscrmn)); 

# fofneg.rasch.score1 <- t(t(fofneg)) %*% t(t(fofneg.rasch.dscrmn)); 
# fofneg.ltm.score1 <- t(t(fofneg)) %*% t(t(fofneg.ltm.dscrmn));

# goodness of fit measures. 
# plot(fofpos.ltm.score1, fofneg.ltm.score1, 
#      xlab="Utility Scores", ylab="Risk Scores", 
#      main="Utility vs. Risk Scores for Friends of Friends Data") #  pch=5, col="blue"); 
# savePlot("util.risk.fof.bin.pos");

grid();
