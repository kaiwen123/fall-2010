# script to compute score of facebook privacy settings. 
# for comparison purposes, i will use both positive and negative
# settings. 
require(ltm); 

####################
# Read in files.   #
####################
# Read in data from files. 
npos <- read.csv('nobody.fof.pos.csv');
nneg <- read.csv('nobody.fof.neg.csv');

# remove the error data. 
#npos <- npos[,2:27]; 
#nneg <- nneg[,2:27]; 

####################
# buiding models.  #
####################
# rasch model, with discriminate parameter setting as one. 
npos.rasch <- rasch(npos, constraint = cbind(ncol(npos) + 1, 1), IRT.param=TRUE); 
nneg.rasch <- rasch(nneg, constraint = cbind(ncol(nneg) + 1, 1), IRT.param=TRUE); 

# ltm model, using one latent variable z1. 
npos.ltm <- ltm(npos ~ z1, IRT.param=TRUE); 
nneg.ltm <- ltm(nneg ~ z1, IRT.param=TRUE); 

####################
# get parameters.  #
####################
# rasch parameters.
npos.rasch.params <- coef(npos.rasch);  
npos.rasch.dffclt <- npos.rasch.params[,1];
npos.rasch.dscrmn <- npos.rasch.params[,2]; 

nneg.rasch.params <- coef(nneg.rasch);  
nneg.rasch.dffclt <- nneg.rasch.params[,1];
nneg.rasch.dscrmn <- nneg.rasch.params[,2]; 

# ltm parameters. 
npos.ltm.params <- coef(npos.ltm); 
npos.ltm.dffclt <- npos.ltm.params[,1]; 
npos.ltm.dscrmn <- npos.ltm.params[,2]; 

nneg.ltm.params <- coef(nneg.ltm); 
nneg.ltm.dffclt <- nneg.ltm.params[,1]; 
nneg.ltm.dscrmn <- nneg.ltm.params[,2]; 

####################
# obtaining scores.# 
####################
# scores given by the model. 
# npos.rasch.score <- factor.scores(npos.rasch, npos); 
# nneg.rasch.score <- factor.scores(nneg.rasch, nneg); 

# npos.ltm.score <- factor.scores(npos.ltm, npos); 
# nneg.ltm.score <- factor.scores(nneg.ltm, nneg); 

####################
# my own score.    #
####################
# sum of discrim * reprent value.
npos.rasch.score1 <- t(t(npos)) %*% t(t(npos.rasch.dscrmn)); 
npos.ltm.score1 <- t(t(npos)) %*% t(t(npos.ltm.dscrmn)); 

nneg.rasch.score1 <- t(t(nneg)) %*% t(t(nneg.rasch.dscrmn)); 
nneg.ltm.score1 <- t(t(nneg)) %*% t(t(nneg.ltm.dscrmn));

plot(npos.ltm.score1, nneg.ltm.score1, 
     xlab="Utility Scores", ylab="Risk Scores", 
     main="Utility vs. Risk Scores for Friends of Friends Data With
     nobody Account"); 

grid();
