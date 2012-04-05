# script to compute score of facebook privacy settings. 
# for comparison purposes, i will use both positive and negative
# settings. 
####################
# Read in files.   #
####################
# Read in data from files. 
spos <- read.csv('simon.f.pos.csv');
sneg <- read.csv('simon.f.neg.csv');

# remove the error data. 
spos <- spos[,2:27]; 
sneg <- sneg[,2:27]; 

####################
# buiding models.  #
####################
# rasch model, with discriminate parameter setting as one. 
spos.rasch <- rasch(spos, constraint = cbind(ncol(spos) + 1, 1)); 
sneg.rasch <- rasch(sneg, constraint = cbind(ncol(sneg) + 1, 1)); 

# ltm model, using one latent variable z1. 
spos.ltm <- ltm(spos ~ z1); 
sneg.ltm <- ltm(sneg ~ z1); 

####################
# get parameters.  #
####################
# rasch parameters.
spos.rasch.params <- coef(spos.rasch);  
spos.rasch.dffclt <- spos.rasch.params[,1];
spos.rasch.dscrmn <- spos.rasch.params[,2]; 

sneg.rasch.params <- coef(sneg.rasch);  
sneg.rasch.dffclt <- sneg.rasch.params[,1];
sneg.rasch.dscrmn <- sneg.rasch.params[,2]; 

# ltm parameters. 
spos.ltm.params <- coef(spos.ltm); 
spos.ltm.dffclt <- spos.ltm.params[,1]; 
spos.ltm.dscrmn <- spos.ltm.params[,2]; 

sneg.ltm.params <- coef(sneg.ltm); 
sneg.ltm.dffclt <- sneg.ltm.params[,1]; 
sneg.ltm.dscrmn <- sneg.ltm.params[,2]; 

####################
# obtaining scores.# 
####################
# scores given by the model. 
# spos.rasch.score <- factor.scores(spos.rasch, spos); 
# sneg.rasch.score <- factor.scores(sneg.rasch, sneg); 

# spos.ltm.score <- factor.scores(spos.ltm, spos); 
# sneg.ltm.score <- factor.scores(sneg.ltm, sneg); 

####################
# my own score.    #
####################
# sum of discrim * reprent value.
spos.rasch.score1 <- t(t(spos)) %*% t(t(spos.rasch.dscrmn)); 
spos.ltm.score1 <- t(t(spos)) %*% t(t(spos.ltm.dscrmn)); 

sneg.rasch.score1 <- t(t(sneg)) %*% t(t(sneg.rasch.dscrmn)); 
sneg.ltm.score1 <- t(t(sneg)) %*% t(t(sneg.ltm.dscrmn));

plot(spos.ltm.score1, sneg.ltm.score1); 

grid
