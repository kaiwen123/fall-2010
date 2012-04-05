# script to compute score of facebook privacy settings. 
# for comparison purposes, i will use both positive and negative
# settings. 
# This file computes the visibility settings for friends data. 
require(ltm); 

####################
# Read in files.   #
####################
# Read in data from files. 
fpos <- read.csv('f.vis.pos.csv');
fneg <- read.csv('f.vis.neg.csv');

# remove the error data. 
#fpos <- fpos[,2:27]; 
#fneg <- fneg[,2:27]; 

####################
# buiding models.  #
####################
# The Grade response model. 
fpos.grm <- grm(fpos); 
fneg.grm <- grm(fneg); 

# The generalized partial credit model. 
#fpos.gpcm <- gpcm(fpos); 
#fneg.gpcm <- gpcm(fneg);

####################
# get parameters.  #
####################
# grm parameters.
fpos.grm.params <- coef(fpos.grm);  
fpos.grm.extrmt1 <- fpos.grm.params[,1];
fpos.grm.extrmt2 <- fpos.grm.params[,2];
fpos.grm.dscrmn <- fpos.grm.params[,3]; 

fneg.grm.params <- coef(fneg.grm);  
fneg.grm.extrmt1 <- fneg.grm.params[,1];
fneg.grm.extrmt2 <- fneg.grm.params[,2];
fneg.grm.dscrmn <- fneg.grm.params[,3];
# gpcm parameters.
#fneg.gpcm.params <- coef(fneg.gpcm);  
#fneg.gpcm.dffclt <- fneg.gpcm.params[,1];
#fneg.gpcm.dscrmn <- fneg.gpcm.params[,2]; 

####################
# obtaining scores.# 
####################
# scores given by the model. 
# fpos.rasch.score <- factor.scores(fpos.rasch, fpos); 
# fneg.rasch.score <- factor.scores(fneg.rasch, fneg); 

# fpos.ltm.score <- factor.scores(fpos.ltm, fpos); 
# fneg.ltm.score <- factor.scores(fneg.ltm, fneg); 

####################
# my own score.    #
####################
# sum of discrim * reprent value.
#fpos.rasch.score1 <- t(t(fpos)) %*% t(t(fpos.rasch.dscrmn)); 
fpos.grm.score1 <- data.matrix(fpos) %*% data.matrix(fpos.grm.dscrmn); 

#fneg.rasch.score1 <- t(t(fneg)) %*% t(t(fneg.rasch.dscrmn)); 
fneg.grm.score1 <- data.matrix(fneg) %*% data.matrix(fneg.grm.dscrmn);

plot(fpos.grm.score1, fneg.grm.score1, 
     xlab="Utility Scores", ylab="Risk Scores", 
     main="Utility vs. Risk Scores for Friends Data");

grid();
