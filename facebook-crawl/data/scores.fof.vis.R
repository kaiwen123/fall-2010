# script to compute score of facebook privacy settings. 
# for comparison purposes, i will use both positive and negative
# settings. 
# This file computes the visibility settings for friends data. 
require(ltm); 

####################
# Read in files.   #
####################
# Read in data from files. 
fofpos <- read.csv('fof.vis.pos.csv');
fofneg <- read.csv('fof.vis.neg.csv');

# remove the error data. 
#fofpos <- fofpos[,2:27]; 
#fofneg <- fofneg[,2:27]; 

####################
# buiding models.  #
####################
# The Grade response model. 
fofpos.grm <- grm(fofpos); 
fofneg.grm <- grm(fofneg); 

# The generalized partial credit model. 
#fofpos.gpcm <- gpcm(fofpos); 
#fofneg.gpcm <- gpcm(fofneg);

####################
# get parameters.  #
####################
# grm parameters.
fofpos.grm.params <- coef(fofpos.grm);  
fofpos.grm.extrmt1 <- fofpos.grm.params[,1];
fofpos.grm.extrmt2 <- fofpos.grm.params[,2];
fofpos.grm.dscrmn <- fofpos.grm.params[,3]; 

fofneg.grm.params <- coef(fofneg.grm);  
fofneg.grm.extrmt1 <- fofneg.grm.params[,1];
fofneg.grm.extrmt2 <- fofneg.grm.params[,2];
fofneg.grm.dscrmn <- fofneg.grm.params[,3];
# gpcm parameters.
#fofneg.gpcm.params <- coef(fofneg.gpcm);  
#fofneg.gpcm.dffclt <- fofneg.gpcm.params[,1];
#fofneg.gpcm.dscrmn <- fofneg.gpcm.params[,2]; 

####################
# obtaining scores.# 
####################
# scores given by the model. 
# fofpos.rasch.score <- factor.scores(fofpos.rasch, fofpos); 
# fofneg.rasch.score <- factor.scores(fofneg.rasch, fofneg); 

# fofpos.ltm.score <- factor.scores(fofpos.ltm, fofpos); 
# fofneg.ltm.score <- factor.scores(fofneg.ltm, fofneg); 

####################
# my own score.    #
####################
# sum of discrim * reprent value.
#fofpos.rasch.score1 <- t(t(fofpos)) %*% t(t(fofpos.rasch.dscrmn)); 
fofpos.grm.score1 <- data.matrix(fofpos) %*% data.matrix(fofpos.grm.dscrmn); 

#fofneg.rasch.score1 <- t(t(fofneg)) %*% t(t(fofneg.rasch.dscrmn)); 
fofneg.grm.score1 <- data.matrix(fofneg) %*% data.matrix(fofneg.grm.dscrmn);

# personalized scores. 
wgt <- fofpos.grm.dscrmn; 
wgt[24,] <- 100; 
wgt[25,] <- 100; 
wgt[26,] <- 100;
fofpos.grm.score2 <- data.matrix(fofpos) %*% data.matrix(wgt); 
fofneg.grm.score2 <- data.matrix(fofneg) %*% data.matrix(wgt);

plot(fofpos.grm.score1, fofneg.grm.score1, 
     xlab="Utility Scores", ylab="Risk Scores", 
     main="Discriminative Utility vs. Risk Scores for Friends of Friends Data");
savePlot("discrmnt.util.risk.vis"); 

plot(fofpos.grm.score2, fofneg.grm.score2, 
     xlab="Utility Scores", ylab="Risk Scores", 
     main="Personalized Discriminative Utility vs. Risk Scores for Friends of Friends Data");
savePlot("personalized.util.risk.vis"); 


grid();
