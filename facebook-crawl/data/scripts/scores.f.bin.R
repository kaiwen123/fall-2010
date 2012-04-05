# script to compute score of facebook privacy settings. 
# for comparison purposes, i will use both positive and negative
# settings. 
require(ltm); 

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
# spos.rasch <- rasch(spos, constraint = cbind(ncol(spos) + 1, 1), IRT.param=TRUE); 
# sneg.rasch <- rasch(sneg, constraint = cbind(ncol(sneg) + 1, 1), IRT.param=TRUE); 

# ltm model, using one latent variable z1. 
spos.ltm <- ltm(spos ~ z1, IRT.param=TRUE); 
sneg.ltm <- ltm(sneg ~ z1, IRT.param=TRUE); 

# plot of ICC curve. 
plot(spos.ltm, main="ICC for Friends Binary Data.", 
     xlab="Perceived Benefit Level", 
     ylab="Visible Probability"); 
savePlot("icc.f.bin.pos"); 

####################
# get parameters.  #
####################
# rasch parameters.
# spos.rasch.params <- coef(spos.rasch);  
# spos.rasch.dffclt <- spos.rasch.params[,1];
# spos.rasch.dscrmn <- spos.rasch.params[,2]; 

# sneg.rasch.params <- coef(sneg.rasch);  
# sneg.rasch.dffclt <- sneg.rasch.params[,1];
# sneg.rasch.dscrmn <- sneg.rasch.params[,2]; 

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
#spos.rasch.score <- factor.scores(spos.rasch, spos); 
# sneg.rasch.score <- factor.scores(sneg.rasch, sneg); 

spos.ltm.score <- factor.scores(spos.ltm, spos); 
sneg.ltm.score <- factor.scores(sneg.ltm, sneg); 

# expected score for each user. 
plot(spos.ltm.score$score.dat$Exp, 
     main="Expected Score from IRT Model for Friends Data", 
     xlab="User Index", 
     ylab="Expected Scores");
savePlot("expscore.f.bin.pos");

###################################################
# my own score,which only considers discrimination.
###################################################
# sum of discrim * reprent value.
spos <- data.matrix(spos); 
sneg <- data.matrix(sneg); 

# spos.rasch.dscrmn <- data.matrix(spos.rasch.dscrmn); 
spos.ltm.dscrmn <- data.matrix(spos.ltm.dscrmn); 

# spos.rasch.score1 <- spos %*% spos.rasch.dscrmn; 
spos.ltm.score1 <- spos %*% spos.ltm.dscrmn; 

# sneg.rasch.score1 <- sneg %*% sneg.rasch.dscrmn; 
sneg.ltm.score1 <- sneg %*% sneg.ltm.dscrmn;
# discriminative score for binary data, which only considers the discrimination as weight. 
plot(spos.ltm.score1/max(spos.ltm.score1), 
    main="Normalized Discriminative Score for Friends Data", 
    xlab="User Index", 
    ylab="Normalized Discriminaative Score."); 
savePlot("discmnt.f.score"); 

# user personalized score with given weight. 
wgt <- spos.ltm.dscrmn; 
# change the weight to reflect the importance. 
 
spos.ltm.score2 <- data.matrix(spos) %*% wgt;
wgt[24,] = 100; 
wgt[25,] = 100; 
wgt[26,] = 100;
sneg.ltm.score2 <- data.matrix(sneg) %*% wgt;
plot(spos.ltm.score1/max(spos.ltm.score1), 
    main="Personalized Discriminative Score for Friends Data", 
    xlab="User Index", 
    ylab="Normalized Discriminaative Score."); 
points(sneg.ltm.score2/max(sneg.ltm.score1)); 
savePlot("personalized.discmnt.f.score"); 


# utility vs. risk plot for friends data. 
plot(spos.ltm.score2, sneg.ltm.score2, 
     xlab="Utility Scores", 
     ylab="Risk Scores", 
     main="Utility vs. Risk Scores for Personalized Discrimitive Friends Data");
savePlot("util.risk.f.bin.pos");

###############################
# Adding weight to the model. 
###############################

grid();
