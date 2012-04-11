# This is the basic statistical analysis of the data. 
# the input data should be the data with multiple levels.

basic.stat <- function (datafile) {
  #
  # read in  the default settings from file. 
  print('loading default settings of profile items from file settings.default.txts');
  default.file <- '/home/ada/research/tradeoff/data/settings.default.txt';
  if (file.exists(default.file)) {
    default.settings <- as.data.frame(read.csv(default.file));
  } else {
    print('default setting file does not exists, please change location.')
    return(NULL);
  }

  # read in data from given file. 
  print(paste('loading multiple level data from file', datafile));
  if(file.exists(datafile)) {
    data <- read.csv(datafile, header=TRUE, sep=','); 
  } else {
    print('Given data file does not exists, please try again.')
    return (NULL);
  }
  
  print('generating basic statistics....');
  basic <- NULL;
  rowcnt <- nrow(data);
  colcnt <- ncol(data);
  basic$is.default <- data; #matrix(nrow=rowcnt, ncol=colcnt);
  ########################
  # Test profile changes #
  ########################
  #basic$is.default <- matrix(as.numeric(data != default.settings)); 
  for (i in 1:nrow(data)) {
    basic$is.default[i,] <- as.numeric(data[i,] != default.settings);
  }
  
  # percentage of people who changed default settings for each item. 
  # ATTENTION: change means different from the default setting, 
  # so, it can either be changing from visible to invisible; 
  # or, it can be changing from invisible to visible.
  basic$change.prob <- colMeans(basic$is.default);
  op <- par(mar=c(12,4,4,2)); # set margin size for the picture.
  barplot(sort(basic$change.prob, decreasing=TRUE), main="Percentage of Profile Item Changes", 
          xlab=NULL, ylab=NULL, las=2, col="skyblue");
  
  # change of items for each user. 
  basic$user.change.cnt <- rowSums(basic$is.default);
  hist(basic$user.change.cnt, freq=FALSE, main="Histgram of Users' profile item change",
       xlab=NULL, ylab=NULL, col="skyblue");
  lines(density(basic$user.change.cnt), col="red");
  rug(jitter(basic$user.change.cnt), col="brown");
  
  #############################################
  # test of items set to friends only visible. 
  basic$friends.only <- data; 
  for (i in 1:nrow(data)) {
    basic$friends.only[i,] <- as.numeric(data[i,] >= rep(0, ncol(data)));
  }
  basic$friends.only.prob <- colMeans(basic$friends.only); 
  barplot(basic$friends.only.prob, main="Percentage only visible to friends or user only.", 
          xlab=NULL, ylab=NULL, las=2, col="skyblue");
  
  ######
  # test of items set to friends of friends only visible.
  basic$fof.only <- data; 
  for (i in 1:nrow(data)) {
    basic$fof.only[i,] <- as.numeric(data[i,] >= rep(1, ncol(data)));
  }
  basic$fof.only.prob <- colMeans(basic$fof.only);
  barplot(basic$fof.only.prob, main="Percentage only visible to friends of friends.", 
          xlab=NULL, ylab=NULL, las=2, col="skyblue");
  
  ######
  # test of items set to everybody visible. 
  basic$everyone <- data; 
  for (i in 1:nrow(data)) {
    basic$everyone[i,] <- as.numeric(data[i,] == rep(2, ncol(data)));
  }
  basic$everyone.prob <- colMeans(basic$everyone);
  barplot(basic$everyone.prob, main="Percentage of items visible to everyone", 
          xlab=NULL, ylab=NULL, las=2, col="skyblue");
  
  return(basic);
}