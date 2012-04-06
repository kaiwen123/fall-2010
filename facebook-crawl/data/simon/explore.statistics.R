# This is the basic statistical analysis of the data. 
# How to use it: 
# Rscript [this-file].R args

args <- commandArgs()

print(args)

data1 <- read.csv(args[6], head=TRUE, sep=",")
# data2 <- read.csv(args[8], head=TRUE, sep=",")

# read in  the default settings. 
default.setting <- read.csv('../settings.default.txt')

default.setting <- matrix(default.setting)

default.setting.result <- default.setting != data1

########################
# Test profile changes #
########################
# change values from TRUE, FALSE to 0 and 1. 
for (i in nrow(default.setting.result)) {
  for (j in ncol(default.setting.result)) {
    if(default.setting.result[i,j] == TRUE) {
      default.setting.result[i,j] <- 1      # does not change settings. 
    } else {
      default.setting.result[i,j] <- 0 # changed settings.
    }
  }
}

# change percentage of profile items. 
prop.profile.change <- colMeans(default.setting.result)
op <- par(mar=c(12,4,4,2)) # set the four side margins. 
barplot(sort(prop.profile.change, decreasing=TRUE), main="Percentage of Profile Item Changes", 
        xlab=NULL, ylab="Percentage of change", las=2, col="skyblue")
rm(op)

# change of items for each user. 
cnt.user.profile.change <- rowSums(default.setting.result)
hist(cnt.user.profile.change, freq=FALSE, main="Histgram of Users' profile item change",
     xlab=NULL, ylab=NULL)
lines(density(cnt.user.profile.change), col="red")
rug(jitter(cnt.user.profile.change), col="brown")

#####################
# Other properties. #
#####################
# heat map.
# heatmap(data.matrix(data),col=topo.colors(3));

# visible setting for each item. 
plot(colMeans(data), col="lightblue", main="Visible Probability that Profile Item.", 
     xlab=NULL, ylab=NULL, las=2); 
points(colMeans(data), col="red");