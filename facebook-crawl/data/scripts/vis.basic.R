# This file is used to analyze the basic information. 
require(ltm); 

####################
# Read in files.   #
####################
# Read in data from files. 
spos <- read.csv('f.vis.pos.csv');
fofpos <- read.csv('fof.vis.pos.csv');

# column name.
names.col <- colnames(spos); 

# get the percentage of visible settings. 
spos.mean <- mean(spos); 
fofpos.mean <- mean(fofpos); 

# plot the graph.
#plot(spos.mean, type="p", col="blue"); 
plot(spos.mean, col="red", 
     main="Average Visibility Level for Multi-level Data.", 
     xlab="Profile Items", 
     ylab="Percentage of Visible Items"); 
lines(spos.mean, col="red");

points(fofpos.mean, col="black"); 
lines(fofpos.mean, col="black");

legend(18, 1.0, c("Friends", "Friends of Friends"),lty=1:1, col=c("red","black"));
savePlot('vis.visible'); 

# heat graph of the friends data. 
# This can be used for clustering purposes. 
heatmap(data.matrix(spos),col=topo.colors(3));
savePlot('vis.heat.map.f'); 

heatmap(data.matrix(fofpos[600:700,]), col=topo.colors(3));
savePlot('vis.heat.map.fof'); 

# get the profile item modify data. 
#for
# modify axis.
# axis(1, 1:length(spos.mean), colnames(spos));

grid();
