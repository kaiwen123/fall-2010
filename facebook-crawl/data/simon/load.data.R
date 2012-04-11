# data loader. 
# loading data from given file. -- default is in csv format. 

load.data <- function(file) {
  data <- read.csv(file, header=TRUE, sep=',');
  return(data);
}