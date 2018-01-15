require(stringi)
require(dplyr)

read_eurostat <- function(filename){
  data1 <- read.table(gzfile(filename), sep='\t', header=T, na.strings=c(": ",": u") ) 
  data2 <- matrix(unlist(stri_split(data1[,1], regex=",")), ncol=4, byrow=T)
  data2 <- as.data.frame(data2)
  names(data2)=c("indic_is","unit","sizen_r2","country")
  output <- data.frame(data2, data1[,-1])
  return(output)
}
