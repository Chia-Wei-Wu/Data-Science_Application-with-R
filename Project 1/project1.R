arg <- commandArgs(trailingOnly=TRUE)

if(length(arg)!=4){
  stop("missing flag", call.=FALSE)
}

i <- 1
while(i<length(arg))
{
  if(arg[i]=='--input'){
  data <- read.csv(arg[i+1],header=T)
  name <- arg[i+1]
  m <- regmatches(name, regexpr("/(.*?).csv", name))
  set <- gsub("^/|.csv", "", basename(m)) 
  max_weight <- round(max(data$weight),2)
  max_height <- round(max(data$height),2)
  set <- c(set)
  weight <- c(max_weight)
  height <- c(max_height)
  result <- data.frame(set,weight,height)
  i <- i+1
  }else if(arg[i]=='--output'){
  name2 <- arg[i+1]
  i <- i+1
  }
  i <- i+1
}

write.csv(result,file=name2,row.names=F,quote = FALSE)