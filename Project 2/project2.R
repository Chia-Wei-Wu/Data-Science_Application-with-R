#Connected CMD
arg <- commandArgs(trailingOnly=TRUE)
if (length(arg)==0) {
  stop("missing flag", call.=FALSE)
  stop("USAGE: Rscript hw2_yourID.R --target bad/good --badthre <threshold> --input meth1 meth2 ... methx --output result.csv", call.=FALSE)
}

#count the number of files
find_file <- function(argarg)
{
  count <- 0
  m <- 1
  while(m < length(argarg))
  {
    if(argarg[i+1] == "--output" | argarg[i+1] == "--target" | argarg[i+1] == "--badthre" | count+7 == length(argarg)){
      return(count)
    }else{
      count <- count + 1
      m <- m+1
      i <- i+1
    }
  }
}

#count sensitivity
sensitivityft <- function(TP,TN,FP,FN)
{
  result_sensitivity <- round(TP/(TP+FN),2)
  #cat(result_sensitivity,"\n")
  return(result_sensitivity)
}

#count specificity
specificityft <- function(TP,TN,FP,FN)
{
  result_specificity <- round(TN/(FP+TN),2)
  return(result_specificity)
}

#count F1
F1ft <- function(TP,TN,FP,FN)
{
  P <- TP/(TP+FP)
  R <- TP/(TP+FN)
  F1 <- round(2*P*R/(P+R),2)
  return(F1)
}

#count log Likelihood
loglikelihoodft <- function(target,data)
{
  y <- 1
  result_log <- 0
  for(y in c(1:length(data$persons))){
    if(data$reference[y] =="bad"){
      p = as.numeric(data$pred.score[y])
      result_log <- result_log + 1*log(p)
    }else{
      p = as.numeric(data$pred.score[y])
      result_log <- result_log + 1*log(1-p)
    }
  }
  #cat(result_log,"\n")
  result_log <- round(result_log,2)
  return(result_log)
}

#count pseudoRsquared
pseudoft <- function(target,loglikelihood,data)
{
  #count_null_MODEL_probablilty
  e2 <- 1
  counte <- 0
  p <- 0
  for(e2 in c(1:length(data$persons))){
    if(data$reference[e2] ==target){
      counte <- counte+1
    }
  }
  p <- counte/length(data$persons)
  #cat(p)
  
  e1 <- 1
  null_log <- 0
  for(e1 in c(1:length(data$persons))){
    if(data$reference[e1]==target){
      null_log <- null_log + 1*log(p)
    }else{
      null_log <- null_log + 1*log(1-p)
    }
  }
  #cat(null_log,"\n")
  result_pse <- 1-(loglikelihood/null_log)
  result_pse <- round(result_pse,2)
  #cat(result_pse,"\n")
  return(result_pse)
}

#main
i3 <- 1
while(i3<length(arg))
{
  if(arg[i3] == "--target"){
    target <- arg[i3+1]
  }
  i3 <- i3+1
}
#cat("Successful to get target:",target,'\n')

i2 <- 1
while(i2<length(arg)){
  if(arg[i2] == '--badthre'){
    badthre <- arg[i2+1]
  }
  i2 <- i2+1
}
#cat('Successful to get badthre:',badthre,'\n')

i <- 1
count_file <- 0
while(i<length(arg)){
  if(arg[i]=="--input"){
    tryCatch({
      count_file <- find_file(arg)
      #cat(count_file)
      #cat("Having",count_file,"method(s) to compare",'\n')
      if(count_file==0){
        }else{
          
          k <- 1
          pred_result <- list()
          for(k in c(1:count_file)){
            data <- read.csv(arg[i+k],header=T,sep = ",")
            n <- 1
            method_len <- length(data$persons)
            for(n in c(1:method_len)){
              if(data$pred.score[n] >= badthre){
                pred_result <- append(pred_result,"bad")
              }else{
                pred_result <- append(pred_result,"good")
              }
            }
            
            data$pred_result <- pred_result
      
            #count confusion matrix
            p <- 1
            True_positive <- 0
            True_negative <- 0
            False_positive <- 0
            False_negative <- 0
            for(p in c(1:length(pred_result))){ 
              if(target=="good"){
                if(data$reference[p] == "good" & data$pred_result[p] == "good"){
                  True_positive <- True_positive + 1
                }else if(data$reference[p] == "good" & data$pred_result[p] == "bad"){
                  False_negative <- False_negative + 1
                }else if(data$reference[p] == "bad" & data$pred_result[p] == "good"){
                  False_positive <- False_positive + 1
                }else if(data$reference[p] == "bad" & data$pred_result[p] == "bad"){
                  True_negative <- True_negative + 1
                }
              }else if(target=="bad"){
                if(data$reference[p] == "bad" & data$pred_result[p] == "bad"){
                  True_positive <- True_positive + 1
                }else if(data$reference[p] == "bad" & data$pred_result[p] == "good"){
                  False_negative <- False_negative + 1
                }else if(data$reference[p] == "good" & data$pred_result[p] == "bad"){
                  False_positive <- False_positive + 1
                }else if(data$reference[p] == "good" & data$pred_result[p] == "good"){
                  True_negative <- True_negative + 1
                }
              }
            }
            
            
            #cat(True_positive,False_negative,False_positive,True_negative,"\n")
            #Compute the result for data
            sensitivity <- sensitivityft(True_positive,True_negative,False_positive,False_negative)
            specificity <- specificityft(True_positive,True_negative,False_positive,False_negative)
            F1 <- F1ft(True_positive,True_negative,False_positive,False_negative)
            logLikelihood <- loglikelihoodft(target,data) 
            pseudoRsquared <- pseudoft(target,logLikelihood,data)
      
            rm(data) #clean data
            rm(pred_result)#clean pred_result
            
      
            #deal with output
            name <- arg[i+k]
            #cat(name)
            m <- regmatches(name, regexpr("/(.*?).csv", name))
            set <- gsub("^/|.csv", "", basename(m))
            method <- c(set)
            sensitivity <- c(sensitivity)
            specificity <- c(specificity)
            F1 <- c(F1)
            logLikelihood <- c(logLikelihood)
            pseudoRsquared <- c(pseudoRsquared)
            
      
            if(k!=1){
              temp <- data.frame(method,sensitivity,specificity,F1,logLikelihood,pseudoRsquared)
              answer <- rbind(answer,temp)
            }else{
              answer <- data.frame(method,sensitivity,specificity,F1,logLikelihood,pseudoRsquared)
            }
            pred_result <- list()
          }
          
          #cat("Successful to compute the sensitivity,specificity,F1,logLikelihood and pseudoRsquared for each method","\n")
    
          #compute Best
          best_sensitivity <- max(answer[,2])
          g2 <- 1
          #cat(best_sensitivity)
          for(g2 in c(1:length(answer[,1]))){
            if(best_sensitivity == answer[g2,2]){
              sensitivity <- answer[g2,1]
              break
            }
          }
      
          best_specificity <- max(answer[,3])
          g3 <- 1
          for(g3 in c(1:length(answer[,1]))){
            if(best_specificity == answer[g3,3]){
            specificity <- answer[g3,1]
            break
            }
          }
    
          best_F1 <- max(answer[,4])
          g4 <- 1
          for(g4 in c(1:length(answer[,1]))){
            if(best_F1 == answer[g4,4]){
              F1 <- answer[g4,1]
              break
            }
          }
    
          best_logLikelihood <- max(answer[,5])
          g5 <- 1
          for(g5 in c(1:length(answer[,1]))){
            if(best_logLikelihood == answer[g5,5]){
              logLikelihood <- answer[g5,1]
              break
            }
          }
    
          best_pseudoRsquared <- max(answer[,6])
          g6 <- 1
          for(g6 in c(1:length(answer[,1]))){
            if(best_pseudoRsquared == answer[g6,6]){
              pseudoRsquared <- answer[g6,1]
              break
            }
          }
    
          method <- "best"
          final_row <- data.frame(method,sensitivity,specificity,F1,logLikelihood,pseudoRsquared)
          answer <- rbind(answer,final_row)
          
        }
        },error = function(e){
          stop("missing flag", call.=FALSE)
          break
          })
    i <- i+1
    }else if(arg[i] == "--output"){
    tryCatch({name2 <- arg[i+1]
      },error = function(e){stop("missing flag", call.=FALSE)
      }
    )
    i <- i+1
  }
  i <- i+1
}

#detect missing value
status <- 7 + count_file
if(status!=length(arg)){
  stop("missing flag", call.=FALSE)
}else{
  tryCatch({
    write.table(answer,file=name2,sep=",",row.names=F,quote = FALSE)
  },error = function(e){stop("missing flag", call.=FALSE)
  }
  )
}
