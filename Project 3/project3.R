library('rpart')

#Connected CMD
arg <- commandArgs(trailingOnly=TRUE)
if (length(arg)!=6) {
  stop("missing flag", call.=FALSE)
  stop("USAGE: Rscript hw3_studentID.R --fold k --input Archaeal_tfpssm.csv --output performance.csv", call.=FALSE)
}

#find fold
i <- 1
num_fold <- 0
while(i<length(arg)){
  if(arg[i]=="--fold"){
    num_fold <- as.numeric(arg[i+1])
  }
  i <- i + 1
}

if(num_fold == 0){
  stop("fold can not be 0", call.=FALSE)
}

#deal with data
j <- 1
while(j<length(arg)){
  if(arg[j]=="--input"){
    data <- read.csv(arg[j+1],header=FALSE)
    data <- data[sample(nrow(data)),]
    
    #partition
    fold_size <- floor(nrow(data)/num_fold)
    last_fold_size <- nrow(data) - fold_size*(num_fold-1)
    #cat(fold_size,last_fold_size, "\n")
    
    folds <- list()

    for(m in 1:num_fold){
      if(m < num_fold){
        fold_indices <- (1:fold_size) + (m - 1) * fold_size
      }else{
        fold_indices <- (1:last_fold_size) + (m - 1) * fold_size
      }
      folds[[m]] <- fold_indices
    }
    
    #cat("123",any(duplicated(c(folds[[1]], folds[[2]]))),"\n")
    
    for(l in 1:num_fold){
      test_index <- folds[[l]]
      test_set <- data[test_index,]
      if(l == num_fold){
        valid_index <- folds[[1]]
      }else{
        valid_index <- folds[[l+1]]
      }
      valid_set <- data[valid_index,]
      #cat(setdiff(seq_len(nrow(data)), c(test_index, valid_index)),"\n")
      train_index <- setdiff(seq_len(nrow(data)), c(test_index, valid_index))
      train_set <- data[train_index,]
      #cat("Fold", num_fold, "Train:", nrow(train_set), "Validation:", nrow(valid_set), "Test:", nrow(test_set), "\n")
      
      #Build DT
      train_label = train_set[,2]
      train_data = train_set[,-c(1,2)]
      valid_label = valid_set[,2]
      valid_data = valid_set[,-c(1,2)]
      test_label = test_set[,2]
      test_data = test_set[,-c(1,2)]
      
      tree <- rpart(train_label ~ .,data = train_data,control=rpart.control(maxdepth=4),method="class")
      
      train_pred <- predict(tree,train_data,type="class")
      train_acc <- round(sum(train_pred == train_label)/length(train_label),2)
      
      valid_pred <- predict(tree,valid_data,type="class")
      valid_acc <- round(sum(valid_pred == valid_label)/length(valid_label),2)
      
      test_pred <- predict(tree,test_data,type="class")
      test_acc <- round(sum(test_pred == test_label)/length(test_label),2)
      
      #cat("Train accuracy:", train_acc,"Validation accuracy:", valid_acc,"Test accuracy:", test_acc, "\n")
      
      fold <- paste0("fold", l)
      set <- c(fold)
      training <- c(train_acc)
      validation <- c(valid_acc)
      test <- c(test_acc)
      
      if(l != 1){
        temp <- data.frame(set,training,validation,test)
        answer <- rbind(answer,temp)
      }else{
        answer <- data.frame(set,training,validation,test)
      }
    }
    
    #count average
    avg_train <-0
    total_train <- 0
    for(o in 1:length(answer[,2])){
      total_train <- total_train + answer[o,2]
    }
    avg_train <- round(total_train/num_fold,2)
    training <- c(avg_train)
    
    avg_valid <- 0
    total_valid <- 0
    for(p in 1:length(answer[,3])){
      total_valid <- total_valid+answer[p,3]
    }
    avg_valid <- round(total_valid/num_fold,2)
    validation <- c(avg_valid)
    
    avg_test <- 0
    total_test <- 0
    for(q in 1:length(answer[,4])){
      total_test <- total_test+answer[q,4]
    }
    avg_test <- round(total_test/num_fold,2)
    test <- c(avg_test)
    
    set <- "ave."
    final_row <- data.frame(set,training,validation,test)
    answer <- rbind(answer,final_row)
  }
  j <- j + 1
}

#get output
k <- 1
while(k<length(arg)){
  if(arg[k]=="--output"){
    name <- arg[k+1]
    write.table(answer,file=name,sep=",",row.names=F,quote = FALSE)
  }
  k <- k+1
}
  