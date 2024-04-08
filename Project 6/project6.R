#install.packages("xgboost",lib="C:/Program Files/R/R-4.0.0/library")
library(xgboost)
set.seed(122)

#Connected CMD
arg <- commandArgs(trailingOnly=TRUE)
if (length(arg)!=6) {
  stop("missing flag", call.=FALSE)
  stop("Rscript hw7_111753141.R --train hw7_train.csv --test hw7_test.csv --predict studentID.csv", call.=FALSE)
}

#read data
i <- 1
while(i<length(arg)){
  if(arg[i]=="--train"){
    df_train <- read.csv(arg[i+1],header=TRUE)
  }
  i = i+1
}
#cat("successful read train data.","\n")
j <- 1
while(j<length(arg)){
  if(arg[j]=="--test"){
    df_test <- read.csv(arg[j+1],header=TRUE)
  }
  j = j + 1
}
#cat("successful read test data.","\n")

#training
df_train$label[df_train$label == -1] <- 0
dtrain <- xgb.DMatrix(data = as.matrix(df_train[, -2]), label = df_train$label)
#cat("successful create dtrain")
dtest <- xgb.DMatrix(data = as.matrix(df_test))

params <- list(objective = "binary:logistic", eval_metric = "logloss")
cv_result <- xgb.cv(params = params,data = dtrain,nrounds = 500,nfold = 20,early_stopping_rounds = 10, verbose = 0)

best_iteration <- which.min(cv_result$evaluation_log$test_logloss_mean)
best_params <- as.list(cv_result$best_params)

model <- xgb.train(params = best_params,data = dtrain,nrounds = best_iteration)

probabilities <- predict(model, dtest,type = "prob")

for (row in 1:length(probabilities)) {
  if (probabilities[row] < 0.4) {
    probabilities[row] <- -1
  }
  if (probabilities[row] >= 0.4) {
    probabilities[row] <- 1
  }
}

submit <- data.frame(id = df_test[,1], label = probabilities)

#submit
l <- 1
while(l<length(arg)){
  if(arg[l]=="--predict"){
    name <- arg[l+1]
    write.table(submit,file=name,sep=",",row.names=F,quote = FALSE)
  }
  l <- l+1
}
