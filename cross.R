library(e1071)
library(xlsx)


dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/glibc_data_balanced.csv",header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/httpd_data_balanced.csv", header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/kernel_data_balanced.csv",header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/mozilla_data_balanced.csv", header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/xen_data_balanced.csv",header = TRUE, sep = ",")


#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/glibc_data.csv",header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/httpd_data.csv", header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/kernel_data.csv",header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/mozilla_data.csv", header = TRUE, sep = ",")
#dataset <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/xen_data.csv",header = TRUE, sep = ",")






um.svm <- cross_svm(dataset[,1:27], dataset[,28], 27)

write.table(um.svm, "glibc_data_balanced_svm.txt", sep="\t")
#write.xlsx(um.svm, "/home/hyuri/Dropbox/graduação/mineracao/vunelability/glibc_data_svm.xlsx");
#=====================================================================================================================

cross_svm <- function(variables, answer, folds) {
  
  meas <- data.frame()
  
  
  k <- folds
  splits <- runif(nrow(variables))
  
  results <- as.data.frame(sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    
    test <- variables[test.idx, , drop=FALSE]
    testClass <- answer[test.idx]
    
    train <- variables[train.idx, , drop=FALSE]
    trainClass <- answer[train.idx]
    
    
    start.time <- Sys.time()
  # classificador=================================================================================
    class.svm <- svm(trainClass~., data = train, cost = 1, gamma = 0.5,
                     type="C-classification", kernel="polynomial", coef0 = 3, degree = 2)
    #class.svm <- naiveBayes(trainClass~., data = train)
      
      
    #class.svm <- knn(trainClass~., data = train, k=3)
    
    total.time <- Sys.time() - start.time
    
    pred <- predict(class.svm, test)
    table <- table(true=testClass, pred=pred)
    
    n = sum(table) 
    nc = nrow(table) 
    diag = diag(as.matrix(table))  
    rowsums = apply(table, 1, sum) 
    colsums = apply(table, 2, sum)
    
    accuracy = sum(diag) / n 
    
    precision = diag / colsums 
    recall = diag / rowsums 
    specificity = (n-rowsums+diag-colsums)/(n-rowsums)
    f1 = 2 * precision * recall / (precision + recall) 
    
 
    table <- rbind(table, precision)
    table <- rbind(table, recall)
    table <- rbind(table, specificity)
    table <- rbind(table, f1)
    table <- rbind(table, c(accuracy, total.time))
    rownames(table) <- c(rownames(table)[1:6], "acc / time")
    
    
    
    for(i in 1:7){
      meas <- rbind(meas, table[i,1:2])  
    }
    
  
    
    meas
    
  }))
  

  
  for(i in 1:folds) {
   
    
    meas <- rbind(meas, as.data.frame(results[,i]))
    
  }
  

  
  

  colnames(meas) <- c("NEUTRAL", "VUNERABLE")
 
  begin = 1; end = 7
  for (i in 1:folds) {
    neutral <- paste0("NEUTRAL_", i)
    vul <- paste0("VULNERABLE_", i)
    pre <- paste0("precision_", i)
    sen <- paste0("Recall_", i)
    spe <- paste0("specificity_", i)
    f1 <- paste0("f1_", i)
    acc <- paste0("acc/time_", i)
    rownames(meas)[begin:end] <- c(neutral, vul, pre, sen, spe, f1, acc)
    begin = begin + 7
    end = end + 7
  }
  
  return (meas)
}

