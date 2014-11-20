library(e1071)
# installed from http://www.csie.ntu.edu.tw/~cjlin/libsvm/

ex1 <- function () {  
#  see slide 11:  w el of Rd --> d dimensional problem
}

exp1 <- function (d,C,Q){
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")

  digits <- features.train
  plot (digits[,-1],
        col = digits[,1])
  
  d <- 7
  digits <- prepOnevAll(features.train, d)
  
  plot (digits[-3],
        col = digits$y+2)
  
  Q<- 2
  C<- 0.1

  model <- svm ( y ~ . , data = digits, cost = C,
                 kernel="polynomial", 
                 gamma=1, coef0=1, degree=Q, 
                 scale=FALSE, shrinking=FALSE,
                 type="C-classification",
                 
                 epsilon=0,
                 nu=0.01,
                 tolerance=0.001
                 )
    
  sv <- 1:length(digits$y) %in% model$index
  
  plot (digits[-3],
        col = digits$y+2)

  points (digits[sv,-3],
      col = c("red", "blue")[(digits[sv,"y"]+1)/2],
      pch = "+")

  model
}

exp2 <- function (C,Q){
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  digits <- features.train
  plot (digits[,-1],
        col = digits[,1])
  
  digits <- prepOnevFive(features.train)
  
  plot (digits[-3],
        col = digits$y+2)
  
  model <- svm ( y ~ . , data = digits, cost = C,
                 kernel="polynomial", 
                 gamma=1, coef0=1, degree=Q, 
                 scale=FALSE, shrinking=FALSE,
                 type="C-classification")
  
  sv <- 1:length(digits$y) %in% model$index
  
  plot (digits[-3],
        col = digits$y+2)
  
  points (digits[sv,-3],
          col = c("red", "blue")[(digits[sv,"y"]+1)/2],
          pch = "+")
  
  model
}

go1 <- function (){
  exp1(1, 0.1, 2)
  
  exp1(1, 0.001, 2)

  exp1(8, 0.1, 2)  
  
  exp1(5, 0.1, 2)  
  exp2(0.1, 2)
}


prepOnevAll <- function (d, digit){
  y<-sapply (d$digit, 
             function (x) {if (x == digit) 1 else -1})
#  y <- factor(y)
  x <- cbind (d, y=y)
  x <- subset (x, select = -digit)
  x
}


do_ex2 <- function () {

  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  C <- 0.01
  Q <- 2
  
  EinErr <- c()
  supportVectorCount <- c()
  for (i in 0:9){
#     y<-sapply (features.train$digit, 
#                function (x) {if (x == i) 1 else -1})
#     x <- cbind (features.train, y=y)
#     x <- subset (x, select = -digit)
    
    x<- prepOnevAll(features.train, i)
    
    model <- svm ( y ~ . , data = x, cost = C,
                   kernel="polynomial", 
                   gamma=1, coef0=1, degree=Q, 
                   scale=FALSE, shrinking=FALSE,
                   type="C-classification")
    
    match <- getError(model, x)
    EinErr <- c(EinErr, match)
    count <- length(model$coefs)
    supportVectorCount <- c(supportVectorCount, count)
  }
  
  cbind(0:9, EinErr, supportVectorCount)  
}

answers <- c("a", "b", "c", "d", "e")
if (! exists("EX2")){
    EX2 <- NULL
}

ex2 <- function () {
  if (is.null(EX2)){
    EX2 <<- do_ex2()}
  ex2 <- cbind (answers,EX2[c(0,2,4,6,8)+1,])
  ex2
}

ex3 <- function () {
  if (is.null(EX2)){
    EX2 <<- do_ex2()}
  
  ex3 <- cbind (answers,EX2[c(1,3,5,7,9)+1,])
  ex3
}

ex4 <- function () {
  if (is.null(EX2)){
    EX2 <<- do_ex2()}
  
  
  r <- c(0,2,4,6,8)+1
  x2 <- EX2[r,]
  i <- which.max (x2[,"EinErr"])
  ex2_svc <- x2[i,"supportVectorCount"]
    
  r <- c(1,3,5,7,9)+1
  x3 <- EX2[r,]
  i <- which.min (x3[,"EinErr"])  
  ex3_svc <- x3[i,"supportVectorCount"]
  
  abs(ex2_svc - ex3_svc)
}

prepXvY <- function (d, digit1, digit2){
  d <- d [(d$digit == digit1) | (d$digit == digit2),]
  y<-sapply (d$digit, 
             function (x) {if (x == digit1) 1 else -1})
  x <- cbind (d, y=y)
  x <- subset (x, select = -digit)
  x
}

prepOnevFive <- function (d){
#   d <- d [(d[,"digit"] == 1) | (d[,"digit"] == 5),]
#   y<-sapply (d$digit, 
#              function (x) {if (x == 1) 1 else -1})
#   x <- cbind (d, y=y)
#   x <- subset (x, select = -digit)
#   x
  prepXvY(d, 1, 5)
}

getError <- function (model, validation){
  pred <- predict (model, validation)
  1- sum(pred == validation$y)/ length(validation$y)
}

ghost <- function (digit1, digit2) {
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  ghost.train <- prepXvY(features.train, digit1, digit2)
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  ghost.test <- prepXvY(features.test, digit1, digit2)

  cost <- 1
  q <- 2
  
  model <- svm ( y ~ . , data = ghost.train, kernel="polynomial", cost = cost, 
                 gamma=1, coef0=1, degree=q, scale=FALSE, shrinking=FALSE, type="C-classification",
                 epsilon=0,
                 nu=0.01,
                 tolerance=0.001)
  
  print(model)
  
  print(getError(model, ghost.train))
  print(getError(model, ghost.test))
  
}


do_ex5 <- function (){
    
    features.train <- read.table("features.train")
    names (features.train) <- c("digit", "symmetry", "intensity")
    OnevFive.train <- prepOnevFive(features.train)
    
    features.test <- read.table("features.test")
    names (features.test) <- c("digit", "symmetry", "intensity")
    OnevFive.test <- prepOnevFive(features.test)
        
    C <- c(0.0001, 0.001, 0.01, 0.1, 1)
    Q<- c(2, 5)
    
    EinErr <- c()
    supportVectorCount <- c()
    EoutErr <- c()
    CV <- c()
    QV <- c()
    
    for (q in Q){
      for (cost in C){
        model <- svm ( y ~ . , data = OnevFive.train, kernel="polynomial", cost = cost, 
                       gamma=1, coef0=1, degree=q, scale=FALSE, shrinking=FALSE, type="C-classification")
        
        CV <- c(CV,cost)
        QV <- c(QV, q)
        match <- getError(model, OnevFive.train)
        EinErr <- c(EinErr, match)
        match <- getError(model, OnevFive.test)
        EoutErr <- c(EoutErr, match)
        count <- length(model$coefs)
        supportVectorCount <- c(supportVectorCount, count)
      }
    }
    
    cbind(Q=QV, C=CV, EinErr, supportVectorCount, EoutErr)    
}


if (! exists("EX5")){
  EX5 <- NULL
}

ex5 <- function (){
  if (is.null(EX5)){
    EX5 <<- do_ex5()}
  t <- EX5
  t[t[,"Q"]==2 & t[,"C"]>0.0001,]
}


ex6 <- function (){
  if (is.null(EX5)){
    EX5 <<- do_ex5()}
  t <- EX5
  a <- t[t[,"C"]==0.0001 & t[,"Q"]==2, "EinErr"] < t[t[,"C"]==0.0001 & t[,"Q"]==5, "EinErr"]
  b <- t[t[,"C"]==0.001 & t[,"Q"]==2, "supportVectorCount"] > t[t[,"C"]==0.001 & t[,"Q"]==5, "supportVectorCount"]
  c <- t[t[,"C"]==0.01 & t[,"Q"]==2, "EinErr"] < t[t[,"C"]==0.01 & t[,"Q"]==5, "EinErr"]
  d <- t[t[,"C"]==1 & t[,"Q"]==2, "EoutErr"] > t[t[,"C"]==1 & t[,"Q"]==5, "EoutErr"]
  
  list(a=a, b=b, c=c, d=d)  
}

library(caret)

ex7 <- function (){
  #http://en.wikipedia.org/wiki/Cross-validation_(statistics)#k-fold_cross-validation
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  OnevFive <- prepOnevFive(features.train)
  
  q<-2
  costs <- c(0.0001, 0.001, 0.01, 0.1, 1)
  score <- costs*0
  errsum <- costs * 0
  all_cverr <- c()
  for (i in 1:100){
    
    folds <- createFolds(OnevFive$y, 10)
    errs <- c()
    
    #    class_i <- sample(1:length((OnevFive$y)), length((OnevFive$y))/10)
    for (fold_i in 1:10){
      OnevFive.train <- OnevFive[unlist(folds[-fold_i]),]
      OnevFive.test <- OnevFive[unlist(folds[fold_i]),]
      
      findClassErr <- function (cost) {
        #print (OnevFive.test[1,])
        model <- svm ( y ~ . , data = OnevFive.train, kernel="polynomial", cost = cost, 
                       gamma=1, coef0=1, degree=q, scale=FALSE, shrinking=FALSE, type="C-classification")
        
        getError(model, OnevFive.test)
      }
      
      errs <- cbind(errs, sapply (costs, findClassErr)) #append a column of errors.  
    }
    
    cverr <- apply(errs,1,mean)  # get the cross validation errs as mean of the error of the 10 folds
    
    all_cverr <- cbind(all_cverr, cverr) #keep track of the errors cross validation errors
    
    best_i <- which.min(cverr)    
    score[best_i] <- score[best_i] + 1
  }
  
  cbind(costs, score, apply(all_cverr,1,mean))
}

ex7ghost <- function (){
  #http://en.wikipedia.org/wiki/Cross-validation_(statistics)#k-fold_cross-validation
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  fourvsix <- prepXvY(features.train, 4,6)
  
  q<-2
  costs <- c(0.0001, 0.001, 0.01, 0.1, 1)
  score <- costs*0
  errsum <- costs * 0
  all_cverr <- c()
  for (i in 1:100){
    
    folds <- createFolds(fourvsix$y, 10)
    errs <- c()
    
    #    class_i <- sample(1:length((OnevFive$y)), length((OnevFive$y))/10)
    for (fold_i in 1:10){
      fourvsix.train <- fourvsix[unlist(folds[-fold_i]),]
      fourvsix.test <- fourvsix[unlist(folds[fold_i]),]
      
      findClassErr <- function (cost) {
        #print (OnevFive.test[1,])
        model <- svm ( y ~ . , data = fourvsix.train, kernel="polynomial", cost = cost, 
                       gamma=1, coef0=1, degree=q, scale=FALSE, shrinking=FALSE, type="C-classification")
        
        getError(model, fourvsix.test)
      }
      
      errs <- cbind(errs, sapply (costs, findClassErr)) #append a column of errors.  
    }
    
    cverr <- apply(errs,1,mean)  # get the cross validation errs as mean of the error of the 10 folds
    
    all_cverr <- cbind(all_cverr, cverr) #keep track of the errors cross validation errors
    
    best_i <- which.min(cverr)    
    score[best_i] <- score[best_i] + 1
  }
  
  cbind(costs, score, apply(all_cverr,1,mean))
}



ex9 <- function (){
  
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  OnevFive.train <- prepOnevFive(features.train)
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  OnevFive.test <- prepOnevFive(features.test)
  
  EinErr <- c()
  EoutErr <- c()

  C <- c(0.01, 1, 100, 10^4, 10^6)
    for (cost in C){
      model <- svm ( y ~ . , data = OnevFive.train, kernel="radial", cost = cost, 
                     scale=FALSE, shrinking=FALSE, type="C-classification",
                     gamma=1)
      
      match <- getError(model, OnevFive.train)
      EinErr <- c(EinErr, match)
      match <- getError(model, OnevFive.test)
      EoutErr <- c(EoutErr, match)
    }
  
  cbind(C, EinErr, EoutErr)    
}
