library(e1071)
# installed from http://www.csie.ntu.edu.tw/~cjlin/libsvm/

warning(" C-classification gave bad results, but default type also. rsults make no sense. where is mistake?")
warning(" check how to use svm call ??")

ex1 <- function () {  
#  see slide 11:  w el of Rd --> d dimensional problem
}

do_ex2 <- function () {

  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  C <- 0.01
  
  EinErr <- c()
  supportVectorCount <- c()
  for (i in 0:9){
    y<-sapply (features.train$digit, 
               function (x) {if (x == i) 1 else -1})
    x <- cbind (features.train, y=y)
    x <- subset (x, select = -digit)
    
    model <- svm ( y ~ . , data = x, kernel="polynomial", cost = C, 
                   gamma=1, coef0=1, degree=2, scale=FALSE, type="C-classification")
    
    predEin <- predict (model, features.train)
    match <- sum(predEin == x$y)/ length(x$y)
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
  EX2 <<- do_ex2()
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

prepOnevFive <- function (d){
  d <- d [(d[,"digit"] == 1) | (d[,"digit"] == 5),]
  y<-sapply (d$digit, 
             function (x) {if (x == 1) 1 else -1})
  x <- cbind (d, y=y)
  x <- subset (x, select = -digit)
  x
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
                       gamma=1, coef0=1, degree=q, scale=FALSE, type="C-classification")
        
        CV <- c(CV,cost)
        QV <- c(QV, q)
        predEin <- predict (model, OnevFive.train)
        match <- sum(predEin == OnevFive.train$y)/ length(OnevFive.train$y)
        EinErr <- c(EinErr, match)
        predEout <- predict (model, OnevFive.test)
        match <- sum(predEout == OnevFive.test$y)/ length(OnevFive.test$y)
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


ex7 <- function (){
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  OnevFive <- prepOnevFive(features.train)
    
  q<-2
  costs <- c(0.0001, 0.001, 0.01, 0.1, 1)
  score <- costs*0
  errsum <- costs * 0
  for (i in 1:100){
    class_i <- sample(1:length((OnevFive$y)), 10)
    OnevFive.train <- OnevFive[-class_i, ]
    OnevFive.test <- OnevFive[class_i, ]
  
    findClassErr <- function (cost) {
      model <- svm ( y ~ . , data = OnevFive.train, kernel="polynomial", cost = cost, 
                   gamma=1, coef0=1, degree=q, scale=FALSE, type="C-classification")
      
      predEout <- predict (model, OnevFive.test)
      match <- sum(predEout == OnevFive.test$y)/ length(OnevFive.test$y)
      match
    }
    errs <- sapply (costs, findClassErr)
    errsum <- errsum + errs
    best <- max(errs)
    score <- score + sapply (errs, function(e){if (e == best) 1 else 0})
    }
  
  cbind(costs, score, errs/100)
}


ex8 <- function (){
  
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
                     scale=FALSE, type="C-classification")
      
      predEin <- predict (model, OnevFive.train)
      match <- sum(predEin == OnevFive.train$y)/ length(OnevFive.train$y)
      EinErr <- c(EinErr, match)
      predEout <- predict (model, OnevFive.test)
      match <- sum(predEout == OnevFive.test$y)/ length(OnevFive.test$y)
      EoutErr <- c(EoutErr, match)
    }
  
  cbind(C, EinErr, EoutErr)    
}
