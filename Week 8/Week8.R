library(e1071)
# installed from http://www.csie.ntu.edu.tw/~cjlin/libsvm/

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
    
    model <- svm ( y ~ . , data = x, kernel="polynomial", cost = C, gamma=1, coef0=1, degree=2, scale=FALSE, type="C-classification")
    
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

do_ex5 <- function (){
    
    features.train <- read.table("features.train")
    names (features.train) <- c("digit", "symmetry", "intensity")
    
    features.test <- read.table("features.test")
    names (features.test) <- c("digit", "symmetry", "intensity")
    
    features.train <- features.train [(features.train[,"digit"] == 1) | (features.train[,"digit"] == 5),]
    y<-sapply (features.train$digit, 
             function (x) {if (x == 1) 1 else -1})
    x <- cbind (features.train, y=y)
    x <- subset (x, select = -digit)

    features.test <- features.test [(features.test[,"digit"] == 1) | (features.test[,"digit"] == 5),]
    testy<-sapply (features.test$digit, 
               function (x) {if (x == 1) 1 else -1})
    features.test <- cbind (features.test, testy=testy)
    
    C <- c(0.0001, 0.001, 0.01, 0.1, 1)
    Q<- c(2, 5)
    
    EinErr <- c()
    supportVectorCount <- c()
    EoutErr <- c()
    CV <- c()
    QV <- c()
    
    for (q in Q){
      for (cost in C){
        model <- svm ( y ~ . , data = x, kernel="polynomial", cost = cost, 
                       gamma=1, coef0=1, degree=q, scale=FALSE, type="C-classification")
        
        CV <- c(CV,cost)
        QV <- c(QV, q)
        predEin <- predict (model, features.train)
        match <- sum(predEin == x$y)/ length(x$y)
        EinErr <- c(EinErr, match)
        predEout <- predict (model, features.test)
        match <- sum(predEout == features.test$testy)/ length(features.test$testy)
        EoutErr <- c(EoutErr, match)
        count <- length(model$coefs)
        supportVectorCount <- c(supportVectorCount, count)
      }
    }
    
    cbind(Q=QV, C=CV, EinErr, supportVectorCount, EoutErr)    
}

ex5 <- function (){
  t <- do_ex5()
  t[t[,"Q"]==2 & t[,"C"]>0.0001,]
}


warning ("ex6 is wrong.  check EVERYTHING :(")
ex6 <- function (){
  t <- do_ex5()
  a <- t[t[,"C"]==0.0001 & t[,"Q"]==2, "EinErr"] < t[t[,"C"]==0.0001 & t[,"Q"]==5, "EinErr"]
  b <- t[t[,"C"]==0.001 & t[,"Q"]==2, "supportVectorCount"] > t[t[,"C"]==0.001 & t[,"Q"]==5, "supportVectorCount"]
  c <- t[t[,"C"]==0.01 & t[,"Q"]==2, "EinErr"] < t[t[,"C"]==0.01 & t[,"Q"]==5, "EinErr"]
  d <- t[t[,"C"]==1 & t[,"Q"]==2, "EoutErr"] > t[t[,"C"]==1 & t[,"Q"]==5, "EoutErr"]
  
  list(a=a, b=b, c=c, d=d)  
}

