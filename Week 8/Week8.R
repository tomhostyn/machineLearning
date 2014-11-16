library(e1071)
# installed from http://www.csie.ntu.edu.tw/~cjlin/libsvm/


ex1 <- function () {  
#  see slide 11:  w el of Rd --> d dimensional problem
}


###################  ex2  #####################3

library(LowRankQP)

lineFromPoints <- function (x, y) {
  r <- c()
  r$slope <- (y[2] - y[1])/(x[2] - x[1])
  r$intercept <- y[1] - r$slope * x[1]
  r
}

drawLine <- function (slope, intercept, c) {
  newx <- c(-1, 1)
  newy <- slope*newx + intercept
  lines (newx, newy, col=c)
}

refresh <- function (p, silent=TRUE , new=FALSE) {
  frame()
  plot (c(-1, 1), c(-1, 1), type = "n")
  
  points (p$x1 [p$clfn > 0] , p$x2[p$clfn > 0], col = "blue")
  points (p$x1 [p$clfn < 0] , p$x2[p$clfn < 0], col = "red")
  drawLine(p$fa, p$fb, "blue")  
  if (!silent) {printf ("clfn") ; print (p$clfn) }
  
  if (! is.null (p$pa)){
    drawLine (p$pa, p$pb, "green")    
  }
  
  if (! is.null (p$svm.a)){
    drawLine (p$svm.a, p$svm.b, "red")    
  }
  
  
  if (! is.null (p$p_clfn)) {
    if (!silent) {
      printf ("p_clfn") ; print (p$p_clfn) 
      printf ("p_match") ; print (p$p_match)} 
    ###  good training points, incorrectly classified in perceptron
    points (p$x1 [p$clfn > 0 & p$p_clfn < 0 ] , p$x2[p$clfn > 0 & p$p_clfn < 0], col = "blue", pch = "x")
    ### bad training points, incorrectly classified in perceptron
    points (p$x1 [p$clfn < 0 & p$p_clfn > 0 ] , p$x2[p$clfn < 0 & p$p_clfn > 0], col = "red", pch = "x")	
  }
  
}


# y -> x2
# x -> x1

getTestData <- function (N=10) {
  fx <- runif (2, -1, 1)
  fy <- runif (fx, -1, 1)
  ab <- lineFromPoints (fx, fy)
  fa <- ab[[1]]
  fb <- ab[[2]]
  
  x1 <- runif (N,-1,1)
  x2 <- runif (x1,-1,1)
  clfn <- sign(x2 - (fa*x1 +fb))
  
  l <- list(fa, fb, x1, x2, clfn)
  names (l) <- c("fa", "fb", "x1", "x2", "clfn")
  
  if (abs(sum(clfn)) == length (clfn)){
    #all samples are on one side of the line. regenerate.
    l <- getTestData(N)
  }
  l
}

updateWeights <- function (p) {
  
  p$p_match <- p$p_clfn * p$clfn
  l<-length (p$p_match[p$p_match < 0 ])
  if (l>0) {
    i <- sample (1:l, 1) 
    p$wx1 <- p$wx1 + p$x1[p$p_match < 0] [i] * p$clfn[p$p_match < 0][i]
    p$wx2 <- p$wx2 + p$x2[p$p_match < 0] [i] * p$clfn[p$p_match < 0][i]
    p$wx3 <- p$wx3 + 1 * p$clfn[p$p_match < 0][i]
    
    ab<-lineFromWeights(c(p$wx3, p$wx1, p$wx2))
    p$pa <- ab[1]
    p$pb <- ab[2]
  }
  p
}

initPerceptron <- function (p) {
  p$wx1 <-  0
  p$wx2 <- 0
  p$wx3 <- 0
  
  p <- calculateClassification(p)
  
  ab<-lineFromWeights(c(p$wx3, p$wx1, p$wx2))
  p$pa <- ab[1]
  p$pb <- ab[2]
  p
}

calculateClassification <- function(p) {
  p$p_clfn <- sign (p$x1 * p$wx1 + p$x2 * p$wx2 + p$wx3)
  #make sure 0's are counted as mismatches
  p$p_clfn <- sign( p$p_clfn - 0.5 )
  p$p_match <- p$p_clfn * p$clfn
  p
}

lineFromWeights <- function (w) {
  #expecting (wx3, wx1, wx2) from lm coefficients
  if (w[3] == 0 ){
    #warning ( "horizontal line!  jitter a little bit")
    w[3] <- 1/1000000
  }
  
  c(-w[2]/w[3],-w[1]/w[3])
}


solvePerceptron <- function(p){
  #run perceptron
  maxLoop <- 1000
  p$loopcounter <- 0
  
  p <- initPerceptron(p)
  #  refresh(p)
  
  while ( !is.na (match(-1,p$p_match)) && p$loopcounter < maxLoop)
  {
    p$loopcounter <- p$loopcounter + 1
    p <- updateWeights(p)
    p <- calculateClassification(p)
    #    refresh(p)
  }
  
  p   
}

#library (matrixcalc)
#library (Matrix)

HUGE <- 1e+20 # because Inf does not work

KVM_Kernel <- function (x, y, r, c){
  y[r]* y [c] * t(x[r,]) %*% x[c,]
}

solveSVM <- function(ds, K=KVM_Kernel, C=HUGE){
  
  #create quadratic coefficients matrix
  
  el <- c() 
  
  x <- cbind (ds$symmetry, ds$intensity)
  y <- ds$y
  
  nrow <- length(y)
  ncol <- length(y)
  k <- 0
  for (r in 1:nrow){
    for (c in 1:ncol){
      el <- c(el, K(x, y, r, c))
    }
  }
  
  p$Vmat <- matrix (el, nrow = nrow, byrow=TRUE)
  p$Dvec <- matrix (-1, nrow= nrow, ncol = 1) 
  p$Amat <- matrix (y, nrow=1, ncol=ncol)
  p$bvec <- matrix (0, nrow=1, ncol=1)
  p$uvec <- matrix (C, nrow=nrow, ncol=1)
  
  p$sol <- LowRankQP(p$Vmat, p$Dvec, p$Amat,p$bvec, p$uvec, method = "LU")
  
  # we now have solved for alpha. calculate weights!  
  p$svm.w1 <- sum(p$sol$alpha * y * x[,1])
  p$svm.w2 <- sum(p$sol$alpha * y * x[,2])
  
  #now estimate b.  solve yn(wt*xn + b) = 1 for *any* non zero support vector
  
  svi <- match (TRUE, p$sol$alpha > 1e-5) # index of first support vector
  
  wtxn <-  p$svm.w1*x[svi,1]+ p$svm.w2*x[svi,2]
  b <- 1/y[svi] - wtxn
  
  p$svm.w0 <- b
  
  ab<-lineFromWeights(c(p$svm.w0, p$svm.w1, p$svm.w2))
  p$svm.a <- ab[1]
  p$svm.b <- ab[2]
  
  refresh(p)
  p
}

ex8_run <- function (N = 10) {
  p <- getTestData(N)
  p <- solveSVM(p)
  p
}

ex8_outOfSamplePoints <- function (p){
  N=1000
  p$oos.x1 <- runif (N,-1,1)
  p$oos.x2 <- runif (p$oos.x1,-1,1)
  p$oos.clfn <- sign(p$oos.x2 - (p$fa*p$oos.x1 +p$fb))
  
  p
}


ex8_SVMoutOfSample <- function (p) {
  p$SVMoos.clfn <- p$oos.clfn 
  p$SVMoos.clfn <- sign (p$oos.x1 * p$svm.w1 + p$oos.x2 * p$svm.w2 + p$svm.w0)  
  
  p$SVM_err <- length (p$oos.clfn [ p$SVMoos.clfn != p$oos.clfn]) / length (p$oos.clfn)
  p
}



ex8 <- function () {
  EX8.PLA_err <<- c()
  EX8.SVM_err <<- c()
  
  for (i in 1:1000) {
    p <-ex8_run(10)
    p <- ex8_outOfSample(p)
    EX8.PLA_err <<- c(EX8.PLA_err, p$PLA_err)
    EX8.SVM_err <<- c(EX8.SVM_err, p$SVM_err)
  }
  sum(EX8.SVM_err <= EX8.PLA_err)/length(EX8.SVM_err)
}


poly_kernel <- function (Q){
  function (x, y, r, c){
      (1 + t(x[r,]) %*% x[c,])^Q
  }
}

vsallmodel <- function (data, digit) {
  
}

do_ex2 <- function () {

  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  kernel <- poly_kernel(2)
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


ex5 <- function (){
  
}