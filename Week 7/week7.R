

setupEx1 <- function (data, trainrange, valrange) {
  e <- list();
  train<- list()
  e$train$x1 <- data$V1[trainrange]
  e$train$x2 <- data$V2[trainrange]
  e$train$y <- data$V3[trainrange]

  val <- list()
  val$x1 <- data$V1[valrange]
  val$x2 <- data$V2[valrange]
  val$y <- data$V3[valrange]
  e$val <- val 
  
  e$train$x1x1 <- e$train$x1 * e$train$x1
  e$train$x2x2 <- e$train$x2 * e$train$x2
  e$train$x1x2 <- e$train$x1 * e$train$x2
  e$train$abssum <- abs(e$train$x1 + e$train$x2)
  e$train$absdif <- abs(e$train$x1 - e$train$x2)
  
  e
}


ex1_clferr <- function (coef, x1, x2, y) {
  if (is.na(coef["x1"])){coef["x1"] <- 0}
  if (is.na(coef["x2"])){coef["x2"] <- 0}
  if (is.na(coef["x1x2"])){coef["x1x2"] <- 0}
  if (is.na(coef["x1x1"])){coef["x1x1"] <- 0}
  if (is.na(coef["x2x2"])){coef["x2x2"] <- 0}
  if (is.na(coef["absdif"])){coef["absdif"] <- 0}
  if (is.na(coef["abssum"])){coef["abssum"] <- 0}
  if (is.na(coef["(Intercept)"])){coef["(Intercept)"] <- 0}
    
  z <- sign (coef["(Intercept)"] +  
               coef["x1"] * x1 +
               coef["x2"] * x2 +
               coef["x1x2"] * x1*x2 +
               coef["x1x1"] * x1*x1 +
               coef["x2x2"] * x2*x2 +
               coef["absdif"] * abs(x1 - x2) +
               coef["abssum"] * abs(x1 + x2))
  sum (z != y)/length(z)
}

validation_ex <- function (trainrange, valrange){
  
  in.dta = read.table("in.dta")
  out.dta = read.table("out.dta")

  e <- setupEx1(in.dta, trainrange, valrange)
  
  k1 <- coef(lm(y ~ 1+ x1, data=e$train))
  k2 <- coef(lm(y ~ 1+ x1 + x2, data=e$train))
  k3 <- coef(lm(y ~ 1+ x1 + x2 + x1x1, data=e$train))
  k4 <- coef(lm(y ~ 1+ x1 + x2 + x1x1 + x2x2, data=e$train))
  k5 <- coef(lm(y ~ 1+ x1 + x2 + x1x1 + x2x2 + x1x2, data=e$train))
  k6 <- coef(lm(y ~ 1+ x1 + x2 + x1x1 + x2x2 + x1x2 + absdif, data=e$train))
  k7 <- coef(lm(y ~ 1+ x1 + x2 + x1x1 + x2x2 + x1x2 + absdif + abssum, data=e$train))

  k <- list(k1, k2, k3, k4, k5, k6, k7)
  
  val_err <- vector ("list", 7)
  outsample_err <- vector ("list", 7)
  for (i in 1:7){
    val_err[i] <- ex1_clferr (k[[i]], e$val$x1, e$val$x2, e$val$y)
    outsample_err[i] <- ex1_clferr (k[[i]], out.dta$V1, out.dta$V2, out.dta$V3)
  }  
  
  
  cbind (validation = val_err, outsample = outsample_err)
}

ex1 <- function (){
  validation_ex(1:25, 26:35)  
}

ex2 <- ex1

ex3 <- function () {
  v <- validation_ex(26:35, 1:25)  
}

ex4 <- ex3

ex5 <- function () {
  ex1 <- ex1()
  ex3 <- ex3()
  
  
  round(c(ex1[[7, 2]],ex3[[6, 2]]),1)
}

#######################################

ex3 <- function (){
  validation_ex(26:35, 1:25)  
}


########################################  


ex6 <- function (){
  
  e1 <- runif(10000, 0, 1) 
  e2 <- runif(10000, 0, 1)
  emin <- pmin(e1, e2)

  answers <- c(0, 0.1, 0.25, 0.4, 0.5) 
  cbind (answers,abs(answers - mean(emin)))
  
  round(c(mean(e1), mean(e2), mean (emin)),4)
}



###################### ex 7 ##############

squared_err <- function (x, val){ sum ((x-val)^2) }

ex7_constant <- function (x, y, val){
  b = y[1] + (y[2] - y[1])/2
  
  squared_err(c(val[1], b), val)
}

ex7_linear <- function (x, y, val){
  a <-(y[2] - y[1])/(x[2] - x [1])
  b <- y[1] - a * x[1]

  squared_err(c(val[1], a*val[1] + b), val)
}


ex7_leaveOneOut <- function(ro){
  x <- c(-1, ro, 1)
  y <- c(0, 1, 0)
  
  const_err <- c()
  lin_err <- c()
  for (i in 1:3) {
    err <- ex7_constant (x[-i], y[-i], c(x[i], y[i]))
    const_err <- c(const_err, err)
    err <- ex7_linear (x[-i], y[-i], c(x[i], y[i]))
    lin_err <- c(lin_err, err)
  }
  
  c(mean (const_err), mean(lin_err))
}

ex7 <- function () {
  sol <- c(sqrt(sqrt(3)+4), sqrt(sqrt(3)-1), sqrt(9 + 4*sqrt(6)), sqrt(9 - sqrt(6)))
  
  sapply (sol, ex7_leaveOneOut)
}

###########################  support vector machines #################

#library (quadprog)
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


# plot8 <- function (e) {
#   frame()
#   plot (c(-1, 1), c(-1, 1), type = "n")
# 
#   drawLine (e$fa, e$fb, "black")
#   points (e$x1[e$clfn > 0], e$x2[e$clfn > 0], col = "blue", pch = "o")
#   points (e$x1[e$clfn <= 0], e$x2[e$clfn <= 0], col = "red", pch = "o")
#   
#   if (! is.null (e$lm)) {
#     coefs <- coef(e$lm)
#     slope <- -coefs["x1"]/coefs["x2"]
#     intercept <- -coefs["(Intercept)"]/coefs["x2"]
#     
#     drawLine (slope, intercept, "red")
#   }
#   
#   if (! is.null (e$lm_clfn)){
#     points (e$x1[e$lm_clfn != e$training], e$x2[e$lm_clfn != e$training], col = "red", pch = "x")
#   }  
# }


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

solveSVM <- function(p){
  
  #create quadratic coefficients matrix
  
  el <- c() 
  
  x <- cbind (p$x1, p$x2)
  y <- p$clfn
  
  nrow <- length(y)
  ncol <- length(y)
  k <- 0
  for (r in 1:nrow){
    for (c in 1:ncol){
      el <- c(el, y[r]* y [c] * t(x[r,]) %*% x[c,])
    }
  }
  HUGE <- 1e+20 # because Inf does not work
  
  p$Vmat <- matrix (el, nrow = nrow, byrow=TRUE)
  p$Dvec <- matrix (-1, nrow= nrow, ncol = 1) 
  p$Amat <- matrix (y, nrow=1, ncol=ncol)
  p$bvec <- matrix (0, nrow=1, ncol=1)
  p$uvec <- matrix (HUGE, nrow=nrow, ncol=1)
  
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


solveSVM_W0 <- function(p){
  warning ("DOES NOT PULL OUT w0")
  #create quadratic coefficients matrix
  el <- c() 
  
  x <- cbind (1, p$x1, p$x2)
  y <- p$clfn
  
  nrow <- length(y)
  ncol <- length(y)
  k <- 0
  for (r in 1:nrow){
    for (c in 1:ncol){
      el <- c(el, y[r]* y [c] * t(x[r,]) %*% x[c,])
    }
  }
  HUGE <- 1e+20 # because Inf does not work
  
  p$Vmat <- matrix (el, nrow = nrow, byrow=TRUE)
  p$Dvec <- matrix (-1, nrow= nrow, ncol = 1) 
  p$Amat <- matrix (y, nrow=1, ncol=ncol)
  p$bvec <- matrix (0, nrow=1, ncol=1)
  p$uvec <- matrix (HUGE, nrow=nrow, ncol=1)
  
  p$sol <- LowRankQP(p$Vmat, p$Dvec, p$Amat,p$bvec, p$uvec, method = "LU")
  
  # we now have solved for alpha. calculate weights!
  
  p$svm.w0 <- sum(p$sol$alpha * y * x[,1])
  p$svm.w1 <- sum(p$sol$alpha * y * x[,2])
  p$svm.w2 <- sum(p$sol$alpha * y * x[,3])
  
  #now estimate b.  solve yn(wt*xn + b) = 1 for *any* non zero support vector
  
  svi <- match (TRUE, p$sol$alpha > 1e-6) # index of first support vector
  
  wtxn <- p$svm.w0*x[svi,1] + p$svm.w1*x[svi,2]+ p$svm.w2*x[svi,3]
  
  p$svm.b <- 1/y[svi] - wtxn
  
  ab<-lineFromWeights(c(p$svm.w0, p$svm.w1, p$svm.w2))
  p$svm.a <- ab[1]
  p$svm.b <- ab[2]
  
  p
}

ex8_run <- function (N = 10) {
  p <- getTestData(N)
  p <- solvePerceptron(p)
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

ex8_PLAoutOfSample <- function (p) {
  p$PLAoos.clfn <- sign (p$oos.x1 * p$wx1 + p$oos.x2 * p$wx2 + p$wx3)  
  p$PLA_err <- length (p$oos.clfn [ p$PLAoos.clfn != p$oos.clfn]) / length (p$oos.clfn)
  p
}

ex8_SVMoutOfSample <- function (p) {
  p$SVMoos.clfn <- p$oos.clfn 
  p$SVMoos.clfn <- sign (p$oos.x1 * p$svm.w1 + p$oos.x2 * p$svm.w2 + p$svm.w0)  
  
  p$SVM_err <- length (p$oos.clfn [ p$SVMoos.clfn != p$oos.clfn]) / length (p$oos.clfn)
  p
}

ex8_outOfSample <- function (p) {
  p <- ex8_outOfSamplePoints(p)
  p <- ex8_PLAoutOfSample(p)
  p <- ex8_SVMoutOfSample(p)
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



ex9 <- function () {
  EX9.PLA_err <<- c()
  EX9.SVM_err <<- c()
  EX9.P <<- list()
  EX10.SVM_NUMVEC <<- c()
  for (i in 1:1000) {
    p <-ex8_run(100)
    p <- ex8_outOfSample(p)
    EX9.PLA_err <<- c(EX8.PLA_err, p$PLA_err)
    EX9.SVM_err <<- c(EX8.SVM_err, p$SVM_err)
    EX10.SVM_NUMVEC <<- c(EX10.SVM_NUMVEC, sum (p$sol$alpha > 1e-6))
    EX9.P[[i]] <<- p
  }
  sum(EX9.SVM_err <= EX9.PLA_err)/length(EX9.SVM_err)
}


ex10 <- function(){
  warning ("run ex9 first")
  mean(EX10.SVM_NUMVEC)
}