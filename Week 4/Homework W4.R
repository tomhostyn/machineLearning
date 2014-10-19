
growthFunctionBound <- function (N, vcdim) {
  N^vcdim
}


deltaFromN <- function (N, vcdim, epsilon) {
  4 * growthFunctionBound(2*N, vcdim)* exp(-1/8* epsilon*epsilon* N)
}

epsilonFromN <- function (N, vcdim, delta){
  sqrt(8/N*log(4*growthFunctionBound(2*N, vcdim)/delta))
}

excercise1 <- function (){
  
  N <- c(400000, 420000, 440000, 460000, 480000)
  
  vcdim <- 10
  epsilon <- 0.05
  delta  <- 5/100
  
  calcdelta <-deltaFromN(N, vcdim, epsilon)
  rounddelta <- round (calcdelta, 4)
  calcepsilon <- epsilonFromN (N, vcdim, delta)
  deltacheck <- calcdelta < delta
  epsiloncheck <- calcepsilon < epsilon
  
  data.frame(N, calcdelta, rounddelta, calcepsilon, deltacheck, epsiloncheck)
  
  
}



##############################################################################


boundFunctions <- function (epsilon=0.01, vcdim = 50, delta = 0.05) {
    
  mH <- function (N) {
    
#    warning ("using simple approximate bound for mh(N). should be valid for large N")
#   N^vcdim
    
    sum(choose (N, 1:vcdim))
  }
  
  vcbound <- function (N) {epsilonFromN (N, vcdim, delta)}
  
  rademacherPenaltyBound <- function(N) {sqrt(2*log(2*N*mH(N))/N) + sqrt(2/N*log(1/delta)) + 1/N}
  
  parrontoVDB <- function (N) {sqrt(1/N*(2*epsilon + log(6*mH(2*N)/delta)))}
  devroye <- function (N) {sqrt(1/(2*N)*(4*epsilon*(1 + epsilon) + log(4*mH(N*N)/delta)))}
  
  list(vcbound = vcbound, 
       rademacherPenaltyBound = rademacherPenaltyBound,
       parrontoVDB = parrontoVDB,
       devroye = devroye)
}


excercise2 <- function (N){
  
  # REMEMBER TO VARY epsilon  ?????????
 
  
  f <- boundFunctions ()
  
  x <- 2000:10000
  vcbound <- f$vcbound(x)
  rademacherPenaltyBound <- f$rademacherPenaltyBound(x)  
  parrontoVDB_0.01 <- boundFunctions (0.01)$parrontoVDB(x)
  parrontoVDB_0.005 <- boundFunctions (0.005)$parrontoVDB(x)
  #  for large numbers, epsilon is very small (~3e-06 - so just pick one )
  devroye_0.01 <- boundFunctions(0.01)$devroye(x)
  devroye_0.005 <- boundFunctions(0.005)$devroye(x)
  
  
#  plot (vcbound)
#  plot (rademacherPenaltyBound, add=TRUE)
  
  matplot (x,
           cbind(vcbound, rademacherPenaltyBound, parrontoVDB_0.01, devroye_0.01),
           type = "l",
           col=c("black", "blue", "red", "purple", "green"))
  
  N <- 10000
  list (vcbound = f$vcbound(N), 
        rademacher = f$rademacherPenaltyBound(N),
        Parrondo_0.01 = boundFunctions (0.01)$parrontoVDB(N),
        devroye_0.01 = boundFunctions(0.01)$devroye(N))        
}



excercise3 <- function (){
  
  # REMEMBER TO VARY epsilon  ?????????
  
  N <- 5
  f <- boundFunctions ()  
  x <- 1:10
  vcbound <- f$vcbound(x)
  rademacherPenaltyBound <- f$rademacherPenaltyBound(x)  
  parrontoVDB_1 <- boundFunctions (1)$parrontoVDB(x)
  parrontoVDB_0.01 <- boundFunctions (0.01)$parrontoVDB(x)
  parrontoVDB_0.005 <- boundFunctions (0.005)$parrontoVDB(x)
  #  for large numbers, epsilon is very small (~3e-06 - so just pick one )
  devroye_1 <- boundFunctions(1)$devroye(x)
  devroye_0.01 <- boundFunctions(0.01)$devroye(x)
  devroye_0.005 <- boundFunctions(0.005)$devroye(x)
  
  
  #  plot (vcbound)
  #  plot (rademacherPenaltyBound, add=TRUE)
  
  matplot (x,
           cbind(vcbound, rademacherPenaltyBound, parrontoVDB_1, devroye_1),
           type = "l",
           col=c("black", "blue", "red", "purple", "green"))
  
  list (vcbound = f$vcbound(N), 
        rademacher = f$rademacherPenaltyBound(N),
        devroye_1 = boundFunctions(1)$devroye(N),
        Parrondo_1 = boundFunctions (1)$parrontoVDB(N),
        devroye_0.0001 = boundFunctions(0.0001)$devroye(N),
        Parrondo_0.0001 = boundFunctions (0.0001)$parrontoVDB(N))
  
}



######################################################
#  Bias and Variance
#######################################################


getxsamples <- function (nrPoints) {2* (0:(nrPoints-1)/(nrPoints-1)) -1}

visualizeEx4 <- function () {  
  x <- getxsamples (1000)
  y <- sin(pi*x) 
  
  calculatedgavg <- 0.78
  
  matplot (x,
          cbind(sin(pi*x),  0.79*x, 1.07*x, 1.58*x, calculatedgavg*x),
          type = "l",
          col=c("black", "green", "blue", "purple", "red"))
  lines (c(-1, 1), c(0, 0))
 # lines(x, mean(slopes)* x, col = "red")
  
}

showslopes <- function (slopes = NULL) {  
  x <- (-1000:1000)/ 1000
  y <- sin(pi*x) 
  
  matplot (x,
           cbind(sin(pi*x), 0),
           type = "l",
           col=c("black", "blue", "red", "purple", "green"))
  lines (c(-1, 1), c(0, 0))
  
  if (!is.null(slopes)){
    x <- c(-1, 1)
    sapply (slopes, function (s) {lines(x, s*x)})    
  }
  lines(x, mean(slopes)* x, col = "red")
  
}


sqErr <- function(d1, d2) {
  sqerr <- ((d1 - d2)^2)
  mean(sqerr)
} 



excercise4 <- function () {
  
  slopes <- c()
  for (n in 1:100000) {
    s <- generateHypothesis()
    slopes <- c(slopes, s)
  }
  mean(slopes) 
}



generateHypothesis <- function () {
  
  x <- runif (2, -1, 1)
  y <- sin(pi*x)

  #line through midpoint is our hypothesis
  hx <- (x[1] + x[2])/2
  hy <- (y[1] + y[2])/2
  
  if (hx == 0 ) {
    warning( "zero hypothesis"); slope = generateHypothesis2()
  } else {
  slope <- hy/hx
  }

  #plotit()
  
  plotit <- function (){
    testSet <- ((-500:500)/ 500) + 0.0001 #just avoid 0 
    slopey <- slope * testSet      
    matplot (testSet,
             cbind(sin(pi*testSet), slopey),
             type = "l",
             col=c("black", "blue", "red", "purple", "green"))
    points (x, y)
    points (hx, hy, pch = 23)
    lines (c(-1, 1), c(0, 0))
  }
  

  slope
}
  



####################  OLD CODE #################################
generateHypothesis2 <- function ()  {
    x <- runif (2, -1, 1)
    y <- sin(pi*x)

    #find a match for these 2 points.  stupidly go through -1 -> 1 and keep minimum error 
    allErrs <- c()
    testSet <- ((-500:500)/ 500) + 0.0001 #just avoid 0 
    for (i in testSet) {
      #calc a for ax
      slope <- sin(pi*i)/i
      
      #get the squared error for each, considering the 2 points
      err <- sum ((slope*x - y)^2)
      allErrs <- c(allErrs, err)
    }
    
    minIndex <- match (min (allErrs), allErrs)
    minx <- testSet[minIndex]
    slope <- sin(pi*minx)/minx
    #plotit()  
  
  plotit <- function (){
    slopey <- sin(pi*minx)/minx * testSet      
    matplot (testSet,
             cbind(sin(pi*testSet), slopey, sqrt(allErrs)),
             type = "l",
             col=c("black", "blue", "red", "purple", "green"))
    points (x, y)
    points (minx, sin(pi*minx), pch = 23)
    lines (c(-1, 1), c(0, 0))
  }
  
  slope
}


getBestHypothesis <- function ()  {
  
  #find a match for these 2 points.  stupidly go through -1 -> 1 and keep minimum error 
  allErrs <- c()
  allSlopes <- c()
  testSet <- ((-5000:5000)/ 5000) + 0.0001 #just avoid 0 
  for (i in testSet) {
    #calc a for ax
    slope <- sin(pi*i)/i
  
    #get the squared error for each, considering the 2 points
    err <- sum ((slope*testSet - sin(pi*testSet))^2/length(testSet))
    allErrs <- c(allErrs, err)
    allSlopes <- c(allSlopes, slope)
    }
  
  minIndex <- match (min (allErrs), allErrs)
  slope <- allSlopes[minIndex]
  
  slope
}


compAnswers <- function ()  {
  
  #find a match for these 2 points.  stupidly go through -1 -> 1 and keep minimum error 
  allErrs <- c()
  allSlopes <- c()
  testSet <- ((-5000:5000)/ 5000) + 0.0001 #just avoid 0 
  slopeSet <- c(0.79, 1.07, 1.58, 0.9550522, 1.432759)
  for (slope in slopeSet) {
    #get the squared error for each, considering the 2 points
    err <- sum ((slope*testSet - sin(pi*testSet))^2/length(testSet))
    allErrs <- c(allErrs, err)
    allSlopes <- c(allSlopes, slope)
  }
  
  data.frame(slope = slopeSet, err = allErrs)
}


###  for constant model
cmCalcBias <- function () {
  nrPoints <- 100000
  samples <- getxsamples(nrPoints)  
  
  yavg <- 0 * samples
  yf <- sin (pi*samples)
  
  sqErr(yavg, yf)
}


#################################  excercise5 #####################

axCalcBias <- function (slope) {
  nrPoints <- 100000
  samples <- getxsamples(nrPoints)  
  
  yavg <- slope * samples
  yf <- sin (pi*samples)  
  sqErr(yavg, yf)
}

ex5 <- function () {
  slope <- 0.79;  # result of ex4
  bias <- sapply (slope, axCalcBias) 
  data.frame(slope = slope, bias = bias)
}

#################################  excercise6 #####################


axCalcVar <- function (s1, s2) {
  nrPoints <- 10000
  samples <- getxsamples(nrPoints)  
  
  y1 <- s1 * samples
  y2 <- s2 * samples
  sqErr(y1, y2)
}


ex6 <- function () {
  gavg <- 0.79
  vars <- sapply (1:10000, function (x) {axCalcVar(gavg, generateHypothesis())}) 
  mean(vars)  
}


######################### ex7() ######################################
#####  NO WORKING CODE 

quadraticRoot <- function(a, b, c){
  
  sq <- sqrt(b^2 - 4*a*c)
  r1 <- (b + sq) / 2*a
  r2 <- (b - sq) / 2*a
  
  c(r1, r2)
}


generateZeroQuadHypothesis <- function () {
  
  x <- runif (2, -1, 1)
  y <- sin(pi*x)
  
  #quadratic through origin and midpoint is our hypothesis
  # WRONG!!!
  hx <- (x[1] + x[2])/2
  hy <- (y[1] + y[2])/2

  
  
  x <- runif (2, -1, 1)
  x <- c(0, 0.5)
  
  y <- sin(pi*x)
  
  x1 <- x[1]
  x2 <- x[2]
  y1 <- y[1]
  y2 <- y[2]
  
  #quadratic through origin is our hypothesis
  qa <- (x1^4 + x2^4)/2
  qb <- -(y1*(x1^2) + y2*(x2^2))
  qc <- (y1^2 + y2^2)/2
  
  a <- quadraticRoot(qa, qb, qc)[1]
  
  
  
  #plotit()
  
  plotit <- function (){
    testSet <- ((-500:500)/ 500) + 0.0001 #just avoid 0 
    slopey <- a * testSet^2      
    matplot (testSet,
             cbind(sin(pi*testSet), slopey),
             type = "l",
             col=c("black", "blue", "red", "purple", "green"))
    points (x, y)
    #points (hx, hy, pch = 23)
    lines (c(-1, 1), c(0, 0))
  }
  
  
  slope
}



################### ex 8 ####################


mH8 <- function(N, q){
  
  if (N == 1) {
    return (2)
  } else {
    return (2*mH8(N-1, q) - choose(N-1, q))
  }
  
}


ex8Table <- function (){
  
  m <- matrix(nrow=10, ncol=11)
  for (N in 1:10) {
   for (q in 1:11){
     m[N,q] <- mH8(N, q)
   } 
  }
  m
}

