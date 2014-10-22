require(graphics)


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


vcdim <- 50
delta <- 0.05

mH <- function (N) {
  if (N[1] < vcdim){
    2^N
  } else {
    sum(choose (N, 1:vcdim))
  }
}

vcbound <- function (N) {epsilonFromN (N, vcdim, delta)}
rademacherPenaltyBound <- function(N) {sqrt(2*log(2*N*mH(N))/N) + sqrt(2/N*log(1/delta)) + 1/N}
parrondoVDB <- function (N, epsilon) {
  sqrt(1/N*(2*epsilon + log(6*mH(2*N)/delta)))
}

devroye <- function (N, epsilon) {sqrt(1/(2*N)*(4*epsilon*(1 + epsilon) + log(4*mH(N^2)/delta)))}
# mh(N^2) ln(10000^2^50 ) = 100*ln(10000). ignore the rest of mH.
#devroye <- function (N, epsilon) {sqrt(1/(2*N)*(4*epsilon*(1 + epsilon) + 2*vcdim*log(N)*log(4/delta)))}

initepsilon <- function (N, bound) {
  epsilon = 1
  print (epsilon)
  repeat {
    new_epsilon <- bound(N, epsilon)
    
    if (new_epsilon < epsilon){
      print (new_epsilon)
      epsilon <- new_epsilon
    }
    else {break}
  }
  new_epsilon
}

  

excercise2 <- function (){
  
  N <- 10000
  
  x <- 2000:N
  vcbound <- vcbound(x)
  rademacherPenaltyBound <- rademacherPenaltyBound(x)
  parrondo_epsilon <- initepsilon(N, parrondoVDB)
  parrondoVDB <- parrondoVDB(x, parrondo_epsilon)
  devroye_epsilon <- initepsilon(N, devroye)
  devroye <- devroye(x, devroye_epsilon)
   
#  plot (vcbound)
#  plot (rademacherPenaltyBound, add=TRUE)
  
  matplot (x,
           cbind(vcbound, rademacherPenaltyBound, parrondoVDB, devroye),
           type = "l",
           col=c("black", "blue", "red", "purple", "green"))
  
  list (vcbound = vcbound(N), 
        rademacher = rademacherPenaltyBound(N),
        Parrondo = parrondoVDB(N, parrondo_epsilon),
        devroye = devroye(N, devroye_epsilon))        
}

excercise3 <- function (){
  
  N <- 5
  
  x <- 1:10
  vcbound <- vcbound(x)
  rademacherPenaltyBound <- rademacherPenaltyBound(x)
  parrondo_epsilon <- initepsilon(N, parrondoVDB)
  parrondoVDB <- parrondoVDB(x, parrondo_epsilon)
  devroye_epsilon <- initepsilon(N, devroye)
  devroye <- devroye(x, devroye_epsilon)
  
  #  plot (vcbound)
  #  plot (rademacherPenaltyBound, add=TRUE)
  
  matplot (x,
           cbind(vcbound, rademacherPenaltyBound, parrondoVDB, devroye),
           type = "l",
           col=c("black", "blue", "red", "purple", "green"))
  
  list (vcbound = vcbound(N), 
        rademacher = rademacherPenaltyBound(N),
        Parrondo = parrondoVDB(N, parrondo_epsilon),
        devroye = devroye(N, devroye_epsilon))        
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
  for (n in 1:10000) {
    s <- generateHypothesis()
    slopes <- c(slopes, s)
  }
  mean(slopes) 
}



generateHypothesis <- function () {
  
  x <- runif (2, -1, 1)
  y <- sin(pi*x)
  
  #use derivative of (f(x1) - y1)^2 + (f(x2) - y2)^2
  
  x1 <- x[1]
  x2 <- x[2]
  y1 <- y[1]
  y2 <- y[2]
  
  a <- x1^2 + x2^2
  b <- x1*y1 + x2*y2
  if (a == 0 ) {
    warning( "zero hypothesis"); slope = generateHypothesis2()
  } else {
  slope <- b/a
  }

#  plotit()
  ploterr()

  ploterr <- function () {
    vara <- ((-500:500)/ 500) + slope  
    y <- (vara*x1 - y1)^2 + (vara*x2 - y2)^2
    matplot (vara,
             cbind(y, y),
             type = "l",
             col=c("black", "blue", "red", "purple", "green"))
    lines (c(-1, 1), c(0, 0))
    
  }
  
  plotit <- function (){
    testSet <- ((-500:500)/ 500) + 0.0001 #just avoid 0 
    slopey <- slope * testSet      
    matplot (testSet,
             cbind(sin(pi*testSet), slopey),
             type = "l",
             col=c("black", "blue", "red", "purple", "green"))
    points (x, y)
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
  slope <- 1.42;  # result of ex4
  bias <- sapply (slope, axCalcBias) 
  data.frame(slope = slope, bias = bias)
}

#################################  excercise6 #####################


axCalcVar <- function (s1, s2) {
  nrPoints <- 1000
  samples <- getxsamples(nrPoints)  
  
  y1 <- s1 * samples
  y2 <- s2 * samples
  sqErr(y1, y2)
}


ex6 <- function () {
  gavg <- 1.42
  vars <- sapply (1:1000, function (x) {axCalcVar(gavg, generateHypothesis())}) 
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

