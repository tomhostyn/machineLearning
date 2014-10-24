

deltaError <- function (N, delta = 0.1, d = 8){
  delta^2*(1-(d+1)/N) 
}


excercise1 <- function(){
  threshold <- 0.008
  
  options <- c(10, 25, 100, 500, 1000)
  deltaError <- deltaError (options)
  match <- deltaError > threshold
  data.frame (options = options, error = deltaError, match = match)
} 


################  Ex 2 ######################################################################


getxsamples <- function (nrPoints) {2* (0:(nrPoints-1)/(nrPoints-1)) -1}

excercise2 <- function (){

  x1 <- getxsamples (10)
  x2 <- x1
  
  optiona <- function (x2, x1) {sign (x1^2)} #row, column -> y, x
  optionb <- function (x2, x1) {sign (x2^2)}
  optionc <- function (x2, x1) {sign (x1^2 + x2^2)}
  optiond <- function (x2, x1) {sign (-x1^2 + x2^2)}
  optione <- function (x2, x1) {sign (x1^2 - x2^2)}
  
  outer(x1, x2, optiond)
}






##################### Ex 3 ####################################################################

ex3 <- function () {
  
  #  theory states vc should be at least nr of transformed params i.e. 15
  
}

##################### Ex 4 ####################################################################

ex4 <- function () {
  
#    u <- x
#    
#   (x*e^v -2* v*e^(-x))^2 
#   derive it on WA
#   
#   2 e^(-2 x) (e^(v+x)+2 v) (-2 v+e^(v+x) x)
#   
#   option e:  2* (e^v + 2*v*e^(-x))*(x*e^v -2*v*e^(-x)) 
#   
#   2 e^(-2 x) (e^(v+x)+2 v) (-2 v+e^(v+x) x) -  2* (e^v + 2*v*e^(-x))*(x*e^v -2*v*e^(-x))    
#   
#   results to 0
#   
#   [e]
  #
  #  we also need derivative to v
  #  (u * e^v -2 * v* e^(-u))^2
  
  #  (u * e^x -2 * x * e^(-u))^2
  #  derivative : 2 e^(-2 u) (-2+e^(u+x) u) (e^(u+x) u-2 x)
  #
}

##################### Ex 5 ####################################################################

gradient <- function (x, y, w){
  # y is scalar, x, w are vectors.  gradient is scalar
  - mean (y*x/(1 + exp(y*w*x)))
}

derivative <- function (x) {
  # this is only the derivative for u
  u <- x[1]
  v <- x[2]
  
  du <- 2 * (exp (v) + 2* v * exp (-u)) * (u*exp(v) - 2* v * exp(-u))
  dv <- 2 * exp(-2*u)*(-2+exp(u+v)*u) * (exp(u+v)* u-2*v)
  
  c(du, dv)
}

error <- function (x) {
  u <- x[1]
  v <- x[2]
  (u * exp(v) -2 * v* exp(-u))^2
}

# gradient descent S19, 53:31

excercise5 <- function (){
  
  w <- c(1,1)
  n <- 0.1
  
  y <- error(w)
  errors <- c()
    
  i <- 0
  repeat {
    i <- i+1
    
    w <- w - n*derivative(w)
    y <- error(w)
    errors <- c(errors, y)
    
    if (y < 10^-14 || i > 1000 ) break;
  }
  print (w)
  errors
}

# > err <- excercise5()
# [1] 0.04473629 0.02395871
# > err
# [1] 1.159510e+00 1.007407e+00 9.900912e-02 8.660645e-03 1.817558e-04 1.297240e-06 7.291525e-09
# [8] 4.009998e-11 2.201683e-13 1.208683e-15
# > length(err)
# [1] 10



######################  Excercise 7 ###############################


excercise7 <- function (){
  
  w <- c(1,1)
  n <- 0.1
  
  errors <- c()
  
  for (i in 1:15) {
    steps <- derivative(w)
    ustep <- c(steps[1], 0)
    w <- w - n * ustep
    
    steps <- derivative(w)
    vstep <- c(0, steps[2])
    w <- w - n * vstep
  }
  
  y <- error(w)
  
  print (w)
  y
}



####################### ex 8 ############################

getLine <- function (x, y) {
  r <- c()
  r$slope <-(y[2] - y[1])/(x[2] - x [1])
  r$intercept <- y[1] - r$slope * x[1]
  r
}

drawLine <- function (slope, intercept, c) {
  newx <- c(-1, 1)
  newy <- slope*newx + intercept
  lines (newx, newy, col=c)
}


getTestData <- function (N=10) {
  fx <- runif (2, -1, 1)
  fy <- runif (fx, -1, 1)
  line <- getLine (fx, fy)
  
  x1 <- runif (N,-1,1)
  x2 <- runif (x1,-1,1)
  y <- sign(x2 - (line$slope*x1 + line$intercept))
  
  list(line = line, x1=x1, x2=x2, y=y)
}

norm_vec <- function(x) sqrt(sum(x^2))

#NOTE: taking point per point, rather than calculating the gradient in 1 go using the formula in slide 23
# this is what the excercise suggests but check on forum!



logisticRegression <- function(p) {
  p$w <- c(0,0,0)
  eta <- 0.1
  
  samples <- cbind ( x0 = 1, x1 = p$x1, x2 = p$x2)
  y <- p$y
  N <- length (p$x1)
  
  epoch <- 1  
  old_w <- p$w
  repeat {
    permutation <- sample (1:N, N)  #shuffle the samples
    
    for (perm in 1:N){
      i <- permutation[perm]
      T <- y[i] * samples[i,]
      div <- 1 + exp(y[i] * sum(p$w* samples [i,]))
      gradient <- T/(N*div)
      #gradient <- T/(div)
      
      p$w <- p$w - eta * gradient 
    }
    #get gradient
    epoch <- epoch +1    
   
    diff <- p$w - old_w
    old_w <- p$w
    if (norm_vec(diff) < 0.01) {break}
  }
    
  p$logistic <-lineFromWeights(p$w)
  p$epoch <- epoch
  p
}

calcEout <- function (p) {
  N <- 1000
  x1 <- runif (N,-1,1)
  x2 <- runif (x1,-1,1)
  
  fy <- sign(x2 - (p$line$slope*x1 + p$line$intercept))
  gy <- sign(x2 - (p$logistic$slope*x1 + p$logistic$intercept))

  sum (fy == gy)/N
}
  

ex9 <- function(N = 100, loops = 1000) {
  
  errors <- c()
  epochs <- c()
  warning ("check if i am calculating cross entropy error!")
  for (i in 1:loops) {
    p <- getTestData(N)
    q <- logisticRegression(p)
    err <- calcEout(q)
    
    errors <- c(errors, err)
    epochs <- c(epochs, q$epoch)
  }
  
  l<-list(errors = errors, epochs = epochs)
  l
}


lineFromWeights <- function (w) {
  #expecting (wx0, wx1, wx2) from lm coefficients
  if (w[3] == 0 ){
    warning ( "horizontal line!  salt it a little bit")
    w[3] <- 1/1000000
  }
  
  c(slope = -w[2]/w[3],intercept = -w[1]/w[3])
}


refresh <- function (p, silent=TRUE , new=FALSE) {
  frame()
  plot (c(-1, 1), c(-1, 1), type = "n")
  
  points (p$x1 [p$y > 0] , p$x2[p$y > 0], col = "blue")
  points (p$x1 [p$y < 0] , p$x2[p$y < 0], col = "red")
  drawLine(p$line$slope, p$line$intercept, "blue")	
  
  if ( ! is.null (p$w)) {
    l <- lineFromWeights(p$w)
    drawLine (l["slope"], l["intercept"], "red")
    ###  good training points, incorrectly classified in lm
    #points (p$x1 [p$y > 0 & p$lm_clfn < 0 ] , p$x2[p$y > 0 & p$lm_clfn < 0], col = "blue", pch = "-")
    ### bad training points, incorrectly classified in lm
    #points (p$x1 [p$y < 0 & p$lm_clfn > 0 ] , p$x2[p$y < 0 & p$lm_clfn > 0], col = "red", pch = "-")    
  }
}
