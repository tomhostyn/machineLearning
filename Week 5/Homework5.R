

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



