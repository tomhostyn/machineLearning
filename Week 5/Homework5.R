

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
}

##################### Ex 5 ####################################################################

gradient <- function (x, y, w){
  y*x/(1 + exp(y*w*x))
}

excercise5 <- function (){
  
  x <- c(1, 1, 1)
  y<- x
  w <- x
  gradient(x,y,w)
  
}

