

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