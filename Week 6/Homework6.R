
############### ex1 #####################


ex1 <- function (){
  
print ("deterministic noise = bias.  which is the ability for the average function to approx the target
       function.  for subsets of the hypothesis space, the average should become less expressive.
       so, in general [b] the noise will increase.")  
}

################# ex2 ###################################

setupEx2 <- function (data = read.table("in.dta")) {
  e <- list();
  e$x1 <- data$V1
  e$x2 <- data$V2
  e$y <- data$V3
  
  e$x1x1 <- e$x1 * e$x1
  e$x2x2 <- e$x2 * e$x2
  e$x1x2 <- e$x1 * e$x2
  e$abssum <- abs(e$x1 + e$x2)
  e$absdif <- abs(e$x1 - e$x2)
  
  e
}


ex2_clferr <- function (e, data) {
  
  coef <- coef(e$lm)
  
  x1 <- data$V1
  x2 <- data$V2
  y <- data$V3
  
  z <- sign (coef["(Intercept)"] +  
               coef["x1"] * x1 +
               coef["x2"] * x2 +
               coef["x1x2"] * x1*x2 +
               coef["x1x1"] * x1*x1 +
               coef["x2x2"] * x2*x2 +
               coef["e$absdif"] * abs(x1 - x2) +
               coef["e$abssum"] * abs(x1 + x2))
  sum (z != y)/length(z)
}

ex2 <- function (){
  
  in.dta = read.table("in.dta")
  out.dta = read.table("out.dta")
  
  e <- setupEx2(in.dta)
  
  e$lm <- lm(y ~ 1+ x1 + x2 + x1x1 + x2x2 + x1x2 + e$absdif + e$abssum, data=e)
  
  insample_err <- ex2_clferr(e, in.dta)
  outsample_err <- ex2_clferr(e, out.dta)
  
  list (insample = insample_err, outsample = outsample_err)
}





#########################  NEURAL NETWORKS #####################


# Slide 14

ex8 <- function () {
 L=2
 d0 = 5
 d1 =3
 d2 = 1
 
 # wx
 forward <- 5*3 + 3*1
 
 #w*delta
 delta_to_weights <-  3*3 
  # assume not for input notes. output node delta calc does not have multiplication.
 #xdelta
 xdelta <- forward
 
 forward+xdelta + delta_to_weights
  
}

ex9 <- function () {
  
  input <- 10
  hidden <- 36
  
# minimize combinations, so all in 1 layer. don't forget output needs weights too.
  (input +1 ) * hidden 
# = 396  but too big

# consider all in 1 line
  input + hidden +1

}


ex10 <- function () {
  
  input <- 10
  hidden <- 36
  
  w <- c()
  
  # minimize combinations, so all in 1 layer. don't forget output needs weights too.
  # w0 for each layer does not take a weight as input
  w <- c(w,10 * 35 + 36) 
  
  # 10 input, 3 * 10 + 6
  w <- c(w, 10*9 +2*9*9 + 9*5 + 5*1 )
  
  # 6 layers of 6
  
  width = 6
  length = 6
  w <- c(w, (input +1)* length + (length * length -1) * (width -1))

  # 18 layers of 2
  width = 18
  length = 2
  w <- c(w, (input +1)* length + (length * length -1) * (width -1))
  
  w
}

