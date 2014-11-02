
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