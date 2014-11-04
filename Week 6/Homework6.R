
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
  
  if (! is.null (e$coef)){
    coef <- e$coef
  } else { 
    coef <- coef(e$lm)
  }
  
  x1 <- data$V1
  x2 <- data$V2
  y <- data$V3
  
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

ex2 <- function (){
  
  in.dta = read.table("in.dta")
  out.dta = read.table("out.dta")
  
  e <- setupEx2(in.dta)
  
  e$lm <- lm(y ~ 1+ x1 + x2 + x1x1 + x2x2 + x1x2 + absdif + abssum, data=e)
  
  insample_err <- ex2_clferr(e, in.dta)
  outsample_err <- ex2_clferr(e, out.dta)
  
  list (insample = insample_err, outsample = outsample_err)
}

sillytest <- function () {
  slope <- 1
  intercept <- 4

  x1 <- runif (100, -1, 1)
  x2 <- slope * x1 + intercept

  y_vector <- matrix (x2, ncol=1)
  x_df <- cbind (x0 = 1, x1 = x1)
  x_matrix <- matrix (x_df, nrow = dim(x_df)[1], ncol = dim(x_df)[2])

  pseudo_inverse <- solve (t(x_matrix) %*% x_matrix) %*% t(x_matrix)
  w <- pseudo_inverse %*% y_vector
}

ex2_bis <- function (){

  #with home grown linear regression
  in.dta = read.table("in.dta")
  out.dta = read.table("out.dta")
  
  e <- setupEx2(in.dta)
  
  y_vector <- matrix (e$y, ncol=1)
  
  x_df <- cbind (x0 = 1,
                 x1 = e$x1, 
                x2 = e$x2,
                x1x1 = e$x1x1,
                x2x2 = e$x2x2, 
                x1x2 = e$x1x2,
                absdif = e$absdif,
                abssum = e$abssum)
  x_matrix <- matrix (x_df, nrow = dim(x_df)[1], ncol = dim(x_df)[2])

  pseudo_inverse <- solve (t(x_matrix) %*% x_matrix) %*% t(x_matrix)
  w <- pseudo_inverse %*% y_vector
  w <- w[,1]
  names (w) <- c("(Intercept)", "x1", "x2", "x1x1", "x2x2", "x1x2", "absdif", "abssum")
  
  check <- lm(y ~ 1+ x1 + x2 + x1x1 + x2x2 + x1x2 + e$absdif + e$abssum, data=e)
  coef <- coef (check)
  
  if (sum ((coef - w)^2) > 10^-10) warning ("home grown regression different than lm")
  
  e$coef <- w
  
  insample_err <- ex2_clferr(e, in.dta)
  outsample_err <- ex2_clferr(e, out.dta)
  
  list (insample = insample_err, outsample = outsample_err)
}

weight_decay <- function (k){
  
  #with home grown linear regression
  in.dta = read.table("in.dta")
  out.dta = read.table("out.dta")
  
  e <- setupEx2(in.dta)
  
  y_vector <- matrix (e$y, ncol=1)
  
  x_df <- cbind (x0 = 1,
                 x1 = e$x1, 
                 x2 = e$x2,
                 x1x1 = e$x1x1,
                 x2x2 = e$x2x2, 
                 x1x2 = e$x1x2,
                 absdif = e$absdif,
                 abssum = e$abssum)
  x_matrix <- matrix (x_df, nrow = dim(x_df)[1], ncol = dim(x_df)[2])

  lambda <- 10^k
  ident <- diag(dim(x_df)[2])
  lambda_matrix <- lambda*ident
  
  pseudo_inverse <- solve (t(x_matrix) %*% x_matrix + lambda_matrix) %*% t(x_matrix)
  w <- pseudo_inverse %*% y_vector
  w <- w[,1]
  names (w) <- c("(Intercept)", "x1", "x2", "x1x1", "x2x2", "x1x2", "absdif", "abssum")
  e$coef <- w
  
  insample_err <- ex2_clferr(e, in.dta)
  outsample_err <- ex2_clferr(e, out.dta)
  
  list (insample = insample_err, outsample = outsample_err)
}

ex3 <- function (){ weight_decay(-3)}

ex4 <- function (){ weight_decay(3)}

ex5 <- function () {
  answers <- c(2, 1, 0, -1, -2)
  errs <- sapply (answers, weight_decay)
  cbind (answers, insample = errs[1,], outsample = errs [2,])
}

ex6 <- function () {
  answers <- -5:5
  errs <- sapply (answers, weight_decay)
  cbind (answers, insample = errs[1,], outsample = errs [2,])
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
  input + hidden

}

ex10 <- function () {
  warning ("wrong :(   should be 510")
  
  c1 <- c()
  c2 <- c()
  for (i in 1:36 ){
    if (36/i == round (36/i)){
      c1 <- c(c1, i)
      c2 <- c(c2, 36/i)
    }    
  }
  
  c3 <-  10*(c1 -1) + (c1 * (c1-1) * (c2 -1)) + c1
  cbind (c1, c2, c3)
  
  
}