

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


#######################################

ex3 <- function (){
  validation_ex(26:35, 1:25)  
}


########################################  

ex6 <- function (){
  
  e1 <- runif(10000, 0, 1) 
  e2 <- runif(10000, 0, 1)
  emin <- min(e1, e2)
  
  c(mean(e1), mean(e2), mean (emin))
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

library (quadprog)


