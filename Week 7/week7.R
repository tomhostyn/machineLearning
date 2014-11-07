

setupEx1 <- function (data = read.table("in.dta")) {
  e <- list();
  train<- list()
  e$train$x1 <- data$V1[1:25]
  e$train$x2 <- data$V2[1:25]
  e$train$y <- data$V3[1:25]

  val <- list()
  val$x1 <- data$V1[26:length(data$V1)]
  val$x2 <- data$V2[26:length(data$V1)]
  val$y <- data$V3[26:length(data$V1)]
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


ex1 <- function (){
  
  in.dta = read.table("in.dta")
  out.dta = read.table("out.dta")
  
  e <- setupEx1(in.dta)
  

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




########################################  

ex6 <- function (){
  
  e1 <- runif(10000, 0, 1) 
  e2 <- runif(10000, 0, 1)
  emin <- min(e1, e2)
  
  c(mean(e1), mean(e2), mean (emin))
}