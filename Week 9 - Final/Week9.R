library(e1071)




ex1<- function (Q=10) {
#  (Q+1)*(d+1) - (Q*d)/2 -1
  
  (Q+2)*(Q+1)/2 -1
}

ex11 <- function (){
  x1 <- c( 1, 0, 0,-1, 0, 0,-2)
  x2 <- c( 0, 1,-1, 0, 2,-2, 0)
  y  <- c(-1,-1,-1, 1, 1, 1, 1)
  
  z1 <- x2^2 -2*x1 -1
  z2 <- x1^2 -2*x2 +1
  
  plot (z1, z2, col = y+3)
  
  w1 <- -1
  w2 <- 1
  b <- -0.5
  abline (b/-w2,-w1/w2)

  w1 <- 1
  w2 <- -1
  b <- -0.5
  abline (b/-w2, -w1/w2, col="blue")
  
  
  w1 <- 1
  w2 <- 1e-28
  b <- -0.5
  abline (v=-b/w1, col="green")
  
  
  w1 <- 0
  w2 <- 1
  b <- -0.5
  abline (b/-w2,-w1/w2,  col="red")
  
}

# weight_decay <- function (k){
#   
#   #with home grown linear regression
#   in.dta = read.table("in.dta")
#   out.dta = read.table("out.dta")
#   
#   e <- setupEx2(in.dta)
#   
#   y_vector <- matrix (e$y, ncol=1)
#   
#   x_df <- cbind (x0 = 1,
#                  x1 = e$x1, 
#                  x2 = e$x2,
#                  x1x1 = e$x1x1,
#                  x2x2 = e$x2x2, 
#                  x1x2 = e$x1x2,
#                  absdif = e$absdif,
#                  abssum = e$abssum)
#   x_matrix <- matrix (x_df, nrow = dim(x_df)[1], ncol = dim(x_df)[2])
#   
#   lambda <- 10^k
#   ident <- diag(dim(x_df)[2])
#   lambda_matrix <- lambda*ident
#   
#   pseudo_inverse <- solve (t(x_matrix) %*% x_matrix + lambda_matrix) %*% t(x_matrix)
#   w <- pseudo_inverse %*% y_vector
#   w <- w[,1]
#   names (w) <- c("(Intercept)", "x1", "x2", "x1x1", "x2x2", "x1x2", "absdif", "abssum")
#   e$coef <- w
#   
#   insample_err <- ex2_clferr(e, in.dta)
#   outsample_err <- ex2_clferr(e, out.dta)
#   
#   list (insample = insample_err, outsample = outsample_err)
# }

lambda_lm <- function (z, y, lambda){
    
  y_vector <- as.matrix(y)
  z_matrix <- as.matrix(z)
  
  ident <- diag(ncol(z_matrix))
  lambda_matrix <- lambda*ident
  
  pseudo_inverse <- solve (t(z_matrix) %*% z_matrix + lambda_matrix) %*% t(z_matrix)
  w <- pseudo_inverse %*% y_vector
  w
}

prepXvAll <- function (d, digit){
  y<-sapply (d$digit, 
             function (x) {if (x == digit) 1 else -1})
  x <- cbind (d, y=y)
  x <- subset (x, select = -digit)
  x
}


prepXvY <- function (d, digit1, digit2){
  d <- d [(d$digit == digit1) | (d$digit == digit2),]
  y<-sapply (d$digit, 
             function (x) {if (x == digit1) 1 else -1})
  x <- cbind (d, y=y)
  x <- subset (x, select = -digit)
  x
}

getError <- function (pred, expect){
  1- sum(sign(pred) == sign(expect))/ length(expect)
}

do_ex7 <- function (d) {
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  digits <- features.train
  plot (digits[,-1],
        col = digits[,1])

  digits <- prepXvAll(features.train, d)
  
  plot (digits[-3],
        col = digits$y+2)
  
  lambda = 1
  w <- lambda_lm(digits[-3], digits$y, lambda)

  getError(as.matrix(digits[-3]) %*% w, digits$y)
}

ex7 <- function () {
  digits <- 5:9
  E_in <- sapply (digits, do_ex7)
  i <- which.min(E_in)
  cbind(digits, E_in, E_in == E_in[i])
}


transform_8 <- function (x1,x2) {
  cbind(1, x1, x2, x1*x2, x1*x1, x2*x2)
}

do_ex8 <- function (d, transform, features.train, features.test) {
  digits <- prepXvAll(features.train, d)
  digits.test <- prepXvAll(features.test, d)  

  z <- transform(digits[1],digits[2])  
  lambda = 1
  w <- lambda_lm(z, digits$y, lambda)
  
  z_out <- transform(digits.test[1],digits.test[2])  
  getError(as.matrix(z_out) %*% w, digits.test$y)
}

ex8 <- function () {
  
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  digits <- 0:4
  E_in <- sapply (digits, function (x) {do_ex8(x,transform_8, features.train, features.test)})
  i <- which.min(E_in)
  cbind(digits, E_in, E_in == E_in[i])
}


id_trans <- function (x,y) {as.matrix(cbind(x,y))}
ex9 <- function () {
  
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  digits <- 0:9
  E_in <- sapply (digits, function (x) {do_ex8(x,id_trans, features.train, features.train)})
  E_out <- sapply (digits, function (x) {do_ex8(x,id_trans, features.train, features.test)})
  E_in_trans <- sapply (digits, function (x) {do_ex8(x,transform_8, features.train, features.train)})
  E_out_trans <- sapply (digits, function (x) {do_ex8(x,transform_8, features.train, features.test)})
  cbind(digits, E_in, E_out, E_in_trans, E_out_trans, "[b]"=E_out_trans <= 0.95 * E_out)
}

do_ex10 <- function (d1, d2, lambda, transform, features.train, features.test) {
  digits <- prepXvY(features.train, d1, d2)
  digits.test <- prepXvY(features.test, d1, d2)  
  
  z <- transform(digits[1],digits[2])  
  w <- lambda_lm(z, digits$y, lambda)
  
  z_out <- transform(digits.test[1],digits.test[2])  
  getError(as.matrix(z_out) %*% w, digits.test$y)
}

ex10 <- function () {
  
  features.train <- read.table("features.train")
  names (features.train) <- c("digit", "symmetry", "intensity")
  
  features.test <- read.table("features.test")
  names (features.test) <- c("digit", "symmetry", "intensity")
  
  lambda <- c(0.01,1)
  E_in <- sapply (lambda, function (x) {do_ex10(1,5, x,transform_8, features.train, features.train)})
  E_out <- sapply (lambda, function (x) {do_ex10(1,5, x,transform_8, features.train, features.test)})
  cbind(lambda, E_in, E_out)
}



ex12 <- function () {
  x1 <- c( 1, 0, 0,-1, 0, 0,-2)
  x2 <- c( 0, 1,-1, 0, 2,-2, 0)
  y  <- c(-1,-1,-1, 1, 1, 1, 1)
  
  ds <- data.frame(x1=x1, x2=x2, y=y)
  
  plot (x1, x2, col = y+3)
  
  model <- svm ( y ~ . , data = ds, cost = 1e25,
                 kernel="polynomial", 
                 gamma=1, coef0=1, degree=2, 
                 scale=FALSE, shrinking=FALSE,
                 type="C-classification",
                 
                 epsilon=0,
                 nu=0.01,
                 tolerance=0.001
  )
  
  sv <- 1:length(ds$y) %in% model$index
  
  points (ds[sv,-3], col = y+3, pch = "+")  
}



############### RBF 

rbf_target <- function (x1,x2){  
  sign(x2 - x1 + 0.25*sin(pi*x1))
}

rbf_get_train <- function (N=100){
  x1 <- runif (N, -1, 1)
  x2 <- runif (x1, -1, 1)
  y <- rbf_target(x1,x2)
  data.frame(x1,x2,y)
}


rbf_plot <- function (d){
  plot (d$x1, d$x2, col=d$y + 2)
}

rbfsvm_getError <- function (model, validation){
  pred <- predict (model, validation)
  1- sum(sign(as.numeric(pred)-1.5) == validation$y)/ length(validation$y)
}

do_ex13 <- function () {
  train <- rbf_get_train (100)
  
  model <- svm ( y ~ . , train, kernel="radial", cost = 1e20, 
                 scale=FALSE, shrinking=FALSE, type="C-classification",
                 gamma=1.5)
  
  match <- rbfsvm_getError (model, train) 
  match
}

ex13 <- function () {
  err <- sapply (1:1000, function (x){do_ex13()})
  print (mean(err))
  print(summary(err))
}

norm_vec <- function(x) sqrt(sum(x^2))

rbf_getError <- function (model, validation, gamma){
  pred <- apply(validation [1:2],1, function (x){
    sum(apply (model$coef, 1, function (coef){coef["w"] * exp(-gamma*norm_vec(x-coef[1:2])^2)}))
  })  
  1- sum(sign(pred) == validation$y)/length(validation$y)
}

lloyd_get_centres <- function (data, clusters){
  centres <- sapply (unique(clusters),
  function (c){
    clusterData <- data[clusters ==c,]
    apply (clusterData, 2, sum)/nrow(clusterData)
  })
  t(centres)
}
  
lloyd_get_clusters <- function (data, centres){
  apply (data, 1, function (r){
    distances <- apply(centres, 1, function (c){norm_vec(r-c)^2})
    which.min(distances)})
}

lloyd_get_weights <- function (data, centres,lambda, y){
  Phi <- apply (data, 1,
                     function (x){
                       apply (centres, 1, function (c){exp(-lambda*norm_vec(x-c)^2)})
                     })
  Phi <- t(as.matrix(Phi))
  
  w <- solve (t(Phi) %*% Phi) %*% t(Phi)%*% as.matrix(y, nrow=1)
  w
}


RBF <- function (train, lambda=1.5, NumCentres=6) {
  #initialize centres at random
  x1 <- runif (NumCentres, -1, 1)
  x2 <- runif (x1, -1, 1)
  centres <- data.frame(x1, x2)
  
  convergence <- 1e20
  while (convergence >= 0.0001){
    clusters <-  lloyd_get_clusters(train[1:2], centres)
    new_centres <- lloyd_get_centres(train[1:2], clusters)
    
    convergence <- sum (apply (centres - new_centres, 1, norm_vec))
    centres <- new_centres
  }
  
  result <- list()
  w <- lloyd_get_weights(train[1:2], centres, lambda, train[3])
  result$coef <- data.frame (centres,w=w) 
  names(result$coef) <- c("x1", "x2", "w")
  result$emptyClusters <- min(table(clusters)) == 0 # running with empty clusters

  if (result$emptyClusters) {warning ("running with empty clusters")}
  result
}


do_ex14 <- function (kernels, gamma){
  train <- rbf_get_train(100)
  test <- rbf_get_train(10000)
    
  rbf_model <- RBF(train, gamma, kernels)
  E_out_rbf <- rbf_getError(rbf_model, test, gamma)
  
  svm_model <-  svm ( y ~ . , train, kernel="radial", cost = 1e20, 
                                scale=FALSE, shrinking=FALSE, type="C-classification",
                                gamma=gamma)
  E_out_rbfsvm <- rbfsvm_getError (svm_model, test) 
  
  if (rbf_model$emptyClusters){
    ## start over
    do_ex14()
  } else {
    list (E_out_rbf=E_out_rbf, E_out_rbfsvm=E_out_rbfsvm)
  }
}

ex14 <- function () {
  results <- sapply (1:100,  function (x) {do_ex14(9, 1.5)})
  firstbest <- apply (t(results), 1, function (x){x$E_out_rbfsvm < x$E_out_rbf})
  sum(firstbest)/length(firstbest)
} 


ex15 <- function () {
  results <- sapply (1:100,  function (x) {do_ex14(12, 1.5)})
  firstbest <- apply (t(results), 1, function (x){x$E_out_rbfsvm < x$E_out_rbf})
  sum(firstbest)/length(firstbest)
}


ex20 <- function () {
  
  # f(x) = 0
  
  g1 <- 0.001
  g2 <- 5
  
  cbind (Eg1=g1^2, Eg2=g2^2, Eavg=((g1^2 + g2^2)/2),
         Eg=((g1+g2)/2)^2)
}