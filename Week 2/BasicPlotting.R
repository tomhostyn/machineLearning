
basicplotting <- function (){

frame()
plot (c(-1, 1), c(-1, 1), type = "n")

#abline uses a & b to plot a line.  

#horizontal line over 0: y = intercept + slope.x   (y = a + bx)
abline (0, 0)

#horizontal line over 0.5: y = slope.x + intercept where slope =0  intercept =0.5 0
abline (0.5, 0)

#line over 0.5: y = slope.x + intercept where slope = 1  intercept = 0
abline (-0.5, 1)

#vertical line (10, 10) -> (10, -10)

x <- c(10, 10)
y <- c(10, -10)
plot (x,y)
lines (x,y)


#######################################################################################
# plotting linear regression line through data
frame()
plot (c(-1, 1), c(-1, 1), type = "n")

e <- list ()
e$x1 <- runif (30,-1,1)
noise <- runif (e$x1,-0.1,0.1)
e$x2 <- 0.5 + e$x1 + noise
plot (e$x1, e$x2)
abline (0.5, 1)

fit <- lm (x2 ~ x1, data = e)
abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 

#######################################################################################
# plotting linear regression line through data

frame()
plot (c(-1, 1), c(-1, 1), type = "n")

e <- list ()
e$x1 <- runif (30,-1,1)
e$x2 <- 0.5 + e$x1
plot (e$x1, e$x2)
abline (0.5, 1)


fit <- lm (x2 ~ x1, data = e)
abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 

##########################################################################################
# classification y = x

frame()
plot (c(-1, 1), c(-1, 1), type = "n")

e <- list ()
e$x1 <- runif (300,-1,1)
e$x2 <- runif (e$x1,-1,1)
points (e$x1, e$x2)
abline (0, 1)

e$f$slope <- 1
e$f$intercept <- 0 
e$training <-  sign (-e$x2 + (e$f$slope*e$x1 + e$f$intercept))
points (e$x1[e$training >0], e$x2[e$training >0], col = "blue")
points (e$x1[e$training <0], e$x2[e$training <0], col = "red")

fit <- lm (e$training ~ x1 + x2  , data = e)
fit2 <- lm (-e$training ~ x1 + x2  , data = e)
fit$coefficients
fit2$coefficients

abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 
abline (fit$coefficients[1], fit$coefficients[3], col = "blue") 

##########################################################################################
# classification y = 1 + x 

frame()
plot (c(-1, 1), c(-1, 1), type = "n")

e <- list ()
e$x1 <- runif (100,-1,1)
e$x2 <- runif (e$x1,-1,1)
points (e$x1, e$x2)
abline (1, 1)

e$f$slope <- 1
e$f$intercept <- 1 
e$training <-  sign (-e$x2 + (e$f$slope*e$x1 + e$f$intercept))
points (e$x1[e$training >0], e$x2[e$training >0], col = "blue")
points (e$x1[e$training <0], e$x2[e$training <0], col = "red")

fit <- lm (e$training ~ x1 + x2  , data = e)
abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 

##########################################################################################
# classification y = 3 + 7x

frame()
plot (c(-1, 1), c(-1, 1), type = "n")

e <- list ()
e$x1 <- runif (100,-1,1)
e$x2 <- runif (e$x1,-1,1)
points (e$x1, e$x2)
abline (3, 7)

e$f$slope <- 7
e$f$intercept <- 3 
e$training <-  sign (-e$x2 + (e$f$slope*e$x1 + e$f$intercept))
points (e$x1[e$training >0], e$x2[e$training >0], col = "blue")
points (e$x1[e$training <0], e$x2[e$training <0], col = "red")

fit <- lm (e$training ~ x1 + x2  , data = e)
abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 


##############################################################################
#3d scatterplot
x <- c(1,2,3,4,5)
y <- c(2,4,6,8,10)
z <- c(100,240,480,580,880)

#install.packages("scatterplot3d")
library(scatterplot3d) 



e <- list ()
e$x1 <- runif (100,-1,1)
e$x2 <- runif (e$x1,-1,1)
points (e$x1, e$x2)
abline (3, 7)

e$f$slope <- 7
e$f$intercept <- 3 
e$training <-  sign (-e$x2 + (e$f$slope*e$x1 + e$f$intercept))
points (e$x1[e$training >0], e$x2[e$training >0], col = "blue")
points (e$x1[e$training <0], e$x2[e$training <0], col = "red")

fit <- lm (e$training ~ x1 + x2  , data = e)
abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 
 
x<- e$x1
y<- e$x2
z<-e$training 
s3d <-scatterplot3d(x,y,z, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot")
  
#######################################################################

# 3D Plot

open3d()
x <- rnorm(100)
y <- rnorm(100)
z <- 0.2*x - 0.3*y + rnorm(100, sd=0.3)
fit <- lm(z ~ x + y)
plot3d(x,y,z, type="s", col="red", size=1)
coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha=0.5)
open3d()
plot3d(x,y,z, type="s", col="red", size=1)
#clipplanes3d(a, b, c, d)

###########################################################################

slope = 1
intercept = 0
frame()
plot (c(-1, 1), c(-1, 1), type = "n")
e <- list ()
e$x1 <- runif (100,-1,1)
e$x2 <- runif (e$x1,-1,1)
points (e$x1, e$x2)
abline (intercept, slope)

e$f$slope <- slope
e$f$intercept <- intercept 
e$training <-  sign (-e$x2 + (e$f$slope*e$x1 + e$f$intercept))
points (e$x1[e$training >0], e$x2[e$training >0], col = "blue")
points (e$x1[e$training <0], e$x2[e$training <0], col = "red")

fit <- lm (e$training ~ x1 + x2  , data = e)
abline (fit, col = "red")
abline (fit$coefficients[1], fit$coefficients[2], col = "green") 

open3d()
x <- e$x1
y <- e$x2
z <- sign(-e$x2 + (e$f$slope*e$x1 + e$f$intercept))
#z <- e$training
fit <- lm(z ~ x + y)
plot3d(x,y,z, type="s", col="red", size=1)
coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha=0.5)
open3d()
plot3d(x,y,z, type="s", col="red", size=1)
  
  
  }
