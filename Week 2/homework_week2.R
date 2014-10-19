
setwd("C:/Users/Tom/Desktop/CALTEC Machine learning/Week 2")

printf <- function(...)print(sprintf(...))




################### LINEAR REGRESSION ##############################################

lineFromPoints <- function (x, y) {
	r <- c()
	r.slope <-(y[2] - y[1])/(x[2] - x [1])
	r.intercept <- y[1] - r.slope * x[1]
	r
}

drawLine <- function (slope, intercept, c) {
	newx <- c(-1, 1)
	newy <- slope*newx + intercept
	lines (newx, newy, col=c)
}

refresh <- function (p, silent=TRUE , new=FALSE) {
	frame()
	plot (c(-1, 1), c(-1, 1), type = "n")

	points (p$x1 [p$clfn > 0] , p$x2[p$clfn > 0], col = "blue")
	points (p$x1 [p$clfn < 0] , p$x2[p$clfn < 0], col = "red")
	drawLine(p$fa, p$fb, "blue")	
	if (!silent) {printf ("clfn") ; print (p$clfn) }
	
	if (!new && ! is.null (p$lm_clfn)) {
		if (!silent) {printf ("lm_clfn") ; print (p$lm_clfn)} 
		drawLine (p$g$slope, p$g$intercept, "red")
		###  good training points, incorrectly classified in lm
		points (p$x1 [p$clfn > 0 & p$lm_clfn < 0 ] , p$x2[p$clfn > 0 & p$lm_clfn < 0], col = "blue", pch = "-")
		### bad training points, incorrectly classified in lm
		points (p$x1 [p$clfn < 0 & p$lm_clfn > 0 ] , p$x2[p$clfn < 0 & p$lm_clfn > 0], col = "red", pch = "-")	
	
		}

		if (new && ! is.null (p$lm_clfn2)) {
		if (!silent) {printf ("lm_clfn") ; print (p$lm_clfn2)} 
		drawLine (p$g$slope, p$g$intercept, "red")
		###  good training points, incorrectly classified in lm
		points (p$x1 [p$clfn > 0 & p$lm_clfn2 < 0 ] , p$x2[p$clfn > 0 & p$lm_clfn2 < 0], col = "blue", pch = "-")
		### bad training points, incorrectly classified in lm
		points (p$x1 [p$clfn < 0 & p$lm_clfn2 > 0 ] , p$x2[p$clfn < 0 & p$lm_clfn2 > 0], col = "red", pch = "-")	
	
		}

	if (! is.null (p$p_clfn)) {
		if (!silent) {
			printf ("p_clfn") ; print (p$p_clfn) 
			printf ("p_match") ; print (p$p_match)} 
		drawLine (p$pa, p$pb, "green")
		###  good training points, incorrectly classified in perceptron
		points (p$x1 [p$clfn > 0 & p$p_clfn < 0 ] , p$x2[p$clfn > 0 & p$p_clfn < 0], col = "blue", pch = "x")
		### bad training points, incorrectly classified in perceptron
		points (p$x1 [p$clfn < 0 & p$p_clfn > 0 ] , p$x2[p$clfn < 0 & p$p_clfn > 0], col = "red", pch = "x")	
	}
	
}

getTestData <- function (N=10) {
	fx <- runif (2, -1, 1)
	fy <- runif (fx, -1, 1)
	ab <- lineFromPoints (fx, fy)
	fa <- ab[1]
	fb <- ab[2]

	x <- runif (N,-1,1)
	y <- runif (x,-1,1)
	clfn <- sign(y - (fa*x +fb))

	w <- LM$coefficients	
	l <- list(fa, fb, x, y, clfn)
	names (l) <- c("fa", "fb", "x1", "x2", "clfn")
	l
}

updateWeights <- function (p) {

	p$p_match <- p$p_clfn * p$clfn
	l<-length (p$p_match[p$p_match < 0 ])
	if (l>0) {
#		i = round (runif (1, 0, l)+0.51)
		i <- sample (1:l, 1) 
		p$wx1 <- p$wx1 + p$x1[p$p_match < 0] [i] * p$clfn[p$p_match < 0][i]
		p$wx2 <- p$wx2 + p$x2[p$p_match < 0] [i] * p$clfn[p$p_match < 0][i]
		p$wx3 <- p$wx3 + 1 * p$clfn[p$p_match < 0][i]

		ab<-lineFromWeights(c(p$wx3, p$wx1, p$wx2))
		p$pa <- ab[1]
		p$pb <- ab[2]
	 }
	 p
}

initPerceptron <- function (p) {
	p$p_clfn <- p$lm_clfn
	p$p_match <- p$p_clfn * p$clfn
	p$wx1 <-  p$lm$coefficients[2]
	p$wx2 <- p$lm$coefficients[3]
	p$wx3 <- p$lm$coefficients[1]
	
	ab<-lineFromWeights(c(p$wx3, p$wx1, p$wx2))
	p$pa <- ab[1]
	p$pb <- ab[2]
	p
}

calculateClassification <- function(p) {
	p$p_clfn <- sign (p$x1 * p$wx1 + p$x2 * p$wx2 + p$wx3)
	#make sure 0's are counted as mismatches
	p$p_clfn <- sign( p$p_clfn - 0.5 )
	p$p_match <- p$p_clfn * p$clfn
	p
 }

lineFromWeights <- function (w) {
#expecting (wx3, wx1, wx2) from lm coefficients
	if (w[3] == 0 ){
		warning ( "horizontal line!  salt it a little bit")
		w[3] <- 1/1000000
	}

	c(-w[2]/w[3],-w[1]/w[3])
}


	
plot8 <- function (e) {
	frame()
	plot (c(-1, 1), c(-1, 1), type = "n")
#	curve (sqrt(abs(0.6-x*x) ))

	points (e$x1[e$training > 0], e$x2[e$training > 0], col = "blue", pch = "o")
	points (e$x1[e$training <= 0], e$x2[e$training <= 0], col = "yellow", pch = "o")
	#points (e$x1[e$flip], e$x2[e$flip], pch = "x")

	if (! is.null (e$lm)) {
#		if (!silent) {printf ("lm_clfn") ; print (p$lm_clfn)} 
		coefs <- coef(e$lm)
		slope <- -coefs["x1"]/coefs["x2"]
		intercept <- -coefs["(Intercept)"]/coefs["x2"]

		drawLine (slope, intercept, "red")
	}

	if (! is.null (e$lm_clfn)){
	points (e$x1[e$lm_clfn != e$training], e$x2[e$lm_clfn != e$training], col = "red", pch = "x")
#	points (e$x1[e$lm_clfn <= 0 & e$lm_clfn != e$training], e$x2[e$lm_clfn <= 0& e$lm_clfn != e$training], col = "red", pch = "x")
	}
	
	###  good training points, incorrectly classified in lm
#		points (p$x1 [p$clfn > 0 & p$lm_clfn < 0 ] , p$x2[p$clfn > 0 & p$lm_clfn < 0], col = "blue", pch = "-")
		### bad training points, incorrectly classified in lm
#		points (p$x1 [p$clfn < 0 & p$lm_clfn > 0 ] , p$x2[p$clfn < 0 & p$lm_clfn > 0], col = "red", pch = "-")	
	
#		}

}

#targetf8 <- function (x1, x2) {
#	sign ( x1 + x2 -0.6)
#}

targetf8 <- function (x1, x2) {
	sign ( x1*x1 + x2*x2 -0.6)
}


setupEx8 <- function (N=1000) {
	e <- list();
	e$x1 <- runif (N,-1,1)
	e$x2 <- runif (e$x1,-1,1)

	e$f <- targetf8(e$x1, e$x2)
	flip <- sample (1:N, N/10)
	e$training <- e$f
	e$training [flip] <- - e$training [flip]
	e
}


ex8 <- function (N=10) {

	loops <- c()
	for (n in 1:N) {
		e <- setupEx8()
		plot8(e)
		
		e$lm <- lm(training ~ x1+ x2, data=e)
		coefs <- coef(e$lm)
		e$lm_clfn <- sign(coefs["(Intercept)"] + coefs["x1"]*e$x1 + coefs["x2"]*e$x2)
		plot8(e)
		
		e$Ein <- sum(e$lm_clfn != e$training)/length(e$training)
		
		# miscl <- sum(e$lm_clfn != e$training)
		# positive <- sum (e$lm_clfn == e$training & e$lm_clfn > 0)
		# negative <- sum (e$lm_clfn == e$training & e$lm_clfn <= 0)
		# print (miscl)
		# print (positive)
		# print (negative)
		# print (miscl + positive+negative )		
		#e$Ein <- sum(e$lm_clfn != e$f)/length(e$f)
		loops <- c(loops,e$Ein)
	}
	loops
 # EX8 <- ex8(1000)
# > summary (EX8)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.1300  0.1490  0.1540  0.1545  0.1600  0.1820 
}	


###############################  EX 9   #######################################################################



targetf9 <- function (x1, x2) {
	sign ( x1*x1 + x2*x2 -0.6)
}


setupEx9 <- function (N=1000) {
	e <- list();
	e$x1 <- runif (N,-1,1)
	e$x2 <- runif (e$x1,-1,1)

	e$f <- targetf9(e$x1, e$x2)
	flip <- sample (1:N, N/10)
	e$training <- e$f
	e$training [flip] <- - e$training [flip]
	e
}

transformEx9 <- function (e) {
	e$x1x2 <- e$x1 * e$x2
	e$x1x1 <- e$x1 * e$x1
	e$x2x2 <- e$x2 * e$x2
	e
}

testex9 <- function (x1, x2, coefs) {
	
	sign(coefs["(Intercept)"] 
		+ coefs["x1"]*x1 
		+ coefs["x2"]*x2
		+ coefs["x1x1"]*x1*x1
		+ coefs["x1x2"]*x1*x2
		+ coefs["x2x2"]*x2*x2)
}

ex9_a <- function () {
	coefs <- c()
	coefs["(Intercept)"] <- -1 
	coefs["x1"] <- -0.05
	coefs["x2"] <- 0.08
	coefs["x1x2"] <- 0.13
	coefs["x1x1"] <- 1.5
	coefs["x2x2"] <-1.5
	coefs
}


ex9_b <- function () {
	coefs <- c()
	coefs["(Intercept)"] <- -1 
	coefs["x1"] <- -0.05
	coefs["x2"] <- 0.08
	coefs["x1x2"] <- 0.13
	coefs["x1x1"] <- 1.5
	coefs["x2x2"] <-15
	coefs
}

ex9_c <- function () {
	coefs <- c()
	coefs["(Intercept)"] <- -1 
	coefs["x1"] <- -0.05
	coefs["x2"] <- 0.08
	coefs["x1x2"] <- 0.13
	coefs["x1x1"] <- 15
	coefs["x2x2"] <-1.5
	coefs
}

ex9_d <- function () {
	coefs <- c()
	coefs["(Intercept)"] <- -1 
	coefs["x1"] <- -1.05
	coefs["x2"] <- 0.08
	coefs["x1x2"] <- 0.13
	coefs["x1x1"] <- 0.05
	coefs["x2x2"] <-0.05
	coefs
}

ex9_e <- function () {
	coefs <- c()
	coefs["(Intercept)"] <- -1 
	coefs["x1"] <- -0.05
	coefs["x2"] <- 0.08
	coefs["x1x2"] <- 1.5
	coefs["x1x1"] <- 0.15
	coefs["x2x2"] <-0.15
	coefs
}

ex9 <- function (N=10) {

	loops <- c(a=c(),b=c(),c=c(),d=c(),e=c())	
	for (n in 1:N) {


		e <- setupEx9()
		e <- transformEx9(e)
		
		e$lm <- lm(training ~ x1+ x2 + x1x2 + x1x1 + x2x2, data=e)
		coefs <- coefs + coef(e$lm)
		
		#take a bunch of random sample points
		e$sx1 <- runif (1000,-1,1)
		e$sx2 <- runif (e$sx1,-1,1)

		baseline <- testex9 (e$sx1, e$sx2, coefs)
		a <- testex9 (e$sx1, e$sx2, ex9_a())
		b <- testex9 (e$sx1, e$sx2, ex9_b())
		c <- testex9 (e$sx1, e$sx2, ex9_c())
		d <- testex9 (e$sx1, e$sx2, ex9_d())
		e <- testex9 (e$sx1, e$sx2, ex9_e())

		loops$a <- c(loops$a, sum (baseline == a))
		loops$b <- c(loops$b, sum (baseline == b))
		loops$c <- c(loops$c, sum (baseline == c))	
		loops$d <- c(loops$d, sum (baseline == d))
		loops$e <- c(loops$e, sum (baseline == e))
	}		
	
	loops
}	

# EX9 <- ex9(1000)
# > sort(sapply (EX9,mean))
      # c       b       d       e       a 
# 485.775 574.242 584.444 596.516 609.884 


##########################################################################################################################


targetf10 <- function (x1, x2) {
	sign ( x1*x1 + x2*x2 -0.6)
}

setupEx10 <- function (N=1000) {
	e <- list();
	e$x1 <- runif (N,-1,1)
	e$x2 <- runif (e$x1,-1,1)

	e$f <- targetf10(e$x1, e$x2)
	flip <- sample (1:N, N/10)
	e$training <- e$f
	e$training [flip] <- - e$training [flip]
	e
}

transformEx10 <- function (e) {
	e$x1x2 <- e$x1 * e$x2
	e$x1x1 <- e$x1 * e$x1
	e$x2x2 <- e$x2 * e$x2
	e
}

testex10 <- function (x1, x2, coefs) {
	
	sign(coefs["(Intercept)"] 
		+ coefs["x1"]*x1 
		+ coefs["x2"]*x2
		+ coefs["x1x1"]*x1*x1
		+ coefs["x1x2"]*x1*x2
		+ coefs["x2x2"]*x2*x2)
}

p10 <- function (e) {
	frame()
	plot (c(-1, 1), c(-1, 1), type = "n")

	points (e$x1[e$training > 0], e$x2[e$training > 0], col = "blue", pch = "o")
	points (e$x1[e$training <= 0], e$x2[e$training <= 0], col = "yellow", pch = "o")

	if (! is.null (e$lm_clfn)){
		points (e$sx1[e$lm_clfn != e$validate], e$sx2[e$lm_clfn != e$validate], col = "red", pch = "x")
	}
}

ex10 <- function (N=10) {

	loops <- c()
	for (n in 1:N) {
		e <- setupEx10()
		e <- transformEx10(e)
		
		e$lm <- lm(training ~ x1+ x2 + x1x2 + x1x1 + x2x2, data=e)
		coefs <- coefs + coef(e$lm)
		
		#take a bunch of random sample points
		SampleSize <- 1000
		e$sx1 <- runif (SampleSize,-1,1)
		e$sx2 <- runif (e$sx1,-1,1)

		e$lm_clfn <- testex10 (e$sx1, e$sx2, coefs)
		e$validate <- targetf10(e$sx1, e$sx2)

		flip <- sample (1:SampleSize, SampleSize/10)
		e$validate [flip] <- - e$validate [flip]

		Eout <- sum (e$lm_clfn != e$validate)/SampleSize
		loops <- c(loops, Eout)
	}		
	
	loops
}	

# EX10 <- ex10(1000)
# > 
# > summary (EX10)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.1060  0.1190  0.1220  0.1226  0.1260  0.1380 

 
 ##########################################################################################################################



ex7 <- function (n=10) {	
	loops <- c()
	for (n in 1:n) {
		p <- getTestData(10)
		p$lm <- lm(clfn ~ x1+ x2, data=p)
		ab<-lineFromWeights(p$lm$coefficients)
		p$ga <- ab[1]
		p$gb <- ab[2]
		p$lm_clfn <- sign(p$x2 - (p$ga*p$x1 +p$gb))		
		refresh(p)
		
		#run perceptron
		maxLoop <- 1000
		p$loopcounter <- 0
		
		p <- initPerceptron(p)
		refresh(p)

		while ( !is.na (match(-1,p$p_match)) && p$loopcounter < maxLoop)
		{
			 p$loopcounter <- p$loopcounter + 1
			 p <- updateWeights(p)
			 p <- calculateClassification(p)
			 refresh(p)
		 }
		
		loops <- c(loops, p$loopcounter)
		p$Ein <- sum(p$lm_clfn != p$clfn)/length(p$clfn)
		if (p$loopcounter == maxLoop) {
			warning("exceeded maxLoop")
			TROUBLE <<- c(TROUBLE, p)
			}
	}
	loops
}	

ex5 <- function (n=10) {	
	allEin <- c()
	allEout <- c()
	for (n in 1:n) {
		p <- getTestData(100)
		p$lm <- lm(clfn ~ x1+ x2, data=p)
		coefs <- coef(p$lm)
		p$g$slope <- -coefs["x1"]/coefs["x2"]
		p$g$intercept <- -coefs["(Intercept)"]/coefs["x2"]
		p$lm_clfn <- sign(coefs["(Intercept)"] + coefs["x1"]*p$x1 + coefs["x2"]*p$x2)
		p$Ein <- sum(p$lm_clfn != p$clfn)/length(p$clfn)
		
		allEin <- c(allEin, p$Ein)
	}
#	list (ein= allEin, eout=allEout)
	allEin
}	


oldex5 <- function (n=10) {	
	allEin <- c()
	allEout <- c()
	for (n in 1:n) {
		p <- getTestData(100)
		p$lm <- lm(clfn ~ x1+ x2, data=p)
		coefs <- coef(p$lm)
		p$lm_clfn <- sign(coefs["(Intercept)"] + coefs["x1"]*p$x1 + coefs["x2"]*p$x2)
		#if (0 != sum (p$lm_clfn != p$lm_clfn2)) {TROUBLE <<- p ; stop("mismatch!"); } 
		p$Ein <- sum(p$lm_clfn != p$clfn)/length(p$clfn)
		if (p$Ein > 0.8)  TROUBLE <<- c(TROUBLE, p)
		
		#ex6
		p$out_x1 <- runif (1000,-1,1)
		p$out_x2 <- runif (p$out_x1,-1,1)
		p$out_fclfn <- sign(p$out_x2 - (p$fa*p$out_x1 +p$fb))
		p$out_gclfn <- sign(p$out_x2 - (p$ga*p$out_x1 +p$gb))
		p$Eout <- sum(p$out_fclfn != p$out_gclfn)/length(p$out_fclfn)
		
		refresh(p)
		allEin <- c(allEin, p$Ein)
		allEout<- c(allEout, p$Eout)
	}
	list (ein= allEin, eout=allEout)
}	
######  result
# problem: with very steep lines, the regression line is good, but all points are misclassified

# > summary (EX5$ein)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.02000 0.03000 0.05824 0.06000 1.00000 
# > summary (EX5$ein[EX5$ein<0.8])
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.02000 0.03000 0.03983 0.05000 0.14000 
# > summary (EX5$eout)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00100 0.02800 0.04400 0.06754 0.06600 0.98400 
# > summary(EX5$eout[EX5$eout < 0.8])
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00100 0.02800 0.04300 0.04963 0.06500 0.18100 

# ex 5: [c] 0.01
# ex 6: [c] 0.01 ? [d]?


############################# EX 1 & 2 ############################################


ex1 <- function () {

	v_first <- c()
	v_rand <- c()
	v_min <- c()
	
	num_iterations = 100000
	rows=1000
	cols=10

		
	for (n in 1:num_iterations) {

		coins = matrix(sample(c(0,1),rows * cols, replace = TRUE, prob=c(0.5, 0.5)), ncol=cols)
		 
		first_row = coins[1,]

		crand_i = sample(1:dim(coins)[1], 1)
		rand_row = coins[crand_i,]

		sums = apply (coins, 1, sum)
		min_index = match (min (sums), sums)
		min_row = coins[min_index,]

		v_first = c(v_first, sum(first_row)/cols)
		v_rand = c(v_rand, sum(rand_row)/cols)
		v_min = c(v_min, sum(min_row)/cols)
	}

	print (summary(v_first))
	print(summary(v_rand))
	print(summary(v_min))


	v_first_error <- abs(mean(v_first) -0.5)
	v_rand_error <- abs(mean(v_rand) -0.5)
	v_min_error <- abs(mean(v_min) -0.5)

# ############ RESULT ################
# ex1()
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.0000  0.4000  0.5000  0.4992  0.6000  1.0000 
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.0000  0.4000  0.5000  0.4997  0.6000  1.0000 
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.00000 0.03771 0.10000 0.20000 	
	
	data.frame(v_first, v_rand, v_min)
	
	# 1:  [b] 0.01
	# 2:  [d] c1 and crand
}
		
		
		
		