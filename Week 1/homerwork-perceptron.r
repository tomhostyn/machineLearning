###  PERCEPTRON http://youtu.be/mbyG85GZ0PI?t=23m38s

lineFromPoints <- function (x, y) {
a <-(y[2] - y[1])/(x[2] - x [1])
b <- y[1] - a * x[1]

c(a,b)
}

colourTrainingSet <- function (x, y, map) {
points (x [map > 0] , y[map > 0], col = "blue")
points (x [map < 0] , y[map < 0], col = "red")
}

colourDataSetX <- function (x, y, map1, map2) {
###  good training points, correctly classified
points (x [map1 > 0 & map2 > 0 ] , y[map1 > 0 & map2 > 0], col = "blue")
### bad training points, correctly classified
points (x [map1 < 0 & map2 > 0 ] , y[map1 < 0 & map2 > 0], col = "red")
###  good training points, incorrectly classified
points (x [map1 > 0 & map2 < 0 ] , y[map1 > 0 & map2 < 0], col = "blue", pch = "x")
### bad training points, incorrectly classified
points (x [map1 < 0 & map2 < 0 ] , y[map1 < 0 & map2 < 0], col = "red", pch = "x")
}

lineFromPerceptorWeights <- function (w) {
 c(-w[1]/w[2], -w[3]/w[2])
}

runPerceptron <- function (w, x, y, s) {
 sign ( s * (x * w[1] + y * w[2] + w[3]) )
}

colourDataSet <- function (p) {
###  good training points, correctly classified
points (p$d$x [p$d$fit > 0 & p$c > 0 ] , p$d$y[p$d$fit > 0 & p$c > 0], col = "blue")
### bad training points, correctly classified
points (p$d$x [p$d$fit < 0 & p$c > 0 ] , p$d$y[p$d$fit < 0 & p$c > 0], col = "red")
###  good training points, incorrectly classified
points (p$d$x [p$d$fit > 0 & p$c < 0 ] , p$d$y[p$d$fit > 0 & p$c < 0], col = "blue", pch = "x")
### bad training points, incorrectly classified
points (p$d$x [p$d$fit < 0 & p$c < 0 ] , p$d$y[p$d$fit < 0 & p$c < 0], col = "red", pch = "x")
}


createDataSet <- function (N=10) {
 idealLine_x <- runif (2, -1, 1)
 idealLine_y <- runif (idealLine_x, -1, 1)
 xy <- lineFromPoints (idealLine_x, idealLine_y)

 TrainingSet_x <- runif (N,-1,1)
 TrainingSet_y <- runif (TrainingSet_x,-1,1)
 TrainingSet_map <- sign(TrainingSet_y - (xy[1]*TrainingSet_x +xy[2]))

 mydataSet <- list(TrainingSet_x, TrainingSet_y, TrainingSet_map, xy[1], xy[2])
 names(mydataSet) <- c(	"x",  #dataset x coordinates vector 
						"y",  #dataset y coordinates vector
						"fit", # dataset fit: 1 for fit, -1 for no fit
						"a", "b")  # y=ax+b line through dataset to define fit 
 mydataSet
}


createPerceptron <- function (dataSet) {
#	p <- list(0.5, 0.5, 0.5, dataSet, dataSet$fit* -1)
	p <- list(0, 0, 0, dataSet, dataSet$fit* -1)
	names(p) <- c("wx", "wy", "w0", #weights
					"d", #dataset 
					"c") #classification map
  p <- calculateClassification(p)
#	p$c <- sign ( p$d$fit * (p$d$x * p$wx + p$d$y * p$wy + p$w0))
	p
}

drawLine <- function (a, b, c) {
newx = c(-1, 1)
newy = a*newx + b
lines (newx, newy, col=c)
}

colourPerceptron <- function(p, c="red")
{
	pa <- p$wx/-p$wy
	pb <- p$w0/-p$wy
	drawLine(pa, pb, c)
} 

refresh <- function (p) {
frame()
plot (p$d$x, p$d$y)
colourTrainingSet (p$d$x, p$d$y, p$d$fit)
drawLine (p$d$a, p$d$b, "green")
colourPerceptron (p)
colourDataSet(p)
#colourDataSet (p$d$x, p$d$y, p$d$fit, p$c)

}

updateWeightsLINEAR_AND_SLOW <- function (p) {
if (length (p$d$x[p$c < 0 ])>0) {
  p$wx <- p$wx + p$d$x[p$c < 0] [1] * p$d$fit[p$c < 0][1]
  p$wy <- p$wy + p$d$y[p$c < 0] [1] * p$d$fit[p$c < 0][1]
  p$w0 <- p$w0  
 }
  p
}

calculateClassification <- function(p) {
# p$c <- sign ( p$d$fit * (p$d$x * p$wx + p$d$y * p$wy + p$w0))
 p$c <- sign ( p$d$fit * (p$d$x * p$wx + p$d$y * p$wy + p$w0))
 #make sure 0's are counted as mismatches
 p$c <- sign( p$c - 0.5 )
 p
 }


updateWeights <- function (p) {

l=length (p$d$x[p$c < 0 ])
if (l>0) {
	i = round (runif (1, 0, l)+0.51)
#points (p$d$x [p$c < 0][i] , p$d$y[p$c < 0][i], col = "purple", pch = "#") ; c(p$d$x [p$c < 0][i] , p$d$y[p$c < 0][i])
	p$wx <- p$wx + p$d$x[p$c < 0] [i] * p$d$fit[p$c < 0][i]
	p$wy <- p$wy + p$d$y[p$c < 0] [i] * p$d$fit[p$c < 0][i]
	p$w0 <- p$w0 + 1 * p$d$fit[p$c < 0][i]
 }
 p
}


testAccuracy <- function (p)
{
 sx <- runif (1000, -1, 1)
 sy <- runif (sx, -1, 1)
 p_fit <- sign (sx * p$wx + sy * p$wy + p$w0)
 d_fit <- sign(sy - (p$d$a*sx +p$d$b))
 
 length (sx [ p_fit != d_fit]) / length (sx)
}

learnPerceptron <- function (p, maxLoop = 1000) {

		loopcounter <- 0
		while ( length (p$d$x[p$c < 0 ]) > 0 && loopcounter < maxLoop)
		{
			  loopcounter <- loopcounter + 1
			  p <- updateWeights(p)
			  p <- calculateClassification(p)
			  #refresh(p)
		 }
	if (loopcounter == maxLoop) NULL else p
}


ex9 <- function(trainingSetSize=10, NrOfExperiments=10) {

	experimentNr <- 0
	maxExperiments <- NrOfExperiments
	failedExperiments <- 0
	results <- c()
	accuracy <- c() 
	
	while (experimentNr < maxExperiments)
	{
		p <- createPerceptron(createDataSet(trainingSetSize))
		#refresh(p);
		
		maxLoop = 100000
		loopcounter <- 0
		while ( length (p$d$x[p$c < 0 ]) > 0 && loopcounter < maxLoop)
		{
			  loopcounter <- loopcounter + 1
			  p <- updateWeights(p)
			  p <- calculateClassification(p)
			  #refresh(p)
		 }
	if (loopcounter == maxLoop) {
			failedExperiments <- failedExperiments +1
		} else {
#			results <- append (results, loopcounter)
			results <- append (results, loopcounter)
			accuracy <- append (accuracy, testAccuracy(p)) 
			experimentNr <- experimentNr +1
			if (0==experimentNr%%10) {
				print ("--------------------------")
				print(experimentNr); flush.console()
				print (" preliminary results: ")
				print (summary (results))
				print ("accuracy: ")
				print (summary (accuracy))
				print ("total/failed/rate")
				print (c(experimentNr, failedExperiments, failedExperiments/experimentNr))				
				}
		}
	}
	print ("************************************")
	print (" FINAL results: ")
	print (summary (results))
	print ("accuracy: ")
	print (summary (accuracy))
	print ("total/failed/rate")
	print (c(experimentNr, failedExperiments, failedExperiments/experimentNr))
	
	ex9 <- list(failedExperiments, results, accuracy)
	names(ex9) <- c("failed", "ïterations", "accuracy")
	ex9
}


runAllTests <- function() {
ex9 <- ex9(10, 1000)	
ex9 <- ex9(100, 1000)
}

simpleTest <- function () {
	p <- createPerceptron(createDataSet(trainingSetSize))
	refresh(p)
	
	sp <- learnPerceptron(p, maxLoop)
	refresh(sp)
	
}


