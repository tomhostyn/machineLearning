

#  calculate hoeffdinger
calcH <- function (epsilon, probability, M) {
  
  log(probability/(2*M)) / (-2 * epsilon*epsilon)
  
}


doExcercise <- function (M) {
  
  epsilon <- 0.05
  prob <- 0.03
  N <- calcH (0.05, 0.03, M)
 
  N <- sort ( c(500, 1000, 1500, 2000, N))
  probability <- round (2*M*exp (-2*epsilon*epsilon*N), 4)
  N [probs <0.03]
  
  data.frame (probability, N)
}


excercise1 <- function (){ 
  doExcercise(1)
}


excercise2 <- function (){ 
  doExcercise(10)
}


excercise3 <- function (){ 
  doExcercise(100)
}


# > excercise1()
# probability        N
# 1      0.1642  500.000
# 2      0.0300  839.941
# 3      0.0135 1000.000
# 4      0.0011 1500.000
# 5      0.0001 2000.000
# 
# [b] 1000

# > excercise2()
# probability        N
# 1      1.6417  500.000
# 2      0.1348 1000.000
# 3      0.0300 1300.458
# 4      0.0111 1500.000
# 5      0.0009 2000.000
# [c] 1500


# > excercise3()
# probability        N
# 1     16.4170  500.000
# 2      1.3476 1000.000
# 3      0.1106 1500.000
# 4      0.0300 1760.975
# 5      0.0091 2000.000
#  [d] 2000


# excercise 4 -- no code
#  [b]  5
# 
# 4 points --> no problem.  take 3 points in a plane, one above it. 
#               2^4 combinations possible using perceptron planes
# 5 points --> break.  take 3 points in a plane (all +)  one above, one below. (all -) 
#               this combo is impossible.  (move both up, then somehow another 3 points form a plane
#               and 2 others can cause the problem)


# Excercise 5 - no code
# 
# growth functions are either polynomial or 2^N

# Excercise 6 - got it wrong.



#excercise 10

combo <- function (n, k){
  factorial(n)/(factorial(k)*factorial(n-k))
  
}