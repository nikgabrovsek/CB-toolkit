#A Bayesian approach to estimating a simple linear regression model
#If we proceed with ML estimator that delivers the familiar OLS estimator our estimates rely solely on information contained in the data.
#Bayesian approach enables us to incorporate prior beliefs into esimation of beta and sigma. 
#Prior beliefs are expressed in the form of a probability distribution. 


DATA <- read.csv("DATA.csv")

#Define data as Y
Y <- DATA[,3]
Y <- as.matrix(Y)
#Define number of obs
TT <- nrow(Y)

#Create lags
X <- mlag(Y, 2)

#Add constant
X <- cbind(X, 1)

#Get rid of first p observations
Y <- Y[3:224,]
X <- X[3:224,]

# Plot data

plot(Y, type = "l", main = "CPI in US", ylab = "YoY", xlab = "Time")




#Set priors and starting values
#Priors for B
B0 <- matrix(0, 3, 1)
Sigma0 <- matrix(0, 3, 3)
diag(Sigma0) <- 1

#Priors for sigma2
T0 <- 1
D0 <- 0.1

#Starting values and total number of Gibbs iterations
B <- B0
sigma2 <- 1
reps <- 5000
burn <- 4000
total <- burn + reps
out1 <- matrix(NA, reps, 3, 1)
out2 <- matrix(NA, reps, 1, 1)

for (i in 1:total) {
  #Sample B conditional on sigma N(M*, V*)
  sigma2 <- as.numeric(sigma2)
  
  M <- solve(solve(Sigma0) + (1/sigma2) * t(X) %*% X) %*% (solve(Sigma0) %*% B0 + (1/sigma2) * t(X) %*% Y)
  
  V <- solve(solve(Sigma0) + (1/sigma2) * (t(X) %*% X))

  
  B <- M + t(matrix(rnorm(3,1), 1, 3) %*% chol(V))

  
  #Sample sigma2 conditional on B from IG(T1, D1); 
  #Compute residuals 
  resids <- Y - X %*% B
  T1 = T0 + TT
  D1 <- D0 + t(resids) %*% resids
  
  #Draw from IG
  z0 <- rnorm(T1,1)
  z0z0 <- t(z0) %*% z0
  sigma2 <- D1 / z0z0
  
  if(i > burn){
    out1[i-burn,] <- t(B) 
    out2[i-burn,] <- sigma2
  }
}

# Plot marginal posterior distributions

hist(out1[,1], main = "Constant", xlab = "") #constant
hist(out1[,2], main = "AR(1)", xlab = "") #AR1
hist(out1[,3], main = "AR(2)", xlab = "") #AR2
hist(out2[,1], main = "Sigma", xlab = "") # Sigma


# Mean of the marginal posterior distribution of B 

MB1 <- mean(out1[,1])
MB2 <- mean(out1[,2])
MB3 <- mean(out1[,3])


# Standard error

VB <- sd(out2[,1])


# Error bands 

EB1 <- quantile(out1[,1], c(0.05, 0.95))
EB2 <- quantile(out1[,2], c(0.05, 0.95))
EB3 <- quantile(out1[,3], c(0.05, 0.95))


# Try in-sample fit of the model 

Yfit <- X %*% B
plot(Y, type = "l")
lines(Yfit, col = 2)











