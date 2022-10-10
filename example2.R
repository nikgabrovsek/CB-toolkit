# Gibbs sampling for a linear regression and forecasting
# We use same model as in example1, however forecasts of inflation are added to the code. 

DATA <- read.csv("DATA.csv")

#Define data as Y
Y <- DATA[,3]
Y <- as.matrix(Y)
#Define number of obs
TT <- nrow(Y)

#Create lags
X <- mlag(Y, 2)

#Add constant
X <- cbind(1,X)

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
forecast <- 14
total <- burn + reps
out1 <- matrix(NA, reps, 3, 1)
out2 <- matrix(NA, reps, 1, 1)
out3 <- array(NA, dim = c(reps, 1, length(Y)+forecast))


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
    
    #Compute forecasts for 2 years ahead
    yhat <- matrix(data = 0, nrow = 14, ncol = 1)
    yhat[1:2,] <- Y[c(length(Y)-1,length(Y))] #starting values
    cfactor <- sqrt(sigma2) #standard deviation of the shocks
    for(m in 3:14){
      yhat[m] <- matrix(data = c(1, yhat[m-1], yhat[m-2]), nrow = 1, ncol = 3) %*% B + rnorm(1,0)*cfactor
    }
    
    yhat <- as.numeric(yhat)
    
    out3[i-burn,,] <- c(Y,yhat)
    
  }
}


# Plot forecasts

median_forecast <- apply(out3, 3, mean)
low_interval <- apply(out3, 3, quantile, 0.16)
high_interval <- apply(out3, 3, quantile, 0.84)


plot(median_forecast, type = "l", col = 3)
lines(low_interval, col = 2)
lines(high_interval, col = 1)











