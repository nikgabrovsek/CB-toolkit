# Bayesian estimation of linear regression with serial correlation in the residuals. 
# an AR2 model for US inflation with autocorrelated AR(1) disturbances

# First set up a file path 
setwd("~/Desktop/CB toolkit tutorial/CB-toolkit")

# Load data 
Y <- read.csv("DATA.csv")
Y <- Y[, 3]
TT <- NROW(Y)

# Create RHS matrix
plag <- 2
X <- mlag(Y, plag)


# Remove missing observations 
Y <- Y[plag+1:NROW(Y)]
X <- X[plag+1:NROW(Y)]


# Set priors and starting values
# Priors for B and var-cov
B0 <- matrix(0, 3, 1)
Sigma0 <- matrix(0, 3, 3)
diag(Sigma0) <- 1

# Priors for sigma2
T0 <- 1
D0 <- 0.1

# Prior for rho 
rho0 <- 0

# Starting values
B <- B0
rho <- rho0
sigma2 <- 1
reps <- 15000
burn <- 12000
forecast <- 14
total <- burn + reps
out1 <- matrix(NA, reps, 3, 1)
out2 <- matrix(NA, reps, 1, 1)
out3 <- array(NA, dim = c(reps, 1, length(Y)+forecast))






















