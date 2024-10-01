# Create a toy data example
p = 200
n = 50

# Generate matrix of covariates
library(mnormt)
set.seed(23945)
Sigma = matrix(0.7, p, p) + diag(rep(0.3, p)) # covariance matrix for covariates
X = rmnorm(n, varcov = Sigma)

# Generate response
beta0 = 2 # intercept
sigma = 0.7 # noise standard deviation
beta = runif(p, min = -2, max = 2)
Y = beta0 + X %*% beta + rnorm(n, sd = sigma)


# Split the data into K = 2 folds
# [ToDo] Create idfold vector of length n indicating the fold id, should have 1 or 2 in each position
K = 2
# idfolds = sample(K, size = n, replace = TRUE) # non-equal fold sizes, head(idfolds), table(idfolds)
idfolds = sample(rep(1:K, length.out=n), size=n) # no replacement

# For loop over folds and lambdas
##########################################
lambda_seq = seq(0.01, 10, length.out = 20)
nlambda = length(lambda_seq)
cvm = rep(NA, nlambda) # want to have CV(lambda)
cvse = rep(NA, nlambda) # want to have SE_CV(lambda)
# Feel free to store additional things
# K = 7
cv_folds = matrix(NA, K, nlambda) # fold-specific errors


idfold= sample(rep(1:K, length.out = n), size = n)
for (fold in 1:K){
  #[ToDo] Create training data xtrain and ytrain, everything except fold
  xtrain = X[idfolds != fold, ]
  ytrain = Y[idfolds != fold]
  #[ToDo] Create testing data xtest and ytest, everything in fold
  xtest = X[idfolds == fold, ]
  ytest = Y[idfolds == fold]
  
  #[ToDo] Center training data 
  meanY = mean(ytrain)
  ytrain_centered = ytrain - meanY
  
  xtrain_centered = scale(xtrain, scale = FALSE) # attributes(), ?scale
  meanX = colMeans(xtrain)
  ntrain = nrow(xtrain)
  #[ToDo] For loop over lambdas
  for (i in 1:length(lambda_seq)){
    # [ToDo] Calculate ridge solution
    beta = solve(t(xtrain_centered)%*%xtrain_centered/ntrain + lambda_seq[i]*diag(p), t(xtrain_centered)%*%ytrain_centered/ntrain)
    
    # Get back the intercept: beta0 = barY - bar X^{top}beta
    beta0 = as.numeric(meanY - crossprod(meanX, beta))
    # [ToDo] Complete with anything else you need for cvm and cvse
    cv_folds[fold, i] = mean((ytest - beta0 - xtest %*% beta)^2)
    }
}

# [ToDo] get cvm and cvse from cv_folds
cvm = colMeans(cv_folds)
cvse = apply(cv_folds, 2, FUN = sd)

# Plot the output from above
plot(lambda_seq, cvm, col = "red", ylim = c(40, 140))
lines(lambda_seq, cvm + cvse)
lines(lambda_seq, cvm - cvse)
