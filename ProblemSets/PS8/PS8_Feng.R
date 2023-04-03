# set the seed of the random number generator
set.seed(100)
# define dimentions
N <- 100000
K <- 10
#Generate X matrix
X <- matrix(rnorm(N * (K-1)), ncol = K-1)
X <- cbind(rep(1, N), X)

# Generate epsilon vector
eps <- rnorm(N,mean=0,sd=0.5)

#Define beta vector
beta <- c(1.5, -1, -0.25,0.75, 3.5, -2, 0.5, 1, 1.25, 2)

#Generate Y vector
Y <- X %*% beta + eps
# OLS estimate of beta
beta_hat_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat_OLS
# compare with the true value of beta
beta

# Gradient descent
learning_rate <- 0.0000003
beta_hat_GD <- rep(0, K)
for (i in 1:100000) {
  beta_hat_GD <- beta_hat_GD - learning_rate * t(X) %*% (X %*% beta_hat_GD - Y) / N
}
beta_hat_GD

# L-BFGS and Nelder-Mead algorithms using nloptr
library(nloptr)
nloptr_obj <- function(theta) {
  beta <- theta[1:(K)]
  sig <- theta[K+1]
  sum((Y - X %*% beta)^2) / (2*sig^2) + N/2*log(2*pi*sig^2)
}
theta_start <- c(beta, 0.5)
res_LBFGS <- nloptr(x0=theta_start, eval_f=nloptr_obj, eval_grad_f=gradient, 
                    lb=c(rep(-Inf, K), 0), ub=c(rep(Inf, K), Inf), 
                    opts=list("algorithm"="NLOPT_LN_BFGS", "xtol_rel"=1e-9))
beta_hat_LBFGS <- res_LBFGS$solution[1:K]
beta_hat_LBFGS

res_NM <- nloptr(x0=theta_start, eval_f=nloptr_obj, eval_grad_f=gradient, 
                 lb=c(rep(-Inf, K), 0), ub=c(rep(Inf, K), Inf), 
                 opts=list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1e-9))
beta_hat_NM <- res_NM$solution[1:K]
beta_hat_NM

# MLE using L-BFGS algorithm
nloptr_obj_MLE <- function(theta) {
  beta <- theta[1:(K)]
  sig <- theta[K+1]
  N/2*log(2*pi*sig^2) + sum((Y - X %*% beta)^2) / (2*sig^2)
}
theta_start_MLE <- c(beta, 0.5)
res_MLE <- nloptr(x0=theta_start_MLE, eval_f=nloptr_obj_MLE, eval_grad_f=gradient, 
                  lb=c(rep(-Inf, K), 0), ub=c(rep(Inf, K), Inf), 
                  opts=list("algorithm"="NLOPT_LN_BFGS", "xtol_rel"=1e-9))
beta_hat_MLE <- res_MLE$solution[1:K]
beta_hat_MLE



ols_model <- lm(Y ~ X - 1)

modelsummary(ols_model, output = "PS8_Feng.tex")

beta_true <- c(2, -1, 0.5)
beta_estimated <- coef(ols_model)
differences <- abs(beta_estimated - beta)


getwd()




