# Generate covariates
Generate_covariates = function(Rep, Pop, num_cov){
  # first covariates: Age Second covariates: Gender Third covariates: race
  X = matrix(data = 0, nrow = Rep*Pop, ncol = num_cov)
  colnames(X) = c('Age', 'Gender', 'Race')
  X[,'Age'] <- runif(n=Rep*Pop, min=1, max=80)
  X[,'Gender'] <- rbinom(n=Rep*Pop, size=1, prob=0.5)
  X[,'Race'] <- sample(0:4, size=Rep*Pop, replace=TRUE, prob=c(.2,.2,.2,.2,.2))
  return(X)
}
# compute tau theoratically
compute_tau = function(beta, beta1, betaT){
  Expect_treatment = function(x){
    1 / (1 + exp( -(t(x) %*% beta2[-1] + beta2[1] + betaT) )) 
  }
  Expect_control = function(x){
    1 / (1 + exp( -(t(x) %*% beta2[-1] + beta2[1]) ))
  }
  F_treatment = function(x){
    1 / (1 + exp( -(X %*% beta[-1] + beta[1]) ))
  }
}

# selection 
Selection = function(s1,s0, Y, Treat){
  temp = runif(n=size(Y))
  selection_index = vector(length=size(Y))
  selection_index[Treat] = temp[Treat] < s1
  selection_index[!Treat] = temp[!Treat] < s0
  return(selection_index)
}
