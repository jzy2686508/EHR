# Generate covariates
Generate_covariates = function(Rep, Pop, num_cov){
  # first covariates: Age Second covariates: Gender Third covariates: race
  X = matrix(data = 0, nrow = Rep*Pop, ncol = num_cov)
  colnames(X) = c('Age', 'Gender', 'Race')
  X[,'Age'] <- runif(n=Rep*Pop, min=0, max=80)
  X[,'Gender'] <- rbinom(n=Rep*Pop, size=1, prob=0.5)
  X[,'Race'] <- sample(0:4, size=Rep*Pop, replace=TRUE, prob=c(.2,.2,.2,.2,.2))
  return(X)
}

# selection 
Selection = function(s1,s0, Y, Treat){
  temp = runif(n=length(Y))
  selection_index = vector(length=length(Y))
  selection_index[Y] = temp[Y] < s1
  selection_index[!Y] = temp[!Y] < s0
  return(selection_index)
}

Misclassification = function(p11,p10,Y){
  temp = runif(n=length(Y))
  Y_obs = vector(length=length(Y))
  Y_obs[Y] = temp[Y] < p11
  Y_obs[!Y] = temp[!Y] < p10
  return(Y_obs)
}