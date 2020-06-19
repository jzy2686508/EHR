source("Func_datageneration.R")
# Set size
Rep = 1000
Pop = 5000
num_cov = 3
# set true treatment assignment mechanism(intercept, X[,1],X[,2],X[,3])
beta = c(-1,0.01,0.2,0.1)
# set true outcome parameters(intercept, X[,1], X[,2], X[,3])
beta2 = c(-1,0.02,0.1,0.05)
betaT = 0.2

# Original X
X = Generate_covariates(Rep=Rep, Pop=Pop, num_cov=num_cov)
# Compute prpensity score
Prop_true = 1 / (1 + exp( -(X %*% beta[-1] + beta[1]) ))
Treat = (runif(n=Rep*Pop, min=0, max=1)<Prop_true)
# Compute outcome
P_positive_true = 1 / (1 + exp( -(X %*% beta2[-1] + beta2[1] + Treat*betaT) ))
Y = (runif(n=Rep*Pop, min=0, max=1)<P_positive_true)
# compute montecarlo treatment effect
tau_montecarlo = mean(Y[Treat] / Prop_true[Treat]) - mean(Y[!Treat] / Prop_true[!Treat])

# selection probability
s1 = 0.9
s0 = 0.7
Select_index = Selection()

