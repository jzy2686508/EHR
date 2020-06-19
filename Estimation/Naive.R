source("Data_generation.R")

# Naive estimation(IPW)

Naiveest = function(DT_onecopy){
  # find selected samples
  sDT = DT_onecopy[DT_onecopy$Select_index,]
  # estimate propensity score
  y1 = mean(sDT$Y_obs[sDT$Treat])
  y0 = mean(sDT$Y_obs[!sDT$Treat])
  tau_hat = y1-y0
}
