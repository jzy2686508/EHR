source("Data_generation.R")

Mis_selection_est = function(DT_onecopy,para){
  # known p11 and p10
  p11 = para$p11
  p10 = para$p10
  # known s1 and s0
  s1 = para$s1
  s0 = para$s0
  
  # find selected samples
  sDT = DT_onecopy[DT_onecopy$Select_index,]
  # estimate propensity score
  Model.prop = glm(Treat~Age+Gender+Race, family='binomial', data=sDT)
  Prop_predicted = Model.prop$fitted.values
  Weighted_y1 = sDT$Y_obs[sDT$Treat] / Prop_predicted[sDT$Treat]
  Weighted_y0 = sDT$Y_obs[!sDT$Treat] / (1 - Prop_predicted[!sDT$Treat])
  A = (Weighted_y1 - p10) / (s1 * (p11-p10))
  B = (Weighted_y0 - p10) / (s1 * (p11-p10))
  est_1 = A*s0 / (1+(s0-s1)*A)
  est_0 = B*s0 / (1+(s0-s1)*A)
  tau = est_1 - est_0 # Here assumed uniform distribution of all covariates.
  return(tau)
}

Results.misclass_select = sapply(DT,FUN=Misclassificationest, para=para)
