# source("Data_generation.R")

Misclassificationest = function(DT_onecopy,para){
  # known p11 and p10
  p11 = para$p11
  p10 = para$p10

  # find selected samples
  sDT = DT_onecopy[DT_onecopy$Select_index,]
  # estimate propensity score
  Model.prop = glm(Treat~Age+Gender+Race, family='binomial', data=sDT)
  Prop_predicted = Model.prop$fitted.values
  Weighted_y1 = sDT$Y_obs[sDT$Treat] / Prop_predicted[sDT$Treat]
  Weighted_y0 = sDT$Y_obs[!sDT$Treat] / (1 - Prop_predicted[!sDT$Treat])
  tau = (sum(Weighted_y1) - sum(Weighted_y0)) / nrow(sDT) / (p11-p10)
  return(tau)
}

# Results.misclass = sapply(DT,FUN=Misclassificationest, para=para)
# mean(Results.misclass)
