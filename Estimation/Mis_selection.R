# source("Data_generation.R")

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
  Prop_predicted[!sDT$Treat] = 1 - Prop_predicted[!sDT$Treat]
  Weighted_y1 = vector(mode='numeric', length=nrow(sDT))
  Weighted_y0 = vector(mode='numeric', length=nrow(sDT))
  Weighted_y1 = sDT$Y_obs * sDT$Treat / Prop_predicted
  Weighted_y0 = sDT$Y_obs * (1 - sDT$Treat) / Prop_predicted

  
  # break into group and compute expectation.
  breaknum_Age = 1
  breaknum_Sex = length(table(sDT$Gender))
  breaknum_Race = length(table(sDT$Race))
  # Group_dict_s = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  # Group_dict = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  Group_est_treat = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  Group_est_control = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  # compute 
  # compute 
  Age_group_s = (sDT$Age %/% (80/breaknum_Age)) + 1
  Age_group = (DT_onecopy$Age %/% (80/breaknum_Age)) + 1
  Group_dict_s = table(Age_group_s,sDT$Gender,sDT$Race) / nrow(sDT)
  Group_dict = table(Age_group,DT_onecopy$Gender,DT_onecopy$Race) / nrow(DT_onecopy)
  for(i in 1:breaknum_Age){
    for(j in 1:breaknum_Sex){
      for(k in 1:breaknum_Race){
        Group_est_treat[i,j,k] = sum(Weighted_y1[Age_group_s==i & sDT$Gender==j-1 &sDT$Race==k-1])/ Group_dict_s[i,j,k] / nrow(sDT)
        Group_est_control[i,j,k] = sum(Weighted_y0[Age_group_s==i & sDT$Gender==j-1 &sDT$Race==k-1])/ Group_dict_s[i,j,k] / nrow(sDT)
        }
    }
  }
  AB_treat = (Group_est_treat - p10) / (s1 * (p11-p10))
  AB_control = (Group_est_control - p10) / (s1 * (p11-p10))
  est_1 = AB_treat*s0 / (1+(s0-s1)*AB_treat)
  est_0 = AB_control*s0 / (1+(s0-s1)*AB_control)
  tau = sum((est_1 - est_0)*Group_dict,na.rm=TRUE) # Here assumed uniform distribution of all covariates.
  return(tau)
}

Mis_selection_est_doublerobust = function(DT_onecopy,para){
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
  Prop_predicted[!sDT$Treat] = 1 - Prop_predicted[!sDT$Treat]
  Model.outcome = glm(Y_obs~Age+Gender+Race+Treat, family='binomial', data=sDT)
  # Outcome_predicted = Model.outcome$fitted.values
  Outcome_predicted_1 = predict(Model.outcome,data.frame(sDT[,1:3],Treat=T))
  Outcome_predicted_0 = predict(Model.outcome,data.frame(sDT[,1:3],Treat=F))
  Weighted_y1 = vector(mode='numeric', length=nrow(sDT))
  Weighted_y0 = vector(mode='numeric', length=nrow(sDT))
  Weighted_y1 = (sDT$Y_obs - Outcome_predicted_1) * sDT$Treat / Prop_predicted + Outcome_predicted_1
  Weighted_y0 = (sDT$Y_obs - Outcome_predicted_0) * (1 - sDT$Treat) / Prop_predicted + Outcome_predicted_0
  
  
  # break into group and compute expectation.
  breaknum_Age = 1
  breaknum_Sex = length(table(sDT$Gender))
  breaknum_Race = length(table(sDT$Race))
  # Group_dict_s = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  # Group_dict = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  Group_est_treat = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  Group_est_control = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  # compute 
  # compute 
  Age_group_s = (sDT$Age %/% (80/breaknum_Age)) + 1
  Age_group = (DT_onecopy$Age %/% (80/breaknum_Age)) + 1
  Group_dict_s = table(Age_group_s,sDT$Gender,sDT$Race) / nrow(sDT)
  Group_dict = table(Age_group,DT_onecopy$Gender,DT_onecopy$Race) / nrow(DT_onecopy)
  for(i in 1:breaknum_Age){
    for(j in 1:breaknum_Sex){
      for(k in 1:breaknum_Race){
        Group_est_treat[i,j,k] = sum(Weighted_y1[Age_group_s==i & sDT$Gender==j-1 &sDT$Race==k-1])/ Group_dict_s[i,j,k] / nrow(sDT)
        Group_est_control[i,j,k] = sum(Weighted_y0[Age_group_s==i & sDT$Gender==j-1 &sDT$Race==k-1])/ Group_dict_s[i,j,k] / nrow(sDT)
      }
    }
  }
  AB_treat = (Group_est_treat - p10) / (s1 * (p11-p10))
  AB_control = (Group_est_control - p10) / (s1 * (p11-p10))
  est_1 = AB_treat*s0 / (1+(s0-s1)*AB_treat)
  est_0 = AB_control*s0 / (1+(s0-s1)*AB_control)
  tau = sum((est_1 - est_0)*Group_dict,na.rm=TRUE) # Here assumed uniform distribution of all covariates.
  return(tau)
}

Mis_selection_est_parametric = function(DT_onecopy,para){
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
  Prop_predicted[!sDT$Treat] = 1 - Prop_predicted[!sDT$Treat]
  Model.outcome = glm(Y_obs~Treat+Age+Gender+Race, family='binomial', data=sDT)
  Outcome_predicted = Model.outcome$fitted.values
  Weighted_y1 = vector(mode='numeric', length=nrow(sDT))
  Weighted_y0 = vector(mode='numeric', length=nrow(sDT))
  Weighted_y1 = Outcome_predicted
  Weighted_y0 = Outcome_predicted
  
  
  # break into group and compute expectation.
  breaknum_Age = 10
  breaknum_Sex = length(table(sDT$Gender))
  breaknum_Race = length(table(sDT$Race))
  # Group_dict_s = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  # Group_dict = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  Group_est_treat = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  Group_est_control = array(dim=c(breaknum_Age,breaknum_Sex,breaknum_Race))
  # compute 
  # compute 
  Age_group_s = (sDT$Age %/% (80/breaknum_Age)) + 1
  Age_group = (DT_onecopy$Age %/% (80/breaknum_Age)) + 1
  Group_dict_s = table(Age_group_s,sDT$Gender,sDT$Race) / nrow(sDT)
  Group_dict = table(Age_group,DT_onecopy$Gender,DT_onecopy$Race) / nrow(DT_onecopy)
  for(i in 1:breaknum_Age){
    for(j in 1:breaknum_Sex){
      for(k in 1:breaknum_Race){
        Group_est_treat[i,j,k] = mean(Weighted_y1[Age_group_s==i & sDT$Gender==j-1 &sDT$Race==k-1 & sDT$Treat==1])
        Group_est_control[i,j,k] = mean(Weighted_y0[Age_group_s==i & sDT$Gender==j-1 &sDT$Race==k-1 & sDT$Treat==0])
      }
    }
  }
  AB_treat = (Group_est_treat - p10) / (s1 * (p11-p10))
  AB_control = (Group_est_control - p10) / (s1 * (p11-p10))
  est_1 = AB_treat*s0 / (1+(s0-s1)*AB_treat)
  est_0 = AB_control*s0 / (1+(s0-s1)*AB_control)
  tau = sum((est_1 - est_0)*Group_dict,na.rm=TRUE) # Here assumed uniform distribution of all covariates.
  return(tau)
}
