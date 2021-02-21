library(parallel)
library(MASS)


rm(list = ls())
source("Data_generation.R")
source("Estimation/Naive.R")
source("Estimation/Misclassification.R")
source("Estimation/Mis_selection.R")
numcores = detectCores()


Results.naive=unlist(mclapply(DT,FUN=Naiveest,mc.cores=numcores))
Results.misclass = unlist(mclapply(DT,FUN=Misclassificationest, para=para,mc.cores=numcores))
Results.misclass_select.doublerobust = unlist(mclapply(DT,FUN=Mis_selection_est_doublerobust, para=para,mc.cores=numcores))
Results.misclass_select.ipw = unlist(mclapply(DT,FUN=Mis_selection_est, para=para,mc.cores=numcores))
Results.misclass_select.para = unlist(mclapply(DT,FUN=Mis_selection_est_parametric, para=para,mc.cores=numcores))


library(ggplot2)
result = data.frame(naive=Results.naive,mis=Results.misclass,mis_sel_DR=Results.misclass_select.doublerobust
                    ,mis_sel_IPW = Results.misclass_select.ipw,mis_sel_PARA = Results.misclass_select.para)
boxplot(result)
epirical_mean = sapply(result,mean)
abline(h=tau_montecarlo, col='red')
