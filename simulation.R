rm(list = ls())
source("Data_generation.R")
source("Estimation/Naive.R")
source("Estimation/Misclassification.R")
source("Estimation/Mis_selection.R")


Results.naive=sapply(DT,FUN=Naiveest)
Results.misclass = sapply(DT,FUN=Misclassificationest, para=para)
Results.misclass_select = sapply(DT,FUN=Mis_selection_est, para=para)


library(ggplot2)
result = data.frame(naive=Results.naive,mis=Results.misclass,mis_select=Results.misclass_select)
boxplot(result)
sapply(result,mean)
