rm(list = ls())
load(file = "Data/Results/df.bmr.RData")
library(mlr)

datasetsid.bench = clas_used$did

load(file = "../Data_BenchmarkOpenMl/Final/Results/Windows/DiffProba.RData")
rbind(Probability.difference)
df.proba = data.frame(matrix(unlist(Probability.difference),nrow = 278, byrow = 2))
datasetsid.proba = clas_used$did

index.keep = which(datasetsid.proba %in% datasetsid.bench)

df.proba.filtered = df.proba[index.keep,]



na.proba.filtered = which(is.na(df.proba.filtered$X1))
df.proba.filtered = df.proba.filtered[-na.proba.filtered,]
df.bmr.diff = df.bmr.diff[-na.proba.filtered,]

plot( log(df.proba.filtered$X1), df.bmr.diff$acc.test.mean)

hist(df.proba.filtered$X1)
