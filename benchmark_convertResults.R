rm(list = ls())
library(mlr)
library(reshape2)


## Load and convert the reasults to a data frame ----
load(file  = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_snow_strat.RData")

# aggregate the results
res.perfs = lapply(result, function(x) getBMRAggrPerformances(x, as.df=TRUE))

# detect and removes the nas
res.perfs.nas = which(sapply(res.perfs, function(x) any(is.na(x))))
res.perfs = res.perfs[-res.perfs.nas]
clas_used = clas_used[-res.perfs.nas,]

# convert to a data.frame
res.perfs.df = do.call("rbind", res.perfs) 

# get the difference of performances
perfsAggr.LR = subset(res.perfs.df, learner.id=="classif.logreg")
perfsAggr.RF = subset(res.perfs.df, learner.id=="classif.randomForest")
perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]
perfsAggr.diff.melted = melt(perfsAggr.diff)
detach(package:reshape2, unload = TRUE)

## Compute the dataset of difference
df.bmr.diff = data.frame(perfsAggr.RF[,c(3:ncol(perfsAggr.RF))]-perfsAggr.LR[,c(3:ncol(perfsAggr.LR))],
                         logp = log(clas_used$NumberOfFeatures), 
                         logn = log(clas_used$NumberOfInstances),
                         logdimension = log(clas_used$dimension),
                         logpsurn = log(clas_used$NumberOfFeatures/clas_used$NumberOfInstances),
                         logdimensionsurn = log(clas_used$dimension/clas_used$NumberOfInstances),
                         lograpportMajorityMinorityClass = log(clas_used$MajorityClassSize/clas_used$MinorityClassSize))


paste("Is there any nas ? :", any(is.na(df.bmr.diff)))


## Compute the ranks
convertModifiedBMRToRankMatrix <- function(bmr.all, measure = NULL, ties.method = "average") {
  
  measure.name = paste(measure$id,".test.mean", sep = "")
  df = aggregate(bmr.all[[measure.name]], by = list(task.id = bmr.all$task.id,
                                                    learner.id = bmr.all$learner.id),
                 FUN = mean)
  
  # calculate ranks, rank according to minimize option of the measure
  if (!measure$minimize)
    df$x = -df$x
  df = plyr::ddply(df, "task.id", function(d) {
    d$alg.rank = rank(d$x, ties.method = ties.method)
    return(d)
  })
  
  # convert into matrix, rows = leaner, cols = tasks
  df = reshape2::melt(df, c("task.id", "learner.id"), "alg.rank")
  df = reshape2::dcast(df, learner.id ~ task.id )
  task.id.names = setdiff(colnames(df), "learner.id")
  mat = as.matrix(df[, task.id.names])
  rownames(mat) = df$learner.id
  colnames(mat) = task.id.names
  return(mat)
}

matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)



## General vizualisation -----
measure.chosen = acc



df = reshape2::melt(matrixRanks)
colnames(df) = c("learner.id", "task.id", "rank")

p = ggplot(df, aes_string("rank", fill = "learner.id"))
p = p + geom_bar(position = "dodge")
p = p + ylab(NULL)
p = p + ggtitle(paste("mesure :",measure.chosen$id))
print(p)


# plots for the measures
p <- ggplot(perfsAggr.diff.melted, aes(variable, value))
p + geom_boxplot(aes(colour = variable))

p <- ggplot(perfsAggr.diff.melted, aes(variable, value))
p + geom_violin(aes(colour = variable))




## Influence of parameters ----

## Plots

# histogram of features
hist(df.bmr.diff$logn)
hist(df.bmr.diff$logp)
hist(df.bmr.diff$logdimension)
hist(df.bmr.diff$logpsurn)
hist(df.bmr.diff$logdimensionsurn)
hist(df.bmr.diff$lograpportMajorityMinorityClass)

# plot performance vs parameter of the dataset
plot(df.bmr.diff$logn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logp, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logdimension, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logpsurn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logdimensionsurn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$lograpportMajorityMinorityClass, df.bmr.diff$acc.test.mean)

# with the linear models
plotLinearModelandCor<-function(feature, measure) {
  plot(df.bmr.diff[[feature]], df.bmr.diff[[measure]], xlab = feature, ylab = measure)
  fit =lm(df.bmr.diff[[measure]]~df.bmr.diff[[feature]])
  print(summary(fit))
  x = seq(-100,100,length.out = 20)
  lines(x, x*fit$coefficients[2]+fit$coefficients[1], col = "red")
  
  df.concordance = data.frame(df.bmr.diff[[feature]], df.bmr.diff[[measure]])
  tau = cor(df.concordance, method="kendall", use="pairwise") # kendall
  rho = cor(df.concordance, method="spearman", use="pairwise")# spearmann
  print(paste("tau", tau[1,2]))
  print(paste("rho", rho[1,2]))
}


plotLinearModelandCor("logn","acc.test.mean")
plotLinearModelandCor("logdimension","acc.test.mean")
plotLinearModelandCor("logpsurn","acc.test.mean")
plotLinearModelandCor("logdimensionsurn","acc.test.mean")
plotLinearModelandCor("lograpportMajorityMinorityClass","acc.test.mean")




## Partial dependance plots analysis
