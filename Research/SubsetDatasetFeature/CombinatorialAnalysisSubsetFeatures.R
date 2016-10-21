library(mlr)
library(OpenML)
library(ggplot2)
rm(list = ls())
OS = "Windows"
set.seed(1)

## Load and Parameters ----

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium)
OMLDATASETS = clas_used$did
source(file = "benchmark_defs.R")

# Parameters
index = 250
gridsize = 10
nObsExp = 1e1

# Load one dataset
dataset.index = clas_used$did[index]

omldataset = getOMLDataSet(dataset.index)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)

# Get the combinatorial possibilities
n = task$task.desc$size
p = sum(task$task.desc$n.feat)
features.names = names(task$env$data)[-which(names(task$env$data) == task$task.desc$target)]
grid = floor(seq(1e1, n-1, length.out = gridsize))



# =============================================================
# First strategy, generate at random, don't care if duplicates ----

benchmark.sub = function(task) {
  # learners
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) #2class
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE) #multiclass
  
  # list of learners
  lrn.list = list(lrn.classif.lr, lrn.classif.rf)
  # measures
  measures = MEASURES
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 2, stratify = TRUE)
  #rdesc = makeResampleDesc("CV", iter = 5, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
  bmr.perfs = getBMRAggrPerformances(bmr, as.df = TRUE)
  res = bmr.perfs$acc.test.mean
  return(res)
}

# For the feature n ----


grid = c(5e1, 1e2, 5e2, 1e3, 5e3)
grid.toString = sapply(grid, function(x) toString(x))
gridsize = length(grid)
nObsExp = 5e1

df = data.frame(row.names = features.names)
results.diff = data.frame(matrix(NA, nrow = nObsExp, ncol= gridsize))
results.rf = data.frame(matrix(NA, nrow = nObsExp, ncol= gridsize))
results.lr = data.frame(matrix(NA, nrow = nObsExp, ncol= gridsize))

names(results.rf) = grid.toString
names(results.lr) = grid.toString
names(results.diff) = grid.toString

for (j in c(1:gridsize)) {
  n.sub = grid[j]
  for (i in c(1:nObsExp)) {
    print(paste0("Debut ","i=",i,", j=",j))
    indexdata.experiment = sapply(c(1:nObsExp),function(x) sample(n,n.sub))
    dataset.sub = task$env$data[indexdata.experiment[,i],]
    task.sub = makeClassifTask(data = dataset.sub, target = task$task.desc$target)
    bmr.sub = benchmark.sub(task.sub)
    results.diff[i,j] = bmr.sub[2]-bmr.sub[1]
    results.lr[i,j] = bmr.sub[1]
    results.rf[i,j] = bmr.sub[2]
  }
  
  # plot temporary
  if (j>1) {
    
    # plots of median value
    result.diff.mean = apply(results.diff[,c(1:j)], 2, median)
    result.rf.mean = apply(results.rf[,c(1:j)], 2, median)
    result.lr.mean = apply(results.lr[,c(1:j)], 2, median)
    par(mfrow=c(1,2))
    plot(grid[c(1:j)], result.diff.mean, main = "Difference", log = "x", xlab = "n", ylab = "Difference in acc")
    lines(grid[c(1:j)], result.diff.mean, main = "Difference", log = "x")
    plot(grid[c(1:j)], result.rf.mean, main = "Acc of Rf and LR", ylim = c(0.5,1), log = "x", col = "blue", , xlab = "n", ylab = "acc")
    points(grid[c(1:j)], result.lr.mean, main = "LR", ylim = c(0.5,1), col = "red")
    
    # boxplots
    library(reshape2)
    df.rf = melt(results.rf[,c(1:j)])
    df.rf$learner = "RandomForest"
    df.lr = melt(results.lr[,c(1:j)])
    df.lr$learner = "Logistic Regression"
    df.all = rbind(df.rf, df.lr)
    names(df.all) = c("n", "acc", "learner")
    detach(package:reshape2, unload = TRUE)
    ggp = ggplot(df.all, aes(n, acc))
    ggp = ggp+ geom_boxplot(aes(fill = learner))
    plot(ggp)
  }

  
}

save(results.lr, results.rf, results.diff, file = "resultsNSubset.RData")






# For the feature p ----

task.tiny = makeClassifTask(data = task$env$data[c(1:1e3),], target = task$task.desc$target)

grid = c(2,3,4,5,6,10,14,18)
grid.toString = sapply(grid, function(x) toString(x))
gridsize = length(grid)
nObsExp = 1e1

df = data.frame(row.names = features.names)
results.diff = data.frame(matrix(NA, nrow = nObsExp, ncol= gridsize))
results.rf = data.frame(matrix(NA, nrow = nObsExp, ncol= gridsize))
results.lr = data.frame(matrix(NA, nrow = nObsExp, ncol= gridsize))

names(results.rf) = grid.toString
names(results.lr) = grid.toString
names(results.diff) = grid.toString

for (j in c(1:gridsize)) {
  p.sub = grid[j]
  for (i in c(1:nObsExp)) {
    print(paste0("Debut ","i=",i,", j=",j))
    indexdata.experiment = sapply(c(1:nObsExp),function(x) sample(p,p.sub))
    task.data = task.tiny$env$data[,-which(names(task.tiny$env$data) %in% task.tiny$task.desc$target)]
    dataset.sub = data.frame(task.tiny$env$data[,indexdata.experiment[,i]], target = task.tiny$env$data[[task.tiny$task.desc$target]])
    task.sub = makeClassifTask(data = dataset.sub, target = "target")
    bmr.sub = benchmark.sub(task.sub)
    results.diff[i,j] = bmr.sub[2]-bmr.sub[1]
    results.lr[i,j] = bmr.sub[1]
    results.rf[i,j] = bmr.sub[2]
  }
  
  # plot temporary
  if (j>1) {
    
    # plots of median value
    result.diff.mean = apply(results.diff[,c(1:j)], 2, median)
    result.rf.mean = apply(results.rf[,c(1:j)], 2, median)
    result.lr.mean = apply(results.lr[,c(1:j)], 2, median)
    par(mfrow=c(1,2))
    plot(grid[c(1:j)], result.diff.mean, main = "Difference", xlab = "p", ylab = "Difference in acc")
    lines(grid[c(1:j)], result.diff.mean, main = "Difference")
    plot(grid[c(1:j)], result.rf.mean, main = "Acc of Rf and LR", ylim = c(0.5,1), col = "blue", xlab = "p", ylab = "acc")
    points(grid[c(1:j)], result.lr.mean, main = "LR", ylim = c(0.5,1), col = "red")
    
    # boxplots
    library(reshape2)
    df.rf = melt(results.rf[,c(1:j)])
    df.rf$learner = "RandomForest"
    df.lr = melt(results.lr[,c(1:j)])
    df.lr$learner = "Logistic Regression"
    df.all = rbind(df.rf, df.lr)
    names(df.all) = c("p", "acc", "learner")
    detach(package:reshape2, unload = TRUE)
    ggp = ggplot(df.all, aes(p, acc))
    ggp = ggp+ geom_boxplot(aes(fill = learner))
    plot(ggp)
  }
  
  
}

save(results.lr, results.rf, results.diff, file = "resultsSubset.RData")


## Plot the results ----


j = ncol(results.diff)-1


result.diff.mean = apply(results.diff[,c(1:j)], 2, median)
result.rf.mean = apply(results.rf[,c(1:j)], 2, median)
result.lr.mean = apply(results.lr[,c(1:j)], 2, median)
par(mfrow=c(1,2))
plot(grid[c(1:j)], result.rf.mean, ylim = c(0.5,1), col = "#99CCFF", xlab = expression(p), ylab = "acc", pch = 0)
points(grid[c(1:j)], result.lr.mean, ylim = c(0.5,1), col = "#990000")
legend("bottomright", c("RF","LR"), pch = c(0,1), col = c("#99CCFF", "#990000"))
plot(grid[c(1:j)], result.diff.mean, xlab = expression(p), ylab = expression(paste(Delta,acc)))
lines(grid[c(1:j)], result.diff.mean)


# boxplots
library(reshape2)
df.rf = melt(results.rf[,c(1:j)])
df.rf$learner = "RandomForest"
df.lr = melt(results.lr[,c(1:j)])
df.lr$learner = "Logistic Regression"
df.all = rbind(df.rf, df.lr)
names(df.all) = c("p", "acc", "Method")
detach(package:reshape2, unload = TRUE)
ggp = ggplot(df.all, aes(p, acc))
ggp = ggp+ geom_boxplot(aes(fill = Method)) + scale_fill_manual(values=c("#99CCFF", "#990000"))
plot(ggp)