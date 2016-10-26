rm(list = ls())
OS = "win"
library(mlr)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(reshape2)
source(file = "DataMining-Benchmark-Conversion/benchmark_defs.R")



################################################################################################################################
# Creation of the dataset
################################################################################################################################

## Load and convert the reasults to a data frame ----
load(file = "Data/Results/benchmark_results_snow_small-medium-allLearners_strat_All.RData")
load(file = "Data/Results/clas_time.RData")

leaner.id.lr = "classif.logreg"
learner.id.randomForest = "classif.randomForest"
unwantedLearners = c( "classif.cvglmnet..elasticnet", "classif.penalized.ridge", "classif.penalized.lasso",
                      "classif.multinom", "classif.cvglmnet.ridge", "classif.cvglmnet.lasso.vanilla", "classif.cvglmnet.lasso")
unwantedMeasures = c("mmce.test.mean")

# models difference
load(file = "Data/Research/DifferenceModel/ImportanceResults-all.RData")
load(file = "Data/Research/DifferenceModel/pdp.weigheddifferenceAll.RData")
load(file = "Data/Research/Target/target.sigma.0.5.RData")

clas_used = clas_used[c(1:length(result)),]

# create the importance.list
importance.list.aggr.l1 = sapply(importance.list, function(x) sum(abs(x$rf.permutation.imp-x$lr.permutation.imp))/length(x$rf.permutation.imp))
importance.list.aggr.l2 = sapply(importance.list, function(x) sqrt(sum((x$rf.permutation.imp-x$lr.permutation.imp)^2))/length(x$rf.permutation.imp))
importance.list.aggr.rank = sapply(importance.list, function(x) sum(abs(rank(x$rf.permutation.imp)-rank(x$lr.permutation.imp)))/length(x$rf.permutation.imp))

importance.df = data.frame(l1 = importance.list.aggr.l1,
                           l2 = importance.list.aggr.l2, 
                           rank = importance.list.aggr.rank)


# remove error message for pdp
res.errorMessages.pdp = which(!sapply(pdp.weigheddifference, function(x) typeof(x)=="list"))
result = result[-res.errorMessages.pdp]
clas_used = clas_used[-res.errorMessages.pdp,]
importance.df = importance.df[-res.errorMessages.pdp,]
pdp.weigheddifference = pdp.weigheddifference[-res.errorMessages.pdp]
target.sigma = target.sigma[-res.errorMessages.pdp]

# pdp.df
pdp.df = do.call("rbind", pdp.weigheddifference) 


# remove n>p
res.highdimension = which(clas_used$NumberOfFeatures>clas_used$NumberOfInstances)
result = result[-res.highdimension]
clas_used = clas_used[-res.highdimension,]
importance.df = importance.df[-res.highdimension,]
pdp.df = pdp.df[-res.highdimension,]
target.sigma = target.sigma[-res.highdimension]


# remove the ones with error messages
res.errorMessages = which(!sapply(result, function(x) typeof(x)=="list"))
result = result[-res.errorMessages]
clas_used = clas_used[-res.errorMessages,]
importance.df = importance.df[-res.errorMessages,]
pdp.df = pdp.df[-res.errorMessages,]
target.sigma = target.sigma[-res.errorMessages]

# aggregate the results
res.perfs = lapply(result, function(x) getBMRAggrPerformances(x, as.df=TRUE))

# remove unwanted learners
res.perfs = lapply(res.perfs, function(x) subset(x,!(learner.id %in% unwantedLearners)))

# remove unwanted measures
if (length(unwantedMeasures)>0) {
  index.unwantedMeasures = which(names(res.perfs[[1]]) %in% unwantedMeasures)
  res.perfs = lapply(res.perfs, function(x) x[,-index.unwantedMeasures])
}

# detect and removes the nas
res.perfs.nas = which(sapply(res.perfs, function(x) any(is.na(x))))
if (!(identical(integer(0), res.perfs.nas))) {
  res.perfs = res.perfs[-res.perfs.nas]
  clas_used = clas_used[-res.perfs.nas,]
  importance.df = importance.df[-res.perfs.nas,]
  pdp.df = pdp.df[-res.perfs.nas,]
  target.sigma = target.sigma[-res.perfs.nas]
}

# convert to a data.frame
res.perfs.df = do.call("rbind", res.perfs) 

# get the difference of performances
perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)
perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]
perfsAggr.diff.melted = melt(perfsAggr.diff)
detach(package:reshape2, unload = TRUE)



## Compute the dataset of difference with the features

# number  of features
p = clas_used$NumberOfFeatures

# number of numeric attributes
pnum = clas_used$NumberOfNumericFeatures

# number of categorical attributes
psymbolic = clas_used$NumberOfSymbolicFeatures

# number of samples
n = clas_used$NumberOfInstances

# n/p
psurn = p/n

# Numerical attributes rate
pnumrate = pnum/p

# Nominal attributes rate
psymbolicrate = psymbolic/p

# %Cmin Percentage of elements of the minority class
Cmin = clas_used$MinorityClassSize/n

# %Cmax Percentage of elements of the majority class
Cmax = clas_used$MajorityClassSize/n


df.bmr.diff = data.frame(perfsAggr.diff,
                         logp = log(clas_used$NumberOfFeatures), 
                         logn = log(clas_used$NumberOfInstances),
                         logdimension = log(clas_used$dimension),
                         logpsurn = log(clas_used$NumberOfFeatures/clas_used$NumberOfInstances),
                         logdimensionsurn = log(clas_used$dimension/clas_used$NumberOfInstances),
                         lograpportMajorityMinorityClass = log(clas_used$MajorityClassSize/clas_used$MinorityClassSize),
                         pnum, psymbolic, pnumrate, psymbolicrate, Cmin, Cmax,
                         brierlogreg = perfsAggr.LR$brier.test.mean,
                         logbrierlogreg = log(perfsAggr.LR$brier.test.mean),
                         sqrtbrierlogreg = sqrt(perfsAggr.LR$brier.test.mean),
                         target.sigma
                         )


paste("Is there any nas ? :", any(is.na(df.bmr.diff)))


measures.names = sapply(MEASURES, function(x) paste0(x$id,".test.mean"))
features.names = names(df.bmr.diff)[which(!(names(df.bmr.diff) %in% measures.names))]

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


# Save it ----
save(df.bmr.diff, res.perfs.df, convertModifiedBMRToRankMatrix, perfsAggr.diff.melted, perfsAggr.diff, pdp.df, clas_used, file = "Data/Results/df.bmr.RData")
