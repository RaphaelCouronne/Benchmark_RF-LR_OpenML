library(mlr)
load("Data/Simulations/pdp.difference.RData")
load("Data/Saved_original/benchmark_parallel_snowfall.RData")


res.perfs = lapply(result, function(x) getBMRAggrPerformances(x, as.df=TRUE))
res.perfs.df = do.call("rbind", res.perfs) 
leaner.id.lr = "classif.logreg"
learner.id.randomForest = "classif.randomForest"
perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)
perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]

nas.data = which(is.na(perfsAggr.diff$acc.test.mean) |
                   (is.na(pdp.difference$pdp.l1)) | 
                   (clas_used$number.of.features<4) | 
                   (clas_used$number.of.symbolic.features>1))# |
#(clas_used$number.of.instances<1e2))

perfsAggr.diff.nonas = perfsAggr.diff[-nas.data,]
pdp.difference.nona = pdp.difference[-nas.data,]

plot(log1p(pdp.difference.nona$pdp.l1), perfsAggr.diff$auc.test.mean[-nas.data])

pdp.difference.nona$data.id[order(pdp.difference.nona$pdp.l1)]
pdp.difference.nona$np=pdp.difference.nona$n*pdp.difference.nona$p
subset(pdp.difference.nona, select = c("task.id", "data.id", "n", "p", "np"))[order(pdp.difference.nona$pdp.l1),]

subset(cbind(perfsAggr.diff.nonas,pdp.difference.nona), select = c("task.id", "data.id", "n","p", "acc.test.mean", "pdp.l1"))[order(perfsAggr.diff.nonas$acc.test.mean),]

## ----
id = 1471
clas_used[which(clas_used$data.id==id),]

data.id = id

omldataset = getOMLDataSet(data.id = data.id, verbosity = 0)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
#pdp = getPdpDifference(mlrtask, seed = 1, visualize = TRUE)

perfsAggr.diff$acc.test.mean[which(clas_used$data.id==data.id)]
mlrtask$env$data

plot_extrem_cases(data.id = id)



