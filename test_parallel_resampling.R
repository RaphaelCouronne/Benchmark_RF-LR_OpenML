library(OpenML)
library(mlr)
library("parallelMap")
rm(list=ls())

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
n_cores = strtoi(args)

# Benchmark Test ----

load("task.list.RData")

benchmark_big_datasets = function(task, id, n_cores) {
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.list = list(lrn.classif.lr,lrn.classif.rf)
  
  measures = list(acc, brier, auc, timetrain)
  
  rdesc = makeResampleDesc("RepCV", folds = 2, reps = 10, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
  
  parallelStartSocket(n_cores, level = "mlr.resample")
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = TRUE, models = FALSE, show.info = TRUE)
  parallelStop()
  
  save(bmr, file = paste0("Data/Results/BigDatasets/BigDatasets_id",id,".RData"))
}


# Launch all bigdatasets
for (i in c(1:length(data.id.list)-1)){
  benchmark_big_datasets(task[[i]],data.id.list[[i]],n_cores)
}

# # Check for a little dataset
# print(n_cores)
# id = 310
# omldataset = getOMLDataSet(310)
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task = convertOMLDataSetToMlr(omldataset)
# benchmark_big_datasets(task,id,n_cores)
# 
# ## Check the results ----
# load(paste0("Data/Results/BigDatasets/BigDatasets_id",id,".RData"))

# ### With OpenML Dataset ----
# 
# data.id.list = c(260,262,270,271,272,273)
# 
# omldataset = getOMLDataSet(clas_used$data.id[260])
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task_260 = convertOMLDataSetToMlr(omldataset)
# 
# omldataset = getOMLDataSet(clas_used$data.id[262])
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task_262 = convertOMLDataSetToMlr(omldataset)
# 
# omldataset = getOMLDataSet(clas_used$data.id[270])
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task_270 = convertOMLDataSetToMlr(omldataset)
# 
# omldataset = getOMLDataSet(clas_used$data.id[271])
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task_271 = convertOMLDataSetToMlr(omldataset)
# 
# omldataset = getOMLDataSet(clas_used$data.id[272])
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task_272 = convertOMLDataSetToMlr(omldataset)
# 
# omldataset = getOMLDataSet(clas_used$data.id[273])
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# task_273 = convertOMLDataSetToMlr(omldataset)
# 
# 
# task.list = list(task_260,task_262,task_270,task_271,task_272,task_273)
# save(task.list, data.id.list, file = "task.list.RData")



omldataset = getOMLDataSet(clas_used$data.id[146])
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)

lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.list = list(lrn.classif.lr,lrn.classif.rf)

measures = list(acc, brier, auc, timetrain)

rdesc = makeResampleDesc("RepCV", folds = 2, reps = 10, stratify = TRUE)
configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
r = resample(lrn.classif.rf, task, rdesc)
bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = TRUE, models = FALSE, show.info = TRUE)
