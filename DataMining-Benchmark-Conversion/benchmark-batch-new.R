rm(list = ls())
detectCores(all.tests = FALSE, logical = TRUE)
library(mlr)
library(plyr)
library(batchtools)
library(OpenML)
saveOMLConfig(apikey = "7a4391537f767ea70db6af99497653e5", arff.reader = "RWeka", overwrite=TRUE)


# which subset of dataset
clas_used = rbind(clas_time_small)
OMLDATASETS = clas_used$did[c(1:50)]
nameExperiment = paste("batchtool-test")


unlink(nameExperiment, recursive = TRUE)
regis = makeExperimentRegistry(nameExperiment, 
                               packages = c("mlr", "OpenML", "methods"), 
                               #source = paste0(dir, "/benchmark_defs.R"),
                               work.dir = paste0("Data/Batchtools"),
                               conf.file = paste0("Data/Batchtools/.batchtools.conf.R")
)

regis$cluster.functions = makeClusterFunctionsMulticore(ncpus = 2) 



# add selected OML datasets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}


# add one generic 'algo' that compares RF and LR
addAlgorithm("eval", fun = function(job, data, instance,  ...) {
  par.vals = list(...)
  
  # get the dataset
  omldataset = getOMLDataSet(data$did)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  
  # learners
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.list = list(lrn.classif.lr,lrn.classif.rf)
  
  # measures
  measures = list(acc, brier, auc, timetrain)
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)
  bmr
})


# finalize experiment
set.seed(1)
ades = data.frame(c(1))
addExperiments(algo.designs = list(eval = ades))
summarizeExperiments()
getStatus()

# test jobs
testJob(1, reg = regis)

# Now submit al
options(batchtools.progress = TRUE)
notDone = findNotDone()
ids = chunkIds(notDone$job.id, chunk.size = 10)
submitJobs(ids = ids, reg = regis, )
waitForJobs(ids = c(1:50), sleep = 10, timeout = 604800, stop.on.error = FALSE, reg = getDefaultRegistry())
resetJobs(reg = regis, ids = c(1:50))
getStatus()
getErrorMessages()



