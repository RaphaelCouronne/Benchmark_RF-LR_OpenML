rm(list = ls())
detectCores(all.tests = FALSE, logical = TRUE)
library(mlr)
library(plyr)
library(batchtools)

OS = "win"

if (OS == "OSX") {
  # OSX
  githubdir = "/Users/Shadok/Programmation/Github/"
  dir = file.path(githubdir, "BenchmarkOpenMl/FinalVersion/")
} else {
  # windows
  githubdir = "C:/Users/couronne/Desktop/GitHub/"
  dir = file.path(githubdir, "BenchmarkOpenMl/FinalVersion/")
}

setwd(dir)
source(file.path(dir,"benchmark_defs.R"))
setwd(file.path(githubdir,"Data_BenchmarkOpenMl/Final/Results/"))

# which subset of dataset
clas_used = rbind(clas_small)
OMLDATASETS = clas_used$did
nameExperiment = paste("benchmark-openML-lr-rf","small","1")


unlink(nameExperiment, recursive = TRUE)
regis = makeExperimentRegistry(nameExperiment, 
                               packages = c("mlr", "OpenML", "methods"), 
                               source = paste0(dir, "/benchmark_defs.R"),
                               work.dir = paste0(githubdir,"/Data_BenchmarkOpenMl/Final/Results"),
                               conf.file = paste0(dir,"/.batchtools.conf.R")
)

regis$cluster.functions = makeClusterFunctionsMulticore(ncpus = 3) 



# add selected OML datasets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}


# add one generic 'algo' that evals the RF in hyperpar space
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
  measures = MEASURES
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
testJob(201, reg = regis)

# submit one job and remove it to test the batchtool
submitJobs(ids = 1, reg = regis)
waitForJobs(ids = 1, sleep = 10, timeout = 604800, stop.on.error = FALSE, reg = getDefaultRegistry())
showLog(id = 1, reg = regis)
ids = chunkIds(findNotDone(), chunk.size = 5, reg = regis)
resetJobs(reg = regis, ids = 1)
getStatus()

# submit one job and remove it to test the batchtool
ids = chunkIds(findNotDone(), chunk.size = 5)
submitJobs(ids = c(1:10), reg = regis)
showLog(id = 1, reg = regis)
getStatus()
resetJobs(reg = regis, ids = c(1:10))
getStatus()

# Now submit all
ids = chunkIds(findNotDone(), chunk.size = 5)
submitJobs(ids = c(101:200), reg = regis)
waitForJobs(ids = c(101:200), sleep = 10, timeout = 604800, stop.on.error = FALSE, reg = getDefaultRegistry())
resetJobs(reg = regis, ids = ids)
getStatus()
getErrorMessages()


## Continue if not terminated ----

# remove if jobs didn't post
regis = loadRegistry(nameExperiment)
getStatus()
getErrorMessages()
resetJobs(reg = regis, ids = c(191:210))

# test jobs
regis$cluster.functions = makeClusterFunctionsMulticore(ncpus = 3) 
testJob(100, reg = regis)
submitJobs(ids = 196, reg = regis)
ids = chunkIds(findNotDone(), chunk.size = 5)

# continue
submitJobs(c(224:250), resources = list(chunk.ncpus = 3))
submitJobs(ids = c(200:220), reg = regis)
waitForJobs(ids = c(100:250), sleep = 10, timeout = 604800, stop.on.error = TRUE, reg = regis)
resetJobs(reg = regis, ids = 196)
