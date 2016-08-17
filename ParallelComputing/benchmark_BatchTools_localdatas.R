rm(list = ls())
library(mlr)
library(plyr)
library(batchtools)
OS = "OSX"

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
load(file = "../../Data_BenchmarkOpenMl/liste_des_taches_MLR/task.listebeforeGBenchmarktiny.RData")
setwd(file.path(githubdir,"Data_BenchmarkOpenMl/Final/Results/"))

# which subset of dataset
nameExperiment = paste("benchmark-openML-lr-rf_localdatas","tiny","1")


unlink(nameExperiment, recursive = TRUE)
regis = makeExperimentRegistry(nameExperiment, 
                               packages = c("mlr", "OpenML", "methods"), 
                               source = paste0(dir, "/benchmark_defs.R"),
                               work.dir = paste0(githubdir,"/Data_BenchmarkOpenMl/Final/Results"),
                               conf.file = paste0(dir,"/.batchtools.conf.R")
)

regis$cluster.functions = makeClusterFunctionsMulticore(ncpus = 3) 



# add selected OML datasets as problems
n = length(task.list)
for (index.list in c(1:n)) {
  data = list(index.list = index.list)
  addProblem(name = as.character(index.list), data = index.list)
}


# add one generic 'algo' that evals the RF in hyperpar space
addAlgorithm("eval", fun = function(job, data, instance,  ...) {
  par.vals = list(...)
  
  task = task.list[[data]]
  
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
testJob(1, reg = regis)
submitJobs(ids = 1, reg = regis)

# submit one job and remove it to test the batchtool
waitForJobs(ids = 1, sleep = 10, timeout = 604800, stop.on.error = FALSE, reg = getDefaultRegistry())
showLog(id = 1, reg = regis)
ids = chunkIds(findNotDone(), chunk.size = 5, reg = regis)
resetJobs(reg = regis, ids = 1)

