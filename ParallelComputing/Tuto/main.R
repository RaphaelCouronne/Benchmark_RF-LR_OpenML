rm(list = ls())
OS = "Windows"


# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas.RData")
clas_used = clas_small
source(file = "FinalVersion/benchmark_defs.R")

# function definition
runBenchmark <- function(data.index) {
  
  library(OpenML)
  library(mlr)
  
  # get the dataset
  omldataset = getOMLDataSet(data.index)
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
  rdesc = makeResampleDesc("RepCV", folds = 2, reps = 2, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)
  
  return(bmr)
}

runBenchmarkTest <- function(data.index) {
  
  library(OpenML)
  library(mlr)
  print(data.index)
  
  # get the dataset
  omldataset = getOMLDataSet(data.index)
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
  rdesc = makeResampleDesc("RepCV", folds = 2, reps = 2, stratify = TRUE)
  #configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = TRUE, models = TRUE, show.info = TRUE)
  print(bmr)
  
  return(bmr)
}


# ================= Method 1 ========================================
## http://www.win-vector.com/blog/2016/01/parallel-computing-in-r/

# Start up a parallel cluster
no_cores = detectCores()-1
parallelCluster <- parallel::makeCluster(no_cores)
print(parallelCluster)

# Bind to environment
source('FinalVersion/ParallelComputing/bindToEnv.R') # Download from: http://winvector.github.io/Parallel/bindToEnv.R
# build the single argument function we are going to pass to parallel
mkWorker <- function() {
  bindToEnv(objNames=c('clas_used','runBenchmark', 'MEASURES', "OMLDATASETS"))
  function(data.index) {
    return(runBenchmark(data.index))
  }
  
}

tryCatch({
  bmr.list <- parallel::parLapply(cl = parallelCluster,X = OMLDATASETS[c(1:3)],fun = mkWorker())
}, error = function(e) {
  print(e)
})


# Shutdown cluster neatly
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}




# ================= Method 2 ========================================
## http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/


## Basic packages

no_cores = detectCores()-1
cl<-makeCluster(no_cores)
clusterExport(cl, c("clas_used", "MEASURES", "OMLDATASETS", 'runBenchmark'))

# Run
parLapply(cl, 
          OMLDATASETS[c(1:3)], 
          function(i) runBenchmark(i))

# Finish
stopCluster(cl)


## Forereach package
u = OMLDATASETS[1:3]
library(foreach)
library(doParallel)
no_cores = detectCores()-1
cl<-makeCluster(no_cores)
registerDoParallel(cl)



bmr.list = foreach(data.index = u, 
                   .packages=c("mlr", "OpenML"))  %dopar%  
                   {
                     tryCatch({
                       runBenchmarkTest(data.index)
                     }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                                          " caused the error: '", e, "'")))
                   }



stopImplicitCluster()


## forereach package dopar


# ================= Method 3 ========================================
# package snowflakes

u = OMLDATASETS[1:3]

#Create cluster
no_cores = detectCores()-1
clus <- makeCluster(no_cores)

#Export it form base workspace
clusterExport(clus,c("runBenchmark", "MEASURES"))

datasets.index <- data.frame(index = u)


#Apply the declared function
aa <- parLapply(clus, u, runBenchmarktest)

stopCluster(clus)
