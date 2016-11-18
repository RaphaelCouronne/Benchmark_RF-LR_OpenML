library(mlr)
rm(list = ls())
OS = "Windows"

print("Begin Parallel computation for benchmark")
print("  Computation can be monitord in Data/Results/benchmark_parallel_snowfall_informations.Rout")

# Load the environment
load(file = "Data/Results/Original/clas_time_original.RData")
clas_used = rbind(clas_time_small, clas_time_medium)
OMLDATASETS = clas_used$did[c(1:10)]
source(file = "DataMining-Benchmark-Conversion/benchmark_defs.R")


## Example 1 - Multi-core on a single computer
sink('Data/Results/benchmark_parallel_snowfall_informations.Rout', split=TRUE)
.Platform
.Machine
R.version
Sys.info()

library(snowfall) 
# 1. Initialisation of snowfall. 
# (if used with sfCluster, just call sfInit()) 
sfInit(parallel=TRUE, cpus=10)

# 2. Loading data. 

# 3. Wrapper, which can be parallelised. 
runBenchmark <- function(data.index) {
  
  library(OpenML)
  library(mlr)
  
  print(paste("debut dataset ", data.index))
  print(Sys.time())
  
  # get the dataset
  omldataset = getOMLDataSet(data.index)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  task$task.desc$id = paste("dataset", data.index)
  
  
  # learners
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) #2class
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE) #multiclass
  
  # list of learners
  lrn.list = list(lrn.classif.lr, #stats package
                  lrn.classif.rf #randomForest package
                  )
  
  # measures
  measures = list(acc, brier, auc, timetrain)
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
  print(paste("fin dataset ", data.index))
  return(bmr)
}

wrapper <- function(data.index) {
tryCatch({
  
  # benchmark
  runBenchmark(data.index)
}, error = function(e) return(paste0("The variable '", data.index, "'", 
                                     " caused the error: '", e, "'")))
}


# 4. Exporting needed data and loading required 
# packages on workers. 
sfExport("runBenchmark") 
sfLibrary(cmprsk) 

# 5. Start network random number generator 
# (as "sample" is using random numbers). 
sfClusterSetupRNG() 

# 6. Distribute calculation
start <- Sys.time(); result <- sfLapply(OMLDATASETS, wrapper) ; Sys.time()-start


# 7. Stop snowfall 
sfStop() 

save(result, clas_used, file = "Data/Results/benchmark_parallel_snowfall.RData")
print("Done with parallel computations")
