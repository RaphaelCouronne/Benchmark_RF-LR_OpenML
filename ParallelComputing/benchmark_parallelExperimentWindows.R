rm(list = ls())
OS = "Windows"
set.seed(1)

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas.RData")
clas_used = clas_medium
OMLDATASETS = clas_used$did
source(file = "FinalVersion/benchmark_defs.R")
if (file.exists("FinalVersion/benchout.txt")) {
  file.remove("FinalVersion/benchout.txt")
}

# function definition
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
  
  
  # learners
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.list = list(lrn.classif.lr,lrn.classif.rf)
  
  # measures
  measures = MEASURES
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
  print(paste("fin dataset ", data.index))
  return(bmr)
}



## Forereach package
library(foreach)
library(doParallel)

length(OMLDATASETS)

OMLDATASETS_used = OMLDATASETS[51:75]
no_cores = detectCores()-2
cl<-makeCluster(no_cores, outfile = "FinalVersion/benchout.txt")
registerDoParallel(cl)



bmr.list = foreach(data.index = OMLDATASETS_used, 
                   .packages=c("mlr", "OpenML"))  %dopar%  
                   {
                     tryCatch({
                       
                       # benchmark
                       runBenchmark(data.index)
                     }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                                          " caused the error: '", e, "'")))
                   }

bmr.list


stopImplicitCluster()

## save it
save(bmr.list, file = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_medium_51-75.RData")
