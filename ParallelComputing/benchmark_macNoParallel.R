rm(list = ls())
OS = "OSX"


# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas.RData")
clas_used = clas_medium
OMLDATASETS = clas_used$did

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
  #rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
  rdesc = makeResampleDesc("CV", iters = 5, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)
  
  return(bmr)
}




nDatasets = length(OMLDATASETS)

# reset
bmr.list.initial = rep(NA, length(OMLDATASETS))
bmr.list <- as.list(bmr.list.initial)

# ou load
#load(file = "../Data_BenchmarkOpenMl/Final/Results/MACnoParallel/res_medium.RData")

save(bmr.list, file = "../Data_BenchmarkOpenMl/Final/Results/MACnoParallel/res_medium1CVtest.RData")

for (j in c(1:nDatasets)) {
  print(j)
  data.index = OMLDATASETS[j]
  tryCatch({
    bmr.list[[j]] = runBenchmark(data.index)
  }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                       " caused the error: '", e, "'")))
  save(bmr.list, file = "../Data_BenchmarkOpenMl/Final/Results/MACnoParallel/res_medium1CVtest.RData")
}

