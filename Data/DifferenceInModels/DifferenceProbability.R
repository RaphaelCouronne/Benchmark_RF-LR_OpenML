library(mlr)
rm(list = ls())
OS = "Windows"
set.seed(1)

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium)
OMLDATASETS = clas_used$did
source(file = "benchmark_defs.R")


## Example 1 - Multi-core on a single computer
sink('SnowFall.permutationImportance.Rout', split=TRUE)
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
runProbability <- function(data.index) {
  
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
  
  fit.rf = train(lrn.classif.rf, task)
  fit.lr = train(lrn.classif.lr, task)
  
  pred.rf = predict(fit.rf, task)
  pred.lr = predict(fit.lr, task)
  
  diff.l1 = sum(abs(pred.rf$data$prob.P-pred.lr$data$prob.P))/length(pred.lr$data$prob.P)
  diff.l2 = sqrt(sum((pred.rf$data$prob.P-pred.lr$data$prob.P)^2))/length(pred.lr$data$prob.P)

  df = data.frame(l1 = diff.l1, l2 = diff.l2)
  
  return(df)
}

wrapper <- function(data.index) {
  tryCatch({
    
    # benchmark
    runProbability(data.index)
  }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                       " caused the error: '", e, "'")))
}


# 4. Exporting needed data and loading required 
# packages on workers. 
sfExport("runProbability") 
sfLibrary(cmprsk) 

# 5. Start network random number generator 
# (as "sample" is using random numbers). 
sfClusterSetupRNG() 

# 6. Distribute calculation
start <- Sys.time(); Probability.difference <- sfLapply(OMLDATASETS, wrapper) ; Sys.time()-start


# 7. Stop snowfall 
sfStop() 

save(Probability.difference, clas_used, file = "../Data_BenchmarkOpenMl/Final/Results/Windows/DiffProba.RData")
print("done with cluster")
