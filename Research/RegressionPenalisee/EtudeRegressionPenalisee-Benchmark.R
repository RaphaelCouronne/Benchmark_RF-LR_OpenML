library(mlr)
rm(list = ls())
OS = "Windows"
set.seed(1)

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium)
OMLDATASETS = clas_used$did[c(1:100)]
source(file = "benchmark_defs.R")


## Example 1 - Multi-core on a single computer
sink('SnowFallExample.Rout', split=TRUE)
.Platform
.Machine
R.version
Sys.info()

library(snowfall) 
# 1. Initialisation of snowfall. 
# (if used with sfCluster, just call sfInit()) 
sfInit(parallel=TRUE, cpus=9)

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
  
  # also use glmnet
  lrn.classif.lr.glm.ridge.min = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 0, s = "lambda.min")
  lrn.classif.lr.glm.ridge.min$id = "classif.cvglmnet.ridge.min"
  lrn.classif.lr.glm.lasso.min = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1, s = "lambda.min")
  lrn.classif.lr.glm.lasso.min$id = "classif.cvglmnet.lasso.min"
  
  lrn.classif.lr.glm.ridge.1se = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 0, s = "lambda.1se")
  lrn.classif.lr.glm.ridge.1se$id = "classif.cvglmnet.ridge.1se"
  lrn.classif.lr.glm.lasso.1se = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1, s = "lambda.1se")
  lrn.classif.lr.glm.lasso.1se$id = "classif.cvglmnet.lasso.1se"
  
  # glm scaled with mlr wrapper
  lrn.glm.scaled.lasso = makePreprocWrapperScale(lrn.classif.lr.glm.lasso.min)
  lrn.glm.scaled.lasso$id = "glm.lasso.mlrscaled"
  lrn.glm.scaled.ridge = makePreprocWrapperScale(lrn.classif.lr.glm.ridge.min)
  lrn.glm.scaled.ridge$id = "glm.ridge.mlrscaled"
  
  # glm scaled with caret wrapper
  #lrn.glm.scaled1.caret = makePreprocWrapperCaret(lrn.classif.lr.glm.lasso, ppc.center = TRUE, ppc.scale = TRUE)
  #lrn.glm.scaled0.caret = makePreprocWrapperCaret(lrn.classif.lr.glm.ridge, ppc.center = TRUE, ppc.scale = TRUE)
  
  # list of learners
  lrn.list = list(lrn.classif.lr,
                  lrn.classif.lr.glm.ridge.min, lrn.classif.lr.glm.lasso.min, #glmnet package
                  lrn.classif.lr.glm.ridge.1se, lrn.classif.lr.glm.lasso.1se,
                  lrn.glm.scaled.lasso, lrn.glm.scaled.ridge)
                  #lrn.glm.scaled1, lrn.glm.scaled0,
                  #lrn.classif.lr.glm.lasso.lambda1se, 
                  #lrn.glm.scaled1.caret, lrn.glm.scaled0.caret) 
  
  # measures
  measures = MEASURES
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
sfExport("MEASURES", "runBenchmark", "makePreprocWrapperScale") 
sfLibrary(cmprsk) 

# 5. Start network random number generator 
# (as "sample" is using random numbers). 
sfClusterSetupRNG() 

# 6. Distribute calculation
start <- Sys.time(); result <- sfLapply(OMLDATASETS, wrapper) ; Sys.time()-start


# 7. Stop snowfall 
sfStop() 

save(result, clas_used, file = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_regressionpenalisee.lambdamin.RData")
print("done with cluster")
