library(mlr)
rm(list = ls())
OS = "Windows"
set.seed(1)

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium)
OMLDATASETS = clas_used$did[1:200]
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
runImportance <- function(data.index) {
  
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
  
  # regularized
  #lrn.classif.lrlasso = makeLearner("classif.penalized.lasso", predict.type = "prob", fix.factors.prediction = TRUE) #two class #no factor
  #lrn.classif.lrridge = makeLearner("classif.penalized.ridge", predict.type = "prob", fix.factors.prediction = TRUE) #two class #no factor
  #lrn.classif.lrfusedlasso = makeLearner("classif.penalized.fusedlasso", predict.type = "prob", fix.factors.prediction = TRUE)#two class 
  
  # nnet
  #lrn.classif.multinom = makeLearner("classif.multinom", predict.type = "prob", fix.factors.prediction = TRUE)
  
  # also use glmnet
  #lrn.classif.lr.glm.ridge = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 0)
  #lrn.classif.lr.glm.ridge$id = "classif.cvglmnet.ridge"
  #lrn.classif.lr.glm.lasso = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1)
  #lrn.classif.lr.glm.lasso$id = "classif.cvglmnet.lasso"
  
  # compute the variable importance
  fv.rf = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.rf)
  fv.lr = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.lr)
  #fv.lrlasso = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.lrlasso)
  #fv.lrridge = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.lrridge)
  #fv.multinom = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.multinom)
  #fv.lr.glm.ridge  = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.lr.glm.ridge)
  #fv.lr.glm.lasso = generateFilterValuesData(task, method = "permutation.importance", imp.learner = lrn.classif.lr.glm.lasso)
  
  # save in a dataset
  df = fv.rf$data
  names(df)[3]<-"rf.permutation.imp"
  df = data.frame(df, 
                  lr.permutation.imp = fv.lr$data$permutation.importance)
                  #lrlasso.permutation.imp = fv.lrlasso$data$permutation.importance
                  #lrridge.permutation.imp = fv.lrridge$data$permutation.importance,
                  #multinom.permutation.imp = fv.multinom$data$permutation.importance,
                  #lr.glm.ridge.permutation.imp = fv.lr.glm.ridge$data$permutation.importance,
                  #lr.glm.lasso.permutation.imp = fv.lr.glm.lasso$data$permutation.importance)
  
  print(paste("fin dataset ", data.index))
  return(df)
}

wrapper <- function(data.index) {
  tryCatch({
    
    # benchmark
    runImportance(data.index)
  }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                       " caused the error: '", e, "'")))
}


# 4. Exporting needed data and loading required 
# packages on workers. 
sfExport("runImportance") 
sfLibrary(cmprsk) 

# 5. Start network random number generator 
# (as "sample" is using random numbers). 
sfClusterSetupRNG() 

# 6. Distribute calculation
start <- Sys.time(); importance.list <- sfLapply(OMLDATASETS, wrapper) ; Sys.time()-start


# 7. Stop snowfall 
sfStop() 

save(importance.list, clas_used, file = "../Data_BenchmarkOpenMl/Final/Results/Windows/ImportanceResults-200first.RData")
print("done with cluster")
