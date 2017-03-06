parallel_computation_snowfall <- function(nCores = 1, 
                                          clas_used,
                                          target_path = "Data/Results/benchmark_parallel_snowfall.RData",
                                          seed = 1) {
  library(snowfall) 
  library(mlr)
  set.seed(seed)
  print("Begin Parallel computation for benchmark")
  print("Computation can be monitord in Data/Results/benchmark_parallel_snowfall_informations.Rout")
  print(paste("Estimated number of s : ",sum(clas_used[-which(is.na(clas_used$time)),]$time)*50))
  start.time <- Sys.time()
  
  # Sort the datasets according to dataid
  omldatasets = sort(clas_used$data.id)
  
  .Platform
  .Machine
  R.version
  Sys.info()
  
  ## - Multi-core on a single computer
  # 1. Initialisation of snowfall. 
  sfInit(parallel=TRUE, cpus=nCores, slaveOutfile = 'Data/Results/benchmark_parallel_snowfall_informations.Rout')
  
  # 2. Wrapper, which can be parallelised. 
  runBenchmark <- function(data.index) {
    
    library(OpenML)
    library(mlr)
    print(paste("Beginning dataset ", data.index, " ======>"))
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
    lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) 
    lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE) 
    
    # list of learners
    lrn.list = list(lrn.classif.lr, #stats package
                    lrn.classif.rf #randomForest package
    )
    
    # measures
    set.seed(seed)
    measures = list(acc, brier, auc, timetrain)
    rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
    configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
    bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
    print(paste(" ======> ", "Ending dataset ", data.index))
    cat("\n")
    cat("\n")
    return(bmr)
  }
  
  wrapper <- function(data.index) {
    tryCatch({
      # benchmark
      runBenchmark(data.index)
    }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                         " caused the error: '", e, "'")))
  }
  
  # 3. Exporting needed data and loading required 
  # packages on workers. 
  sfExport("runBenchmark") 
  sfLibrary(cmprsk) 
  
  # 4. Start network random number generator 
  # (as "sample" is using random numbers). 
  sfClusterSetupRNG() 

  # 5. Distribute calculation
  start <- Sys.time(); result <- sfLapply(omdatasets, wrapper) ; Sys.time()-start
  
  # 6. Stop snowfall 
  sfStop() 
  end.time <- Sys.time()
  save(result, clas_used, file = target_path)
  time.taken <- end.time - start.time
  print("Done with parallel computations")
  print(paste("Effective duration time : ", time.taken))
}
