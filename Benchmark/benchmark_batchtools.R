require(mlr)
require(plyr)
require(batchtools)
require(OpenML)
require(tuneRanger)



setBatchtoolsExperiment = function(seed = 1, ncpus = 2, 
                                   clas_used,
                                   nameExperiment =  paste("Data/Results/Batchtools/batchtool_benchmark"),
                                   tune = FALSE) {
  
  # which subset of dataset
  omldatasets = clas_used$data.id
  
  
  unlink(nameExperiment, recursive = TRUE)
  regis = makeExperimentRegistry(nameExperiment, seed = seed,
                                 packages = c("mlr", "OpenML", "methods"), 
                                 #source = paste0(dir, "/benchmark_defs.R"),
                                 work.dir = paste0("Data/Results/Batchtools"),
                                 #conf.file = paste0("Data/Batchtools/.batchtools.conf.R")
  )
  
  regis$cluster.functions = makeClusterFunctionsMulticore(ncpus = ncpus) 
  
  
  
  # add selected OML datasets as problems
  for (did in omldatasets) {
    data = list(did = did)
    addProblem(name = as.character(did), data = data)
  }
  
  
  # add one generic 'algo' that compares RF and LR
  addAlgorithm("eval", fun = function(job, data, instance,  ...) {
    par.vals = list(...)
    
   # tryCatch({
      
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
      lrn.classif.tuneranger = makeLearner("classif.tuneRanger", predict.type = "prob", fix.factors.prediction = TRUE, num.threads=1)
      
      
      if (tune) {
        lrn.list = list(lrn.classif.lr,lrn.classif.rf, lrn.classif.tuneranger)
      } else {
        lrn.list = list(lrn.classif.lr,lrn.classif.rf)
      }
      
      # measures
      measures = list(acc, brier, auc, timetrain)
      
      if (tune) {
        rdesc = makeResampleDesc("CV", iters = 5, stratify = TRUE)
      } else {
        rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
      }
      
      configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
      bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = TRUE, models = FALSE, show.info = TRUE)
      return(bmr)
      
    #}, error = function(e) return(paste0("The variable '", data$did, "'", 
    #                                     " caused the error: '", e, "'")))
    
  })
  
  
  # finalize experiment
  ades = data.frame(c(1))
  addExperiments(algo.designs = list(eval = ades))
  summarizeExperiments()
  getStatus()
}

