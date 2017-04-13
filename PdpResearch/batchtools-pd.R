rm(list = ls())
library(mlr)
library(plyr)
library(batchtools)
library(OpenML)
saveOMLConfig(apikey = "7a4391537f767ea70db6af99497653e5", arff.reader = "RWeka", overwrite=TRUE)


setBatchtoolsPDPExperiment = function(seed = 1, ncpus = 2, 
                                   clas_used,
                                   nameExperiment =  paste("Data/Results/Batchtools/batchtool_PartialDependance")) {
  
  # which subset of dataset
  omldatasets = clas_used$data.id
  
  
  unlink(nameExperiment, recursive = TRUE)
  regis.pdp = makeExperimentRegistry(nameExperiment, seed = seed,
                                      packages = c("mlr", "OpenML", "methods"), 
                                      #source = paste0("PdpResearch/ComputeModelDifference.R"),
                                      work.dir = paste0("Data/Results/Batchtools"),
                                      #conf.file = paste0("Data/Batchtools/.batchtools.conf.R")
  )
  
  regis.pdp$cluster.functions = makeClusterFunctionsMulticore(ncpus = ncpus) 
  
  
  
  # add selected OML datasets as problems
  for (did in omldatasets) {
    data = list(did = did)
    addProblem(name = as.character(did), data = data)
  }
  
  
  # add one generic 'algo' that compute the difference in models
  addAlgorithm("eval", fun = function(job, data, instance,  ...) {
    par.vals = list(...)
    
    
    getPercentageGrid = function(grid, data.values) {
      grid.limits = c(grid-diff(grid)[1]/2, grid[length(grid)]+diff(grid)[1]/2)
      countBins = function(i) sum(data.values >= grid.limits[i] & data.values < grid.limits[i+1] )
      countValues = sapply(1:length(grid), countBins)
      countValuesPercentage = countValues/sum(countValues)
      return(countValuesPercentage)
    }
    
    
    getAccPerfsandProbabilityDifference = function(task) {
      lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
      lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
      lrn.list = list(lrn.classif.rf, lrn.classif.lr)
      rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4, stratify = TRUE)
      configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
      bmr = benchmark(lrn.list, task, rdesc, acc, keep.pred = TRUE, models = FALSE, show.info = FALSE)
      probability.difference.l1 = bmr$learners$classif.logreg
      u = getBMRAggrPerformances(bmr, as.df = TRUE)
      
      # Get the performance and difference for acc
      AggrPerformances = getBMRAggrPerformances(bmr, as.df = TRUE)
      bmr.res = data.frame(
        rf.acc = AggrPerformances$acc.test.mean[which(AggrPerformances$learner.id=="classif.randomForest")],
        lr.acc = AggrPerformances$acc.test.mean[which(AggrPerformances$learner.id=="classif.logreg")],
        diff.acc = AggrPerformances$acc.test.mean[which(AggrPerformances$learner.id=="classif.randomForest")]-
          AggrPerformances$acc.test.mean[which(AggrPerformances$learner.id=="classif.logreg")])
      
      # Get the difference in probability
      predictions = getBMRPredictions(bmr)
      
      pred.proba.rf = predictions[[1]]$classif.randomForest$data[,3]
      pred.proba.lr = predictions[[1]]$classif.logreg$data[,3]
      
      pred.proba.diff.l1 = sum(abs(pred.proba.rf - pred.proba.lr))/length(pred.proba.lr)
      pred.proba.diff.l2 = sum(abs(pred.proba.rf - pred.proba.lr)^2)/length(pred.proba.lr)
      
      bmr.res$pred.proba.diff.l1=pred.proba.diff.l1
      bmr.res$pred.proba.diff.l2=pred.proba.diff.l2
      
      return(bmr.res)
    }
    
    # Parameters
    configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
    
    # Loading the dataset
    omldataset = getOMLDataSet(data$did)
    if (identical(omldataset$target.features, character(0))) {
      omldataset$target.features="Class"
      omldataset$desc$default.target.attribute="Class"
    }
    task = convertOMLDataSetToMlr(omldataset)
    
    # Get the basic task data
    target_position = which(names(task$env$data)%in%task$task.desc$target)
    if (identical(target_position,integer(0))) {
      print("Error : target not found")
    }
    
    features.list = names(task$env$data)[-target_position]
    nFeatures = length(features.list)
    
    ## See which features are numeric or symbolic
    target = task$task.desc$target
    X.train = task$env$data[!colnames(task$env$data) %in% c(target)]
    
    # get the number and type of features
    type.list = sapply(X.train, class)
    
    # get the index of types
    index.numeric = which(type.list == "numeric")
    index.factor = which(type.list == "factor")
    features.list.numeric = features.list[index.numeric] 
    features.list.factor = features.list[index.factor] 
    
    # Computation of partial dependence
    
    ## rf and lr lmodels
    lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", importance=TRUE)
    fit.classif.rf = train(lrn.classif.rf, task)
    lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob")
    fit.classif.lr = train(lrn.classif.lr, task)
    
    importances = fit.classif.rf$learner.model$importance
    permutation.importance = importances[,3]
    
    # Remove the negative importance from RF permutation
    permutation.importance[permutation.importance < 0] <- 0
    
    # If all 0 then set for all the same weight
    if (sum(permutation.importance)==0) permutation.importance[permutation.importance <= 0] <- 1
    
    permutation.percentage = permutation.importance/sum(permutation.importance)
    permutation.percentage = sapply(permutation.percentage,
                                    function(x) max(x,0))
    
    
    fv = generateFilterValuesData(task, method = "information.gain")
    fv.values = fv$data$information.gain[order(fv$data$name)]
    
    
    # Create dataframe to save results
    df_diff_pdp = data.frame(features = features.list, difference = NA, difference.weight = NA)
    
    # Order according to feature name
    df_diff_pdp = df_diff_pdp[order(df_diff_pdp$features),]
    df_diff_pdp$permutation.importance = permutation.importance[order(rownames(importances))]
    df_diff_pdp$permutation.percentage = permutation.percentage[order(rownames(importances))]
    
    for (i in c(1:nFeatures)) {
      if (features.list[i] %in% features.list.numeric) {
        gridsize = 10
      } else if (features.list[i] %in% features.list.factor) {
        gridsize = length(levels(task$env$data[[features.list[i]]]))
      }
      
      # Compute difference in partial dependence
      pd.rf = generatePartialDependenceData(
        fit.classif.rf,
        task,
        features = features.list[i],
        gridsize = gridsize
      )
      pd.lr = generatePartialDependenceData(
        fit.classif.lr,
        task,
        features = features.list[i],
        gridsize = gridsize
      )
      
      
      pd.diff = pd.rf$data$Probability[order(pd.rf$data[[features.list[i]]])] - pd.lr$data$Probability[order(pd.lr$data[[features.list[i]]])]
      
      # Compute the weights
      if (features.list[i] %in% features.list.numeric) {
        grid = pd.rf$data[[features.list[i]]]
        data.values = task$env$data[[features.list[i]]]
        weights = getPercentageGrid(grid, data.values)
      } else if (features.list[i] %in% features.list.factor) {
        
        # compute the density of observation according to several classes of this categorical feature
        summed = rep(NA, length(levels(task$env$data[[features.list[i]]])))
        summed = sapply(pd.rf$data[[features.list[i]]], function(x) length(which(task$env$data[[features.list[i]]]==x)))
        weights = summed / sum(summed)
      }
      
      
      
      # Save the information
      df_diff_pdp[i,c(2,3)] = c(mean(abs(pd.diff)),sum(weights%*%abs(pd.diff)))
    }
    
    # Differences for all features
    difference.all = data.frame(t(data.matrix(apply(df_diff_pdp[,c(2,3)], 2, mean))))
    difference.imp.all = data.frame(permutation.percentage%*%data.matrix(df_diff_pdp[,c(2,3)]))
    names(difference.imp.all) = c("difference.imp.all", "difference.imp.weight.all")
    
    difference.fv.all = data.frame(((fv.values)/sum(fv.values))%*%data.matrix(df_diff_pdp[,c(2,3)]))
    names(difference.fv.all) = c("difference.fv.all", "difference.fv.weight.all")
    
    # Difference for top 1 feature
    feature.top1 = df_diff_pdp$features[order(df_diff_pdp$permutation.importance)][length(df_diff_pdp$permutation.importance)]
    difference.top1 = subset(df_diff_pdp, features == feature.top1)[,c(2,3)]
    names(difference.top1) = c("difference.top1", "differene.weight.top1")
    
    # Difference for top 3 feature
    n.feat = length(df_diff_pdp$permutation.importance)
    feature.top3 = df_diff_pdp$features[order(df_diff_pdp$permutation.importance)][n.feat:max((n.feat-2),0)]
    df_diff_pdp.top3 = subset(df_diff_pdp, features %in% feature.top3)
    difference.top3 = data.frame(t(data.matrix(apply(df_diff_pdp.top3[,c(2,3)], 2, mean))))
    names(difference.top3) = c("difference.top3", "difference.weight.top3")
    
    difference.importance.top3 = data.frame(((df_diff_pdp.top3$permutation.importance)/sum(df_diff_pdp.top3$permutation.importance))%*%data.matrix(df_diff_pdp.top3[,c(2,3)]))
    names(difference.importance.top3) = c("difference.imp.top3", "difference.imp.weight.top3")
    
    # # Get performances
    perfs = getAccPerfsandProbabilityDifference(task)
    
    res = data.frame(perfs, difference.all, difference.imp.all, difference.fv.all, difference.top1, difference.top3, difference.importance.top3)
    return(res)
    
  })

  
  
  # finalize experiment
  # set.seed(1)
  ades = data.frame(c(1))
  addExperiments(algo.designs = list(eval = ades))
  summarizeExperiments()
  getStatus()
  }
