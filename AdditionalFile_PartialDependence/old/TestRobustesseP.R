
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)
omldatasets = clas_used$data.id

## Probailitités

test = function(x) {

# get the dataset
omldataset = getOMLDataSet(x)
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
measures = list(acc, brier, auc, timetrain)
rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = TRUE, models = FALSE, show.info = TRUE)

bmr.res = getBMRAggrPerformances(bmr, as.df = TRUE)
predictions = getBMRPredictions(bmr)
pred.proba.rf = predictions[[1]]$classif.randomForest$data[,3]
pred.proba.lr = predictions[[1]]$classif.logreg$data[,3]

pred.proba.diff.l1 = sum(abs(pred.proba.rf - pred.proba.lr))/length(pred.proba.lr)
pred.proba.diff.l2 = sum(abs(pred.proba.rf - pred.proba.lr)^2)/length(pred.proba.lr)

bmr.res$pred.proba.diff.l1=pred.proba.diff.l1
bmr.res$pred.proba.diff.l2=pred.proba.diff.l2

return(bmr.res)
}


results = lapply(rep(721,20), test)

res.perfs.df = do.call("rbind", results) 




## Importances


testImportanceRobustesse = function(x) {
  omldataset = getOMLDataSet(x)
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
  
  #fv = generateFilterValuesData(task, method = "randomForestSRC.rfsrc")
  #fv.values = fv$data[,3][order(fv$data$name)]
  
  #res = data.frame(permutation.importance)
  res = data.frame(permutation.percentage)
  return(res)
}

omldatasets
i = 41
data.id = omldatasets[i]

results = lapply(rep(data.id,150), testImportanceRobustesse)

res.perfs.df = do.call("cbind", results) 

for (i in c(1:9)) hist(data.matrix(res.perfs.df[i,]))

## Partial dependance juste

## Difference in Partial dependance
res.perfs.df2 = res.perfs.df
for (i in c(1:9)) hist(data.matrix(res.perfs.df2[i,]))



hist(data.matrix(res.perfs.df[2,]))
hist(data.matrix(res.perfs.df2[2,]))



testPdp10Robustesse = function(x) {
  
  getPercentageGrid = function(grid, data.values) {
    grid.limits = c(grid-diff(grid)[1]/2, grid[length(grid)]+diff(grid)[1]/2)
    countBins = function(i) sum(data.values >= grid.limits[i] & data.values < grid.limits[i+1] )
    countValues = sapply(1:length(grid), countBins)
    countValuesPercentage = countValues/sum(countValues)
    return(countValuesPercentage)
  }
  
  # Parameters
  configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
  grid.size = 10
  
  # Loading the dataset
  omldataset = getOMLDataSet(x)
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
  print(permutation.percentage)
  
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
    
    print(pd.diff)
    
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
  # perfs = getAccPerfs(task)
  # perfs = perfs$acc.test.mean[1]-perfs$acc.test.mean[2]
  
  res = data.frame(difference.all, difference.imp.all, difference.fv.all, difference.top1, difference.top3, difference.importance.top3)
  return(res)
}




omldatasets
i = 43
data.id = omldatasets[i]
data.id

results = lapply(rep(data.id,100), testPdp10Robustesse)

res.perfs.df = do.call("rbind", results) 

hist(res.perfs.df$difference)
hist(res.perfs.df$difference.weight)
hist(res.perfs.df$difference.imp.all)
hist(res.perfs.df$difference.fv.all)
hist(res.perfs.df$difference.fv.weight.all)
hist(res.perfs.df$difference.top1)
hist(res.perfs.df$differene.weight.top1)
hist(res.perfs.df$difference.top3)
hist(res.perfs.df$difference.weight.top3)
hist(res.perfs.df$difference.imp.top3)
hist(res.perfs.df$difference.imp.weight.top3)

