testPdpComp = function(rapportBeta) {
  
  
  # Parameters
  configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
  grid.size = 10
  
  # Raw gaussian distribution
  n = 5e3
  X1<-runif(n, min = -0.5, max = 1)
  X2<-runif(n, min = -1, max = 1)
  X3<-rbinom(n, 3, 0.5)
  X3 = as.factor(X3)
  
  # Adapt to model chosen
  Beta0 = 0
  Beta1 = 8
  Beta2 = 0
  BetaNonLinear = -rapportBeta*Beta1
  
  # Model 2 is interraction
  
  Beta=c(Beta0,Beta1, Beta2)
  X=cbind(1,X1,X2)
  product=X%*%Beta+BetaNonLinear*abs(X2)
  
  
  Y<-as.factor(rbinom(n,1,prob = plogis(product)))
  df<-data.frame(X1,X2,X3, X3, X3, X3, X3, Y)
  df<-data.frame(X1,X2,X3, Y)
  classif.task<-makeClassifTask(data = df, target="Y")
  classif.task
  task = classif.task
  
  # Plot it
  # Visualization of the generated datas
  plot.data <- ggplot(data=df, aes(x=X1, y=X2, colour=Y, shape = Y))
  plot.data <- plot.data + geom_point(size=2) 
  plot.data = plot.data + scale_colour_grey(start = 0,end = 0.6) 
  print(plot.data)
  
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
  permutation.permutation.percentage = permutation.importance/sum(permutation.importance)
  permutation.permutation.percentage = sapply(permutation.permutation.percentage,
                                              function(x) max(x,0))
  
  
  # Create dataframe to save results
  df_diff_pdp = data.frame(features = features.list, difference = NA, difference.weight = NA)
  
  # Order according to feature name
  df_diff_pdp = df_diff_pdp[order(df_diff_pdp$features),]
  df_diff_pdp$permutation.importance = permutation.importance[order(names(permutation.importance))]
  df_diff_pdp$permutation.permutation.percentage = permutation.permutation.percentage[order(names(permutation.importance))]
  
  for (i in c(1:nFeatures)) {
    
    # Compute difference in partial dependence
    pd.rf = generatePartialDependenceData(
      fit.classif.rf,
      task,
      features = features.list[i],
    )
    pd.lr = generatePartialDependenceData(
      fit.classif.lr,
      task,
      features = features.list[i],
    )
    pd.diff = pd.rf$data$Probability - pd.lr$data$Probability
    
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
  difference.all = permutation.permutation.percentage%*%data.matrix(df_diff_pdp[,c(2,3)])
  difference.imp.all = data.frame(((df_diff_pdp$permutation.importance)/sum(df_diff_pdp$permutation.importance))%*%data.matrix(df_diff_pdp[,c(2,3)]))
  names(difference.imp.all) = c("difference.imp.all", "difference.imp.weight.all")
  
  # Difference for top 1 feature
  feature.top1 = names(permutation.permutation.percentage)[order(permutation.permutation.percentage)][length(permutation.permutation.percentage)]
  difference.top1 = subset(df_diff_pdp, features == feature.top1)[,c(2,3)]
  names(difference.top1) = c("difference.top1", "differene .weight.top1")
  
  # Difference for top 3 feature
  n.feat = length(permutation.permutation.percentage)
  feature.top3 = names(permutation.permutation.percentage)[order(permutation.permutation.percentage)][n.feat:(n.feat-2)]
  df_diff_pdp.top3 = subset(df_diff_pdp, features %in% feature.top3)
  difference.top3 = t(data.matrix(apply(df_diff_pdp.top3[,c(2,3)], 2, mean)))
  names(difference.top3) = c("difference.top3", "difference .weight.top3")
  
  difference.importance.top3 = data.frame(((df_diff_pdp.top3$permutation.importance)/sum(df_diff_pdp.top3$permutation.importance))%*%data.matrix(df_diff_pdp.top3[,c(2,3)]))
  names(difference.importance.top3) = c("difference.imp.top3", "difference.imp.weight.top3")
  
  # Get performances
  perfs = getAccPerfs(mlrtask)
  perfs = perfs$acc.test.mean[1]-perfs$acc.test.mean[2]
  
  res = data.frame(perfs, difference.all, difference.imp.all, difference.top1, difference.top3, difference.importance.top3)
  return(res)
  
  
  # Functions 
  getPercentageGrid = function(grid, data.values) {
    grid.limits = c(grid-diff(grid)[1]/2, grid[length(grid)]+diff(grid)[1]/2)
    countBins = function(i) sum(data.values >= grid.limits[i] & data.values < grid.limits[i+1] )
    countValues = sapply(1:length(grid), countBins)
    countValuesPercentage = countValues/sum(countValues)
    return(countValuesPercentage)
  }
  
  getAccPerfs = function(task) {
    lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
    lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
    lrn.list = list(lrn.classif.rf, lrn.classif.lr)
    rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4, stratify = TRUE)
    configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
    bmr = benchmark(lrn.list, task, rdesc, acc, keep.pred = FALSE, models = FALSE, show.info = FALSE)
    bmr
    u = getBMRAggrPerformances(bmr, as.df = TRUE)
    return(u)
  }
  
}


testPdpComp(1)


rapport.vect = seq(from = 0.1, to = 1.5, length.out = 10)
res = lapply(rapport.vect, testPdpComp)
res2= do.call("rbind", res) 

plot(res2$perfs)
plot(res2$difference, res2$acc.test.mean)
plot(res2$difference.weight)
plot(res2$difference.1)
plot(res2$difference.weight.1)
plot(res2$difference.top1)
plot(res2$difference.2)
plot(res2$difference.3)
plot(res2$difference.weight.3)
plot(res2$difference)
plot(res2$difference)

