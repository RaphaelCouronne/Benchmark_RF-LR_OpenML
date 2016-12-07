



plot_extrem_cases <- function(id, seed) {
  
  load(file = "Data/Results/Original/df.bmr_original.RData")
  source(file = "Research/DifferenceInModels/pdpInterpretabilityFunction.R")
  
  print(paste("acc :", df.bmr.diff$acc[id]))
  
  # load the task
  dataset_id = clas_used$did[id]
  omldataset = getOMLDataSet(dataset_id)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  task$task.desc$id = paste("dataset", dataset_id)
  task
  task$env$data
  
  
  # train the models
  lrn.classif.rf = makeLearner("classif.randomForest", 
                               importance = TRUE, predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.rf = train(lrn.classif.rf, task)
  
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.lr = train(lrn.classif.lr, task)
  
  fv2 = fit.classif.rf$learner.model$importance[,3]
  feature_importance_order = order(-fv2)
  
  # Do the PDP between 5% and 95% of the feature data
  gridsize = 10
  features.list = names(task$env$data)
  
  
  plot_pdp_list = NULL 
  
  for (i in c(1:3)) {
    index.temp = feature_importance_order[i]
    feature.temp = features.list[index.temp]
    
    quantiles = quantile(task$env$data[[feature.temp]], probs = c(0.05, 0.95))
    quantiles_list = as.list(quantiles)
    names(quantiles_list) = c(feature.temp, feature.temp)
    fmin=(quantiles_list[1])
    fmax=(quantiles_list[2])
    
    set.seed(seed)
    pd.rf = generatePartialDependenceData(fit.classif.rf, task, 
                                          features.list[index.temp], gridsize = gridsize,
                                          fmin = fmin, fmax = fmax)
    set.seed(seed)
    pd.lr = generatePartialDependenceData(fit.classif.lr, task, 
                                          features.list[index.temp], gridsize = gridsize)
    
    library(ggplot2)
    df.plot = data.frame(grid = pd.rf$data[[feature.temp]], 
                         rf = pd.rf$data$Probability,
                         lr = pd.lr$data$Probability)
    
    library(reshape2)
    df.plot.reshaped = reshape2::melt(df.plot, "grid")
    detach(package:reshape2, unload = TRUE)
    p = ggplot(df.plot.reshaped, aes_string(x = "grid", y="value", colour = "variable"))
    p = p+geom_line(size=1) + geom_point(size=3)
    print(p)
    
    plot_pdp_list[[i]] = p
  }
  
  library(cowplot)
  p.grid = plot_grid(plot_pdp_list[[1]],
                     plot_pdp_list[[2]],
                     plot_pdp_list[[3]],
                     ncol = 3, nrow = 1)
  

  
  print(p.grid)
}
