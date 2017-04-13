



plot_extrem_cases <- function(data.id, seed=1, deltaAcc="NA", deltaModele = "NA") {
  
  
  # load the task
  omldataset = getOMLDataSet(data.id)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  task$task.desc$id = paste("dataset", data.id)
  task
  task$env$data
  
  #  Get the basic task data
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
  
  
  # train the models
  lrn.classif.rf = makeLearner("classif.randomForest", 
                               importance = TRUE, predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.rf = train(lrn.classif.rf, task)
  
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.lr = train(lrn.classif.lr, task)
  
  fv2 = fit.classif.rf$learner.model$importance[,3]
  feature_importance_order = order(-fv2)
  
  permutation.importance = fv2
  
  # Remove the negative importance from RF permutation
  permutation.importance[permutation.importance < 0] <- 0
  
  # If all 0 then set for all the same weight
  if (sum(permutation.importance)==0) permutation.importance[permutation.importance <= 0] <- 1
  
  permutation.percentage = permutation.importance/sum(permutation.importance)
  permutation.percentage = sapply(permutation.percentage,
                                  function(x) max(x,0))
  

  
  plot_pdp_list = NULL 
  
  for (i in c(1:min(3,length(features.list)))) {
    index.temp = feature_importance_order[i]
    feature.temp = features.list[index.temp]
    
    grid.size = 50
    
    if (features.list[index.temp] %in% features.list.numeric) {
      
      quantiles = quantile(task$env$data[[feature.temp]], probs = c(0, 1))
      quantiles_list = as.list(quantiles)
      names(quantiles_list) = c(feature.temp, feature.temp)
      fmin=(quantiles_list[1])
      fmax=(quantiles_list[2])
      
      set.seed(seed)
      pd.rf = generatePartialDependenceData(fit.classif.rf, task, 
                                            features.list[index.temp], gridsize = grid.size,
                                            fmin = fmin, fmax = fmax, resample = "bootstrap")
      set.seed(seed)
      pd.lr = generatePartialDependenceData(fit.classif.lr, task, 
                                            features.list[index.temp], gridsize = grid.size,
                                            fmin = fmin, fmax = fmax, resample = "bootstrap")
      
    } else if (features.list[index.temp] %in% features.list.factor) {
      set.seed(seed)
      pd.rf = generatePartialDependenceData(fit.classif.rf, task, 
                                            features.list[index.temp], gridsize = grid.size,
                                            resample = "bootstrap")
      set.seed(seed)
      pd.lr = generatePartialDependenceData(fit.classif.lr, task, 
                                            features.list[index.temp], gridsize = grid.size,
                                            resample = "bootstrap")
    }
    
    library(ggplot2)
    df.plot = data.frame(grid = pd.rf$data[[feature.temp]], 
                         rf = pd.rf$data$Probability,
                         lr = pd.lr$data$Probability)
    
    ###### Compute difference
    
    if (features.list[index.temp] %in% features.list.numeric) {
      gridsize = 10
    } else if (features.list[index.temp] %in% features.list.factor) {
      gridsize = length(levels(task$env$data[[features.list[index.temp]]]))
    }
    
    # Compute difference in partial dependence
    pd.rf = generatePartialDependenceData(
      fit.classif.rf,
      task,
      features = features.list[index.temp],
      gridsize = gridsize
    )
    pd.lr = generatePartialDependenceData(
      fit.classif.lr,
      task,
      features = features.list[index.temp],
      gridsize = gridsize
    )
    
    
    pd.diff = pd.rf$data$Probability[order(pd.rf$data[[features.list[index.temp]]])] - pd.lr$data$Probability[order(pd.lr$data[[features.list[index.temp]]])]
    
    # Compute the weights
    if (features.list[index.temp] %in% features.list.numeric) {
      grid = pd.rf$data[[features.list[index.temp]]]
      data.values = task$env$data[[features.list[index.temp]]]
      weights = getPercentageGrid(grid, data.values)
    } else if (features.list[index.temp] %in% features.list.factor) {
      
      # compute the density of observation according to several classes of this categorical feature
      summed = rep(NA, length(levels(task$env$data[[features.list[index.temp]]])))
      summed = sapply(pd.rf$data[[features.list[index.temp]]], function(x) length(which(task$env$data[[features.list[index.temp]]]==x)))
      weights = summed / sum(summed)
    }
    
    
    # Save the information
    df_diff_pdp = sum(weights%*%abs(pd.diff))
    ######
    
    library(reshape2)
    df.plot.reshaped = reshape2::melt(df.plot, "grid")
    detach(package:reshape2, unload = TRUE)
    p = ggplot(df.plot.reshaped, aes_string(x = "grid", y="value", colour = "variable"))
    p = p+geom_line(size=1) + geom_point(size=3) + ylim(0,1) +
      xlab(features.list[index.temp]) + 
      ylab("Probability") +
      ggtitle(paste("Relative importance :", format(round(permutation.percentage[index.temp], 3), nsmall = 3),
                    "Difference :", format(round(df_diff_pdp, 3), nsmall = 3),
                    "Difference*Importance", format(round(df_diff_pdp*permutation.percentage[index.temp], 3), nsmall = 3)))
    print(p)
    
    plot_pdp_list[[i]] = p
  }
  
  # Plot the points for the 2 most important

  
  p.2 = ggplot(task$env$data, aes_string(x = task$env$data[[features.list[feature_importance_order[1]]]],
                                         y = task$env$data[[features.list[feature_importance_order[2]]]],
                                         colour = task$env$data[[target]])) +
    xlab(features.list[feature_importance_order[1]]) +
    ylab(features.list[feature_importance_order[2]]) +
    ggtitle(label = "Plot on the 2 most importance features (RF VIM)", subtitle = paste("Delta acc is", deltaAcc,
                                                                                        "and Delta pd is", deltaModele)) +
    theme(legend.title=element_blank()) +
    geom_point(size=1)
  
  print(p.2)
  
  
  # Plot the 3
  library(cowplot)
  p.grid = plot_grid(plot_pdp_list[[1]],
                     plot_pdp_list[[2]],
                     plot_pdp_list[[3]],
                     ncol = 1, nrow = 3)
  

  
  print(p.grid)
}


#plot_extrem_cases(data.id)


getPercentageGrid = function(grid, data.values) {
  grid.limits = c(grid-diff(grid)[1]/2, grid[length(grid)]+diff(grid)[1]/2)
  countBins = function(i) sum(data.values >= grid.limits[i] & data.values < grid.limits[i+1] )
  countValues = sapply(1:length(grid), countBins)
  countValuesPercentage = countValues/sum(countValues)
  return(countValuesPercentage)
}


