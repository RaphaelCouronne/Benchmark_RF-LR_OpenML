rm(list = ls())
library(mlr)

library(OpenML)


pdp_difference <- function(task, seed, visualize = FALSE) {
  
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE, show.info = TRUE)
  
  ## Get the basic task data
  target_position = which(names(task$env$data)%in%task$task.desc$target)
  if (identical(target_position,integer(0))) {
    print("Error : target not found")
  }
  
  features.list = names(task$env$data)[-target_position]
  nFeatures = length(features.list)
  df_diff_pdp = data.frame(matrix(
    data = NA,
    nrow = nFeatures,
    ncol = 3
  ))
  names(df_diff_pdp) = c("l1", "l2", "linf")
  row.names(df_diff_pdp) <- features.list
  
  ## rf and lr lmodels
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", importance=TRUE)
  fit.classif.rf = train(lrn.classif.rf, task)
  lrn.classif.lr = makeLearner("classif.multinom", predict.type = "prob")
  fit.classif.lr = train(lrn.classif.lr, task)
  
  importances = fit.classif.rf$learner.model$importance
  permutation_importance = importances[,3]
  permutation_importance_percentage = permutation_importance/sum(permutation_importance)
  
  # Text bar progression
  pb <- txtProgressBar(min = 1, max = nFeatures, style = 3)
  
  for (i in c(1:nFeatures)) {
    ## Generate partial dependance
    set.seed(seed)
    pd.rf = generatePartialDependenceData(
      fit.classif.rf,
      task,
      features = features.list[i],
      resample = "subsample",
      gridsize = task$task.desc$size
    )
    
    set.seed(seed)
    pd.lr = generatePartialDependenceData(
      fit.classif.lr,
      task,
      features = features.list[i],
      resample = "subsample",
      gridsize = task$task.desc$size
    )
    
    pd.diff = pd.rf$data$Probability - pd.lr$data$Probability
    
    ## plot the partial dependance
    
    if (visualize) {
      # Generate the dataframe of values
      pd.plot <-
        data.frame(
          feature = pd.rf$data[, 3],
          RF = pd.rf$data$Probability,
          LR = pd.lr$data$Probability
        )
      pd.plot
      names(pd.plot)[1] <- features.list[i]
      
      # reshape it
      library(reshape2)
      pd.plot.long <- melt(pd.plot, id = features.list[i])
      detach("package:reshape2", unload = TRUE)
      names(pd.plot.long)[c(2, 3)] = c("Algorithm", "Probability")
      
      # ggplot it
      plot.PartialDependanceData <- ggplot(
        data = pd.plot.long,
        aes_string(
          x = features.list[i],
          y = "Probability",
          colour = "Algorithm"
        )
      ) +
        geom_line(data = pd.plot.long,
                  aes(linetype = Algorithm) ,
                  size = 1) +
        labs(y = "Probability") +
        ylim(0, 1) + geom_point()
      
      print(plot.PartialDependanceData)
    }
    
    occurences = table(task$env$data[[features.list[i]]])
    
    # Check the length of occurence
    
    if(length(occurences)!=length(pd.diff)) {
      print("Problem with the length of occurence")
      print("Occurence has been set to 1")
      occurences = rep(1,length(pd.diff))
    }
    
    weights = occurences/sum(occurences)
    
    df_diff_pdp[i, ] = c(abs(pd.diff) %*% weights,
                         sqrt(pd.diff ^ 2 %*% weights),
                         max(abs(pd.diff)))
    

    
    setTxtProgressBar(pb, i)
    
  }
  res =  t(data.matrix(permutation_importance_percentage)) %*% data.matrix(df_diff_pdp) 
  return(res)
}


# test OML task
# Loading the dataset
load(file = "Data/Results/clas_time.RData")
clas_time$did
omldataset = getOMLDataSet(data.id = 346, verbosity = 0)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
mlrtask$env$data
pdp_difference(mlrtask, seed = 1, visualize = TRUE)

# test sonar.task
#pdp_difference(sonar.task, seed = 1, visualize = FALSE)
