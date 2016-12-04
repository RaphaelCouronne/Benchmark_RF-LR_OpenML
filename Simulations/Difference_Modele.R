rm(list = ls())
library(mlr)


pdp_difference <- function(task, seed, visualize = FALSE) {
  ## Get the basic task data
  features.list = names(task$env$data)
  nFeatures = length(features.list)
  df_diff_pdp = data.frame(matrix(
    data = NA,
    nrow = nFeatures,
    ncol = 3
  ))
  names(df_diff_pdp) = c("l1", "l2", "linf")
  
  # Text bar progression
  pb <- txtProgressBar(min = 1, max = nFeatures, style = 3)
  
  for (i in c(1:nFeatures)) {
    ## rf and lr lmodels
    lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob")
    fit.classif.rf = train(lrn.classif.rf, task)
    lrn.classif.lr = makeLearner("classif.multinom", predict.type = "prob")
    fit.classif.lr = train(lrn.classif.lr, task)
    
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
      names(pd.plot)[1] <- feature.chosen.name[1]
      
      # reshape it
      library(reshape2)
      pd.plot.long <- melt(pd.plot, id = feature.chosen.name[1])
      detach("package:reshape2", unload = TRUE)
      names(pd.plot.long)[c(2, 3)] = c("Algorithm", "Probability")
      
      # ggplot it
      plot.PartialDependanceData <- ggplot(
        data = pd.plot.long,
        aes_string(
          x = feature.chosen.name,
          y = "Probability",
          colour = "Algorithm"
        )
      ) +
        geom_line(data = pd.plot.long,
                  aes(linetype = Algorithm) ,
                  size = 1) +
        labs(y = "Probability") +
        ylim(0, 1)
      
      plot.PartialDependanceData
    }
    
    
    df_diff_pdp[i, ] = c(abs(pd.diff / length(pd.diff)),
                         sqrt(pd.diff ^ 2 / length(pd.diff)),
                         max(abs(pd.diff)))
    
    # Puis diviser par nombre de features aussi !
    
    setTxtProgressBar(pb, i)
    
  }
  return(df_diff_pdp)
}


# test
pdp_difference(sonar.task, seed = 1, visualize = FALSE)
