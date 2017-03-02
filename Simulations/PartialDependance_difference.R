library(mlr)
library(OpenML)


# Computation of the individual pdp difference (on all dataset)
pdpDifferenceAllDatasets = function(clas, seed=1, force = FALSE, visualize = TRUE,
                                     target.path = "Data/Simulations/pdp.difference.RData") {

n.row = nrow(clas)
options(warn = -1)

  if (file.exists(target.path) && !force) {
    
    # load the file
    print("load file")
    load(file = target.path)
    
    # check integrity of datasets
    if (!identical(clas$data.id, pdp.difference$data.id)) {
      stop("Stop : conflict in the datasets")
    }
    i_beginning = which(pdp.difference$done)[length(which(pdp.difference$done))]
    
    # check if already done
    if (i_beginning==n.row) {
      stop("Pdp difference is already computed")
    }
    
  } else {
    # Create the pdp.difference dataframe
    pdp.difference = data.frame(matrix(data = NA, nrow = n.row, ncol = 15))
    names(pdp.difference) = c("index", "data.id", "task.id", "n", "p", "began", "done", 
                              "loaded","converted", "pdp.l1", "pdp.l2", "pdp.linf", 
                              "pdp.l1_first3", "pdp.l2_first3", "pdp.linf_first3")
    pdp.difference$index = c(1:n.row)
    pdp.difference$data.id = clas$data.id
    pdp.difference$task.id = clas$task.id
    pdp.difference$n = clas$n
    pdp.difference$p = clas$p
    # Initialize i (for loop)
    i_beginning = 1
  }
  
  pb <- txtProgressBar(min = i_beginning, max = n.row, style = 3)
  
  for(j in c(i_beginning:n.row)){
    
    # begin
    pdp.difference$began[j] = TRUE
    
    tryCatch({
      
      set.seed(seed)
      
      # Loading the dataset
      omldataset = getOMLDataSet(data.id = clas$data.id[j], verbosity = 0)
      if (identical(omldataset$target.features, character(0))) {
        omldataset$target.features="Class"
        omldataset$desc$default.target.attribute="Class"
      }
      pdp.difference$loaded[j] = "TRUE" 
      
      
      # Transform to mlr task
      configureMlr(on.learner.error = "warn", show.learner.output = TRUE, show.info = FALSE)
      mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
      pdp.difference$converted[j] = TRUE
      
      # Get the Pdp difference
      pdp.difference.all <- getPdpDifference(mlrtask, seed = seed, 
                                             visualize = visualize, progression_bar = FALSE)
      pdp.difference[j,c(10:12)] <- pdp.difference.all$all
      pdp.difference[j,c(13:15)] <- pdp.difference.all$first3
      
    }, error = function(e) return(paste0("The variable '", j, "'", 
                                         " caused the error: '", e, "'")))
    
    try(setTxtProgressBar(pb, j))
    pdp.difference$done[j] = TRUE
    save(pdp.difference, file = target.path)
  }
  options(warn = 0)
  return(pdp.difference)
}



# =========================================
# Individual pdp difference (on one dataset)

getPdpDifference = function(task, seed, visualize = FALSE, progression_bar = TRUE) {
  
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
  permutation_importance_percentage = sapply(permutation_importance_percentage,
                                             function(x) max(x,0))
  
  # Text bar progression
  if (progression_bar) pb <- txtProgressBar(min = 1, max = nFeatures, style = 3)
  
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
      pd.plot = data.frame(
        feature = pd.rf$data[, 3],
        RF = pd.rf$data$Probability,
        LR = pd.lr$data$Probability
      )
      pd.plot
      names(pd.plot)[1] <- features.list[i]
      
      # Keep the 5%-95% part of the data if numeric
      if (is.numeric(task$env$data[[features.list[i]]])) {
        quantile.10 = as.numeric(quantile(pd.plot[,1], probs = 0.1))
        quantile.90 = as.numeric(quantile(pd.plot[,1], probs = 0.9))
        
        pd.plot = pd.plot[which((pd.plot[,1]>quantile.10)&(pd.plot[,1]<quantile.90)),]
      }
      
      # reshape it
      library(reshape2)
      pd.plot.long = melt(pd.plot, id = features.list[i])
      detach("package:reshape2", unload = TRUE)
      names(pd.plot.long)[c(2, 3)] = c("Algorithm", "Probability")
      
      # ggplot it
      plot.PartialDependanceData = ggplot(
        data = pd.plot.long,
        aes_string(
          x = features.list[i],
          y = "Probability",
          colour = "Algorithm"
        )
      ) + labs(y = "Probability") +
        ylim(0, 1) 
      
      if (is.numeric(task$env$data[[features.list[i]]])) {
        
        plot.PartialDependanceData = plot.PartialDependanceData + 
          geom_line(data = pd.plot.long,
                    aes(linetype = Algorithm) ,
                    size = 1) + geom_point()
        
      } else {
        plot.PartialDependanceData = plot.PartialDependanceData + 
          geom_point(data = pd.plot.long,
                     aes(linetype = Algorithm) ,
                     size = 1)
      }
      
      print(plot.PartialDependanceData)
    }
    
    occurences = table(task$env$data[[features.list[i]]])
    
    # Check the length of occurence
    if(length(occurences)!=length(pd.diff)) {
      print("Problem with the length of occurence")
      print("Occurence has been set to 1")
      occurences = rep(1,length(pd.diff))
    }
    
    df_diff_pdp[i, ] = c((abs(pd.diff) %*% occurences)/sum(occurences),
                         sqrt(pd.diff ^ 2 %*% occurences)/sum(occurences),
                         max(abs(pd.diff)))
    
    
    
    if (progression_bar) setTxtProgressBar(pb, i)
    
  }
  res = NULL
  res$all =  t(data.matrix(permutation_importance_percentage)) %*% data.matrix(df_diff_pdp) 
  
  # first three features
  first3 = order(permutation_importance_percentage)[c(1:min(3,nFeatures))]
  res$first3 = t(data.matrix(apply(df_diff_pdp[first3,], 2, mean)))
  return(res)
}


# 
# task.id = 4361
# 
# 
# omldataset = getOMLDataSet(data.id = clas$data.id[j], verbosity = 0)
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# pdp.difference$loaded[j] = "TRUE" 
# 
# 
# # Transform to mlr task
# configureMlr(on.learner.error = "warn", show.learner.output = TRUE, show.info = FALSE)
# mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
# pdp.difference$converted[j] = TRUE
# 
# # Get the Pdp difference
# pdp.difference.all <- getPdpDifference(mlrtask, seed = seed, 
#                                        visualize = visualize, progression_bar = FALSE)
# 
# 



# 
# 
#test OML task
#Loading the dataset
# load(file = "Data/Results/clas_time.RData")
# clas_time$data.id
# omldataset = getOMLDataSet(data.id = 905, verbosity = 0)
# if (identical(omldataset$target.features, character(0))) {
#   omldataset$target.features="Class"
#   omldataset$desc$default.target.attribute="Class"
# }
# mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
# mlrtask$env$data
# pdp = getPdpDifference(mlrtask, seed = 1, visualize = TRUE)
# pdp$all
# 
# # test sonar.task
# #pdp.difference(sonar.task, seed = 1, visualize = FALSE)
# 
# 
# # test all datasets
# load(file = "Data/Results/Original/clas_time_original.RData")
# clas_used = clas_time
# u = pdp.difference.allDatasets(clas_used, seed= 1, force = FALSE, visualize = TRUE, dataset_count = 12)
# 
# load("Data/Results/pdp.difference/pdp.difference.RData")


# Test the code with simulated examples from PDP examples
