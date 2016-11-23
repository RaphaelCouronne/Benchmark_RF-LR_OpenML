convert_results <- function(clas_used, result, target_path) {
  
  library(mlr)
  library(gridExtra)
  library(ggplot2)
  library(cowplot)

  
  MEASURES = list(acc, auc, brier, timetrain)
  
  ################################################################################################################################
  # Creation of the dataset
  ################################################################################################################################
  
  ## Load and convert the reasults to a data frame ----
  leaner.id.lr = "classif.logreg"
  learner.id.randomForest = "classif.randomForest"
  
  
  
  clas_used = clas_used[c(1:length(result)),]
  
  
  
  # remove the ones with error messages
  res.errorMessages = which(!sapply(result, function(x) typeof(x)=="list"))
  if (length(res.errorMessages)>0) {
    result = result[-res.errorMessages]
    clas_used = clas_used[-res.errorMessages,]
  }
  
  # aggregate the results
  res.perfs = lapply(result, function(x) getBMRAggrPerformances(x, as.df=TRUE))
  
  # detect and removes the nas
  res.perfs.nas = which(sapply(res.perfs, function(x) any(is.na(x))))
  if (!(identical(integer(0), res.perfs.nas))) {
    res.perfs = res.perfs[-res.perfs.nas]
    clas_used = clas_used[-res.perfs.nas,]
  }
  
  # convert to a data.frame
  res.perfs.df = do.call("rbind", res.perfs) 
  
  # get the difference of performances
  perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
  perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)
  perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]
  library(reshape2)
  perfsAggr.diff.melted = melt(perfsAggr.diff)
  detach(package:reshape2, unload = TRUE)
  
  
  
  ## Compute the dataset of difference with the features
  
  # number  of features
  p = clas_used$NumberOfFeatures
  
  # number of numeric attributes
  pnum = clas_used$NumberOfNumericFeatures
  
  # number of categorical attributes
  psymbolic = clas_used$NumberOfSymbolicFeatures
  
  # number of samples
  n = clas_used$NumberOfInstances
  
  # n/p
  psurn = p/n
  
  # Numerical attributes rate
  pnumrate = pnum/p
  
  # %Cmax Percentage of elements of the majority class
  Cmax = clas_used$MajorityClassSize/n
  
  
  df.bmr.diff = data.frame(perfsAggr.diff,
                           logp = log(clas_used$NumberOfFeatures), 
                           logn = log(clas_used$NumberOfInstances),
                           logdimension = log(clas_used$dimension),
                           logpsurn = log(clas_used$NumberOfFeatures/clas_used$NumberOfInstances),
                           logdimensionsurn = log(clas_used$dimension/clas_used$NumberOfInstances),
                           lograpportMajorityMinorityClass = log(clas_used$MajorityClassSize/clas_used$MinorityClassSize),
                           pnum, psymbolic, pnumrate, Cmax
  )
  
  
  paste("Is there any nas ? :", any(is.na(df.bmr.diff)))
  
  
  measures.names = sapply(MEASURES, function(x) paste0(x$id,".test.mean"))
  features.names = names(df.bmr.diff)[which(!(names(df.bmr.diff) %in% measures.names))]
  
  ## Compute the ranks
  convertModifiedBMRToRankMatrix <- function(bmr.all, measure = NULL, ties.method = "average") {
    
    measure.name = paste(measure$id,".test.mean", sep = "")
    df = aggregate(bmr.all[[measure.name]], by = list(task.id = bmr.all$task.id,
                                                      learner.id = bmr.all$learner.id),
                   FUN = mean)
    
    # calculate ranks, rank according to minimize option of the measure
    if (!measure$minimize)
      df$x = -df$x
    df = plyr::ddply(df, "task.id", function(d) {
      d$alg.rank = rank(d$x, ties.method = ties.method)
      return(d)
    })
    
    # convert into matrix, rows = leaner, cols = tasks
    df = reshape2::melt(df, c("task.id", "learner.id"), "alg.rank")
    df = reshape2::dcast(df, learner.id ~ task.id )
    task.id.names = setdiff(colnames(df), "learner.id")
    mat = as.matrix(df[, task.id.names])
    rownames(mat) = df$learner.id
    colnames(mat) = task.id.names
    return(mat)
  }
  print("Results converted")
  
  # Save it ----
  save(df.bmr.diff, res.perfs.df, convertModifiedBMRToRankMatrix, perfsAggr.diff.melted, perfsAggr.diff, clas_used, file = target_path)
}
