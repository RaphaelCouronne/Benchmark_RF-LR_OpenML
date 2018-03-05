convert_results <- function(clas_used, regis, target_path) {
  
  library(mlr)
  library(gridExtra)
  library(ggplot2)
  library(cowplot)
  library(batchtools)
  
  
  
  ## 1. Remove the nas in result, and create the dataframe associated with results
  result = reduceResultsList(ids = 1:278, reg = regis)
  clas_used_original = clas_used
  
  # Defines measures and learners id
  measures = list(acc, auc, brier, timetrain)
  leaner.id.lr = "classif.logreg"
  learner.id.randomForest = "classif.randomForest"
  
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
  results.nas = lapply(result, function(x) getBMRAggrPerformances(x, as.df=TRUE))[res.perfs.nas]
  clas_used.nas = clas_used_original[res.perfs.nas,]
  save(results.nas, clas_used.nas , file = "Data/Results/results_nas.RData")
  res.perfs.df.nas = do.call("rbind", results.nas) 
  
  print(paste(length(results.nas),"datasets produced nas"))

  print(paste("Logistic regression produced nas in",  
              sum(is.na(res.perfs.df.nas[which(res.perfs.df.nas$learner.id=="classif.logreg"),]$acc.test.mean)),
              "datasets"))
  
  print(paste("Random forest produced nas in",  
              sum(is.na(res.perfs.df.nas[which(res.perfs.df.nas$learner.id=="classif.randomForest"),]$acc.test.mean)),
              "datasets"))
  
  # convert to a data.frame
  res.perfs.df = do.call("rbind", res.perfs) 
  
  # get the difference of performances
  perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
  perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)
  perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]
  library(reshape2)
  perfsAggr.diff.melted = melt(perfsAggr.diff)
  detach(package:reshape2, unload = TRUE)
  
  
  
  ## 2. Compute the dataset of difference with the features
  
  # number  of features
  p = clas_used$number.of.features
  
  # number of numeric attributes
  pnum = clas_used$number.of.numeric.features
  
  # number of categorical attributes
  psymbolic = clas_used$number.of.symbolic.features
  
  # number of samples
  n = clas_used$number.of.instances
  
  # n/p
  psurn = p/n
  
  # Numerical attributes rate
  pnumrate = pnum/p
  
  # %Cmax Percentage of elements of the majority class
  Cmax = clas_used$majority.class.size/n
  
  
  df.bmr.diff = data.frame(perfsAggr.diff,
                           logp = log(clas_used$number.of.features), 
                           logn = log(clas_used$number.of.instances),
                           logdimension = log(clas_used$dimension),
                           logp.dividedby.n = log(clas_used$number.of.features/clas_used$number.of.instances),
                           logdimension.dividedby.n = log(clas_used$dimension/clas_used$number.of.instances),
                           lograteMajorityMinorityClass = log(clas_used$majority.class.size/clas_used$minority.class.size),
                           pnum, psymbolic, pnumrate, Cmax
  )
  
  
  paste("Is there any nas ? :", any(is.na(df.bmr.diff)))
  
  
  measures.names = sapply(measures, function(x) paste0(x$id,".test.mean"))
  features.names = names(df.bmr.diff)[which(!(names(df.bmr.diff) %in% measures.names))]
  
  # Compute the ranks
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
