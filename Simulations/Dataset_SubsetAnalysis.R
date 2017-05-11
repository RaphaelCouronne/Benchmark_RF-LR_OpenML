library(mlr)
library(OpenML)
library(ggplot2)
library(reshape2)
library(doParallel)



# == Description
# We want to study, for one dataset, the influence of n and p on the results of a benchmark comparing random forest and logistic regression
# Thus, for a grid of values n.sub<n, we will compute for each value of n.sub n.simulation-times the comparison between rf and lr. 
# We will then be able to plot boxplots of accuracy of both lr and rf regarding to n.sub.
# 
# Note that we do this simulation on a real dataset from OpenML. 
# We consider only a subset of the datasets as candidates for this simulation, such that p>20, n>5000 and the features are all numeric
# for the sake of simmplicity. You can do the simulation on other dataset using the parameter index.



## Load the datasets and set the parameter ----

subsetAnalysis_computeParallel <- function(clas_used, nCores=1, index = 1, seed=1) {
  
  set.seed(seed)
  start.time <- Sys.time()
  
  # Parameters 
  gridsize = 6 # gridsize :  Number of values of p.sub and n.sub considered
  n.simulation = 50 # n.simulation : For each value of p.sub and n.sub, how many samples and then computations ?
  n.max = 1e3 # n.max : Max n for the tiny dataset used for the computation of p
  grid.n.percentage = seq(0.2, 0.9, length.out = gridsize)
  grid.p.percentage = seq(0.2, 0.8, length.out = gridsize)
  
  ## Load a dataset ----
  # We want a dataset such that p high and n high so we have latitude on
  # the choice of the intermediate values of p and n
  
  data.id.candidates = clas_used$data.id[which((clas_used$p>20) & (clas_used$n>5e3) & (clas_used$number.of.symbolic.features==1))]
  data.id = data.id.candidates[index] #(we choose dataset 1496 here)
  
  # Load and convert one dataset
  print("Loading dataset")
  omldataset = getOMLDataSet(data.id)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  
  # Get the dataset infos
  n.dataset = task$task.desc$size
  p.dataset = sum(task$task.desc$n.feat)
  features.names = names(task$env$data)[-which(names(task$env$data) == task$task.desc$target)]
  
  
  # Benchmark function for tasks corresponding to chosen subsets
  
  benchmark.sub = function(task) {
    # learners
    lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
    lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
    
    # list of learners
    lrn.list = list(lrn.classif.lr, lrn.classif.rf)
    # measures
    measures = list(acc, brier, auc, timetrain)
    rdesc = makeResampleDesc("RepCV", folds = 5, reps = 2, stratify = TRUE)
    #rdesc = makeResampleDesc("CV", iter = 5, stratify = TRUE)
    configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
    bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
    bmr.perfs = getBMRAggrPerformances(bmr, as.df = TRUE)
    res = bmr.perfs$acc.test.mean
    return(res)
  }
  
  
  
  ## ===============
  ## Computation : Subset Analysis ----
  print("Computing subsets for feature n")
  
  
  # For the feature n 
  grid.n = floor(n.dataset*grid.n.percentage)
  grid.toString = sapply(grid.n, function(x) toString(x))
  gridsize = length(grid.n)
  
  df = data.frame(row.names = features.names)
  results.diff = data.frame(matrix(NA, nrow = n.simulation, ncol= gridsize))
  results.rf = data.frame(matrix(NA, nrow = n.simulation, ncol= gridsize))
  results.lr = data.frame(matrix(NA, nrow = n.simulation, ncol= gridsize))
  
  names(results.rf) = grid.toString
  names(results.lr) = grid.toString
  names(results.diff) = grid.toString
  
  cl <- makeCluster(nCores)  
  registerDoParallel(cl)  
  res.subsetn = foreach (j=1:gridsize, .combine = rbind) %dopar% {
    library(mlr)
    library(reshape2)
    n.sub = grid.n[j]
    results.temp = data.frame(matrix(NA, nrow = n.simulation, ncol= 4))
    names(results.temp) = c("RF","LR","Difference","n")
    
    for (i in c(1:n.simulation)) {
      print(paste0("Beginning iteration ","i=",i,", j=",j))
      indexdata.experiment = sapply(c(1:n.simulation),function(x) sample(n.dataset,n.sub))
      dataset.sub = task$env$data[indexdata.experiment[,i],]
      task.sub = makeClassifTask(data = dataset.sub, target = task$task.desc$target)
      bmr.sub = benchmark.sub(task.sub)
      
      results.temp[i,]=c(bmr.sub,bmr.sub[2]-bmr.sub[1],n.sub)
    }
    res = melt(results.temp, id=c("n"))
  }
  stopCluster(cl)  
  
  
  
  
  
  # For the feature p
  print("Computing subsets for feature p")
  
  # Use task.tiny with a lower value of n to reduce computation time
  sample.tiny = sample(n.dataset,n.max)
  task.tiny = makeClassifTask(data = task$env$data[sample.tiny,], target = task$task.desc$target)
  
  grid.p = floor(p.dataset*grid.p.percentage)
  grid.toString = sapply(grid.p, function(x) toString(x))
  gridsize = length(grid.p)
  
  df = data.frame(row.names = features.names)
  results.diff = data.frame(matrix(NA, nrow = n.simulation, ncol= gridsize))
  results.rf = data.frame(matrix(NA, nrow = n.simulation, ncol= gridsize))
  results.lr = data.frame(matrix(NA, nrow = n.simulation, ncol= gridsize))
  
  names(results.rf) = grid.toString
  names(results.lr) = grid.toString
  names(results.diff) = grid.toString
  
  cl <- makeCluster(nCores)  
  registerDoParallel(cl)  
  res.subsetp = foreach (j=1:gridsize, .combine = rbind) %dopar% {
    library(mlr)
    library(reshape2)
    p.sub = grid.p[j]
    results.temp = data.frame(matrix(NA, nrow = n.simulation, ncol= 4))
    names(results.temp) = c("RF","LR","Difference","p")
    
    for (i in c(1:n.simulation)) {
      print(paste0("Beginning iteration ","i=",i,", j=",j))
      indexdata.experiment = sapply(c(1:n.simulation),function(x) sample(p.dataset,p.sub))
      task.data = task.tiny$env$data[,-which(names(task.tiny$env$data) %in% task.tiny$task.desc$target)]
      dataset.sub = data.frame(task.tiny$env$data[,indexdata.experiment[,i]], target = task.tiny$env$data[[task.tiny$task.desc$target]])
      task.sub = makeClassifTask(data = dataset.sub, target = "target")
      bmr.sub = benchmark.sub(task.sub)
      results.temp[i,]=c(bmr.sub,bmr.sub[2]-bmr.sub[1],p.sub)
    }
    res = melt(results.temp, id=c("p"))
  }
  stopCluster(cl)  
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  save(res.subsetp, res.subsetn, file = "Data/Simulations/SubsetAnalysis.RData")
}



subsetAnalysis_visualization<-function() {
  load(file = "Data/Simulations/SubsetAnalysis.RData")
  police.size = 18
  
  ## ===============
  ## Visualization : Boxplots----
  
  
  ## Boxplots ----
  
  print("Beginning plot without facet")
  # Plots
  names(res.subsetn) = c("n", "algorithm", "acc")
  names(res.subsetp) = c("p", "algorithm", "acc")
  
  # boxplots 
  
  # n
  ggp = ggplot(subset(res.subsetn, algorithm %in% c("RF","LR")), aes(factor(n), acc))
  ggp = ggp + geom_boxplot(aes(fill = algorithm)) + 
    scale_fill_grey(start = 0.4,end = 1, labels=c("LR", "RF")) +
    theme(legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_blank(),  text = element_text(size=police.size)) +
    xlab("n") + ylim(c(0.65,0.95))
  plot(ggp)
  ggp.acc.n = ggp
  
  # p
  ggp = ggplot(subset(res.subsetp, algorithm %in% c("RF","LR")), aes(factor(p), acc))
  ggp = ggp + geom_boxplot(aes(fill = algorithm)) + 
    scale_fill_grey(start = 0.4,end = 1, labels=c("LR", "RF")) +
    theme(legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_blank(),  text = element_text(size=police.size))+
    xlab("p") + ylim(c(0.55,0.95))
  plot(ggp)
  ggp.acc.p = ggp
  
  
  
  
  
  ## Boxplot of difference for several values
  
  # n
  ggp = ggplot(subset(res.subsetn, algorithm %in% c("Difference")), aes(factor(n), acc))
  ggp = ggp + geom_boxplot() + 
    scale_fill_grey(start = 0.4,end = 1) +
    theme(legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_blank(),  text = element_text(size=police.size)) +
    xlab("n") + ylab(expression(paste(Delta, "acc"))) #+ ylim(c(0.15,0.28))
  plot(ggp)
  ggp.deltaacc.n = ggp
  
  # p
  ggp = ggplot(subset(res.subsetp, algorithm %in% c("Difference")), aes(factor(p), acc))
  ggp = ggp + geom_boxplot() + 
    scale_fill_grey(start = 0.4,end = 1) +
    theme(legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_blank(),  text = element_text(size=police.size)) +
    xlab("p") + ylab(expression(paste(Delta, "acc")))  #+ ylim(c(0.15,0.28))
  plot(ggp)
  ggp.deltaacc.p = ggp
  
  library(cowplot)
  plot.grid = plot_grid(ggp.acc.p,
            ggp.acc.n,
            ggp.deltaacc.p, 
            ggp.deltaacc.n, align = "v",
            ncol = 2, nrow = 2)
  
  print(plot.grid)
  
  jpeg(filename = "Data/Pictures/Figure4_SubsetSimulation.jpeg", width = 600, height = 400)
  plot(plot.grid)
  dev.off()
  
  print("Beginning plot with facets")
  
  ## Boxplot facet grid
  res.subsetn2=res.subsetn
  res.subsetp2=res.subsetp
  
  names(res.subsetn2)[1]="featureValue"
  names(res.subsetp2)[1]="featureValue"
  
  res.subsetp2$feature = "p"
  res.subsetn2$feature = "n"
  
  res.tot = rbind(res.subsetp2, res.subsetn2)
  
  v = function(algorithm) {
    switch(algorithm,
           "RF"="acc",
           "LR"="acc",
           "Difference"="Deltaacc")
  }
  res.tot$measure = factor(sapply(res.tot$algorithm,v))
  res.tot$algorithm = factor(res.tot$algorithm, levels=c("LR","Difference","RF"))
  levels(res.tot$measure) = c(expression("acc"),
                              expression(paste(Delta,"acc")))
  
  
  
  
  
  pp <- ggplot(res.tot, aes(factor(featureValue), acc))+
    geom_boxplot(aes(fill = algorithm)) + 
    facet_grid(measure~feature, margins = FALSE, scales = c("free"), switch = "y", labeller = label_parsed, space = "fixed")+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position="none") +
    scale_fill_grey(start = 0.4,end = 1)
  print(pp)
  
  jpeg(filename = "Data/Pictures/AdditionalFigures/Figure4_SubsetSimulation_facet.jpeg", width = 1000, height = 800)
  plot(pp)
  dev.off()
  
  
}

