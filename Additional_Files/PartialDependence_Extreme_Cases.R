# requires




partialDependenceAnalysis_extremCases = function(regis, seed = 1) {
  
  police.size = 18
  
  # Get the datasets without errors
  ids = 1:259
  ids = ids[-which(ids %in% c(14))] # error on dataset 14
  result.pdp = reduceResultsList(ids = ids, reg = regis)
  result.pdp.df = do.call("rbind", result.pdp) 
  Partiaresult.pdp = result.pdp.df
  #clas_used.pdp = clas_used[ids,]
  
  
  # Plot of the Partial Dependence for all datasets
  
   # df.all = data.frame(result.pdp.df, data.id = clas_used.pdp$data.id, 
   #                     n = clas_used.pdp$number.of.instances,
   #                     p = clas_used.pdp$number.of.features,
   #                     logpn = log(clas_used.pdp$number.of.features/clas_used.pdp$number.of.instances))
   # 

  df.all = data.frame(result.pdp.df, data.id = regis$defs$problem[ids])
  df.all = df.all[df.all$data.id %in% clas_used$data.id,]
  df.all$data.id = as.integer(sapply(df.all$data.id, toString))
  
  # get the extrem cases
  data.id.highDiff = df.all$data.id[order(df.all$difference.imp.weight.all)][length(df.all$data.id)-2] # max diff model
  data.id.lowDiff = df.all$data.id[order(df.all$difference.imp.weight.all)][2] # min diff model
  data.id.highAcc = df.all$data.id[order(df.all$diff.acc)][length(df.all$diff.acc)-2] # max diff acc
  
  id.highDiff = which(df.all$data.id==data.id.highDiff)
  id.lowDiff = which(df.all$data.id==data.id.lowDiff)
  id.highAcc = which(df.all$data.id==data.id.highAcc)
  
  # Plot the Difference of Partial Dependence vs Difference Accuracy
  police.size = 18
  df.all$case = 0
  df.all$case[id.highDiff]=2
  df.all$case[id.lowDiff]=1
  df.all$case[id.highAcc]=3
  df.all$is.case = df.all$case >0
  df.all$case = as.factor(df.all$case)
  
  
  p <- ggplot(df.all, aes(difference.imp.weight.all, diff.acc))
  p = p + geom_point(aes(colour=factor(case), 
                         fill = factor(is.case),
                         shape = is.case,
                         size = is.case) )
  p = p + labs(x = bquote(paste(Delta, "PartialDependence")), y = bquote(paste(Delta, "acc")))
  p = p + theme(legend.title=element_blank(), text = element_text(size=police.size))
  p = p + guides(shape = FALSE, fill = FALSE, size = FALSE)
  p = p + scale_colour_manual(breaks=c("1","2","3"), 
                              labels = c("Case 1", "Case 2", "Case 3"),
                              values=c("black", "#D55E00","#0072B2","#009E73"))
  p = p + scale_shape_manual(values = c(1, 15))
  p = p + scale_size_manual(values = c(2, 3))
  print(p)
  
  jpeg(filename = "Data/Pictures/AdditionalFigures/AdditionalFigure2_PDP.jpeg", width = 700, height = 400)
  plot(p)
  dev.off()
  
  ## Before : 1479, 923, 1460
  ## Now Clas Used 1443, 846, 720
  ## Now real class
  
  print(paste(data.id.lowDiff, data.id.highDiff, data.id.highAcc))

  # plot the extrem cases
  # Case 1 Low difference in Partial Dependence
  extrem_cases(data.id.lowDiff, seed = 1, path.out = "Data/Pictures/AdditionalFigures/AdditionalFigure3_PDPcase1.jpeg") 
  
  # Case 2 High difference in Partial Dependence
  extrem_cases(data.id.highDiff, seed = 1, path.out = "Data/Pictures/AdditionalFigures/AdditionalFigure4_PDPcase2.jpeg") 
  
  # Case 3 High difference in Accuracy
  extrem_cases(data.id.highAcc, seed = 1, path.out = "Data/Pictures/AdditionalFigures/AdditionalFigure4_PDPcase3.jpeg") 
}
  

# Further analysis for the extreme cases

# Case 1 : High Accuracy Low Difference


extrem_cases = function(data.id, seed = 1, path.out = "Data/Pictures/AdditionalFigures/AdditionalFigure2_PDP.jpeg") {
  print(data.id)
  set.seed(1)
  police.size = 18
  
  # load the task
  omldataset = getOMLDataSet(data.id)
  #omldataset = getOMLDataSet(334) test
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  task$task.desc$id = paste("dataset")
  task
  task$env$data
  
  # ## See which features are numeric or symbolic
  # target = task$task.desc$target
  # X.train = task$env$data[!colnames(task$env$data) %in% c(target)]
  # 
  # # get the number and type of features
  # type.list = sapply(X.train, class)
  # 
  # # get the index of types
  # index.numeric = which(type.list == "numeric")
  # index.factor = which(type.list == "factor")
  # features.list.numeric = features.list[index.numeric] 
  # features.list.factor = features.list[index.factor] 
  
  
  # train the models
  lrn.classif.rf = makeLearner("classif.randomForest", 
                               importance = TRUE, predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.rf = train(lrn.classif.rf, task)
  
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.lr = train(lrn.classif.lr, task)
  
  fv = fit.classif.rf$learner.model$importance[,3]
  fv[fv<0]=0
  feature_importance_order = order(-fv)
  
  fv.percentage = fv/sum(fv)
  
  # Do the PDP between 5% and 95% of the feature data
  gridsize = 20
  features.list = row.names(fit.classif.rf$learner.model$importance)
  target = task$task.desc$target
  
  plot_pdp_list = NULL
  feature.chosen = NULL
  fv.percentage.chosen = NULL
  
  for (i in c(1:2)) {
    print(i)
    index.temp = feature_importance_order[i]
    feature.temp = features.list[index.temp]
    feature.chosen[i] = feature.temp
    fv.percentage.chosen[i] = fv.percentage[index.temp]
    
    # quantiles = quantile(task$env$data[[feature.temp]], probs = c(0.05, 0.95))
    # quantiles_list = as.list(quantiles)
    # names(quantiles_list) = c(feature.temp, feature.temp)
    # fmin=(quantiles_list[1])
    # fmax=(quantiles_list[2])
    # 
    set.seed(seed)
    pd.rf = generatePartialDependenceData(fit.classif.rf, task, 
                                          features.list[index.temp], uniform = FALSE)#,
                                          #fmin = fmin, fmax = fmax)
    set.seed(seed)
    pd.lr = generatePartialDependenceData(fit.classif.lr, task, 
                                          features.list[index.temp], uniform = FALSE)#,
                                          #fmin = fmin, fmax = fmax)
    
    library(ggplot2)
    df.plot = data.frame(grid = pd.rf$data[[feature.temp]], 
                         rf = pd.rf$data[,1],
                         lr = pd.lr$data[,1])
    
    library(reshape2)
    df.plot.reshaped = reshape2::melt(df.plot, "grid")
    detach(package:reshape2, unload = TRUE)
    p = ggplot(df.plot.reshaped, aes_string(x = "grid", y="value", colour = "variable"))
    p = p+geom_line(size=1) + geom_point(size=3) +
      theme(legend.position="none", text = element_text(size=police.size), legend.title=element_blank()) +
      scale_colour_grey(start = 0,end = 0.7) + ylim(0,1) + xlab(feature.temp)+ylab("Probability")
    print(p)
    
    plot_pdp_list[[i]] = p
  }
  
  library(cowplot)
  
  #quantiles1 = quantile(task$env$data[[features.list[1]]], probs = c(0.05, 0.95))
  #quantiles2 = quantile(task$env$data[[features.list[2]]], probs = c(0.05, 0.95))
  
  
  p = ggplot(task$env$data, aes_string(x = feature.chosen[1], y=feature.chosen[2], colour = target))
  p = p + geom_point(size=1) + #coord_cartesian(xlim = quantiles2, ylim = quantiles1) +
    theme(legend.position="none", text = element_text(size=police.size), legend.title=element_blank())
  print(p)
  
  print("Partial dependence of the 2 main feaures according to RF importance", quote = FALSE)
  print(paste(" Relative importance of feature 1 (",feature.chosen[1],") is ", 
              format(round(fv.percentage.chosen[1], 3), nsmall = 3), "%" ), quote = FALSE)
  print(paste(" Relative importance of feature 2 (",feature.chosen[2],") is ", 
              format(round(fv.percentage.chosen[2], 3), nsmall = 3), "%" ), quote = FALSE)
  
  
  p.grid = plot_grid(plot_pdp_list[[2]] + 
                       coord_flip(),
                     p , NULL,  plot_pdp_list[[1]],
                     ncol = 2, nrow = 2, align = 'v')
  
  print(p.grid)
  
  
  jpeg(filename = path.out, width = 500, height = 500)
  plot(p.grid)
  dev.off()
}
