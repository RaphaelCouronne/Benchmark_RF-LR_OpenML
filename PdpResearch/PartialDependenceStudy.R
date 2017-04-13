# create dataset
library(mlr)
library(ggplot2)
library(cowplot)
library(OpenML)

## Simulated case

plotDistrSimul = function (rapportBeta) {
  
  # Raw gaussian distribution
  n = 1e3
  X1<-runif(n, min = -0.5, max = 1)
  X2<-runif(n, min = -1, max = 1)
  
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
  df<-data.frame(X1,X2,Y)
  classif.task<-makeClassifTask(data = df, target="Y")
  classif.task
  
  # Plot it
  # Visualization of the generated datas
  plot.data <- ggplot(data=df, aes(x=X1, y=X2, colour=Y, shape = Y))
  plot.data <- plot.data + geom_point(size=2) 
  plot.data = plot.data + scale_colour_grey(start = 0,end = 0.6) 
  print(plot.data)
  
  
  # Get performances
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
  bmr = benchmark(lrn.list, classif.task, rdesc, acc, keep.pred = FALSE, models = FALSE, show.info = FALSE)
  bmr
  u = getBMRAggrPerformances(bmr, as.df = TRUE)
  
  # Get Pdp difference
  fit.rf = train(learner = lrn.classif.rf, task = classif.task)
  fit.lr = train(learner = lrn.classif.lr, task = classif.task)
  pd.rf = generatePartialDependenceData(fit.rf, classif.task, gridsize = 30)
  pd.lr = generatePartialDependenceData(fit.lr, classif.task, gridsize = 30)
  
  plot.rf = plotPartialDependence(pd.rf)
  plot.lr = plotPartialDependence(pd.lr)
  
  gridplot = plot_grid(plot.rf, plot.lr, ncol = 1)
  print(gridplot)
  Sys.sleep(2)
  
  difference = sum(abs(pd.rf$data$Probability-pd.lr$data$Probability))
  
  pd.rf.X2 = subset(pd.rf$data, subset = is.na(X1))
  pd.lr.X2= subset(pd.lr$data, subset = is.na(X1))
  
  difference.X2 = sum(abs(pd.rf.X2$Probability-pd.lr.X2$Probability))
  
  return(data.frame(t(u$acc.test.mean), difference, difference.X2))
}


rapport.vect = c(seq(0.1,1.5, by = 0.1),2,3,4,5,6)
res = lapply(rapport.vect, plotDistrSimul)
res2= do.call("rbind", res) 

plot(res2$difference, res2$X1)
plot(res2$difference.X2, res2$X1)

plot(res2$difference, res2$X2)
plot(res2$difference.X2, res2$X2)

plot(res2$difference, res2$X2-res2$X1)
plot(res2$difference.X2, res2$X2-res2$X1)



## Real case
rm(list=ls())
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

clas_subset =subset(clas_used, (number.of.features < 10) &
                      (number.of.symbolic.features == 1) &
                      (number.of.instances < 10000) & 
                      (number.of.instances > 100) & 
                      (number.of.features > 2) )

data.id = clas_subset$data.id



getBasicRealdatasetDifference = function(j, plot.bool = FALSE) {
  
  print(j)
  omldataset = getOMLDataSet(data.id = clas_subset$data.id[j], verbosity = 0)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  
  mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
  
  fv = generateFilterValuesData(mlrtask, method = "information.gain")
  plotFilterValues(fv)
  
  # Get performances
  perfs = getAccPerfs(mlrtask)
  
  # Get difference in PDP
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  fit.rf = train(learner = lrn.classif.rf, task = mlrtask)
  fit.lr = train(learner = lrn.classif.lr, task = mlrtask)
  pd.rf = generatePartialDependenceData(fit.rf, mlrtask, gridsize = 20)
  pd.lr = generatePartialDependenceData(fit.lr, mlrtask, gridsize = 20)
  
  if (plot.bool == TRUE) {
    plot.rf = plotPartialDependence(pd.rf)
    plot.lr = plotPartialDependence(pd.lr)
    gridplot = plot_grid(plot.rf, plot.lr, ncol = 1)
    print(gridplot)
    Sys.sleep(2)
  }
  
  difference = mean(abs(pd.rf$data$Probability-pd.lr$data$Probability))
  
  
  feature.mostImp = fv$data$name[order(fv$data$information.gain)][length(fv$data$name)]
  pd.rf.ImpFeatureProbability = pd.rf$data$Probability[which(!(is.na(pd.rf$data[[feature.mostImp]])))]
  pd.lr.ImpFeatureProbability = pd.lr$data$Probability[which(!(is.na(pd.lr$data[[feature.mostImp]])))]
  
  pd.rf.ImpFeatureValue = pd.rf$data[[feature.mostImp]][which(!(is.na(pd.rf$data[[feature.mostImp]])))]
  pd.lr.ImpFeatureValue = pd.lr$data[[feature.mostImp]][which(!(is.na(pd.lr$data[[feature.mostImp]])))]
  
  difference.MostImpFeature = sum(abs(pd.rf.ImpFeatureProbability-pd.lr.ImpFeatureProbability))
  
  countValuesPercentage = getPercentageGrid(grid = pd.lr.ImpFeatureValue, data.values = mlrtask$env$data[[feature.mostImp]])
  
  difference.MostImpFeature.Weighted = abs(pd.rf.ImpFeatureProbability-pd.lr.ImpFeatureProbability)%*% countValuesPercentage
  
  # Et on refait la prediction juste avec la most Imp feature
  filtered.task = filterFeatures(mlrtask, fval = fv, abs = 1)
  data.values = filtered.task$env$data[[feature.mostImp]]
  fit.rf.filtered = train(learner = lrn.classif.rf, task = filtered.task)
  fit.lr.filtered = train(learner = lrn.classif.lr, task = filtered.task)
  pd.rf = generatePartialDependenceData(fit.rf.filtered, filtered.task, gridsize = 20)
  pd.lr = generatePartialDependenceData(fit.lr.filtered, filtered.task, gridsize = 20)
  difference.1feature = mean(abs(pd.rf$data$Probability-pd.lr$data$Probability))
  
  # pondération avec distr empirique
  grid = pd.lr$data[,3]
  grid.limits = c(grid-diff(grid)[1]/2, grid[length(grid)]+diff(grid)[1]/2)
  countBins = function(i) sum(data.values >= grid.limits[i] & data.values < grid.limits[i+1] )
  countValues = sapply(1:length(grid), countBins)
  countValuesPercentage = countValues/sum(countValues)
  difference.1feature.percentage = abs(pd.rf$data$Probability-pd.lr$data$Probability)%*%countValuesPercentage
  
  # Difference in 2D weighed or not
  grid.size = 10
  
  feature.count = length(fv$data$name)
  feature.mostImp.2 = fv$data$name[order(fv$data$information.gain)][c(feature.count-1,feature.count)]
  
  pd.rf = generatePartialDependenceData(fit.rf, mlrtask, gridsize = grid.size, features = feature.mostImp.2, interaction = TRUE)
  pd.lr = generatePartialDependenceData(fit.lr, mlrtask, gridsize = grid.size, features = feature.mostImp.2, interaction = TRUE)
  
  plotPartialDependence(pd.rf, geom = "tile")
  plotPartialDependence(pd.lr, geom = "tile")
  
  difference.2D = mean(abs(pd.rf$data$Probability-pd.lr$data$Probability))
  
  grid.1 = unique(pd.rf$data[[feature.mostImp.2[1]]])
  grid.2 = unique(pd.rf$data[[feature.mostImp.2[2]]])
  
  matrix.grid.1 = c(grid.1-diff(grid.1)[1]/2, grid.1[length(grid.1)]+diff(grid.1)[1]/2)
  matrix.grid.2 = c(grid.2-diff(grid.2)[1]/2, grid.2[length(grid.2)]+diff(grid.2)[1]/2)

  data.values.1 = mlrtask$env$data[[feature.mostImp.2[1]]]
  data.values.2 = mlrtask$env$data[[feature.mostImp.2[2]]]
  
  diff.1 = diff(grid.1)[1]
  diff.2 = diff(grid.2)[2]
  
  
  matrix.count = pd.rf$data[c(3,4)]
  matrix.count$count = NA
  
  for (i in c(1:nrow(matrix.count))) {
    count = sum((data.values.1 >=  matrix.count[[feature.mostImp.2[1]]][i] - diff.1/2 & data.values.1 < matrix.count[[feature.mostImp.2[1]]][i] + diff.1/2) & 
                  (data.values.2 >=  matrix.count[[feature.mostImp.2[2]]][i] - diff.2/2 & data.values.2 < matrix.count[[feature.mostImp.2[2]]][i] + diff.2/2)  )
    matrix.count$count[i]=count
    }
  matrix.count$percentage = matrix.count$count/sum(matrix.count$count)
  
  difference.2D.weighed = abs(pd.rf$data$Probability-pd.lr$data$Probability)%*%matrix.count$percentage
  
  
  return(data.frame(t(perfs$acc.test.mean), difference, difference.MostImpFeature, difference.MostImpFeature.Weighted, 
                    difference.1feature, difference.1feature.percentage,difference.2D, difference.2D.weighed))
}



rapport.vect = c(1:46)
res = lapply(rapport.vect, getBasicRealdatasetDifference)
res2= do.call("rbind", res) 

plot(res2$difference, res2$X1)
plot(res2$difference.MostImpFeature, res2$X1)

plot(res2$difference, res2$X2)
plot(res2$difference.MostImpFeature, res2$X2)

plot(res2$difference, res2$X2-res2$X1)
plot(res2$difference.MostImpFeature, res2$X2-res2$X1)

plot(res2$difference.MostImpFeature.Weighted, res2$X2-res2$X1)
plot(res2$difference.1feature, res2$X2-res2$X1)
plot(res2$difference.1feature.percentage, res2$X2-res2$X1)

plot(res2$difference.2D, res2$X2-res2$X1)
plot(res2$difference.2D.weighed, res2$X2-res2$X1)




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


plot(res2$)
