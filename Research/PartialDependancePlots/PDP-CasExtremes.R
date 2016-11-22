rm(list = ls())
library(OpenML)
library(mlr)

# Get the data
load(file = "Data/Results/Original/df.bmr_original.RData")
source(file = "Research/DifferenceInModels/pdpInterpretabilityFunction.R")



# Isolate the values of importance
names(df.bmr.diff)


plot(log(pdp.df$l2), df.bmr.diff$acc.test.mean, xlim=c(-6,-1), xlab = "Difference de dependance partielle, echelle log", ylab = "Difference en accuracy")
plot(log(pdp.df$l1), df.bmr.diff$acc.test.mean, xlim=c(-6,-1))


dataset_id = clas_used$did[1]

omldataset = getOMLDataSet(dataset_id)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
task$task.desc$id = paste("dataset", dataset_id)

weightedPdpDistance(task = task, gridsize = 10)

perfsAggr.LR = subset(res.perfs.df, learner.id == "classif.logreg")
perfsAggr.RF = subset(res.perfs.df, learner.id == "classif.randomForest")


plot(log((perfsAggr.LR$acc.test.mean+perfsAggr.RF$acc.test.mean)/2),log(pdp.df$l2))

# ==========================================
## Case study ==============================
# ==========================================

check_benchmark <- function(task) {
  # check benchmark rf and logreg
  # learners
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) #2class
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE) #multiclass
  
  # list of learners
  lrn.list = list(lrn.classif.lr, #stats package
                  lrn.classif.rf #randomForest package
  )
  
  # measures
  measures = list(acc, brier, auc, timetrain)
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
  configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
  bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)
}

## Plot Bland altman and perfs ----

rbPal <- colorRampPalette(c('red','blue'))
Col_data <- rbPal(10)[as.numeric(cut(pdp.df$l2,breaks = 10))]

plot((perfsAggr.RF$acc.test.mean+perfsAggr.LR$acc.test.mean)/2, 
     (perfsAggr.RF$acc.test.mean-perfsAggr.LR$acc.test.mean)/2,
     col = Col_data, pch = 20)

## Case 2 PDP diff big----

# 166 -->
# 242 --> low delta acc

# Large difference little delta acc ----
sort(pdp.df$l2)
order(pdp.df$l2)

data = data.frame(pdp = pdp.df$l2, perf = df.bmr.diff$acc.test.mean)
data = data[order(pdp.df$l2),]
data

# find one
which(max(pdp.df$l2)==pdp.df$l2)
id = 170
df.bmr.diff$acc[id]
pdp.df$l2[id]

plot(log(pdp.df$l1), df.bmr.diff$acc.test.mean, xlim = c(-6,-0.5))
points(log(pdp.df$l1[id]),df.bmr.diff$acc[id], col = "red", pch = 19)



plot((perfsAggr.RF$acc.test.mean+perfsAggr.LR$acc.test.mean)/2, 
     (perfsAggr.RF$acc.test.mean-perfsAggr.LR$acc.test.mean)/2)

points(((perfsAggr.RF$acc.test.mean+perfsAggr.LR$acc.test.mean)/2)[id],
       ((perfsAggr.RF$acc.test.mean-perfsAggr.LR$acc.test.mean)/2)[id], 
       col = "red", pch = 19)


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



fv2 = generateFilterValuesData(task, method = c("chi.squared"))
plotFilterValues(fv2)

features.list = names(task$env$data)

lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE, importance = TRUE)
fit.classif.rf = train(lrn.classif.rf, task)
library(randomForest)
varImpPlot(fit.classif.rf$learner.model,type=1)

lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
fit.classif.lr = train(lrn.classif.lr, task)


# Plots 1D ----

plot_list = NULL

for (i in c(1:4)) {
  
  #index_importance = c(1:length(features.list))[order(-fv2$data$chi.squared)]
  
  index_importance = c(1:length(features.list))[order(-fit.classif.rf$learner.model$importance[,3])]
  index_importance
  
  index.temp = index_importance[i]
  feature.temp = features.list[index.temp]
  
  set.seed(1)
  pd.rf = generatePartialDependenceData(fit.classif.rf, task, features.list[index.temp], gridsize = gridsize,
                                        resample = "subsample")
  set.seed(1)
  pd.lr = generatePartialDependenceData(fit.classif.lr, task, features.list[index.temp], gridsize = gridsize, 
                                        resample = "subsample")
  
  #plotPartialDependence(pd.lr)
  
  
  df.plot = data.frame(grid = pd.rf$data[[features.list[index.temp]]], 
                       rf = pd.rf$data$Probability,
                       lr = pd.lr$data$Probability)
  
  library(ggplot2)
  library(reshape2)
  df.plot.reshaped = reshape2::melt(df.plot, "grid")
  detach(package:reshape2, unload = TRUE)
  p = ggplot(df.plot.reshaped, aes_string(x = "grid", y="value", colour = "variable"))
  p = p+geom_line(size=1) + geom_point(size=3) + ylim(c(0,1))
  print(p)
  plot_list[[i]]=p
}
library(cowplot)
plot_grid(plot_list[[1]],plot_list[[2]],
          plot_list[[3]],plot_list[[4]])

# Plots 2D ----
gridsize = 10
feature_chosen = fv2$data$name[order(-fv2$data$chi.squared)][c(1,2)]

feature1 = task$env$data[[feature_chosen[1]]]
feature1_quantile = quantile(feature1, probs = c(0.05,0.95))

feature2 = task$env$data[[feature_chosen[2]]]
feature2_quantile = quantile(feature2, probs = c(0.05,0.95))

liste_min = list(feature1 = feature1_quantile[1], feature2 = feature2_quantile[1])
liste_max = list(feature1 = feature1_quantile[2], feature2 = feature2_quantile[2])
names(liste_min) = feature_chosen
names(liste_max) = feature_chosen

set.seed=1
pd.rf = generatePartialDependenceData(fit.classif.rf, task, feature_chosen, 
                                      gridsize = gridsize, interaction = TRUE,
                                      fmin = liste_min, fmax = liste_max)
set.seed=1
pd.lr = generatePartialDependenceData(fit.classif.lr, task, feature_chosen,
                                      gridsize = gridsize, interaction = TRUE,
                                      fmin = liste_min, fmax = liste_max)

plotPartialDependence(pd.rf, geom = "tile")
plotPartialDependence(pd.lr, geom = "tile")

pd.diff = pd.rf
pd.diff$data$Probability=pd.diff$data$Probability-pd.lr$data$Probability
plotPartialDependence(pd.diff, geom = "tile")
