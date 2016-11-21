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

## Case 1 ----
# Low difference in models and acc

order(pdp.df$l2)
# find one
which(min(pdp.df$l2)==pdp.df$l2)
#35 48 54 136 176 244

id = 113

df.bmr.diff$acc[id]

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

# check the benchmark 
check_benchmark(task)
task
head(task$env$data)


task_id = clas_used$task.id[136]
omltask = getOMLTask(task_id)
mlrtask = convertOMLTaskToMlr(omltask)















