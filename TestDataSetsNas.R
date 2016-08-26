library(mlr)
library(OpenML)
rm(list=ls())
OS = "win"
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")
source("benchmark_defs.R")
clas_used = rbind(clas_time_small, clas_time_medium)

## index ----

# Does not work with glm
# 1 5

# does not work with penalized
# 28 26 25 24 19 17 16 13 11

i = 26
data.index = clas_used$did[i]


omldataset = getOMLDataSet(data.index)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
task$task.desc$id = paste("dataset", data.index)


## bench ----

# learners
lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) #2class

# liblinear
#lrn.classif.lrl1 = makeLearner("classif.LiblineaRL1LogReg", predict.type = "prob", fix.factors.prediction = TRUE) #multiclass #no factor
#lrn.classif.lrl2 = makeLearner("classif.LiblineaRL2LogReg", predict.type = "prob", fix.factors.prediction = TRUE) #multiclass #no factor
#We want the factors

# regularized
lrn.classif.lrlasso = makeLearner("classif.penalized.lasso", predict.type = "prob", fix.factors.prediction = TRUE) #two class #no factor

# nnet
lrn.classif.multinom = makeLearner("classif.multinom", predict.type = "prob", fix.factors.prediction = TRUE)

# also use glmnet
lrn.classif.lr.glm.ridge = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 0)
lrn.classif.lr.glm.ridge$id = "classif.cvglmnet.ridge"
lrn.classif.lr.glm.lasso = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1)
lrn.classif.lr.glm.lasso$id = "classif.cvglmnet.lasso"
lrn.classif.lr.glm.elasticnet = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.classif.lr.glm.elasticnet$id = "classif.cvglmnet..elasticnet"

# list of learners
lrn.list = list(lrn.classif.lr, #stats package
                lrn.classif.lrlasso,
                lrn.classif.lr.glm.ridge) #glmnet package

# measures
measures = MEASURES
rdesc = makeResampleDesc("CV", iters = 5, stratify = TRUE)
configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)

