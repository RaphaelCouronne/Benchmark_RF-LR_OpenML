rm(list=ls())
library(mlr)
library(tuneRanger)

lrn.tuneranger = makeLearner("classif.tuneRanger", iters= 20, num.threads=1)
lrn.rf= makeLearner("classif.randomForest")
lrn.lr = makeLearner("classif.logreg")
lrn.list = list(lrn.tuneranger, lrn.rf)

iris.task = makeClassifTask(data = iris, target = "Species")
measures = list(acc,timetrain)
rdesc = makeResampleDesc("CV", iters=3, stratify = TRUE)

bmr = benchmark(lrn.list, iris.task, rdesc, measures, keep.pred = TRUE, models = FALSE, show.info = TRUE)
