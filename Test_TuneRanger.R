library(tuneRanger)
library(mlr)

# A mlr task has to be created in order to use the package
# We make an mlr task with the iris dataset here 
# (Classification task with makeClassifTask, Regression Task with makeRegrTask)
iris.task = makeClassifTask(data = iris, target = "Species")

# Rough Estimation of the Tuning time
estimateTimeTuneRanger(iris.task)


# Tuning process (takes around 1 minute); Tuning measure is the multiclass brier score
res_tuneRanger = tuneRanger(iris.task, measure = list(multiclass.brier), num.threads = 1, iters = 30)

# Mean of best 5 % of the results
res
# Model with the new tuned hyperparameters
res$model

# Comapre with a basic RF 
lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.classif.ranger = makeLearner("classif.ranger", predict.type = "prob", fix.factors.prediction = TRUE)
 

mod = train(lrn.classif.rf, iris.task)
mod

## benchmark TuneRanger vs RandomForest

library(tuneRanger)
library(mlr)
tasks = list(iris.task, bc.task, pid.task, sonar.task)

lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.classif.tuneranger = makeLearner("classif.tuneRanger", predict.type = "prob", fix.factors.prediction = TRUE, num.threads=1)
lrn.classif.ranger = makeLearner("classif.ranger", predict.type = "prob", fix.factors.prediction = TRUE, num.threads=1)

lrn.list = list(lrn.classif.rf, lrn.classif.tuneranger, lrn.classif.ranger)

measures = list(acc, multiclass.brier, multiclass.au1u, multiclass.au1p, timetrain)

rdesc = makeResampleDesc("CV", iters = 5, stratify = TRUE)
bmr = benchmark(lrn.list, tasks, rdesc, measures, keep.pred = TRUE, models = FALSE, show.info = TRUE)
save(file = "bmr.RData",bmr)


load("bmr.RData")
