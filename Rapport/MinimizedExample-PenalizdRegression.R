rm(list = ls())
OS = "win"
source("benchmark_defs.R")
load(file ="../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_snow_small-medium-allLearnersFoctor_strat_All.RData")
library(mlr)
library(OpenML)
library(glmnet)

# get the dataset and create task
index = 100
data.index = clas_used$did[index]
data.index = 336
omldataset = getOMLDataSet(data.index)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
n = getTaskSize(task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)

pairs(task$env$data)

# get perfs for logistic regression with mlr
lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) #2class
mod = train(lrn.classif.lr, task, subset = train.set)
pred = predict(mod, task = task, subset = test.set)
performance(pred, measures = acc)

# get perfs for lasso regression with mlr
lrn.classif.lr.glm.lasso.1se = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1, s="lambda.1se")
lrn.classif.lr.glm.lasso.1se$id = "classif.cvglmnet.lasso.1se"
mod.lasso = train(lrn.classif.lr.glm.lasso.1se, task, subset = train.set)
pred.lasso = predict(mod.lasso, task = task, subset = test.set)
performance(pred.lasso, measures = acc) 

# get perfs for lasso regression with mlr
lrn.classif.lr.glm.lasso.min = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1, s="lambda.min")
lrn.classif.lr.glm.lasso.min$id = "classif.cvglmnet.lasso.lambda.min"
mod.lasso = train(lrn.classif.lr.glm.lasso.min, task, subset = train.set)
pred.lasso = predict(mod.lasso, task = task, subset = test.set)
performance(pred.lasso, measures = acc) 

# little benchmark
set.seed(2)
lrn.list = list(lrn.classif.lr, #stats package
                lrn.classif.lr.glm.lasso.min, lrn.classif.lr.glm.lasso.1se) #glmnet package
rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4, stratify = FALSE)
configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
bmr = benchmark(lrn.list, task, rdesc, acc, keep.pred = FALSE, models = FALSE, show.info = TRUE)

# get perfs for lasso with glm package
# data : task to a matrix
data = task$env$data
target.position = which(names(data) == task$task.desc$target)
Y = as.vector(data[target.position])
Y = as.factor(Y[,1])
X = data.matrix(data[-target.position])

X.train = X[train.set,]
X.test = X[test.set,]
  
Y.train = Y[train.set]
Y.test = Y[test.set]

# model
cv.glmmod.train <- cv.glmnet(x = X.train, y =  Y.train, family = "binomial", alpha = 1)
plot(cv.glmmod.train)
cv.glmmod.train$lambda.min

# prediction
prediction = predict(cv.glmmod.train, newx = X.test, type = "class", s = "lambda.min")
prediction.factor = as.factor(prediction)
levels(prediction.factor)=levels(Y.test)
coef(cv.glmmod.train, s = "lambda.min")
mean(prediction.factor==(as.factor(Y.test))) # performance higher than log reg

prediction = predict(cv.glmmod.train, newx = X.test, type = "class") # without lambda.min as parameter
prediction.factor = as.factor(prediction)
levels(prediction.factor)=levels(Y.test)
mean(prediction.factor==(as.factor(Y.test))) 
