rm(list = ls())

# libraries and source
OS = "Windows"
library(mlr)
library(OpenML)
source(file = "benchmark_defs.R")

# load the benchmark and see which dataset 
# have low performances for regularized regression
load(file = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_regressionpenalisee.RData")

# For example the 100th dataset
index = 96
data.index = clas_used$did[index]
omldataset = getOMLDataSet(data.index)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)

# check that performs poorly ----

# learners
lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE) #2class

# also use glmnet
lrn.classif.lr.glm.ridge = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 0)
lrn.classif.lr.glm.ridge$id = "classif.cvglmnet.ridge"
lrn.classif.lr.glm.lasso = makeLearner("classif.cvglmnet", predict.type = "prob", fix.factors.prediction = TRUE, alpha = 1)
lrn.classif.lr.glm.lasso$id = "classif.cvglmnet.lasso"

# glm scaled with mlr wrapper
lrn.glm.scaled1 = makePreprocWrapperScale(lrn.classif.lr.glm.lasso)
lrn.glm.scaled1$id = "glm1.mlrscaled"
lrn.glm.scaled0 = makePreprocWrapperScale(lrn.classif.lr.glm.ridge)
lrn.glm.scaled0$id = "glm0.mlrscaled"

# glm scaled with caret wrapper
lrn.glm.scaled1.caret = makePreprocWrapperCaret(lrn.classif.lr.glm.lasso, ppc.center = TRUE, ppc.scale = TRUE)
lrn.glm.scaled0.caret = makePreprocWrapperCaret(lrn.classif.lr.glm.ridge, ppc.center = TRUE, ppc.scale = TRUE)

# list of learners
lrn.list = list(lrn.classif.lr, #stats package
                lrn.classif.lr.glm.ridge, lrn.classif.lr.glm.lasso, #glmnet package
                lrn.glm.scaled1, lrn.glm.scaled0,
                lrn.glm.scaled1.caret, lrn.glm.scaled0.caret) 

# measures
rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
configureMlr(on.learner.error = "warn", show.learner.output = TRUE)
bmr = benchmark(lrn.list, task, rdesc, acc, keep.pred = FALSE, models = FALSE, show.info = TRUE)

## Check the dataset ----
data = task$env$data

# avec juste un rdesc
rdesc = makeResampleDesc("CV", iters = 5)
resample(lrn.classif.lr, task, rdesc)
resample(lrn.classif.lr.glm.lasso, task, rdesc)

# avec un holdotut a la main
n = getTaskSize(task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)
mod = train(lrn.classif.lr, task, subset = train.set)
pred = predict(mod, task = task, subset = test.set)
performance(pred, measures = acc)

mod.lasso = train(lrn.classif.lr.glm.lasso, task, subset = train.set)
pred.lasso = predict(mod.lasso, task = task, subset = test.set)
performance(pred.lasso, measures = acc)

lambdavect = mod.lasso$learner.model$lambda
perfvect = mod.lasso$learner.model$cvsd

plot(lambdavect, perfvect)
lambdamin = mod.lasso$learner.model$lambda.min
lines(rep(lambdamin,2),c(0,2))

# Avec un lambda que je fais moi ----
library(glmnet)
target.position = which(names(data) == task$task.desc$target)
Y = as.vector(data[target.position])
Y = as.factor(Y[,1])
X = data.matrix(data[-target.position])

lambdavect = seq(-6,-3,length.out = 40)
lambdavect = exp(lambdavect)
lambdavect = c(0, lambdavect)


glmmod = glmnet(x = X, y =  Y, family = "binomial", lambda = lambdavect, alpha = 1)
plot(glmmod,xvar="lambda")

cv.glmmod.lambdachosen <- cv.glmnet(x = X, y =  Y, family = "binomial", lambda = lambdavect, alpha = 1)
plot(cv.glmmod.lambdachosen)
cv.glmmod.lambdachosen$lambda.min

cv.glmmod <- cv.glmnet(x = X, y =  Y, family = "binomial", alpha = 1)
plot(cv.glmmod)
cv.glmmod$lambda.min


X.train = X[train.set,]
X.test = X[test.set,]

Y.train = Y[train.set]
Y.test = Y[test.set]

cv.glmmod.train <- cv.glmnet(x = X.train, y =  Y.train, family = "binomial", alpha = 1)
plot(cv.glmmod.train)
cv.glmmod$lambda.min

prediction = predict(cv.glmmod.train, newx = X.test, type = "class", s = 0.02)
coef(cv.glmmod.train, s = "lambda.min")
mean(as.numeric(prediction)==(as.numeric(Y.test)-1))



mod.lasso.mlr = mod.lasso$learner.model
prediction = predict(mod.lasso.mlr, newx = X.test, type = "class", s = 0.02)
coef(cv.glmmod.train, s = "lambda.min")
mean(as.numeric(prediction)==(as.numeric(Y.test)-1))


info = getTrainingInfo(mod.lasso.mlr)
.newdata = as.matrix(fixDataForLearner(.newdata, info))



