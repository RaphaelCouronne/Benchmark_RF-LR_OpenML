



# Dataset test ----
n = 5e3
p = 5e1
matrixGenerated = matrix(rnorm(n*p), ncol=p, nrow = n)
matrixGenerated = 10*runif(p)*matrixGenerated
df = data.frame(matrixGenerated)

# Linear dependancies
Beta.linear = c(-5:5)
dummyTarget.linear = t(Beta.linear%*%t(data.matrix(df[,c(1:11)])))

# Non-linear dependancies
Beta.nonlinear = c(-2:2)
dummyTarget.nonlinear = t(Beta.nonlinear%*%t(data.matrix(df[,c(12:16)]^2)))

# Interractions
Beta.interraction = c(-2:2)
dummyTarget.interraction = -5*df[,17]*df[,18]+5*df[,19]*df[,20]
                          
# generation of target
dummyTarget.sum = dummyTarget.linear+dummyTarget.nonlinear+dummyTarget.interraction
target = as.factor(rbinom(n,1,prob = plogis(dummyTarget.sum)))




# train and assess the performances ----

# mlr
library(mlr)
task = makeClassifTask(data = data.frame(df, target = target), target = "target")
rdesc = makeResampleDesc(method = "CV", iters = 5)

set.seed(1)
lrn.lr = makeLearner("classif.logreg")
r.lr = resample(lrn.lr, task, rdesc)

set.seed(1)
lrn.glm = makeLearner("classif.cvglmnet")
r.glm = resample(lrn.glm, task, rdesc)

lambda.user = c(0,0.01,0.02,0.05,0.1,0.2,1,10,100)

index.train = c(1:floor(n/2))
index.test = c(floor(1+n/2):n)
train.lr = train(lrn.lr, task, index.train)
train.glm = train(lrn.glm, task, index.train)

train.glm
lambdavalues = train.glm$learner.model$lambda
cvmvalues = train.glm$learner.model$cvm

plot(lambdavalues, cvmvalues)
lambdamin = train.glm$learner.model$lambda.min
lines(rep(lambdamin,2),c(0,2))
lines(lambdavalues, train.glm$learner.model$cvup, col = "red")
lines(lambdavalues, train.glm$learner.model$cvlo, col = "blue")

# tuning ----
library(mlr)
task = makeClassifTask(data = data.frame(df, target = target), target = "target")
rdesc = makeResampleDesc(method = "CV", iters = 5)


makePreprocWrapperScale = function(learner, center = TRUE, scale = TRUE) {
  trainfun = function(data, target, args = list(center, scale)) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    x = as.matrix(data[, nums, drop = FALSE])
    x = scale(x, center = args$center, scale = args$scale)
    control = args
    if (is.logical(control$center) && control$center)
      control$center = attr(x, "scaled:center")
    if (is.logical(control$scale) && control$scale)
      control$scale = attr(x, "scaled:scale")
    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(x))
    return(list(data = data, control = control))
  }
  predictfun = function(data, target, args, control) {
    cns = colnames(data)
    nums = cns[sapply(data, is.numeric)]
    x = as.matrix(data[, nums, drop = FALSE])
    x = scale(x, center = control$center, scale = control$scale)
    data = data[, setdiff(cns, nums), drop = FALSE]  
    data = cbind(data, as.data.frame(x))
    return(data)
  }
  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeLogicalLearnerParam("center"),
      makeLogicalLearnerParam("scale")
    ),
    par.vals = list(center = center, scale = scale)
  )
}


rdesc = makeResampleDesc(method = "CV", iters = 5)
lrn = makePreprocWrapperScale("classif.cvglmnet")
ps = makeParamSet(
  makeLogicalParam("center"),
  makeLogicalParam("scale")
)
ctrl = makeTuneControlGrid()
res = tuneParams(lrn, task, rdesc, par.set = ps, control = ctrl, show.info = TRUE)
as.data.frame(res$opt.path)


# With a benchmark ----
library(mlr)
task = makeClassifTask(data = data.frame(df, target = target), target = "target")
rdesc = makeResampleDesc(method = "CV", iters = 5)

# logreg
lrn.lr = makeLearner("classif.logreg")

# glm normal
lrn.glm1 = makeLearner(id = "glm1", "classif.cvglmnet", alpha = 1)
lrn.glm0 = makeLearner(id = "glm0", "classif.cvglmnet", alpha = 0)

# glm scaled with mlr wrapper
lrn.glm.scaled1 = makePreprocWrapperScale(lrn.glm1)
lrn.glm.scaled1$id = "glm1.mlrscaled"
lrn.glm.scaled0 = makePreprocWrapperScale(lrn.glm0)
lrn.glm.scaled0$id = "glm0.mlrscaled"

# glm scaled with caret wrapper
lrn.glm.scaled1.caret = makePreprocWrapperCaret(lrn.glm1, ppc.center = TRUE, ppc.scale = TRUE)
lrn.glm.scaled0.caret = makePreprocWrapperCaret(lrn.glm0, ppc.center = TRUE, ppc.scale = TRUE)

lrns = list(lrn.lr, 
            lrn.glm1, lrn.glm0, 
            lrn.glm.scaled1, lrn.glm.scaled0,
            lrn.glm.scaled1.caret, lrn.glm.scaled0.caret)

bmr = benchmark(lrns, task, rdesc)

# with the hand ----
library(glmnet)
fit = glmnet(target, matrixGenerated, family = "binomial")
