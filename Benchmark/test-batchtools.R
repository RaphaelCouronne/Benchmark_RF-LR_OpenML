
rm(list=ls())
library(batchtools)
# Pi Example ----

file.dir = "test/batchtools"
unlink(file.dir, recursive = TRUE)
reg = makeRegistry(file.dir = file.dir, seed = 1)
#reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = 3) 

piApprox = function(n) {
  nums = matrix(runif(2 * n), ncol = 2)
  d = sqrt(nums[, 1]^2 + nums[, 2]^2)
  4 * mean(d <= 1)
}
piApprox(1000)

batchMap(fun = piApprox, n = rep(1e5, 100), )
names(getJobTable())
submitJobs(resources = list(walltime = 3600, memory = 1024))
getStatus()


# Machine Learning Example ----


library(batchtools)
file.dir = "test/batchtools"
unlink(file.dir, recursive = TRUE)
reg = makeExperimentRegistry(file.dir = file.dir, seed = 1)
#reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = 3) 

subsample = function(data, job, ratio, ...) {
  n = nrow(data)
  train = sample(n, floor(n * ratio))
  test = setdiff(seq_len(n), train)
  list(test = test, train = train)
}

data("iris", package = "datasets")
addProblem(name = "iris", data = iris, fun = subsample, seed = 42)

svm.wrapper = function(data, job, instance, ...) {
  library("e1071")
  mod = svm(Species ~ ., data = data[instance$train, ], ...)
  pred = predict(mod, newdata = data[instance$test, ], type = "class")
  table(data$Species[instance$test], pred)
}
addAlgorithm(name = "svm", fun = svm.wrapper)


forest.wrapper = function(data, job, instance, ...) {
  library("ranger")
  mod = ranger(Species ~ ., data = data[instance$train, ], write.forest = TRUE)
  pred = predict(mod, data = data[instance$test, ])
  table(data$Species[instance$test], pred$predictions)
}
addAlgorithm(name = "forest", fun = forest.wrapper)


# problem design: try two values for the ratio parameter
pdes = list(iris = data.table(ratio = c(0.67, 0.9)))

# algorithm design: try combinations of kernel and epsilon exhaustively,
# try different number of trees for the forest
ades = list(
  svm = CJ(kernel = c("linear", "polynomial", "radial"), epsilon = c(0.01, 0.1)),
  forest = data.table(ntree = c(1e5,2e5))
)

addExperiments(pdes, ades, repls = 5)

submitJobs()

reduce = function(res) list(mce = (sum(res) - sum(diag(res))) / sum(res))
results = unwrap(reduceResultsDataTable(fun = reduce))
head(results)
