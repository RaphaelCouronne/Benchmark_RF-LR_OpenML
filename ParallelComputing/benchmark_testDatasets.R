rm(list = ls())
library(mlr)
library(OpenML)
OS = "Windows"
set.seed(1)

## Can we load the datasets ??

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas.RData")
clas_used = clas_medium
OMLDATASETS = clas_used$did
source(file = "FinalVersion/benchmark_defs.R")

# test the loading of datasets

n = length(OMLDATASETS)
task.list = rep(NA, n)
task.list = as.list(task.list)
count = 0

for (i in OMLDATASETS) {
  count = count + 1
  print(count)
  print(paste("dataset ", i))
  tryCatch({
  omldataset = getOMLDataSet(i)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  task.list[[i]]<-task
  }, error = function(e) return(paste0("The variable '", i, "'", 
                                        " caused the error: '", e, "'")))
}





## Can we run the benchmark on the datasets (in finite time if possible)

