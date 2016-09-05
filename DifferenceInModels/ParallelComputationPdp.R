library(mlr)
rm(list = ls())
OS = "Windows"
set.seed(1)

# Load the environment
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")
source(file = "DifferenceInModels/pdpInterpretabilityFunction.R")
clas_used = rbind(clas_time_small, clas_time_medium)
OMLDATASETS = clas_used$did
source(file = "benchmark_defs.R")


## Example 1 - Multi-core on a single computer
sink('../Data_BenchmarkOpenMl/Final/Results/Windows/SnowFall.computationPdpDifference.Rout', split=TRUE)
.Platform
.Machine
R.version
Sys.info()

library(snowfall) 
# 1. Initialisation of snowfall. 
# (if used with sfCluster, just call sfInit()) 
sfInit(parallel=TRUE, cpus=10)

# 2. Loading data. 

# 3. Wrapper, which can be parallelised. 
runPDP <- function(data.index) {
  
  library(OpenML)
  library(mlr)
  
  print(paste("debut dataset ", data.index))
  print(Sys.time())
  # get the dataset
  omldataset = getOMLDataSet(data.index)
  if (identical(omldataset$target.features, character(0))) {
    omldataset$target.features="Class"
    omldataset$desc$default.target.attribute="Class"
  }
  task = convertOMLDataSetToMlr(omldataset)
  task$task.desc$id = paste("dataset", data.index)
  
  weightedPdpInterpretability = weightedPdpInterpretability(task,10)
  
  return(weightedPdpInterpretability)
}

wrapper <- function(data.index) {
  tryCatch({
    
    # benchmark
    runPDP(data.index)
  }, error = function(e) return(paste0("The variable '", data.index, "'", 
                                       " caused the error: '", e, "'")))
}


# 4. Exporting needed data and loading required 
# packages on workers. 
sfExport("runPDP", "weightedPdpInterpretability") 
sfLibrary(cmprsk) 

# 5. Start network random number generator 
# (as "sample" is using random numbers). 
sfClusterSetupRNG() 

# 6. Distribute calculation
start <- Sys.time(); pdp.weigheddifference <- sfLapply(OMLDATASETS, wrapper) ; Sys.time()-start

# 7. Stop snowfall 
sfStop() 

save(pdp.weigheddifference, clas_used, file = "../Data_BenchmarkOpenMl/Final/pdp.weigheddifferenceAll.RData")
print("done with cluster")
