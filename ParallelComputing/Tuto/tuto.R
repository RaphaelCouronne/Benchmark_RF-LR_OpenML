## http://www.win-vector.com/blog/2016/01/parallel-computing-in-r/

d <- iris # let "d" refer to one of R's built in data sets
vars <- c('Sepal.Length','Sepal.Width','Petal.Length')
yName <- 'Species'
yLevels <- sort(unique(as.character(d[[yName]])))
print(yLevels)
## [1] "setosa"     "versicolor" "virginica" 


# let's code first the fitting

fitOneTargetModel <- function(yName,yLevel,vars,data) {
  formula <- paste('(',yName,'=="',yLevel,'") ~ ',
                   paste(vars,collapse=' + '),sep='')
  fit = glm(as.formula(formula),family=binomial,data=data)
  return(fit)
}

# “embarrassingly parallel”
for(yLevel in yLevels) {
  print("*****")
  print(yLevel)
  print(fitOneTargetModel(yName,yLevel,vars,d))
}


# Start up a parallel cluster
nCoresDispo = detectCores()-1
parallelCluster <- parallel::makeCluster(nCoresDispo)
print(parallelCluster)
## socket cluster with 4 nodes on host ‘localhost’


# launch cluster ?
tryCatch(
  models <- parallel::parLapply(parallelCluster,
                                yLevels,fitOneTargetModel),
  error = function(e) print(e)
)


# build the single argument function we are going to pass to parallel
mkWorker <- function(yName,vars,d) {
  # make sure each of the three values we need passed 
  # are available in this environment
  force(yName)
  force(vars)
  force(d)
  # define any and every function our worker function 
  # needs in this environment
  # fitOneTargetModel <- function(yName,yLevel,vars,data) {
  #   formula <- paste('(',yName,'=="',yLevel,'") ~ ',
  #                    paste(vars,collapse=' + '),sep='')
  #   glm(as.formula(formula),family=binomial,data=data)
  # }
  force(fitOneTargetModel)
  # Finally: define and return our worker function.
  # The function worker's "lexical closure" 
  # (where it looks for unbound variables)
  # is mkWorker's activation/execution environment 
  # and not the usual Global environment.
  # The parallel library is willing to transport 
  # this environment (which it does not
  # do for the Global environment).
  worker <- function(yLevel) {
    fitOneTargetModel(yName,yLevel,vars,d)
  }
  return(worker)
}

models <- parallel::parLapply(parallelCluster,yLevels,
                              mkWorker(yName,vars,d))
names(models) <- yLevels
print(models)


# Bind to environment
source('FinalVersion/ParallelComputing/bindToEnv.R') # Download from: http://winvector.github.io/Parallel/bindToEnv.R
# build the single argument function we are going to pass to parallel
mkWorker <- function() {
  bindToEnv(objNames=c('yName','vars','d','fitOneTargetModel'))
  function(yLevel) {
    fitOneTargetModel(yName,yLevel,vars,d)
  }
}

models <- parallel::parLapply(parallelCluster,yLevels,
                              mkWorker())
names(models) <- yLevels
print(models)


# Shutdown cluster neatly
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}
