# gets the dimensionality of a mlr dataset associated with the given task
getTaskDimension = function(task) {
  
  nCol = ncol(task$env$data)
  
  nbNumeric = task$task.desc$n.feat[1]
  nbFactorDimension = sum(sapply(c(1:nCol), function(x) getDimensionalityColumn(task$env$data[,x]) ))
  nbFactorDimension = nbFactorDimension - 1 # because of the added 2nd level of the target
  
  dimension = as.integer(nbNumeric+nbFactorDimension) # because of the column target
  return(dimension)
}

getDimensionalityColumn = function(col) {
  res = 0
  if (is.factor(col)) {
    res = length(levels(col))-1
  }
  return(res)
}

