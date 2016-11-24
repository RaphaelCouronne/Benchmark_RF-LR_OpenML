data_mining_OpenML <- function(target_path = "Data/Results/clas_time.RData", size = "normal", dataset_count = 329) {
  
  options(java.parameters = "-Xmx8g")
  library( "RWeka" )
  library(OpenML)
  saveOMLConfig(apikey = "7a4391537f767ea70db6af99497653e5", arff.reader = "RWeka", overwrite=TRUE)
  
  source("DataMining-Benchmark-Conversion/benchmark_dataMiningOpenML_functions.R")
  
  print("Begin Datamining OpenML")
  
  # =============================
  # Part 1 : Get the basic informations 
  # ============================= ----
  
  print("1. Load the tasks")
  
  # Load the classification tasks informations if it does not exist yet
  if (!file.exists("Data/OpenML/classifTasks.infos.RData")) {
    tasks = listOMLTasks()
    classifTasks.infos = subset(tasks, task.type == "Supervised Classification" &    # classification
                                  NumberOfClasses == 2 &                             # binary classification
                                  NumberOfInstancesWithMissingValues == 0)           # no missing values
    save(classifTasks.infos, file = "Data/OpenML/classifTasks.infos.RData" )
  } else {
    load("Data/OpenML/classifTasks.infos.RData")
  }
  
  datasets.index = sort(unique(classifTasks.infos$did))
  
  clas = classifTasks.infos
  print(paste("  Number of datasets at the start :", dim(clas)[1]))
  
  
  # =============================
  # Part 2 : More detailed transformations
  # ============================= ----
  
  print("2. Remove datasets using the tasks features")
  
  
  
  
  # remove the redundancies : 473 tasks
  clas = clas[order(clas$did),]
  logic = diff(clas$did)>0
  clas = clas[logic,]
  print(paste("  Number of datasets after removing the redundacies of datasets's IDs :", dim(clas)[1]))
  
  
  # Friedman-, volcanoes- und trX-Datasets : 393 tasks
  clas = clas[substr(clas$name,1,9) != "volcanoes" & substr(clas$name,1,4) != "fri_" & substr(clas$name,1,3) != "tr1" & substr(clas$name,1,3) != "tr2" & substr(clas$name,1,3) != "tr3" & substr(clas$name,1,3) != "tr4", ]
  print(paste("  Number of datasets after removing the obviously simulated datasets :", dim(clas)[1]))
  print("  Datasets removed : Friedman, volcanoes, TrX-Datasets")
  
  # remove the datasets with the same name, they correspond often to datasets with only very slight changes : 383
  doublon = names(sort(table(clas$name)[table(clas$name) > 1]))
  doublon = clas[clas$name %in% doublon,]
  doublon = doublon[order(doublon$name), ]
  
  diff.categorical <- function(x) {
    x = as.factor(x)
    n = length(x)
    res = rep(NA,n)
    res[1] = TRUE
    
    for (i in c(2:n)) {
      res[i] = !identical(x[i-1],x[i])
    }
    res = res*1
    return(res)
  }
  
  indexdoublon.useful = which(diff.categorical(doublon$name)==1)
  indexdoublon.notuseful = which(diff.categorical(doublon$name)==0)
  task.id.notuseful = doublon$task.id[indexdoublon.notuseful]
  indexclas.notuseful = which(clas$task.id %in% task.id.notuseful)
  clas = clas[-indexclas.notuseful,]
  
  
  # Removing High dimentional datasets
  index.highdimension = which(clas$NumberOfFeatures>clas$NumberOfInstances)
  clas= clas[-index.highdimension,]
  print(paste("  Number of datasets after removing the high dimentional datasets p>n:", dim(clas)[1]))
  
  # Ordering according to size (n*p)
  clas = clas[order(clas$NumberOfFeatures * clas$NumberOfInstances), ]
  
  if (size == "tiny") {
    print("Tiny version of benchmark")
    clas = clas[c(1:dataset_count),]
  } else if (size =="normal") {
    print("normal version of benchmark")
  } else {
    stop("Vestion of benchmark not known")
  }
  
  ## Load the tasks to perform actions ----
  
  print("2 Load the datasets for analysis")
  
  # Random permutation for the computation time vizualisation
  #sample_used = sample(nrow(clas))
  #clas = clas[sample_used,]
  
  # time to respond : 380 datasets
  task.id.notresponding = c(7395, 7396, 10111, 4216)
  # 75127, 75144, 75145
  if (length(which(clas$task.id %in% task.id.notresponding))>0) {
    clas = clas[-which(clas$task.id %in% task.id.notresponding),]
  }
  
  print("2.1 Testing dataset's response")
  print("  Results are printed in Data/OpenMLNAsDatasets.Rout")
  print("  Should last around 1 hour")
  
  # categorical target and test loading the datas
  nans = character(nrow(clas))
  file.remove("Data/OpenML/NAsDatasets.Rout")
  nas.file <- file("Data/OpenML/NAsDatasets.Rout", open = "wt")
  pb <- txtProgressBar(min = 0, max = nrow(clas), style = 3)
  
  for(j in 1:nrow(clas)){
    
    sink(nas.file)
    sink(nas.file, type = "message")
    
    tryCatch({
      # Loading the dataset
      omldataset = getOMLDataSet(did = clas$did[j], verbosity = 0)
      if (identical(omldataset$target.features, character(0))) {
        omldataset$target.features="Class"
        omldataset$desc$default.target.attribute="Class"
      }
      nans[j] = class(omldataset$data[, omldataset$target.features])
      
      save(nans, file = "Data/OpenML/NAsDatasets.RData")
      print(j)
      print(nans[j])
      gc()
    }, error = function(e) return(paste0("The variable '", j, "'", 
                                         " caused the error: '", e, "'")))
    sink() 
    sink(type="message")
    setTxtProgressBar(pb, j)
  }
  
  
  
  
  
  # =============================
  # Part 3 : Get the dimension as a feature
  # ============================= ----
  
  print("2.2 Computing dimension")
  print("  Should last around 1 hour")
  file.remove("Data/OpenML/Dimension.Rout")
  dimension.file <- file("Data/OpenML/Dimension.Rout", open = "wt")
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  
  ## Dimension
  dimension = rep(NA, nrow(clas))
  pb = txtProgressBar(min = 0, max = nrow(clas), initial = 0) 
  
  # Begin loop
  for (j in c(1:nrow(clas)) ) {
    
    sink(dimension.file)
    sink(dimension.file, type = "message")
    
    try({
      omldataset = getOMLDataSet(did = clas$did[j], verbosity = 0)
      if (identical(omldataset$target.features, character(0))) {
        omldataset$target.features="Class"
        omldataset$desc$default.target.attribute="Class"
      }
      
      mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
      res = getTaskDimension(mlrtask)
      dimension[j] = res
    })
    
    sink() 
    sink(type="message")
    setTxtProgressBar(pb,j)
  }
  
  clas$dimension = dimension
  
  
  # =============================
  # Part 4 : Save it
  # ============================= ----
  
  # Ordering according to size (n*dimension)
  clas = clas[order(clas$dimension * clas$NumberOfInstances), ]
  
  clas_small = clas[which(clas$NumberOfInstances < 1e3 & clas$dimension < 1e3),]
  clas_big = clas[which(clas$NumberOfInstances * clas$dimension > 1e6 | clas$dimension > 1e3 | clas$NumberOfInstances > 1e5),]
  clas_medium = clas[which(!(clas$task.id %in% c(clas_big$task.id, clas_small$task.id))),]
  
  save(clas, clas_small, clas_medium, clas_big, file = "Data/Results/clas.RData" )
  
  # =============================
  # Part 5 : (optional) Add the time of training
  # ============================= ----
  
  print("Adding the time of training of a Random Forest")
  print("Resampling method: Holdout, 0.2; ntree = 500")
  load(file = "Data/Results/clas.RData" )
  
  ## time train of a randomforest
  rf.timetrain = rep(NA, nrow(clas))
  pb = txtProgressBar(min = 0, max = nrow(clas), initial = 0) 
  
  # Begin loop
  for (j in c(1:nrow(clas)) ) {
    try({
      omldataset = getOMLDataSet(did = clas$did[j], verbosity = 0)
      if (identical(omldataset$target.features, character(0))) {
        omldataset$target.features="Class"
        omldataset$desc$default.target.attribute="Class"
      }
      mlrtask = convertOMLDataSetToMlr(omldataset)
      
      # get the time of training
      lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
      measures = list(timetrain)
      rdesc = makeResampleDesc("Holdout", split = 0.2, stratify = TRUE)
      configureMlr(on.learner.error = "quiet", show.learner.output = FALSE, show.info = FALSE)
      bmr = benchmark(lrn.classif.rf, mlrtask, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
      perfs = getBMRPerformances(bmr, as.df = TRUE)
      time.train = sum(perfs$timetrain)
      rf.timetrain[j] = time.train
      df.time = data.frame(did = clas$did, time = rf.timetrain)
      save(df.time, file = "Data/OpenML/rf.timetrain.RData" )
      setTxtProgressBar(pb,j)
    })
  }
  
  clas_time = clas
  clas_time$rf.timetrain = rf.timetrain
  
  # reorder according to time and na
  clas_time = clas_time[order(clas_time$rf.timetrain), ]
  
  
  # =============================
  # Part 6 : Save it
  # ============================= ----
  
  # clas_time small medium big non supported
  clas_time_small = clas_time[which(clas_time$rf.timetrain < 1),]
  clas_time_medium = clas_time[which(clas_time$rf.timetrain > 1 &  clas_time$rf.timetrain<10 ) ,]
  clas_time_big = clas_time[which(clas_time$rf.timetrain >10),]
  clas_time_NA = clas_time[which(is.na(clas_time$rf.timetrain)),]
  
  # save it
  save(clas_time, clas_time_small, clas_time_medium, clas_time_big, clas_time_NA,  file = target_path )
}
