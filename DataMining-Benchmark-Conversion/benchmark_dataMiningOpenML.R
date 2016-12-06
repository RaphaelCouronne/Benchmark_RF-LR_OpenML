data_mining_OpenML <- function(target_path = "Data/Results/clas_time.RData", force = FALSE, dataset_count = 329) {
  
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
  # Part 2 : Select datasets using OenML task features
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
  
  
  ## Load the tasks to perform actions ----
  
  print("3 Load the datasets for analysis")
  
  
  
  # =============================
  # Part 3 : Loading the datasets
  # ============================= ----
  
  print("3.1 Testing the datasets ")
  print("  Should last around 1-2 hour for all the datasets")
  
  # categorical target and test loading the datas ----
  n.row = nrow(clas)
  
  
  if (file.exists("Data/OpenML/df.infos.RData") && !force) {
    
    # laod the file
    print("load file")
    load(file = "Data/OpenML/df.infos.RData")
    
    # check integrity of datasets
    if (!identical(clas$did,df.infos$did)) {
      stop("Stop : conflict in the datasets")
    }
    i_beginning = which(df.infos$done)[length(which(df.infos$done))]
    
  } else {
    df.infos = data.frame(matrix(data = NA, nrow = n.row, ncol = 13))
    names(df.infos) = c("index", "did", "taskid", "began", "done", 
                        "loaded","converted", "target_type", "dimension", 
                        "rf_time", "lr_time", "rf_NA", "lr_NA")
    df.infos$index = c(1:n.row)
    df.infos$did = clas$did
    df.infos$taskid=clas$task.id
    
    i_beginning = 1
    
    if (file.exists("Data/OpenML/df.infos.Rout")) {file.remove("Data/OpenML/df.infos.Rout")}
  }
  
  
  df.infos.file <- file("Data/OpenML/df.infos.Rout", open = "w")
  pb <- txtProgressBar(min = i_beginning, max = dataset_count, style = 3)
  
  for(j in c(i_beginning:dataset_count)){
    
    # begin
    df.infos$began[j] = TRUE
    
    # Try
    tryCatch({
      
      # Loading the dataset
      omldataset = getOMLDataSet(data.id = clas$did[j], verbosity = 0)
      if (identical(omldataset$target.features, character(0))) {
        omldataset$target.features="Class"
        omldataset$desc$default.target.attribute="Class"
      }
      df.infos$loaded[j] = "TRUE" 
      
      # check the target
      df.infos$target_type = class(omldataset$data[, omldataset$target.features])
      
      # Transform to mlr task
      configureMlr(on.learner.error = "warn", show.learner.output = TRUE, show.info = FALSE)
      mlrtask = convertOMLDataSetToMlr(omldataset, verbosity = 0)
      df.infos$converted[j] = TRUE
      
      # Get the dimension
      df.infos$dimension[j] = getTaskDimension(mlrtask)

      
      # Compute the time for lr andrf
      learners = list(makeLearner("classif.randomForest"),
                      makeLearner("classif.logreg"))
      rdesc = makeResampleDesc("Holdout", split = 0.8, stratify = TRUE)
      configureMlr(on.learner.error = "warn", show.learner.output = TRUE, show.info = TRUE)
      
      sink(df.infos.file)
      sink(df.infos.file, type = "message")
      print(paste("Iteration",j,"dataset",clas$did[j]))
      bmr = benchmark(learners, mlrtask, rdesc, list(acc,timetrain), 
                      keep.pred = TRUE, models = FALSE, show.info = FALSE)
      sink() 
      sink(type="message")
      
      perfs=NA
      perfs = getBMRPerformances(bmr, as.df = TRUE)
      time.train = sum(perfs$timetrain)
      
      df.infos$rf_time[j]=perfs$timetrain[which(perfs$learner.id=="classif.randomForest")]
      df.infos$lr_time[j]=perfs$timetrain[which(perfs$learner.id=="classif.logreg")]
      
      df.infos$rf_NA[j] = is.na(perfs$acc[which(perfs$learner.id=="classif.randomForest")])
      df.infos$lr_NA[j] = is.na(perfs$acc[which(perfs$learner.id=="classif.logreg")])
      
    }, error = function(e) return(paste0("The variable '", j, "'", 
                                         " caused the error: '", e, "'")))
    
    setTxtProgressBar(pb, j)
    df.infos$done[j] = TRUE
    save(df.infos, file = "Data/OpenML/df.infos.RData")
  }
  
  
  # =============================
  # Part 6 : Select the observations
  # ============================= ----
  print("")
  print("Removing datasets that failed")
  clas_select = data.frame(clas, df.infos)
  clas_select$time = clas_select$rf_time+clas_select$lr_time
  
  # select the number we decided
  clas_select = clas_select[c(1:dataset_count),]
  print(paste("Removing the non considered datasets :",nrow(clas_select), "Datasets"))
  
  # remove the one with loading unsuccessfull
  did_loading_failed = clas_select$did[which(is.na(clas_select$loaded))]
  if (!identical(which(clas_select$did %in% did_loading_failed), integer(0))) {
    clas_select = clas_select[-which(clas_select$did %in% did_loading_failed),]
  }
  
  print(paste("Removing the datasets which loading failed :",nrow(clas_select), "Datasets"))
  
  # remove the one with conversion unsuccessfull
  did_convert_failed = clas_select$did[which(is.na(clas_select$converted))]
  if (!identical(which(clas_select$did %in% did_convert_failed), integer(0))) {
    clas_select = clas_select[-which(clas_select$did %in% did_convert_failed),]
  }
  print(paste("Removing the datasets which conversion failed :",nrow(clas_select), "Datasets"))
  
  # remove the one with NAs on LR and RF
  did_learner_failed = clas_select$did[which(is.na(clas_select$rf_NA) |
                                            is.na(clas_select$lr_NA) |
                                            clas_select$rf_NA        |
                                            clas_select$lr_NA  )]
  if (!identical(which(clas_select$did %in% did_learner_failed), integer(0))) {
    clas_select = clas_select[-which(clas_select$did %in% did_learner_failed),]
  }
  print(paste("Removing the datasets which lr or rf failed :",nrow(clas_select), "Datasets"))
  
  
  
  # =============================
  # Part 6 : Save it
  # ============================= ----
  
  clas_time = clas_select
  
  # reorder according to time and na
  clas_time = clas_time[order(clas_time$time), ]
  
  # clas_time small medium big non supported
  clas_time_small = clas_time[which(clas_time$time < 1),]
  clas_time_medium = clas_time[which(clas_time$time > 1 &  clas_time$time<10 ) ,]
  clas_time_big = clas_time[which(clas_time$time >10),]
  clas_time_NA = clas_time[which(is.na(clas_time$time)),]
  
  # save it
  save(clas_time, clas_time_small, clas_time_medium, clas_time_big, clas_time_NA,  file = target_path )
}
