# Remove non usefull datasets using the tasks attributes
# Load all the datasets, test them, compute their dimension and computation time
get_data_OpenML <- function(target_path = "Data/Results/clas_time.RData", force = FALSE, computeTime = FALSE, seed = 1) {

  library(RWeka)
  library(OpenML)
  
  print("Begin Datamining OpenML", quote = FALSE)
  
  # =============================
  # Part 1 : Get the basic informations 
  # ============================= ----
  
  print("1. Load the tasks", quote = FALSE)
  
  # Load the classification tasks informations if it does not exist yet
  
  # tasks = listOMLTasks(limit = NULL)
  # classifTasks.infos = subset(tasks, task.type == "Supervised Classification" &    # classification
  #                               number.of.classes == 2 &                             # binary classification
  #                               number.of.instances.with.missing.values == 0)           # no missing values
  # save(classifTasks.infos, file = "Data/OpenML/classifTasks.infos.RData" )
  
  # We work with the given data from OpenML
  load("Data/OpenML/classifTasks.infos.RData")
  datasets.index = sort(unique(classifTasks.infos$data.id))
  clas = classifTasks.infos
  print(paste("  Number of datasets for supervised binary classification without missing values :", dim(clas)[1]), quote = FALSE)
  
  
  # =============================
  # Part 2 : Select datasets using OpenML task features
  # ============================= 
  
  print("2. Remove datasets using the tasks features", quote = FALSE)

  # remove the redundancies : 470 datasets
  clas = clas[order(clas$data.id),]
  logic = diff(clas$data.id)>0
  clas = clas[logic,]
  print(paste("  Number of datasets after removing the redundacies of datasets's IDs :", dim(clas)[1]), quote = FALSE)
  
  # Friedman-, volcanoes- und trX-Datasets : 393 datasets
  clas = clas[substr(clas$name,1,9) != "volcanoes" & substr(clas$name,1,4) != "fri_" & substr(clas$name,1,3) != "tr1" & substr(clas$name,1,3) != "tr2" & substr(clas$name,1,3) != "tr3" & substr(clas$name,1,3) != "tr4", ]
  print(paste("  Number of datasets after removing the obviously simulated datasets :", dim(clas)[1]), quote = FALSE)
  print("  Datasets removed : Friedman, volcanoes, TrX-Datasets", quote = FALSE)
  
  # remove the datasets with the same name, they correspond often to datasets with only very slight changes : 380 datasets
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
  
  indexDuplicate.useful = which(diff.categorical(doublon$name)==1)
  indexDuplicate.notuseful = which(diff.categorical(doublon$name)==0)
  task.id.notuseful = doublon$task.id[indexDuplicate.notuseful]
  indexclas.notuseful = which(clas$task.id %in% task.id.notuseful)
  clas = clas[-indexclas.notuseful,]
  print(paste("  Number of datasets after removing the redundancies in dataset's names:", dim(clas)[1]), quote = FALSE)
  
  # Removing High dimentional datasets : 326 datasets
  index.highdimension = which(clas$number.of.features>clas$number.of.instances)
  clas= clas[-index.highdimension,]
  print(paste("  Number of datasets after removing the high dimentional datasets p>n:", dim(clas)[1]), quote = FALSE)
  
  # Ordering according to size (n*p)
  clas = clas[order(clas$number.of.features * clas$number.of.instances), ]
  

  # =============================
  # Part 3 : Loading the datasets
  # ============================= ----
  print("3. Load the datasets for analysis", quote = FALSE)
  print("  Testing the datasets ", quote = FALSE)
  
  # categorical target and test loading the datas ----
  n.row = nrow(clas)
  
  if (file.exists("Data/OpenML/df.infos.RData") && !force) {
    print("  File \"Data/OpenML/df.infos.RData\" has been found and loaded, it will not be recomputed", quote = FALSE)
    
    # laod the file
    load(file = "Data/OpenML/df.infos.RData")
    
    # check integrity of datasets
    if (!identical(clas$data.id,df.infos$data.id)) {
      print("  Difference between df.infos and clas", quote = FALSE)
      
      # reorganise the data.id
      notcomputed = subset(clas, select = c("data.id", "task.id", 
                                            "number.of.instances","number.of.features"))[which(!clas$data.id %in% df.infos$data.id),]
      df.infos.new = data.frame(matrix(data = NA, nrow = length(df.infos$data.id) + length(notcomputed$data.id), ncol = 15))
      names(df.infos.new) = c("index", "data.id", "task.id","n","p", "began", "done", 
                              "loaded","converted", "target_type", "dimension", 
                              "rf_time", "lr_time", "rf_NA", "lr_NA")
      
      df.infos.new[c(1:length(df.infos$data.id)),] = df.infos
      df.infos.new[c((length(df.infos$data.id)+1):length(df.infos.new$data.id)),c(2,3,4,5)] = notcomputed
      df.infos.new = df.infos.new[order(df.infos.new$data.id),]
      df.infos.new = df.infos.new[order(df.infos.new$n*df.infos.new$p),]
      df.infos.new$index = c(1:length(df.infos.new$index))
      df.infos = df.infos.new
    }
    
  
  } else {
    
    print("  Creating a new df.infos, total computation should last 1-2 days", quote = FALSE)
    
    # Create new df.infos
    df.infos = data.frame(matrix(data = NA, nrow = n.row, ncol = 15))
    names(df.infos) = c("index", "data.id", "task.id","n","p", "began", "done", 
                        "loaded","converted", "target_type", "dimension", 
                        "rf_time", "lr_time", "rf_NA", "lr_NA")
    df.infos$index = c(1:n.row)
    df.infos$data.id = clas$data.id
    df.infos$task.id=clas$task.id
    df.infos$n = clas$number.of.instances
    df.infos$p = clas$number.of.features
    
    if (file.exists("Data/OpenML/df.infos.Rout")) {file.remove("Data/OpenML/df.infos.Rout")}
  }
  
  
  index.not.done = which(is.na(df.infos$done))
  
  
  # If there are new datasets, try loading, conversion, computation of dimension, and time
  if (!(identical(index.not.done, integer(0))))  {
    df.infos.file <- file("Data/OpenML/df.infos.Rout", open = "w")
    
    pb <- txtProgressBar(min = 1, max = length(index.not.done), style = 3)
    
    for(index in c(1:length(index.not.done))){
      
      # Index
      j = index.not.done[index]
      
      # begin
      df.infos$began[j] = TRUE
      
      # Try
      tryCatch({
        
        # Loading the dataset
        omldataset = getOMLDataSet(data.id = clas$data.id[j], verbosity = 0)
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
        
        if (computeTime) {
          # Compute the time for lr andrf
          learners = list(makeLearner("classif.randomForest"),
                          makeLearner("classif.logreg"))
          rdesc = makeResampleDesc("Holdout", split = 0.8, stratify = TRUE)
          configureMlr(on.learner.error = "warn", show.learner.output = TRUE, show.info = TRUE)
          
          sink(df.infos.file)
          sink(df.infos.file, type = "message")
          print(paste("Iteration",j,"dataset",clas$data.id[j]), quote = FALSE)
          set.seed(seed)
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
        }
        
      }, error = function(e) return(paste0("The variable '", j, "'", 
                                           " caused the error: '", e, "'")))
      
      setTxtProgressBar(pb, index)
      df.infos$done[j] = TRUE
      save(df.infos, file = "Data/OpenML/df.infos.RData")
    }
  }
  
  
  
  # =============================
  # Part 6 : Select the datasets
  # ============================= ----
  print("4. Remove datasets that failed", quote = FALSE)
  clas_select = data.frame(clas, df.infos)
  clas_select = clas_select[, -which(names(clas_select) %in% c("task.id.1", "data.id.1"))]
  clas_select$time = clas_select$rf_time+clas_select$lr_time
  
  # remove the one with loading unsuccessfull
  data.id_loading_failed = clas_select$data.id[which(is.na(clas_select$loaded))]
  if (!identical(which(clas_select$data.id %in% data.id_loading_failed), integer(0))) {
    clas_select = clas_select[-which(clas_select$data.id %in% data.id_loading_failed),]
  }
  
  print(paste("  Removing the datasets which loading failed :",nrow(clas_select), "Datasets"), quote = FALSE)
  
  # remove the one with conversion unsuccessfull
  data.id_convert_failed = clas_select$data.id[which(is.na(clas_select$converted))]
  if (!identical(which(clas_select$data.id %in% data.id_convert_failed), integer(0))) {
    clas_select = clas_select[-which(clas_select$data.id %in% data.id_convert_failed),]
  }
  print(paste("  Removing the datasets which conversion failed :",nrow(clas_select), "Datasets"), quote = FALSE)
  
  
  # =============================
  # Part 6 : Save it
  # ============================= 
  
  clas_time = clas_select
  
  # clas_time : small medium big and too big
  # Too big dataset corresponds to a training time of lr+rf that exceeds 100s on a Holdout 80% train and 20% test on the dataset
  # It corresponds to datasets such that n*p > 3000000 on our dataset pool
  clas_time_small = clas_time[which(clas_time$n*clas_time$p < 20000),]
  clas_time_medium = clas_time[which(clas_time$n*clas_time$p  > 20000 &  clas_time$n*clas_time$p  <  100000) ,]
  clas_time_big = clas_time[which(clas_time$n*clas_time$p  > 100000 &  clas_time$n*clas_time$p <  3000000) ,]
  clas_time_toobig = clas_time[which(clas_time$n*clas_time$p > 3000000),] 
  
  # save it
  save(clas_time, clas_time_small, clas_time_medium, clas_time_big, clas_time_toobig,  file = target_path )
}



# gets the dimensionality of a mlr dataset associated with the given task
getTaskDimension = function(task) {
  
  nCol = ncol(task$env$data)
  
  nbNumeric = task$task.desc$n.feat[1]
  nbFactorDimension = sum(sapply(c(1:nCol), function(x) getColumnDimension(task$env$data[,x]) ))
  nbFactorDimension = nbFactorDimension - 1 # because of the added 2nd level of the target
  
  dimension = as.integer(nbNumeric+nbFactorDimension) # because of the column target
  return(dimension)
}

# Get dimension column
getColumnDimension = function(col) {
  res = 0
  if (is.factor(col)) {
    res = length(levels(col))-1
  }
  return(res)
}

