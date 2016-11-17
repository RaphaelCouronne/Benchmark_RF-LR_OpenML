rm(list = ls())
options( java.parameters = "-Xmx16g" )
library( "RWeka" )
library(OpenML)
saveOMLConfig(apikey = "7a4391537f767ea70db6af99497653e5", arff.reader = "RWeka", overwrite=TRUE)

OS = "Windows"

if (OS == "OSX") {
  # OSX
  githubdir = "/Users/Shadok/Programmation/Github"
  dir = file.path(githubdir, "BenchmarkOpenMl/FinalVersion/")
} else {
  # windows
  githubdir = "Z:/Raphael/GiHub/"
  dir = file.path(githubdir, "IBE_Benchmark-OpenML/")
}

setwd(file.path(githubdir, "IBE_Benchmark-OpenML"))

source("benchmark_dataMiningOpenML_functions.R")


# =============================
# Part 1 : Get the basic informations 
# ============================= ----

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



# =============================
# Part 2 : More detailed transformations
# ============================= ----

clas = classifTasks.infos

## Selecting the tasks only with tasks.infos ----

# remove the redundancies : 473 tasks
clas = clas[order(clas$did),]
logic = diff(clas$did)>0
clas = clas[logic,]

# Friedman-, volcanoes- und trX-Datasets : 393 tasks
clas = clas[substr(clas$name,1,9) != "volcanoes" & substr(clas$name,1,4) != "fri_" & substr(clas$name,1,3) != "tr1" & substr(clas$name,1,3) != "tr2" & substr(clas$name,1,3) != "tr3" & substr(clas$name,1,3) != "tr4", ]

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

diff.categorical(doublon$name)

indexdoublon.useful = which(diff.categorical(doublon$name)==1)
indexdoublon.notuseful = which(diff.categorical(doublon$name)==0)
task.id.notuseful = doublon$task.id[indexdoublon.notuseful]
indexclas.notuseful = which(clas$task.id %in% task.id.notuseful)
clas = clas[-indexclas.notuseful,]

# Ordering according to size (n*p)
clas = clas[order(clas$NumberOfFeatures * clas$NumberOfInstances), ]


## Test the tasks loading each one of them ----

# time to respond : 380 datasets
task.id.notresponding = c(7395, 7396, 10111,75127, 75144, 75145)
clas = clas[-which(clas$task.id %in% task.id.notresponding),]

# categorical target and test loading the datas
nans = character(nrow(clas))
nas.file <- file("../Data_BenchmarkOpenMl/Final/DataMining/nasRUN.Rout", open = "wt")
sink(nas.file)
sink(nas.file, type = "message")

for(j in 1:nrow(clas)){
  tryCatch({
    print(j)
    
    # with dataset
    omldataset = getOMLDataSet(did = clas$did[j], verbosity = 0)
    if (identical(omldataset$target.features, character(0))) {
      omldataset$target.features="Class"
      omldataset$desc$default.target.attribute="Class"
    }
    nans[j] = class(omldataset$data[, omldataset$target.features])
    
    save(nans, file = "../Data_BenchmarkOpenMl/Final/DataMining/nans_clas.RData")
    print(nans[j])
    gc()
  }, error = function(e) return(paste0("The variable '", j, "'", 
                                       " caused the error: '", e, "'"))
)}

sink()
sink(type = "message")
file.show("../Data_BenchmarkOpenMl/Final/DataMining/nasRUN.Rout")
print("end of nas")

# what up with the logicals and data.frame ?
# seems like no problem fo the logicals
# for the data.frame it seems the target has not been specified. It usually is "class"




# =============================
# Part 3 : Get the dimension as a feature
# ============================= ----

## Dimension

dimension = rep(NA, nrow(clas))

# Begin loop
for (j in c(1:nrow(clas)) ) {
  print(paste("iteration ", j))
  try({
    omldataset = getOMLDataSet(did = clas$did[j], verbosity = 0)
    if (identical(omldataset$target.features, character(0))) {
      omldataset$target.features="Class"
      omldataset$desc$default.target.attribute="Class"
    }
    mlrtask = convertOMLDataSetToMlr(omldataset)
    res = getTaskDimension(mlrtask)
    print(res)
    dimension[j] = res
  })
}

clas$dimension = dimension



# =============================
# Part 4 : Save it
# ============================= ----

# Ordering according to size (n*dimension)
clas = clas[order(clas$dimension * clas$NumberOfInstances), ]

plot(clas$NumberOfInstances)
plot(clas$dimension)

hist(clas$NumberOfFeatures)
hist(clas$NumberOfInstances)

clas_small = clas[which(clas$NumberOfInstances < 1e3 & clas$dimension < 1e3),]
clas_big = clas[which(clas$NumberOfInstances * clas$dimension > 1e6 | clas$dimension > 1e3 | clas$NumberOfInstances > 1e5),]
clas_medium = clas[which(!(clas$task.id %in% c(clas_big$task.id, clas_small$task.id))),]

save(clas, clas_small, clas_medium, clas_big, file = "../Data_BenchmarkOpenMl/Final/DataMining/clas.RData" )

plot(clas_big$NumberOfInstances)
plot(clas_big$dimension)

plot(clas_medium$NumberOfInstances)
plot(clas_medium$dimension)

# =============================
# Part 5 : (optional) Add the time of training
# ============================= ----

load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas.RData" )

## time train of a randomforest
rf.timetrain = rep(NA, nrow(clas))

load(file = "Data/OpenML/rf.timetrain.RData")

# Begin loop
for (j in c(351:nrow(clas)) ) {
  print(paste("iteration ", j))
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
    configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
    bmr = benchmark(lrn.classif.rf, mlrtask, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = FALSE)
    perfs = getBMRPerformances(bmr, as.df = TRUE)
    time.train = sum(perfs$timetrain)
    save(rf.timetrain, file = "Data/OpenML/rf.timetrain.RData" )
    
    print(time.train)
    rf.timetrain[j] = time.train
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
save(clas_time, clas_time_small, clas_time_medium, clas_time_big, clas_time_NA,  file = "Data/Results/clas_time.RData" )

rm(list = ls())
load(file = "Data/Results/clas_time.RData")



# Low dimension
rm(list = ls())
load(file = "Data/Results/clas_time.RData")

index.highdimension = which(clas_time$NumberOfFeatures>clas_time$NumberOfInstances)
clas_time_lowdim = clas_time[-index.highdimension,]

clas_time_lowdim_small = clas_time_lowdim[which(clas_time_lowdim$rf.timetrain < 1),]
clas_time_lowdim_medium = clas_time_lowdim[which(clas_time_lowdim$rf.timetrain > 1 &  clas_time_lowdim$rf.timetrain<10 ) ,]
clas_time_lowdim_big = clas_time_lowdim[which(clas_time_lowdim$rf.timetrain >10),]
clas_time_lowdim_NA = clas_time_lowdim[which(is.na(clas_time_lowdim$rf.timetrain)),]

save(clas_time_lowdim, clas_time_lowdim_small, 
     clas_time_lowdim_medium, 
     clas_time_lowdim_big, 
     clas_time_lowdim_NA,  
     file = "Data/Results/clas_time_lowdim.RData" )


