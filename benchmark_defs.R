library(mlr)

if (OS == "OSX") {
  # OSX
  githubdir = "/Users/Shadok/Programmation/Github"
  dir = file.path(githubdir, "BenchmarkOpenMl/FinalVersion/")
} else {
  # windows
  githubdir = "C:/Users/couronne/Desktop/GitHub"
  dir = file.path(githubdir, "BenchmarkOpenMl/FinalVersion/")
}

# datasets
load(file = file.path(githubdir,"Data_BenchmarkOpenMl/Final/DataMining/clas.RData"))


# performance meqsures
#source(file = file.path(githubdir,"MulticlassAUC/AUCmlr.R"))
MEASURES = list(acc, ber, mmce, brier, timetrain, auc)
