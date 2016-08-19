library(mlr)

if (OS == "OSX") {
  # OSX
  githubdir = "/Users/Shadok/Programmation/Github"
  dir = file.path(githubdir, "IBE_Benchmark/")
} else {
  # windows
  githubdir = "Z:/Raphael/GiHub/"
  dir = file.path(githubdir, "IBE_Benchmark/")
}


# performance meqsures
#source(file = file.path(githubdir,"MulticlassAUC/AUCmlr.R"))
MEASURES = list(acc, ber, mmce, brier, timetrain, auc, logloss)
