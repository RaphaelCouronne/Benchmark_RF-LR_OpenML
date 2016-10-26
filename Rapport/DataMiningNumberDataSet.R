
## get task list ----
tasks = listOMLTasks()



## Select the datasets ----
classifTasks.infos = tasks
print(paste("At the beginning we have",length(unique(tasks$did)),"datasets"))


## binary classification 19504
classifTasks.infos = subset(classifTasks.infos, task.type == "Supervised Classification" &    # classification
                              NumberOfClasses == 2)   

print(paste("We consider only the binary classification problems to have",length(unique(classifTasks.infos$did)),"datasets"))


## No missing values 557
classifTasks.infos = subset(classifTasks.infos, NumberOfInstancesWithMissingValues == 0)   

print(paste("We remove the missing values to have",length(unique(classifTasks.infos$did)),"datasets"))

## Remove simulated datasets 474
clas = classifTasks.infos

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


print(paste("We remove the simulated datasets to have",length(unique(clas$did)),"datasets"))

# remove the non working datasets
task.id.notresponding = c(7395, 7396, 10111,75127, 75144, 75145)
clas = clas[-which(clas$task.id %in% task.id.notresponding),]

print(paste("We remove the non working datasets to have",length(unique(clas$did)),"datasets"))

# Remove nas to have 