library(mlr)
library(randomForest)


fit = train("classif.randomForest", iris.task, importance = TRUE)

fit = randomForest(Species~., data=iris, importance = TRUE)
varImpPlot(fit, type = 1)
varImpPlot(fit, type = 2)


   