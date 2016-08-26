
library(mlr)
library(ggplot2)
library(scatterplot3d)
library()

scatterplot3d(x, y=NULL, z=NULL)
set.seed(1)

## Generating the data ----
n=5e3



X1<-runif(n, min = -2, max= 2)
X2<-runif(n, min = -2, max= 2)
X3<-runif(n, min = -2, max= 2) #dummy 

product=2+X1^3+2*X2+2*X1*X2+X1

probabilities =  plogis(product)

Y<-as.factor(rbinom(n,1,prob = probabilities))
df<-data.frame(X1,X2,X3,Y)




# Visualization of the datas
plot.data <- ggplot(data=df, aes(x=X1, y=X2, colour=Y))
plot.data <- plot.data + geom_point(size=3) # also geom_points
plot.data <- plot.data + ggtitle("Simulated Dataset")
print(plot.data)


scatterplot3d(x=X1, y=X2, z=probabilities)
image(x=X1, y=X2, z=probabilities) # a retrouver

## mlr partial prediction plots ----

task = makeClassifTask(data = df, target = "Y")

# Learners
lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)

# Fit log regression and random forest
fit.lr.mlr = train(lrn.classif.lr, task)
fit.rf.mlr = train(lrn.classif.rf, task)


# How to handle the fixed fmin and fmax ?


# Partial prediction plot lr
pd.lr.x1 = generatePartialDependenceData(fit.lr.mlr, task, features = "X1")
pd.lr.x2 = generatePartialDependenceData(fit.lr.mlr, task, features = "X2")
pd.lr.x3 = generatePartialDependenceData(fit.lr.mlr, task, features = "X3")
pd.lr.x4 = generatePartialDependenceData(fit.lr.mlr, task, features = "X4")

plotPartialDependence(pd.lr.x1)
plotPartialDependence(pd.lr.x2)
plotPartialDependence(pd.lr.x3)
plotPartialDependence(pd.lr.x4)

# Partial prediction plot rf
pd.rf.x1 = generatePartialDependenceData(fit.rf.mlr, task, features = "X1")
pd.rf.x2 = generatePartialDependenceData(fit.rf.mlr, task, features = "X2")
pd.rf.x3 = generatePartialDependenceData(fit.rf.mlr, task, features = "X3")
pd.rf.x4 = generatePartialDependenceData(fit.rf.mlr, task, features = "X4")

plotPartialDependence(pd.rf.x1)
plotPartialDependence(pd.rf.x2)
plotPartialDependence(pd.rf.x3)
plotPartialDependence(pd.rf.x4)


# Partial prediction plot with real model

