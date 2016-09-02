
library(mlr)
library(ggplot2)
library(scatterplot3d)
library(plot3D)
library(fields)
library(randomForest)

set.seed(1)

## Generating the data ----
n=5e2


X1<-runif(n, min = -2.2, max= 2.2)
X2<-runif(n, min = -2.2, max= 2.2)


X3<-runif(n, min = -2, max= 2) #dummy 

product=2+X1^3-2*X2-2*X1*X2+X1

probabilities =  plogis(product)

Y.numeric = rbinom(n,1,prob = probabilities)
Y<-as.factor(Y.numeric)
df<-data.frame(X1,X2,X3,Y)


# Visualization of the datas


# plot the probability distribution
hmap.func <- function(a, f, xlab, ylab) 
{
  image.plot(a, a, outer(a, a, f), zlim = c(0, 1), xlab = xlab, ylab = ylab)
}


f <- function(X1,X2) {
  product=2+X1^3-2*X2-2*X1*X2+X1
  probabilities =  plogis(product)
  return(probabilities)
}

a = seq(-2, 2, len = 100)
hmap.func(a, f, "x1", "x2")

# plot the raw data
plot.data <- ggplot(data=df, aes(x=X1, y=X2, colour=Y))
plot.data <- plot.data + geom_point(size=3) # also geom_points
plot.data <- plot.data + ggtitle("Simulated Dataset")
print(plot.data)




# plot training data representation (right hand plot below)

z = tapply(Y.numeric,  list(cut(df$X1, breaks = seq(-2, 2, len=25))
                       , cut(df$X2, breaks = seq(-2, 2, len=25))), mean)

image.plot(seq(-2, 2, len=25), seq(-2, 2, len=25), z, zlim = c(0, 1)
           , xlab = "X1", ylab = "X2")




## Visualization of decision margin ----
# Fit log regression and random forest
fit.lr = glm(Y~., family = binomial, data = df)
fit.rf = randomForest(as.factor(Y)~., data = df, ntree = 20)

# Create funtions in x1, x2 to give model predictions
# while setting x3, x4 at origin
g.lr.sig = function(x, y) predict(fit.lr, data.frame(X1 = x, X2 = y, X3 = 0), type = "response") 
g.rf.sig = function(x, y) predict(fit.rf, data.frame(X1 = x, X2 = y, X3 = 0), type = "prob")[, 2] 

# Map model predictions in x1 and x2
hmap.func(a, g.lr.sig, "X1", "X2")
hmap.func(a, g.rf.sig, "X1", "X2")


## mlr partial prediction plots ----

task = makeClassifTask(data = df, target = "Y")

# Learners
lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE, ntree = 50)
fit.rf.mlr = train(lrn.classif.rf, task)


# How to handle the fixed fmin and fmax ?

# Partial prediction plot rf individual
pd.rf.x1 = generatePartialDependenceData(fit.rf.mlr, task, features = "X1", individual = TRUE)
pd.rf.x2 = generatePartialDependenceData(fit.rf.mlr, task, features = "X2", individual = TRUE)
pd.rf.x3 = generatePartialDependenceData(fit.rf.mlr, task, features = "X3", individual = TRUE)

plotPartialDependence(pd.rf.x1)
plotPartialDependence(pd.rf.x2)
plotPartialDependence(pd.rf.x3)



# Partial prediction plot rf not individual
pd.rf.x1 = generatePartialDependenceData(fit.rf.mlr, task, features = "X1")
pd.rf.x2 = generatePartialDependenceData(fit.rf.mlr, task, features = "X2")
pd.rf.x3 = generatePartialDependenceData(fit.rf.mlr, task, features = "X3")



df.plot = data.frame(grid = pd.rf.x1$data$X1, 
                     X1 = pd.rf.x1$data$Probability,
                     X2 = pd.rf.x2$data$Probability,
                     X3 = pd.rf.x3$data$Probability)

library(reshape2)
df.plot.reshaped = reshape2::melt(df.plot, "grid")
detach(package:reshape2, unload = TRUE)
p = ggplot(df.plot.reshaped, aes_string(x = "grid", y="value", colour = "variable"))
p = p+geom_line(size=1) + geom_point(size=3)
print(p)
# Partial prediction plot with real model




## for LR

lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)


# Fit log regression and random forest
fit.lr.mlr = train(lrn.classif.lr, task)



# Partial prediction plot lr
pd.lr.x1 = generatePartialDependenceData(fit.lr.mlr, task, features = "X1")
pd.lr.x2 = generatePartialDependenceData(fit.lr.mlr, task, features = "X2")
pd.lr.x3 = generatePartialDependenceData(fit.lr.mlr, task, features = "X3")

plotPartialDependence(pd.lr.x1)
plotPartialDependence(pd.lr.x2)
plotPartialDependence(pd.lr.x3)
plotPartialDependence(pd.lr.x4)

