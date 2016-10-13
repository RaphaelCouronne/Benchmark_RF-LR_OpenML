#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_graphicsdoc.htm#inter
# http://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-016-0995-8

X = iris[,c(1:4)]
Y = iris[,5]

# with random forest
library(randomForestSRC)

iris.obj <- rfsrc(Species ~., data = iris, importance = TRUE)
find.interaction(iris.obj, method = "vimp", nrep = 3)
find.interaction(iris.obj)


# with GBM
library(dismo)
angaus.tc5.lr005 <- gbm.step(data=Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                             + family = "bernoulli", tree.complexity = 5,
                             + learning.rate = 0.005, bag.fraction = 0.5)

