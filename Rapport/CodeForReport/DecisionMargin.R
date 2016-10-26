rm(list = ls())


# from http://www.overkillanalytics.net/more-is-always-better-the-power-of-simple-ensembles/

## Test Visualization : Projection on 2D subspace ----

require(fields) # for heatmap plot with legend
require(randomForest) # requires installation, for random forest models
set.seed(20120926)
nObs = 1e4

# heatmap wrapper, plotting func(x, y) over range a by a
hmap.func <- function(a, f, xlab, ylab) 
{
  image.plot(a, a, outer(a, a, f), zlim = c(0, 1), xlab = xlab, ylab = ylab)
}


# define probability distribution
g <- function(x, y) 1 / (1 + exp(x^3+ y+x*y))
g <- function(x, y) 1 / (1 + exp(y^3+ x^2+ y+x*y))
g <- function(x, y) 1 / (1 + exp(y^2+ 2*x+ x*y))

# create training data
d <- data.frame(x1 = rnorm(nObs), x2 = rnorm(nObs)
                ,x3 = rnorm(nObs), x4 = rnorm(nObs))
d$y = with(d, ifelse(runif(nObs) < g(x1, x2), 1, 0))

# plot probability distribution 
a = seq(-2, 2, len = 100)
hmap.func(a, g, "x1", "x2")

# plot training data representation (right hand plot below)
z = tapply(d$y,  list(cut(d$x1, breaks = seq(-2, 2, len=25))
                      , cut(d$x2, breaks = seq(-2, 2, len=25))), mean)
image.plot(seq(-2, 2, len=25), seq(-2, 2, len=25), z, zlim = c(0, 1)
           , xlab = "x1", ylab = "x2")


# Fit log regression and random forest
fit.lr = glm(y~x1+x2+x3+x4, family = binomial, data = d)
fit.rf = randomForest(as.factor(y)~x1+x2+x3+x4, data = d, ntree = 20)

# Create funtions in x1, x2 to give model predictions
# while setting x3, x4 at origin
g.lr.sig = function(x, y) predict(fit.lr, data.frame(x1 = x, x2 = y, x3 = 0, x4 = 0), type = "response") 
g.rf.sig = function(x, y) predict(fit.rf, data.frame(x1 = x, x2 = y, x3 = 0, x4 = 0), type = "prob")[, 2] 

# Map model predictions in x1 and x2
hmap.func(a, g.lr.sig, "x1", "x2")
hmap.func(a, g.rf.sig, "x1", "x2")




## Test Visualization : Partial dependance plot with mlr ----


# Datas in a mlr task
d$y = as.factor(1-d$y)
task = makeClassifTask(data = d, target = "y")

# Learners
lrn.classif.lr = makeLearner("classif.multinom", predict.type = "prob", fix.factors.prediction = TRUE)
lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)

# Fit log regression and random forest
fit.lr.mlr = train(lrn.classif.lr, task)
fit.rf.mlr = train(lrn.classif.rf, task)

# Prediction
predict.lr.mlr = predict(fit.lr.mlr, task)
predict.rf.mlr = predict(fit.rf.mlr, task)

## Partial Prediction Data 1D
fmin = as.list(qnorm(0.05))
fmax = as.list(qnorm(0.95))

# plot of all the space
hmap.func(a, g, "x1", "x2")
hmap.func(a, g.lr.sig, "x1", "x2")
hmap.func(a, g.rf.sig, "x1", "x2")

# plot the learn set
plot <- ggplot(data=d, aes(x=x1, y=x2, colour=y))
plot <- plot + geom_point(size=2) # also geom_points
print(plot)


# Plot the model applied on learn set
plot <- ggplot(data=d, aes(x=x1, y=x2, colour=predict.lr.mlr$data$prob.0))
plot <- plot + geom_point(size=1) 
plot <- plot + scale_colour_gradient(high="grey10", low="grey90") + theme_bw()
print(plot)
plot <- plot + xlim(c(-2,2)) + ylim(c(-2,2))
print(plot)

plot <- ggplot(data=d, aes(x=x1, y=x2, colour=predict.rf.mlr$data$prob.0))
plot <- plot + geom_point(size=1) 
plot <- plot + scale_colour_gradient(high="grey10", low="grey90") + theme_bw()
print(plot)
plot <- plot + xlim(c(-2,2)) + ylim(c(-2,2))
print(plot)



# Partial prediction plot lr
pd.lr.x1 = generatePartialPredictionData(fit.lr.mlr, task, features = "x1", fmin = fmin, fmax = fmax)
pd.lr.x2 = generatePartialPredictionData(fit.lr.mlr, task, features = "x2", fmin = fmin, fmax = fmax)
pd.lr.x3 = generatePartialPredictionData(fit.lr.mlr, task, features = "x3", fmin = fmin, fmax = fmax)
pd.lr.x4 = generatePartialPredictionData(fit.lr.mlr, task, features = "x4", fmin = fmin, fmax = fmax)

plotPartialPrediction(pd.lr.x1)
plotPartialPrediction(pd.lr.x2)
plotPartialPrediction(pd.lr.x3)
plotPartialPrediction(pd.lr.x4)


# Partial prediction plot rf
pd.rf.x1 = generatePartialPredictionData(fit.rf.mlr, task, features = "x1", fmin = fmin, fmax = fmax)
pd.rf.x2 = generatePartialPredictionData(fit.rf.mlr, task, features = "x2", fmin = fmin, fmax = fmax)
pd.rf.x3 = generatePartialPredictionData(fit.rf.mlr, task, features = "x3", fmin = fmin, fmax = fmax)
pd.rf.x4 = generatePartialPredictionData(fit.rf.mlr, task, features = "x4", fmin = fmin, fmax = fmax)

plotPartialPrediction(pd.rf.x1)
plotPartialPrediction(pd.rf.x2)
plotPartialPrediction(pd.rf.x3)
plotPartialPrediction(pd.rf.x4)



# Partial Prediction Data 2D
fmin = as.list(qnorm(0.05))
fmax = as.list(qnorm(0.95))
pd.lr.x1 = generatePartialPredictionData(fit.lr.mlr, task, features = "x1", fmin = fmin, fmax = fmax)
pd.lr.x2 = generatePartialPredictionData(fit.lr.mlr, task, "x2", fmin = fmin, fmax = fmax)




