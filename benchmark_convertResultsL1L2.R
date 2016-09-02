rm(list = ls())
OS = "win"
library(mlr)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(reshape2)
source(file = "benchmark_defs.R")

## Load and convert the reasults to a data frame ----
load( file = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_snow_small-medium-allLearnersFoctor_strat_All.RData")
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")

leaner.id.lr = "classif.logreg"
learner.id.randomForest = "classif.randomForest"
unwantedLearners = c( "classif.cvglmnet..elasticnet", "classif.penalized.ridge", "classif.penalized.lasso")
unwantedMeasures = c("mmce.test.mean")

# remove n>p
res.highdimension = which(clas_used$NumberOfFeatures>clas_used$NumberOfInstances)
result = result[-res.errorMessages]
clas_used = clas_used[-res.errorMessages,]


# remove the ones with error messages
res.errorMessages = which(!sapply(result, function(x) typeof(x)=="list"))
result = result[-res.errorMessages]
clas_used = clas_used[-res.errorMessages,]

# aggregate the results
res.perfs = lapply(result, function(x) getBMRAggrPerformances(x, as.df=TRUE))

# remove unwanted learners
res.perfs = lapply(res.perfs, function(x) subset(x,!(learner.id %in% unwantedLearners)))

# remove unwanted measures
if (length(unwantedMeasures)>0) {
  index.unwantedMeasures = which(names(res.perfs[[1]]) %in% unwantedMeasures)
  res.perfs = lapply(res.perfs, function(x) x[,-index.unwantedMeasures])
}

# detect and removes the nas
res.perfs.nas = which(sapply(res.perfs, function(x) any(is.na(x))))
if (!(identical(integer(0), res.perfs.nas))) {
  res.perfs = res.perfs[-res.perfs.nas]
  clas_used = clas_used[-res.perfs.nas,]
}

# convert to a data.frame
res.perfs.df = do.call("rbind", res.perfs) 

# get the difference of performances
perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)
perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]
perfsAggr.diff.melted = melt(perfsAggr.diff)
detach(package:reshape2, unload = TRUE)

## Compute the dataset of difference
df.bmr.diff = data.frame(perfsAggr.diff,
                         logp = log(clas_used$NumberOfFeatures), 
                         logn = log(clas_used$NumberOfInstances),
                         logdimension = log(clas_used$dimension),
                         logpsurn = log(clas_used$NumberOfFeatures/clas_used$NumberOfInstances),
                         logdimensionsurn = log(clas_used$dimension/clas_used$NumberOfInstances),
                         lograpportMajorityMinorityClass = log(clas_used$MajorityClassSize/clas_used$MinorityClassSize))


paste("Is there any nas ? :", any(is.na(df.bmr.diff)))


measures.names = sapply(MEASURES, function(x) paste0(x$id,".test.mean"))
features.names = names(df.bmr.diff)[which(!(names(df.bmr.diff) %in% measures.names))]

## Compute the ranks
convertModifiedBMRToRankMatrix <- function(bmr.all, measure = NULL, ties.method = "average") {
  
  measure.name = paste(measure$id,".test.mean", sep = "")
  df = aggregate(bmr.all[[measure.name]], by = list(task.id = bmr.all$task.id,
                                                    learner.id = bmr.all$learner.id),
                 FUN = mean)
  
  # calculate ranks, rank according to minimize option of the measure
  if (!measure$minimize)
    df$x = -df$x
  df = plyr::ddply(df, "task.id", function(d) {
    d$alg.rank = rank(d$x, ties.method = ties.method)
    return(d)
  })
  
  # convert into matrix, rows = leaner, cols = tasks
  df = reshape2::melt(df, c("task.id", "learner.id"), "alg.rank")
  df = reshape2::dcast(df, learner.id ~ task.id )
  task.id.names = setdiff(colnames(df), "learner.id")
  mat = as.matrix(df[, task.id.names])
  rownames(mat) = df$learner.id
  colnames(mat) = task.id.names
  return(mat)
}



## Measure correlation ----

correlationPearson = cor(perfsAggr.diff)
correlationSpearman = cor(perfsAggr.diff, method = "spearman")


correlationPearsonabs = 1-abs(correlationPearson)
correlationSpearmanabs = 1-abs(correlationSpearman)

plot(hclust(as.dist(correlationPearsonabs)))
plot(hclust(as.dist(correlationSpearmanabs)))

hclust(correlationPearsonabs)

library(Hmisc)
rcorr(perfsAggr.diff, type=" ") # type can be pearson or spearman


## General vizualisation -----

measure.chosen = acc
matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)

df = reshape2::melt(matrixRanks)
colnames(df) = c("learner.id", "task.id", "rank")

p = ggplot(df, aes_string("rank", fill = "learner.id"))
p = p + geom_bar(position = "dodge")
p = p + ylab("Number")
p = p + ggtitle(paste("mesure :",measure.chosen$id))
print(p)


# plots for the measures
names(perfsAggr.diff.melted)<-c("Measure","Performance")
p <- ggplot(perfsAggr.diff.melted[-which(perfsAggr.diff.melted$Measure %in% c("timetrain.test.mean", "logloss.test.mean", "mmce.test.mean")),], aes(Measure, Performance))
p + geom_boxplot(aes(colour = Measure))

p <- ggplot(perfsAggr.diff.melted[-which(perfsAggr.diff.melted$variable %in% c("timetrain.test.mean", "logloss.test.mean",  "mmce.test.mean")),], aes(variable, value))
p + geom_violin(aes(colour = variable))



# plot of the mean of accuracy rank
measure.chosen = brier
measure.name = measure.chosen$id

matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)
dim = dim(df.bmr.diff)
n = dim[1]

learners.meanrank = apply(matrixRanks, 1,mean)
learners.meanrank = sort(learners.meanrank)
learners.name = names(learners.meanrank)
names(learners.meanrank) <- NULL
learners.meanrank.df = data.frame(learners = factor(learners.name, levels = learners.name), average_rank = learners.meanrank)
learners.meanrank.df = learners.meanrank.df[order(learners.meanrank.df$average_rank),]

print(ggplot(learners.meanrank.df, aes(x = learners, y = average_rank)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian(ylim=c(1,4)) +
        ggtitle(paste0("Comparison of ", measure.name, " of random forest and several logistic regression algorithms")) + ylab(paste("Mean of",measure.name, "rank on", n ,"classification datasets")) + xlab("learner"))



## Influence of parameters ----

## Plots

# histogram of features
hist(df.bmr.diff$logn)
hist(df.bmr.diff$logp)
hist(df.bmr.diff$logdimension)
hist(df.bmr.diff$logpsurn)
hist(df.bmr.diff$logdimensionsurn)
hist(df.bmr.diff$lograpportMajorityMinorityClass)

# plot performance vs parameter of the dataset
plot(df.bmr.diff$logn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logp, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logdimension, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logpsurn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logdimensionsurn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$lograpportMajorityMinorityClass, df.bmr.diff$acc.test.mean)

# with the linear models
plotLinearModelandCor<-function(feature, measure) {
  plot(df.bmr.diff[[feature]], df.bmr.diff[[measure]], xlab = feature, ylab = measure)
  fit =lm(df.bmr.diff[[measure]]~df.bmr.diff[[feature]])
  print(summary(fit))
  x = seq(-100,100,length.out = 20)
  lines(x, x*fit$coefficients[2]+fit$coefficients[1], col = "red")
  
  df.concordance = data.frame(df.bmr.diff[[feature]], df.bmr.diff[[measure]])
  tau = cor(df.concordance, method="kendall", use="pairwise") # kendall
  rho = cor(df.concordance, method="spearman", use="pairwise")# spearmann
  print(paste("tau", tau[1,2]))
  print(paste("rho", rho[1,2]))
}


plotLinearModelandCor("logn","acc.test.mean")
plotLinearModelandCor("logdimension","acc.test.mean")
plotLinearModelandCor("logp","acc.test.mean")
plotLinearModelandCor("logpsurn","acc.test.mean")
plotLinearModelandCor("logdimensionsurn","acc.test.mean")
plotLinearModelandCor("lograpportMajorityMinorityClass","acc.test.mean")



# Importance of the variables

# With linear regression model
fit.all = lm(df.bmr.diff$acc.test.mean~
               df.bmr.diff$logp+
               df.bmr.diff$logn+
               df.bmr.diff$logdimension+
               df.bmr.diff$logpsurn+
               df.bmr.diff$logdimensionsurn+
               df.bmr.diff$lograpportMajorityMinorityClass)

summary(fit.all)

# with a random forest
measure.chosen = "acc.test.mean"
df.regr = data.frame(subset(df.bmr.diff, select = measure.chosen), subset(df.bmr.diff, select = features.names))
task.regr = makeRegrTask(data = df.regr, target = measure.chosen)
fv = generateFilterValuesData(task.regr, method = "randomForestSRC.rfsrc")
fv

## Anne Laure visualization

# with ggplot
boxplot.threshold.ggplot <- function(df, measure, feature, threshold) {
  names = names(df)
  
  if (!(measure %in% names) | !(feature %in% names) ) {
    print("Error, measure or feature was not found")
  }
  
  thresh = df[[feature]]>threshold
  df$thresh = thresh
  p <- ggplot(df, aes_string("thresh", "acc.test.mean"))
  p = p + geom_boxplot(aes_string(fill = "thresh"))
  return(p)
}

feature.boxplot <- function(df, measure, feature, threshold.vect) {
  n = length(threshold.vect)
  
  v = lapply(threshold.vect, function(x) boxplot.threshold.ggplot(df, measure, feature, x))
  labels = sapply(threshold.vect, function(x) return(paste(">",x, sep = "")))
  plot_grid(plotlist = v,  labels=labels, ncol = 3, nrow = 1)
}



measure.chosen = "acc.test.mean"
measure.chosen = "auc.test.mean"

# logp
feature.chosen = "logp"
feature.boxplot(df.bmr.diff, measure.chosen, feature.chosen,c(1.5,2.5,3.5))

# logn
feature.chosen = "logn"
feature.boxplot(df.bmr.diff, measure.chosen, feature.chosen,c(4.5,5.5,6.5))

#logdimension
feature.chosen = "logdimension"
feature.boxplot(df.bmr.diff, measure.chosen, feature.chosen,c(1.5,2.5,3.5))

#logdimension
feature.chosen = "logpsurn"
feature.boxplot(df.bmr.diff, measure.chosen, feature.chosen,c(-4,-3,-2))

#logdimension
feature.chosen = "logdimensionsurn"
feature.boxplot(df.bmr.diff, measure.chosen, feature.chosen,c(-5,-4,-2))

#logMajorityClass MinorityClass
feature.chosen = "lograpportMajorityMinorityClass"
feature.boxplot(df.bmr.diff, measure.chosen, feature.chosen,c(0.2,0.6,1,2))




## Partial dependance plots analysis

## Classification

measure.chosen = acc
matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)

lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob")


df.classif = data.frame(rankrf = matrixRanks[2,], subset(df.bmr.diff, select = features.names))
index.egalite = which(df.classif$rankrf==1.5)
df.classif = df.classif[-index.egalite,]
df.classif$rankrf = as.factor(df.classif$rankrf)

task.classif = makeClassifTask(data = df.classif, target = "rankrf")
task.classif$env$data$logn = as.numeric(task.classif$env$data$logn)

fit.classif.rf = train(lrn.classif.rf, task.classif)

# 1D partial dependance plots
pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, "logn")
plotPartialDependence(pd.classif)

pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, "logp")
plotPartialDependence(pd.classif)

pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, "logdimension")
plotPartialDependence(pd.classif)

pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, "logpsurn")
plotPartialDependence(pd.classif)

pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, "logdimensionsurn")
plotPartialDependence(pd.classif)

pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, "lograpportMajorityMinorityClass")
plotPartialDependence(pd.classif)


# 2D Partial dependance plots
pd.classif = generatePartialDependenceData(fit.classif.rf, task.classif, c("logn", "logp"), interaction = TRUE)
plotPartialDependence(pd.classif, geom = "tile")



## regression
measure.chosen = "acc.test.mean"
task.regr = makeRegrTask(data = df.regr, target = measure.chosen)
lrn.regr = makeLearner("regr.randomForest")
task.regr$env$data$logn=as.numeric(task.regr$env$data$logn)
fit.regr.rf = train(lrn.regr, task.regr)

# 1D partial dependance plots
pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("logn"), fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialDependence(pd.regr.rf)

pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("logp"),fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialDependence(pd.regr.rf)

pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("logdimension"), fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialDependence(pd.regr.rf)

pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("logpsurn"),fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialDependence(pd.regr.rf)

pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("logdimensionsurn"),fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialDependence(pd.regr.rf)

pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("lograpportMajorityMinorityClass"),fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialDependence(pd.regr.rf)



# 2D Partial dependance plots
pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.regr,features = c("logdimensionsurn", "lograpportMajorityMinorityClass"), interaction = TRUE)
plotPartialDependence(pd.regr.rf, geom = "tile")
