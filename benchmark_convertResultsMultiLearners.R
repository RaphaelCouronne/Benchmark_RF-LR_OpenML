rm(list = ls())
OS = "win"
library(mlr)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(reshape2)
source(file = "benchmark_defs.R")



################################################################################################################################
# Part 1 : Creation of the dataset
################################################################################################################################

## Load and convert the reasults to a data frame ----
load( file = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_snow_small-medium-allLearnersFoctor_strat_All.RData")


load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")

leaner.id.lr = "classif.logreg"
learner.id.randomForest = "classif.randomForest"
unwantedLearners = c( "classif.cvglmnet..elasticnet", "classif.penalized.ridge", "classif.penalized.lasso",
                      "classif.multinom", "classif.cvglmnet.ridge", "classif.cvglmnet.lasso.vanilla", "classif.cvglmnet.lasso")
unwantedMeasures = c("mmce.test.mean")

# models difference
load(file = "../Data_BenchmarkOpenMl/Final/Interpretability/ImportanceResults-all.RData")
load(file = "../Data_BenchmarkOpenMl/Final/pdp.weigheddifferenceAll.RData")
clas_used = clas_used[c(1:length(result)),]

# create the importance.list
importance.list.aggr.l1 = sapply(importance.list, function(x) sum(abs(x$rf.permutation.imp-x$lr.permutation.imp))/length(x$rf.permutation.imp))
importance.list.aggr.l2 = sapply(importance.list, function(x) sqrt(sum((x$rf.permutation.imp-x$lr.permutation.imp)^2))/length(x$rf.permutation.imp))
importance.list.aggr.rank = sapply(importance.list, function(x) sum(abs(rank(x$rf.permutation.imp)-rank(x$lr.permutation.imp)))/length(x$rf.permutation.imp))

importance.df = data.frame(l1 = importance.list.aggr.l1,
                           l2 = importance.list.aggr.l2, 
                           rank = importance.list.aggr.rank)


# remove error message for pdp
res.errorMessages.pdp = which(!sapply(pdp.weigheddifference, function(x) typeof(x)=="list"))
result = result[-res.errorMessages.pdp]
clas_used = clas_used[-res.errorMessages.pdp,]
importance.df = importance.df[-res.errorMessages.pdp,]
pdp.weigheddifference = pdp.weigheddifference[-res.errorMessages.pdp]

# pdp.df
pdp.df = do.call("rbind", pdp.weigheddifference) 


# remove n>p
res.highdimension = which(clas_used$NumberOfFeatures>clas_used$NumberOfInstances)
result = result[-res.highdimension]
clas_used = clas_used[-res.highdimension,]
importance.df = importance.df[-res.highdimension,]
pdp.df = pdp.df[-res.highdimension,]


# remove the ones with error messages
res.errorMessages = which(!sapply(result, function(x) typeof(x)=="list"))
result = result[-res.errorMessages]
clas_used = clas_used[-res.errorMessages,]
importance.df = importance.df[-res.errorMessages,]
pdp.df = pdp.df[-res.errorMessages,]

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
  importance.df = importance.df[-res.perfs.nas,]
  pdp.df = pdp.df[-res.perfs.nas,]
}

# convert to a data.frame
res.perfs.df = do.call("rbind", res.perfs) 

# get the difference of performances
perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)
perfsAggr.diff = perfsAggr.RF[,3:ncol(perfsAggr.RF)]-perfsAggr.LR[,3:ncol(perfsAggr.LR)]
perfsAggr.diff.melted = melt(perfsAggr.diff)
detach(package:reshape2, unload = TRUE)



## Compute the dataset of difference with the features

# number  of features
p = clas_used$NumberOfFeatures

# number of numeric attributes
pnum = clas_used$NumberOfNumericFeatures

# number of categorical attributes
psymbolic = clas_used$NumberOfSymbolicFeatures

# number of samples
n = clas_used$NumberOfInstances

# n/p
psurn = p/n

# Numerical attributes rate
pnumrate = pnum/p

# Nominal attributes rate
psymbolicrate = psymbolic/p

# %Cmin Percentage of elements of the minority class
Cmin = clas_used$MinorityClassSize/n

# %Cmax Percentage of elements of the majority class
Cmax = clas_used$MinorityClassSize/n



df.bmr.diff = data.frame(perfsAggr.diff,
                         logp = log(clas_used$NumberOfFeatures), 
                         logn = log(clas_used$NumberOfInstances),
                         logdimension = log(clas_used$dimension),
                         logpsurn = log(clas_used$NumberOfFeatures/clas_used$NumberOfInstances),
                         logdimensionsurn = log(clas_used$dimension/clas_used$NumberOfInstances),
                         lograpportMajorityMinorityClass = log(clas_used$MajorityClassSize/clas_used$MinorityClassSize),
                         pnum, psymbolic, pnumrate, psymbolicrate, Cmin, Cmax,
                         brierlogreg = perfsAggr.LR$brier.test.mean,
                         logbrierlogreg = log(perfsAggr.LR$brier.test.mean),
                         sqrtbrierlogreg = sqrt(perfsAggr.LR$brier.test.mean),
                         acclogreg = perfsAggr.LR$acc.test.mean,
                         auclogreg = perfsAggr.LR$auc.test.mean
                         )


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

pairs(df.bmr.diff[,c(7:12)])

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










################################################################################################################
## Part 2 : General vizualisation -----
################################################################################################################


measure.chosen = acc
matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)


df = reshape2::melt(matrixRanks)
colnames(df) = c("learner.id", "task.id", "rank")

p = ggplot(df, aes_string("rank", fill = "learner.id"))
p = p + geom_bar(position = "dodge")
p = p + ylab("Number")
p = p + ggtitle(paste("mesure :",measure.chosen$id))
print(p)

values = sapply(c(1:nrow(matrixRanks)), function(x) mean(matrixRanks[x,]))
row.names(matrixRanks)

test = matrixRanks[,1]

rank.shape = function(x) {
  df = NA
  if (x[1]==2) {
    df=data.frame(rank = as.factor(1), learner = "RF")
  } else if (x[1]==1.5) {
    df=data.frame(rank = as.factor(c("Equal performance","Equal performance")), learner = c("RF","LR"))
  } else {
    df=data.frame(rank = 1, learner = "LR")
  }
  return(df)
}

list.shape = lapply(matrixRanks[1,], rank.shape)
list.shape.df = do.call("rbind", list.shape) 
names(list.shape.df)[2] = "Method"

p = ggplot(list.shape.df, aes_string("rank", fill = "Method"))
p = p + geom_bar(position = "dodge")
p = p + ylab("Number")
#p = p + ggtitle(paste("mesure :",measure.chosen$id))
print(p)



# plots for the measures
names(perfsAggr.diff.melted)<-c("Measure","Performance")
p <- ggplot(perfsAggr.diff.melted[-which(perfsAggr.diff.melted$Measure %in% c("timetrain.test.mean", "logloss.test.mean", "mmce.test.mean")),], aes(Measure, Performance))
p + geom_boxplot(aes(colour = Measure))

p <- ggplot(perfsAggr.diff.melted[-which(perfsAggr.diff.melted$Measure %in% c("timetrain.test.mean", "logloss.test.mean", "mmce.test.mean")),], aes(Measure, Performance))
p + geom_violin(aes(colour = Measure))



# plot of the mean of accuracy rank

# for one measure
measure.chosen = logloss
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
learners.meanrank.df


print(ggplot(learners.meanrank.df, aes(x = learners, y = average_rank)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian(ylim=c(1,4)) +
        ggtitle(paste0("Comparison of ", measure.name, " of random forest and several logistic regression algorithms")) + ylab(paste("Mean of",measure.name, "rank on", n ,"classification datasets")) + xlab("learner"))


# compute the matrix of the means of ranks
measures.list = list(acc, auc, brier, ber, logloss, timetrain)

getmatrixRanks <- function(res.perfs.df, measure) {
  matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure)
  learners.meanrank = apply(matrixRanks, 1,mean)
  return(learners.meanrank)
}


df.ranks = data.frame(sapply(measures.list, function(x) getmatrixRanks(res.perfs.df,x)))
names(df.ranks) = sapply(measures.list, function(x) x$id)


# compute matrix with mean on perfs
res.perfs.df

learner.list = levels(res.perfs.df$learner.id)[-c(3,4,8)]



getmatrixPerfs = function(res.perfs.df, learner) {
  a = subset(res.perfs.df, learner.id == learner)
  res = apply(a[,c(3:dim(a)[2])],2,mean)
  return(res)
}

df.values = sapply(learner.list, function(x) getmatrixPerfs(res.perfs.df, x))
df.values = t(df.values)
colnames(df.values) = c("acc", "ber", "brier", "timetrain", "auc", "logloss")
df.values = df.values[,c(1,5,3,2,6,4)]
df.values

# get the difference of performances
perfsAggr.LR = subset(res.perfs.df, learner.id == leaner.id.lr)
perfsAggr.RF = subset(res.perfs.df, learner.id == learner.id.randomForest)


# Plot for log reg and rf 
# boxplots
lr.acc = perfsAggr.LR$acc.test.mean
rf.acc = perfsAggr.RF$acc.test.mean
df.acc = data.frame(lr.acc = lr.acc, rf.acc = rf.acc)
names(df.acc) = c("LR", "RF")
df.acc.melted = reshape2::melt(df.acc)
diff.acc = perfsAggr.diff$acc.test.mean
names(df.acc.melted) = c("Method", "Accuracy")


p <- ggplot(df.acc.melted, aes(Method, Accuracy))
p = p +  scale_fill_manual(values=c("#990000", "#99CCFF"))
p = p + geom_boxplot(aes_string(fill = "Method"), outlier.shape = NA, notch = FALSE)
p = p + labs(y = "acc")
print(p)



diff.acc = perfsAggr.diff$acc.test.mean
p <- ggplot(perfsAggr.diff, aes( brier.test.mean, acc.test.mean))
p = p + geom_boxplot(aes_string(fill = "acc.test.mean"), outlier.shape = NA, notch = FALSE)
p = p + scale_fill_manual(values=c("#CC6666"))
p = p + labs(y = (expression(paste(Delta, "acc"))))
p = p + ylim(c(-0.10,0.08))
print(p)

p<-geom_boxplot(diff.acc)

# avec le boxplot normal
boxplot(diff.acc, outline = FALSE, ylab =  expression(paste(Delta, "acc")),  col="#009900", notch = TRUE)
lines(x =c(0.5,1.5), y=c(0,0), col="red")

boxplot(Accuracy~Method, data = df.acc.melted, outline = FALSE, ylab =  expression(paste("acc")),  col=c("#99CCFF", "#990000"), notch = TRUE)

library(vioplot)

vioplot(diff.acc, outline = FALSE, ylab =  expression(paste(Delta, "acc")),  col="#009900", notch = TRUE)
lines(x =c(0.5,1.5), y=c(0,0), col="red")

vioplot(diff.acc)


df.dummy = data.frame(diff.acc,1)
p <- ggplot(df.dummy, aes(X1,diff.acc))
p = p + geom_boxplot()
print(p)

qplot(y=diff.acc, x= 1, geom = "boxplot")

################################################################################################################################
# Part 3 : Analysis
################################################################################################################################

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

# plot with the permutation measure
plot(log(importance.df$l1), df.bmr.diff$acc.test.mean, xlim = c(-10,0))
plot(log(importance.df$l2), df.bmr.diff$acc.test.mean, xlim = c(-10,0))
plot(log(importance.df$rank), df.bmr.diff$acc.test.mean)

x = (importance.df$l2)+1e-6
y = df.bmr.diff$acc.test.mean

index.x.remove = which(x<(1e-3))
x = x[-index.x.remove]
y = y[-index.x.remove]

plot(x,y, ylab = "Difference in accuracy", log = "x", xlab = "Difference of variable importance")

cor.test(x,y,method = "kendall")
cor.test(x,y,method = "spearman")


# plot with the pdp
hist(pdp.df$l2)
hist(log(pdp.df$l2))
hist(log1p(log1p(pdp.df$l2)))

plot(log(pdp.df$l1), df.bmr.diff$acc.test.mean, xlab = "Log difference in partial dependance", ylab = "Difference in accuracy")
cor.test(pdp.df$l1,df.bmr.diff$acc.test.mean,method = "kendall")

plot(log(pdp.df$l2), df.bmr.diff$acc.test.mean, xlab = "Log difference in partial dependance", ylab = "Difference in accuracy")
cor.test(pdp.df$l2,df.bmr.diff$acc.test.mean,method = "kendall")

plot(pdp.df$linf, df.bmr.diff$acc.test.mean)
cor.test(pdp.df$linf,df.bmr.diff$acc.test.mean,method = "kendall")


# study variance pdp
plot(log(pdp.df$l2), df.bmr.diff$acc.test.mean, 
     xlab = "Log difference in partial dependance", ylab = "Difference in accuracy",
     xlim = c(-6,-1))

x.considered = log(pdp.df$l2)

subsetl2 <- function(x) {
  res = NA
  if (x<(-4)) {
    res = "<-4"
  } else if (x>=(-4) && x<(-3)) {
    res = ">-4 & <-3"
  } else if (x>=(-3) && x<(-2)) {
    res = ">-3 & <-2"
  } else if (x>=(-2)) {
    res = ">-2"
  }
  return(res)
}


hist(log(pdp.df$l2))
l2.categories = sapply(x.considered, subsetl2)

df.l2test = data.frame(l2.categories = l2.categories, x.considered = x.considered, acc.test.mean = df.bmr.diff$acc.test.mean)
levels(df.l2test$l2.categories) = levels(df.l2test$l2.categories)[c(1,4,3,2)]
p <- ggplot(df.l2test, aes(factor(l2.categories), acc.test.mean))
p = p+geom_boxplot()
print(p)





# plot the p with colors
df.plot = df.bmr.diff
logical = df.plot$acc.test.mean>0
df.plot$positive = sapply(logical, function(x) if(x) {("Positive")} else {"Negative"})
p <- ggplot(data = df.plot, aes(x = exp(logp), y = acc.test.mean, colour = df.bmr.diff$logn))
p <- p + geom_point(size = 1)
p = p + labs(x = "r (logaritmic scale)", y = "Difference in acc")
p = p + labs(colour = "n")
p = p + scale_x_log10()
p = p + scale_colour_gradient(limits=c(3, 11), low="white", high="red")
print(p)

# with categories
subsetn <- function(x) {
  res = NA
  if (x<50) {
    res = 1
  } else if (x>=50 && x<100) {
    res = 2
  } else if (x>=100 && x<1000) {
    res = 3
  } else if (x>=1000 && x<10000) {
    res = 4
  } else if (x>=10000) {
    res = 5
  }
  return(res)
}

df.bmr.diff.subsetn = df.bmr.diff
df.bmr.diff.subsetn$subsetn = sapply(exp(df.bmr.diff$logn), subsetn)

df.plot = df.bmr.diff.subsetn
p <- ggplot(data = df.plot, aes(x = exp(logp), y = acc.test.mean, colour = df.bmr.diff.subsetn$subsetn))
p <- p + geom_point(size = 1)
p = p + labs(x = "r (logaritmic scale)", y = "Difference in acc")
p = p + labs(colour = "n")
p = p + scale_x_log10()
p = p + scale_colour_gradient(limits=c(1, 5), low="red", high="white")
print(p)



# 3D Plot
library(scatterplot3d)
scatterplot3d(df.bmr.diff$logn, df.bmr.diff$logp, df.bmr.diff$acc.test.mean, type = "h")

library(plot3D)
scatter3D(df.bmr.diff$logn, df.bmr.diff$logp, df.bmr.diff$acc.test.mean)

library(car)
scatter3d(df.bmr.diff$logn, df.bmr.diff$logp, df.bmr.diff$acc.test.mean)
scatter3d(x = df.bmr.diff$logn, y = df.bmr.diff$logp, z = df.bmr.diff$acc.test.mean)

library(plotly)
plot_ly(df, x = df.bmr.diff$logn, y = df.bmr.diff$logp, z = df.bmr.diff$acc.test.mean)


df.matlab = data.frame (x = df.bmr.diff$logn, y= df.bmr.diff$logp, z = df.bmr.diff$acc.test.mean)
save(df.matlab, file = "df.matlab.RData")
write.table(df.matlab, file = "df.matlab.csv")

# splines
#splines = smooth.spline(x = x, y = y)

x = df.bmr.diff$logpsurn
y = df.bmr.diff$acc.test.mean
lo <- loess(formula = y~x)
plot(x,y)
lines(predict(lo), col='red', lwd=1)

smoothingSpline = smooth.spline(x, y, spar=0.9)
plot(x,y, xlab = "log(dimension/n)", ylab = "Difference in acc : lasso - lr")
lines(smoothingSpline, col = "red")


scatter.smooth(x,y)

require(graphics)
plot(dist ~ speed, data = cars, main = "data(cars)  &  smoothing splines")
cars.spl <- with(cars, smooth.spline(speed, dist))
cars.spl


## Analysis

# with the linear models
plotLinearModelandCor<-function(feature, measure, log = FALSE) {
  plot(df.bmr.diff[[feature]], df.bmr.diff[[measure]], xlab = feature, ylab = measure)
  fit =lm(df.bmr.diff[[measure]]~df.bmr.diff[[feature]])
  
  
  if (log == TRUE) {
    fit =lm(df.bmr.diff[[measure]]~log1p(df.bmr.diff[[feature]]))
    plot(log1p(df.bmr.diff[[feature]]), df.bmr.diff[[measure]], xlab = feature, ylab = measure)
  }
  
  print(summary(fit))
  x = seq(-100,100,length.out = 20)
  lines(x, x*fit$coefficients[2]+fit$coefficients[1], col = "red")
  
  df.concordance = data.frame(df.bmr.diff[[feature]], df.bmr.diff[[measure]])
  tau = cor(df.concordance, method="kendall", use="pairwise") # kendall
  rho = cor(df.concordance, method="spearman", use="pairwise")# spearmann
  print(paste("tau", tau[1,2]))
  print(paste("rho", rho[1,2]))
  
  a = cor.test(x = df.concordance[,1], y = df.concordance[,2], method="kendall", use="pairwise")
  b = cor.test(x = df.concordance[,1], y = df.concordance[,2], method="spearman", use="pairwise")
  
  print(a)
  print(b)
}



plotLinearModelandCor("logn","acc.test.mean")
plotLinearModelandCor("logdimension","acc.test.mean")
plotLinearModelandCor("logp","acc.test.mean")
plotLinearModelandCor("logpsurn","acc.test.mean")
plotLinearModelandCor("logdimensionsurn","acc.test.mean")
plotLinearModelandCor("lograpportMajorityMinorityClass","acc.test.mean")
plotLinearModelandCor("Cmax","acc.test.mean")
plotLinearModelandCor("brierlogreg","acc.test.mean")
plotLinearModelandCor("sqrtbrierlogreg","acc.test.mean")
plotLinearModelandCor("auclogreg","acc.test.mean")


plotLinearModelandCor("pnum","acc.test.mean", log = TRUE)
plotLinearModelandCor("psymbolic","acc.test.mean")
plotLinearModelandCor("pnumrate","acc.test.mean")
plotLinearModelandCor("psymbolicrate","acc.test.mean")
plotLinearModelandCor("Cmin","acc.test.mean")
plotLinearModelandCor("Cmax","acc.test.mean")
plotLinearModelandCor("brierlogreg","acc.test.mean")
plotLinearModelandCor("logbrierlogreg","acc.test.mean")
plotLinearModelandCor("sqrtbrierlogreg","acc.test.mean")


## ML Analysis

task.analysis = 

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



#### Boxplots and all

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
  p = p + geom_boxplot(aes_string(fill = "thresh"), outlier.shape = NA, notch = FALSE)
  p = p + scale_y_continuous(limits = c(-0.08, 0.15))
  return(p)
}

feature.boxplot <- function(df, measure, feature, threshold.vect) {
  n = length(threshold.vect)
  
  v = lapply(threshold.vect, function(x) boxplot.threshold.ggplot(df, measure, feature, x))
  labels = sapply(threshold.vect, function(x) return(paste(">",x, sep = "")))
  plot_grid(plotlist = v,  labels=labels, ncol = 3, nrow = 1)
}


# With one frame ggplot

feature = "logp"
thresholdvect = c(2,3)

feature.boxplot.oneplot <- function(df, measure, feature, thresholdvect) {
  
  df.all = data.frame()
  
  for (i in c(1:length(thresholdvect))) {
    df.temp = df
    threshold = thresholdvect[i]
    logthreshold = log(threshold)
    df.temp$thresholdValue = threshold
    df.temp$logical.threshold = df[[feature]]>logthreshold
    
    df.all = rbind(df.all, df.temp) 
  }
  df.all$thresholdValue = as.factor(df.all$thresholdValue)
  
  p <- ggplot(df.all, aes_string("thresholdValue", measure))
  print(p)
  p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = FALSE)
  p = p + scale_y_continuous(limits = c(-0.08, 0.2))
  p = p + labs(x = "Value of the threshold", y = "Difference in acc ")
  p = p + labs(fill = "Above threshold")
  print(p)
}

measure.chosen = "acc.test.mean"
feature.chosen = "logpsurn"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(2e-3,1e-2,1e-1))


measure.chosen = "acc.test.mean"
feature.chosen = "logn"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(50,500,1000))

measure.chosen = "acc.test.mean"
feature.chosen = "lograpportMajorityMinorityClass"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(2,3,5))


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
