rm(list = ls())
load(file = "Data/Results/df.bmr.RData")
library(mlr)




################################################################################################################################
# Meta-Analysis
################################################################################################################################

## Influence of parameters ----


## Plots

# histogram of features
par(mfrow=c(2,4))
hist(df.bmr.diff$logn)
hist(df.bmr.diff$logp)
hist(df.bmr.diff$logdimension)
hist(df.bmr.diff$logpsurn)
hist(df.bmr.diff$logdimensionsurn)
hist(df.bmr.diff$Cmin)
hist(df.bmr.diff$Cmax)

# plot performance vs parameter of the dataset

par(mfrow=c(2,4))
plot(df.bmr.diff$logn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logp, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logdimension, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logpsurn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$logdimensionsurn, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$lograpportMajorityMinorityClass, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$Cmin, df.bmr.diff$acc.test.mean)
plot(df.bmr.diff$Cmax, df.bmr.diff$acc.test.mean)


# Bland Altman
perfsAggr.LR = subset(res.perfs.df, learner.id == "classif.logreg")
perfsAggr.RF = subset(res.perfs.df, learner.id == "classif.randomForest")
plot(1/2*(perfsAggr.LR$acc.test.mean+perfsAggr.RF$acc.test.mean),-perfsAggr.LR$acc.test.mean+perfsAggr.RF$acc.test.mean, ylim = c(-0.4,0.4))
lines(c(0.3,1),c(0,0), col = "red")
lines(c(0.3,1),c(0.1,0.1), col = "blue")
lines(c(0.3,1),c(-0.1,-0.1), col = "blue")

plot(1/2*(perfsAggr.LR$auc.test.mean+perfsAggr.RF$auc.test.mean),-perfsAggr.LR$auc.test.mean+perfsAggr.RF$auc.test.mean, ylim = c(-0.4,0.4))
lines(c(0.3,1),c(0,0), col = "red")
lines(c(0.3,1),c(0.1,0.1), col = "blue")
lines(c(0.3,1),c(-0.1,-0.1), col = "blue")

plot(1/2*(perfsAggr.LR$brier.test.mean+perfsAggr.RF$brier.test.mean),-perfsAggr.LR$brier.test.mean+perfsAggr.RF$brier.test.mean, ylim = c(-0.4,0.4))
lines(c(0,1),c(0,0), col = "red")
lines(c(0,1),c(0.1,0.1), col = "blue")
lines(c(0,1),c(-0.1,-0.1), col = "blue")

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


# plot with the target
df.plot = df.bmr.diff
p <- ggplot(data = df.bmr.diff, aes(x = logp, y = logn, colour = target.sigma))
p <- p + geom_point(size = 1)
print(p)








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


## ML Analysis ----

perfs.lr = subset(res.perfs.df, learner.id == "classif.logreg")

df.analysis = df.bmr.diff
df.analysis$lr.acc = perfs.lr$acc.test.mean
target = df.analysis$acc.test.mean>0
df.analysis$target = target

prop.table(table(df.bmr.diff$target.sigma))
prop.table(table(target))

learner.classif.rf = makeLearner("classif.randomForest", predict.type = "prob")
learner.clas.rpart.small = makeLearner("classif.rpart", maxdepth = 4, predict.type = "prob")
learner.clas.logreg = makeLearner("classif.logreg", predict.type = "prob")
list.learner.classif = list(learner.classif.rf,
                            learner.clas.rpart.small,
                            learner.clas.logreg)

learner.regr.rf = makeLearner("regr.randomForest")
learner.regr.lr = makeLearner("regr.lm")
learner.regr.gbm = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))
learner.regr.rpart = makeLearner("regr.rpart")
learner.regr.rpart.small = makeLearner("regr.rpart", maxdepth = 3)
learner.regr.rpart.small$id="regr.rpart.small"



learner.regr = list(learner.regr.rf, 
                    learner.regr.lr, 
                    learner.regr.gbm,
                    learner.regr.rpart,
                    learner.regr.rpart.small)

myvars <- c("logp", "logn", 
            "logpsurn",
            "psymbolic",
            "Cmax")

df.mlanalysis.regr <- df.analysis[c(myvars, "acc.test.mean")]
df.mlanalysis.clas <- df.analysis[c(myvars, "target")]


task.analysis.classif = makeClassifTask(id = "analysis.rank", data = df.mlanalysis.clas, target = "target")
task.analysis.regr = makeRegrTask(id = "analysis.perfs", data = df.mlanalysis.regr, target = "acc.test.mean")

rdesc = makeResampleDesc("CV", iters = 5)

# classification
fit.classif = train(learner = learner.classif.rf, task = task.analysis.classif)
benchmark(list.learner.classif, tasks = task.analysis.classif,resamplings = rdesc, measures = list(acc, f1,tnr, tpr, auc))

# Regression
fit.regr = train(learner = learner.regr.rf, task = task.analysis.regr)
benchmark(learner.regr, tasks = task.analysis.regr, resamplings= rdesc)



plotLearnerPrediction(learner = learner.regr.rf, task = task.analysis.regr)



## ROC curve ----
ms = list(auc, mmce)
bmr = benchmark(list.learner.classif, tasks = task.analysis.classif, resampling = rdesc, measures = ms, show.info = FALSE)


## Extract predictions
preds = getBMRPredictions(bmr)[[1]]

## Convert predictions
ROCRpreds = lapply(preds, asROCRPrediction)

## Calculate true and false positive rate
ROCRperfs = lapply(ROCRpreds, function(x) ROCR::performance(x, "tpr", "fpr"))

## lda average ROC curve
plot(ROCRperfs[[1]], col = "blue", avg = "vertical", spread.estimate = "stderror",
     show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "blue", plotCI.lwd = 2, lwd = 2)
## lda individual ROC curves
plot(ROCRperfs[[1]], col = "blue", lty = 2, lwd = 0.25, add = TRUE)

## ksvm average ROC curve
plot(ROCRperfs[[2]], col = "red", avg = "vertical", spread.estimate = "stderror",
     show.spread.at = seq(0.1, 0.6, 0.1), plotCI.col = "red", plotCI.lwd = 2, lwd = 2, add = TRUE)
## ksvm individual ROC curves
plot(ROCRperfs[[2]], col = "red", lty = 2, lwd = 0.25, add = TRUE)

legend("bottomright", legend = getBMRLearnerIds(bmr), lty = 1, lwd = 2, col = c("blue", "red"))






## Tuning a RF in mlr ----

# regr
lrn.regr.tuned = makeLearner("regr.randomForest")

ps = makeParamSet(
  makeDiscreteParam(id = "ntree", values = c ( 500, 750,2000)),
  makeLogicalParam(id = "replace"),
  makeDiscreteParam(id = "nodesize", values = c ( 30,60,90 )),
  makeDiscreteParam(id = "mtry", values = c ( 1,2,3,4,5 ))
)
ctrl = makeTuneControlRandom(maxit = 20L)
res = tuneParams(lrn.regr.tuned, task = task.analysis.regr, resampling = rdesc, par.set = ps, control = ctrl, measures = rmse)
lrn.regr.tuned = setHyperPars(lrn.regr.tuned, par.vals = res$x)


# clas
lrn.clas.tuned = makeLearner("classif.randomForest", predict.type = "prob")

ps = makeParamSet(
  makeDiscreteParam(id = "ntree", values = c ( 750,2000,5000)),
  makeLogicalParam(id = "replace"),
  makeDiscreteParam(id = "nodesize", values = c ( 30,60,90 )),
  makeDiscreteParam(id = "mtry", values = c ( 1,2,3,4 ))
)
ctrl = makeTuneControlRandom(maxit = 20L)
res = tuneParams(lrn.clas.tuned, task = task.analysis.classif, resampling = rdesc, par.set = ps, control = ctrl, measures = acc)
lrn.clas.tuned = setHyperPars(lrn.clas.tuned, par.vals = res$x)





## PDP Analysis ----

## Selection of variables

ctrl = makeFeatSelControlRandom(maxit = 20L)
rdesc = makeResampleDesc("Holdout")
sfeats = selectFeatures(learner = "regr.randomForest", task = task.analysis.regr, resampling = rdesc,
                        control = ctrl, show.info = FALSE)

# On garde logdimensionsurn logp Cmax n

## New Task for analysis
myvars <- c("logp", "logn", 
            "logdimensionsurn",
            "psymbolic",
            "Cmax")

df.mlanalysis.regr.filtered <- df.analysis[c(myvars, "acc.test.mean")]
df.mlanalysis.clas.filtered <- df.analysis[c(myvars, "target")]


task.analysis.regr.filtered = makeRegrTask(id = "analysis.perfs", data = df.mlanalysis.regr.filtered, target = "acc.test.mean")
task.analysis.clas.fileterd = makeClassifTask(id = "analysis.rank", data = df.mlanalysis.clas.filtered, target = "target")





# Regression
fit.regr = train(lrn.regr.tuned, task.analysis.regr.filtered)

pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr.filtered, "logp",
                                      fun = function(x) quantile(x, c(.25, .5, .75)))
pd.regr.logp = plotPartialDependence(pd.regr)


pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr.filtered, "logn",
                                        fun = function(x) quantile(x, c(.25, .5, .75)))
pd.regr.logn = plotPartialDependence(pd.regr)


pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr.filtered, "logdimensionsurn",
                                        fun = function(x) quantile(x, c(.25, .5, .75)))
pd.regr.logdimensionsurn = plotPartialDependence(pd.regr)


pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr.filtered, "Cmax",
                                        fun = function(x) quantile(x, c(.25, .5, .75)))
pd.regr.Cmax= plotPartialDependence(pd.regr)


pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr.filtered, "psymbolic",
                                        fun = function(x) quantile(x, c(.25, .5, .75)))
pd.regr.psymbolic= plotPartialDependence(pd.regr)


plot_grid(pd.regr.logp,
          pd.regr.logn, 
          pd.regr.logdimensionsurn,
          pd.regr.Cmax, 
          #pd.regr.psymbolic,
          #labels=c("A", "B"), 
          ncol = 2, nrow = 2)





# Classification
fit.clas = train(lrn.clas.tuned, task.analysis.clas.fileterd)


pd.clas = generatePartialDependenceData(fit.clas, task.analysis.clas.fileterd, "logp")
pd.clas.logp = plotPartialDependence(pd.clas)

pd.clas = generatePartialDependenceData(fit.clas, task.analysis.clas.fileterd, "logn")
pd.clas.logn = plotPartialDependence(pd.clas)

pd.clas = generatePartialDependenceData(fit.clas, task.analysis.clas.fileterd, "logdimensionsurn")
pd.clas.logdimensionsurn = plotPartialDependence(pd.clas)

pd.clas = generatePartialDependenceData(fit.clas, task.analysis.clas.fileterd, "Cmax")
pd.clas.Cmax = plotPartialDependence(pd.clas)

plot_grid(pd.clas.logp,
          pd.clas.logn, 
          pd.clas.logdimensionsurn,
          pd.clas.Cmax, 
          #labels=c("A", "B"), 
          ncol = 2, nrow = 2)





# Tree rcart analysis ----
library(rpart.plot)
library(rpart)
library(rattle)

# tree mlr regr
fit = train(learner = learner.regr.rpart.small, task = task.analysis.regr)
fit$learner.model
fancyRpartPlot(fit$learner.model)	

# tree mlr clas
fit = train(learner = learner.clas.rpart.small, task = task.analysis.classif)
fit$learner.model
fancyRpartPlot(fit$learner.model)	





# tree clas
form = as.formula(target~.)
tree.2 <- rpart(form,df.mlanalysis.clas, control=rpart.control(maxdepth = 3))		
fancyRpartPlot(tree.2)	

# tree regr
form = as.formula(acc.test.mean~.)
tree.2 <- rpart(form,df.mlanalysis.regr, control=rpart.control(maxdepth = 3))		
fancyRpartPlot(tree.2)	
