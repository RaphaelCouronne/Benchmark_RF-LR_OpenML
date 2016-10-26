rm(list = ls())
load(file = "Data/Results/df.bmr.RData")




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


## ML Analysis

df.analysis = df.bmr.diff
target = df.analysis$acc.test.mean>0
df.analysis$target = target

prop.table(table(df.bmr.diff$target.sigma))
prop.table(table(target))

learner.classif.rf = makeLearner("classif.randomForest", predict.type = "prob")
learner.classif.rpart = makeLearner("classif.rpart", predict.type = "prob")
learner.regr.rf = makeLearner("regr.randomForest")
learner.regr.rpart = makeLearner("regr.rpart")

myvars <- c("logp", "logn", 
            "logdimension", "logpsurn", "logdimensionsurn",
            "pnum", "psymbolic", 
            "pnumrate", "psymbolicrate", 
            "brierlogreg",
            "Cmin", "Cmax")
df.mlanalysis.regr <- df.analysis[c(myvars, "acc.test.mean")]
df.mlanalysis.clas <- df.analysis[c(myvars, "target")]
df.mlanalysis.target.sigma <- df.analysis[c(myvars, "target.sigma")]

task.analysis.classif = makeClassifTask(id = "analysis.rank", data = df.mlanalysis.clas, target = "target")
task.analysis.target.sigma <- makeClassifTask(id = "analysis.rank", data = df.mlanalysis.clas, target = "target.sigma")
task.analysis.regr = makeRegrTask(id = "analysis.perfs", data = df.mlanalysis.regr, target = "acc.test.mean")

fit.classif = train(learner = learner.classif.rf, task = task.analysis.classif)
benchmark(learner.classif.rf, tasks = task.analysis.classif,resamplings = makeResampleDesc("CV", iters = 5), measures = list(acc, f1,tnr, tpr, auc))

fit.classif = train(learner = learner.classif.rf, task = task.analysis.target.sigma)
benchmark(learner.classif.rf, tasks = task.analysis.classif,resamplings = makeResampleDesc("CV", iters = 5), measures = list(acc, f1,tnr, tpr, auc))

fit.regr = train(learner = learner.regr.rf, task = task.analysis.regr)
benchmark(learner.regr.rf, tasks = task.analysis.regr,resamplings = makeResampleDesc("CV", iters = 5))


plotLearnerPrediction(learner = learner.classif.rpart, task = task.analysis.classif)
plotLearnerPrediction(learner = learner.regr.rpart, task = task.analysis.regr)


# tree clas
library(rpart.plot)
library(rpart)
form = as.formula(target~.)
tree.2 <- rpart(form,df.mlanalysis.clas, control=rpart.control(maxdepth = 2))		
fancyRpartPlot(tree.2)	

# tree regr
form = as.formula(acc.test.mean~.)
tree.2 <- rpart(form,df.mlanalysis.regr, control=rpart.control(maxdepth = 2))		
fancyRpartPlot(tree.2)	
