library(mlr)
library(ggplot2)

ResultsMetaLearning = function(df.bmr.diff = df.bmr.diff) {
  
  ################################################################################################################################
  # Meta-Learning
  ################################################################################################################################
  
  ## Plot tnfluence of parameters ----
  
  ## Plots
  
  # Reshape dataset
  df.all = subset(df.bmr.diff, select = c("acc.test.mean", "logp", "logn", "logp.dividedby.n", "Cmax"))
  library(reshape2)
  mdata <- melt(df.all, id = c("acc.test.mean")) 
  detach("package:reshape2", unload=TRUE)
  mdata$performanceMeasure = "acc"
  mdata$performanceMeasure = factor(mdata$performanceMeasure)
  levels(mdata$performanceMeasure) = expression(paste(Delta,"acc"))
  names(mdata) = c("acc", "feature", "featureValue", "performanceMeasure")
  
  levels(mdata$feature)[levels(mdata$feature)=="logp.dividedby.n"] = "logp/n"
  
  
  # histogram of features
  plot.histograms = ggplot(mdata, aes(featureValue))+
    geom_histogram(bins = 20) + 
    facet_grid(~feature, margins = FALSE, scales = c("free"), switch = "y", labeller = label_parsed)+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position="none") 
  
  plot.histograms
  
  jpeg(filename = "Data/Pictures/AdditionalFigures/Figure8_Distributions.jpeg", width = 1000, height = 400)
  plot(plot.histograms)
  dev.off()
  
  
  # Plot the performances
  plot.performances = ggplot(mdata, aes(featureValue, acc))+
    geom_point() +
    facet_grid(performanceMeasure~feature, switch = "y", margins = FALSE, scales = c("free"), labeller = label_parsed)+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position="none") 
  
  plot.performances
  
  jpeg(filename = "Data/Pictures/AdditionalFigures/Figure7_Performances.jpeg", width = 1000, height = 400)
  plot(plot.performances)
  dev.off()
  
  
  
  # Bland Altman
  perfsAggr.LR = subset(res.perfs.df, learner.id == "classif.logreg")
  perfsAggr.RF = subset(res.perfs.df, learner.id == "classif.randomForest")
  Bland.Sum = perfsAggr.RF+perfsAggr.LR
  Bland.Difference = perfsAggr.RF-perfsAggr.LR
  
  df.blandAltman = data.frame(mean = (1/2)*(perfsAggr.RF$acc.test.mean+perfsAggr.LR$acc.test.mean), 
                              difference = (1/2)*(perfsAggr.RF$acc.test.mean-perfsAggr.LR$acc.test.mean))
  

  
  plot.BlandAltman = ggplot(df.blandAltman, aes(mean, difference))+
    geom_point() + geom_hline(yintercept = 0)
  
  plot.BlandAltman
  
  jpeg(filename = "Data/Pictures/AdditionalFigures/Figure9_BlandAltman.jpeg", width = 800, height = 500)
  plot(plot.BlandAltman)
  dev.off()
  
  
  ## Meta learning ------
  
  
  ## Linear models
  
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
  plotLinearModelandCor("logp","acc.test.mean")
  plotLinearModelandCor("logdimension","acc.test.mean")
  plotLinearModelandCor("logp.dividedby.n","acc.test.mean")
  plotLinearModelandCor("logdimension.dividedby.n","acc.test.mean")
  plotLinearModelandCor("pnum","acc.test.mean", log = TRUE)
  plotLinearModelandCor("psymbolic","acc.test.mean")
  plotLinearModelandCor("pnumrate","acc.test.mean")
  plotLinearModelandCor("Cmax","acc.test.mean")
  
  ## ML Analysis ----
  # We use a random forest and assess its performance for the meta-learning task
  
  learner.regr.rf = makeLearner("regr.randomForest")
  
  myvars <- c("logp", "logn", 
              "logp.dividedby.n",
              "Cmax")
  
  df.mlanalysis.regr <- df.bmr.diff[c(myvars, "acc.test.mean")]
  task.analysis.regr = makeRegrTask(id = "analysis.perfs", data = df.mlanalysis.regr, target = "acc.test.mean")
  rdesc = makeResampleDesc("RepCV", folds = 5, reps = 4)
  fit.regr = train(learner = learner.regr.rf, task = task.analysis.regr)
  benchmark(learner.regr.rf, tasks = task.analysis.regr, resamplings = rdesc)
  
  
  
  plotLearnerPrediction(learner = learner.regr.rf, task = task.analysis.regr)
  
  
  
  
  
  ## PDP Analysis ----
  
  fit.regr = train(learner.regr.rf, task.analysis.regr)
  
  generatePartialDependenceDataFeature = function(feature.name) {
    pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr, feature.name,
                                            fun = function(x) quantile(x, c(.25, .5, .75)))
    pd.regr.logp = plotPartialDependence(pd.regr) + 
      labs(y = bquote(paste(Delta, .("acc")))) +
      ylim(-0.01,0.11)
    return(pd.regr.logp)
  }
  
  
  pd.regr.logp = generatePartialDependenceDataFeature("logp") + labs(x = bquote(log(p)))
  pd.regr.logn = generatePartialDependenceDataFeature("logn") + labs(x = bquote(log(n)))
  pd.regr.logdimension.dividedby.n = generatePartialDependenceDataFeature("logp.dividedby.n") +labs(x = bquote(log(p/n)))
  pd.regr.Cmax = generatePartialDependenceDataFeature("Cmax") + labs(x = bquote(Cmax))
  
  
  plot.grid = plot_grid(pd.regr.logp,
                        pd.regr.logn, 
                        pd.regr.logdimension.dividedby.n,
                        pd.regr.Cmax, 
                        ncol = 4, nrow = 1)
  
  print(plot.grid)
  
  jpeg(filename = "Data/Pictures/Figure6_MetaLearning.jpeg", width = 1000, height = 300)
  plot(plot.grid)
  dev.off()
  
}

# # plot performance vs parameter of the dataset
# par(mfrow=c(2,2))
# plot(df.bmr.diff$logn, df.bmr.diff$acc.test.mean)
# plot(df.bmr.diff$logp, df.bmr.diff$acc.test.mean)
# plot(df.bmr.diff$logp.dividedby.n, df.bmr.diff$acc.test.mean)
# plot(df.bmr.diff$Cmax, df.bmr.diff$acc.test.mean)


# par(mfrow=c(2,2))
# hist.logp = hist(df.bmr.diff$logn, xlab = "logn", main = NULL)
# hist.logn = hist(df.bmr.diff$logp, xlab = "logp", main = NULL)
# hist.logp.dividedby.n = hist(df.bmr.diff$logp.dividedby.n, xlab = "logp.dividedby.n", main = NULL)
# hist.Cmax = hist(df.bmr.diff$Cmax, xlab = "Cmax", main = NULL)

