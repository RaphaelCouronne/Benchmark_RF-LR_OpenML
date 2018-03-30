library(mlr)
library(ggplot2)
library(cowplot)

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
  
  learner.regr.rf = makeLearner("regr.randomForest", predict.type = "se")
  
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
    pd.regr = generatePartialDependenceData(fit.regr, task.analysis.regr, feature.name)
    pd.regr.logp = plotPartialDependence(pd.regr) + 
      labs(y = bquote(paste(Delta, .("acc")))) #+
      #ylim(-0.01,0.11)
    return(pd.regr.logp)
  }
  
  police.size = 18
  pd.regr.logp = generatePartialDependenceDataFeature("logp") + labs(x = bquote(log(p))) + theme(text = element_text(size=police.size))
  pd.regr.logn = generatePartialDependenceDataFeature("logn") + labs(x = bquote(log(n)))+ theme(text = element_text(size=police.size))
  pd.regr.logdimension.dividedby.n = generatePartialDependenceDataFeature("logp.dividedby.n") +labs(x = bquote(log(p/n)))+ theme(text = element_text(size=police.size))
  pd.regr.Cmax = generatePartialDependenceDataFeature("Cmax") + labs(x = bquote(Cmax))+ theme(text = element_text(size=police.size))
  
  
  plot.grid = plot_grid(pd.regr.logn, 
                        pd.regr.logp,
                        pd.regr.logdimension.dividedby.n,
                        pd.regr.Cmax, 
                        ncol = 4, nrow = 1, align = "v")
  
  print(plot.grid)
  
  jpeg(filename = "Data/Pictures/Figure6_MetaLearning.jpeg", width = 1000, height = 300)
  plot(plot.grid)
  dev.off()
  
}





ResultsMetaLearning_bio = function(df.bmr.diff = df.bmr.diff) {
  
  ################################################################################################################################
  # Meta-Learning
  ################################################################################################################################
  
  
  ## ML Analysis ----
  # We use a random forest and assess its performance for the meta-learning task
  
  learner.regr.rf = makeLearner("regr.randomForest", predict.type = "se")
  
  myvars <- c("logp", "logn", 
              "logp.dividedby.n",
              "Cmax")
  
  
  # RF
  df.mlanalysis.regr.rf <- df.bmr.diff[df.bmr.diff$rf_type=="RF",c(myvars, "acc.test.mean")]
  task.analysis.regr.rf = makeRegrTask(id = "analysis.perfs", data = df.mlanalysis.regr.rf, target = "acc.test.mean")
  fit.regr.rf = train(learner.regr.rf, task.analysis.regr.rf)
  
  # TR
  df.mlanalysis.regr.tr <- df.bmr.diff[df.bmr.diff$rf_type=="TR",c(myvars, "acc.test.mean")]
  task.analysis.regr.tr = makeRegrTask(id = "analysis.perfs", data = df.mlanalysis.regr.tr, target = "acc.test.mean")
  fit.regr.tr = train(learner.regr.rf, task.analysis.regr.tr)
  
  ## PDP Analysis ----
  

  
  generatePartialDependenceDataFeature = function(feature.name) {
    pd.regr.rf = generatePartialDependenceData(fit.regr.rf, task.analysis.regr.rf, feature.name)
    pd.regr.tr = generatePartialDependenceData(fit.regr.tr, task.analysis.regr.tr, feature.name)
    
    df_rf = pd.regr.rf$data
    df_tr = pd.regr.tr$data
    
    df_rf$method="RF"
    df_tr$method="TR"
    
    df_all = rbind(df_rf, df_tr)
    
    p = ggplot(df_all,aes_string(x=feature.name,y="acc.test.mean",group="method", col="method",fill="method",linetype="method"))+
      geom_point()+geom_line()+
      # Lower
      geom_line(mapping = aes_string(y = "lower", x = feature.name))+
      geom_point(mapping = aes_string(y = "lower", x = feature.name))+
      #Upper
      geom_line(mapping = aes_string(y = "upper", x = feature.name))+
      geom_point(mapping = aes_string(y = "upper", x = feature.name))+
      labs(y = bquote(paste(Delta, .("acc")))) #+
    #ylim(-0.01,0.11)
    return(p)
  }


    
  
  police.size = 18
  pd.regr.logp = generatePartialDependenceDataFeature("logp") + labs(x = bquote(log(p))) + theme(text = element_text(size=police.size))+ theme(legend.position="none")
  pd.regr.logn = generatePartialDependenceDataFeature("logn") + labs(x = bquote(log(n)))+ theme(text = element_text(size=police.size))+ theme(legend.position="none")
  pd.regr.logdimension.dividedby.n = generatePartialDependenceDataFeature("logp.dividedby.n") +labs(x = bquote(log(p/n)))+ theme(text = element_text(size=police.size))+ theme(legend.position="none")
  pd.regr.Cmax = generatePartialDependenceDataFeature("Cmax") + labs(x = bquote(Cmax))+ theme(text = element_text(size=police.size))+theme(legend.position = c(0.7,0.1), legend.title = element_blank())
  
  
  plot.grid = plot_grid(pd.regr.logn, 
                        pd.regr.logp,
                        pd.regr.logdimension.dividedby.n,
                        pd.regr.Cmax, 
                        ncol = 4, nrow = 1, align = "h")
  
  print(plot.grid)
  
  jpeg(filename = "Data/Pictures/Figure6_MetaLearning_bio.jpeg", width = 1000, height = 300)
  plot(plot.grid)
  dev.off()
  
}