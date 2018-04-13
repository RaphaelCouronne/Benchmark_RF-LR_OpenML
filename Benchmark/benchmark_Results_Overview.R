
library(boot)

benchmark_ResultsOverview = function(df.bmr.diff = df.bmr.diff, res.perfs.df = res.perfs.df) {
  
  
  perfsAggr.LR = subset(res.perfs.df, learner.id == "classif.logreg")
  perfsAggr.RF = subset(res.perfs.df, learner.id == "classif.randomForest")
  
  PrintBasicStatistics <- function(x, title=NULL, R=2000) {
    
    set.seed(1)
    
    ## Accuracy
    print(title, quote = FALSE)
    
    # Mean value
    print(paste("  Mean value is", 
                format(round(mean(x), 5), nsmall = 5)
    ), quote = FALSE)
    
    # Standard deviation
    print(paste("  Standard deviation is", 
                format(round(sd(x), 5), nsmall = 5)
    ), quote = FALSE)
    
    
    # Bootstrap confidence interval
    meanFunc <- function(x,i){mean(x[i])}
    boot_obj = boot(x, statistic = meanFunc, R = R)
    
    res.boot.ci = boot.ci(boot_obj, conf = c(0.10, 0.95),
                          type = c("norm", "basic", "perc", "bca"))
    
    print(paste0("  95% (Bca) bootstrap confidence interval for the mean is [",
                 format(round(res.boot.ci$bca[2,4], 5), nsmall = 5),
                 ", ",
                 format(round(res.boot.ci$bca[2,5], 5), nsmall = 5),
                 "]"), quote = FALSE)
  }
  
  PrintBasicStatistics(df.bmr.diff$acc.test.mean, title = "RF-LR difference in performance for acc")
  PrintBasicStatistics(perfsAggr.LR$acc.test.mean, title = "LR performance for acc")
  PrintBasicStatistics(perfsAggr.RF$acc.test.mean, title = "RF performance for acc")
  
  PrintBasicStatistics(df.bmr.diff$auc.test.mean, title = "RF-LR difference in performance for auc")
  PrintBasicStatistics(perfsAggr.LR$auc.test.mean, title = "LR performance for auc")
  PrintBasicStatistics(perfsAggr.RF$auc.test.mean, title = "RF performance for auc")
  
  PrintBasicStatistics(df.bmr.diff$brier.test.mean, title = "RF-LR difference in performance for brier")
  PrintBasicStatistics(perfsAggr.LR$brier.test.mean, title = "LR performance for brier")
  PrintBasicStatistics(perfsAggr.RF$brier.test.mean, title = "RF performance for brier")
  
  
  ## Power of the test
  
  alpha = 0.05
  beta = 0.2
  delta = 0.02
  sigma = sd(df.bmr.diff$acc.test.mean)
  
  Mreq = ((qnorm(1-beta)+qnorm(1-alpha/2))^2*sigma^2)/delta^2
  
  print(paste0("Value of power calculation is ",
               floor(Mreq+1),
               " with parameters : alpha = ",alpha,
               ", beta = ",beta,", delta = ",delta,
               ", sigma = ",sigma), quote = FALSE)
  
}
