rm(list = ls())
library(ggplot2)
load(file = "Data/Results/df.bmr.RData")
memory.size()






################################################################################################################################
# Part 4 : Boxplots
################################################################################################################################



## 1. functions ----

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

feature.boxplot.oneplot <- function(df, measure, feature, thresholdvect, feature.name = "feature") {
  
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
  p = p + labs(x = "Threshold", y = expression(paste(Delta,"acc")))
  p = p + labs(fill = paste( feature.name ,"> Threshold"))
  #p = p + labs(fill = expression(paste( over(p,n)," > Threshold")))
  print(p)
}


## 2. Plots ----

expression(paste(delta,"acc"))

hist(df.bmr.diff$logn)
measure.chosen = "acc.test.mean"
feature.chosen = "logn"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(50,500,1000), "n")

hist(df.bmr.diff$logp)
measure.chosen = "acc.test.mean"
feature.chosen = "logp"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(5,8,20), "p")

hist(df.bmr.diff$logpsurn)
measure.chosen = "acc.test.mean"
feature.chosen = "logpsurn"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(1e-2,4e-2,1e-1), expression(over(p,n)))

hist(df.bmr.diff$Cmax)
measure.chosen = "acc.test.mean"
feature.chosen = "Cmax"
feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(0.1,0.25,0.4), "Cmax")



