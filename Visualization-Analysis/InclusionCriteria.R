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

feature.boxplot.oneplot <- function(df, measure, feature, thresholdvect, feature.name = "feature", featuremax = "toto") {
  
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
  p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
  p = p + scale_fill_grey(start = 0.4,end = 1)
  print(p)
  return(p)
}


## 2. Plots ----

expression(paste(delta,"acc"))

hist(df.bmr.diff$logn)
measure.chosen = "acc.test.mean"
feature.chosen = "logn"
p.acc = feature.boxplot.oneplot(df.bmr.diff, measure.chosen, feature.chosen,c(50,500,1000), "n")


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


# ===========================================================================
## ACC - AUC- Brier ==========================================================
# ============================================================================


measure.name = "brier"
measure = "brier.test.mean"
y.limits = c(-0.2, 0.1)

# launch visualization ----

# n ---


feature = "logn"
nmax = max(exp(df.bmr.diff$logn))
thresholdvect.original = c(100,500,2000)
thresholdvect = c(thresholdvect.original, nmax+1)
df = df.bmr.diff

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
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = FALSE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(bar(n)), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(0.8,0.8), legend.position=c(1,1), legend.title=element_blank())
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("n < ",bar(n))),expression(paste("n > ",bar(n)))))
p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(n[max])))
print(p)
p.n = p

p.histo.n = qplot(exp(df.bmr.diff$logn), geom="histogram") 

datatest = data.frame(n = exp(df.bmr.diff$logn))
p.histo.n = ggplot(datatest, aes(x = n)) + geom_histogram(binwidth = 0.1) + scale_x_log10(breaks=c(100,500,2000))
print(p.histo.n)

library(scales)

p.density.n = ggplot(datatest, aes(x=n)) +
  stat_density(aes(y=..count..), color="black", fill="grey", alpha=0.3) +
  scale_x_continuous(breaks=c(100,500,2000), trans="log", expand=c(0,0)) +
  theme_bw()
print(p.density.n)

# p ---



feature = "logp"
pmax = max(exp(df.bmr.diff$logp))
thresholdvect.original = c(5,10,20)
thresholdvect = c(thresholdvect.original, pmax+1)
df = df.bmr.diff

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
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = FALSE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(bar(p)), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("p < ",bar(p))),
                                                      expression(paste("p > ",bar(p)))))
p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(p[max])))
print(p)
p.p = p

datatest = data.frame(p = exp(df.bmr.diff$logp))
p.histo.p = ggplot(datatest, aes(x = p)) + geom_histogram(binwidth = 0.1) + scale_x_log10(breaks=thresholdvect.original)
print(p.histo.p)

library(scales)

p.density.p = ggplot(datatest, aes(x=p)) +
  stat_density(aes(y=..count..), color="black", fill="grey", alpha=0.3) +
  scale_x_continuous(breaks=thresholdvect.original, trans="log", expand=c(0,0)) +
  theme_bw()
print(p.density.p)

# p sur n ---

feature = "logpsurn"
psurnmax = max(exp(df.bmr.diff$logpsurn))
thresholdvect.original = c(1e-2,3e-2,1e-1)
thresholdvect = c(thresholdvect.original, psurnmax+1)
df = df.bmr.diff

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
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = FALSE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(bar(p/n)), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("p/n < ",bar(p/n))),
                                                      expression(paste("p/n > ",bar(p/n)))))

p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(p/n[max])))
print(p)
p.psurn = p


datatest = data.frame(psurn = exp(df.bmr.diff$logpsurn))
p.histo.psurn = ggplot(datatest, aes(x = psurn)) + geom_histogram(binwidth = 0.1) +
 scale_x_log10(breaks=thresholdvect.original) + labs(x = expression(p/n))

print(p.histo.psurn)

library(scales)

p.density.psurn = ggplot(datatest, aes(x=psurn)) +
  stat_density(aes(y=..count..), color="black", fill="grey", alpha=0.3) +
  scale_x_continuous(breaks=thresholdvect.original, trans="log", expand=c(0,0)) +
  theme_bw()
print(p.density.psurn)

# plot all of it ---
library(cowplot)


plot_grid(p.n,
          p.histo.n, 
          p.p,
          p.histo.p, 
          p.psurn,
          p.histo.psurn,

          #labels=c("A", "B"), 
          ncol = 2, nrow = 3)




