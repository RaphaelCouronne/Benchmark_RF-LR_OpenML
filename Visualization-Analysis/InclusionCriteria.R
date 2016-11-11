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


measure.name = "acc"
measure = "acc.test.mean"
y.limits = c(-0.08, 0.22)


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
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = TRUE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(t), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("n < ",t)),
                                                      expression(paste("n > ",t))))
p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(n[max])))
print(p)
p.n = p

p.histo.n = qplot(exp(df.bmr.diff$logn), geom="histogram") 

datatest = data.frame(n = exp(df.bmr.diff$logn))
p.histo.n = ggplot(datatest, aes(x = n)) + geom_histogram(binwidth = 0.1, alpha = 0.4) + 
  scale_x_log10(breaks=c(100,500,2000,10000),
                labels = c("  100", "  500", " 2000", "10000")) +
  #scale_x_log10() +
  geom_vline(xintercept=thresholdvect.original)+
  theme(axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))




print(p.histo.n)


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
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = TRUE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(t), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("p < ",t)),
                                                      expression(paste("p > ",t))))
p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(p[max])))
print(p)
p.p = p

datatest = data.frame(p = exp(df.bmr.diff$logp))
p.histo.p = ggplot(datatest, aes(x = p)) + geom_histogram(binwidth = 0.1, alpha = 0.4) + 
  scale_x_log10(breaks=c(thresholdvect.original,100),
                labels = c("      5", "      10", "      20", "     100")) +
  #scale_x_log10() +
  geom_vline(xintercept=thresholdvect.original)+
  theme(axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

print(p.histo.p)



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
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = TRUE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(t), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank(),
              legend.background = element_rect(colour = "black"))
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("p/n < ",t)),
                                                      expression(paste("p/n > ",t))))

p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(p/n[max])))
print(p)
p.psurn = p




datatest = data.frame(psurn = exp(df.bmr.diff$logpsurn))
p.histo.psurn = ggplot(datatest, aes(x = psurn)) + geom_histogram(binwidth = 0.1, alpha = 0.4) +
 scale_x_log10(breaks=c(0.001, thresholdvect.original,0.5), 
               labels = c("0.001", " 0.01", " 0.03", "  0.1", "  0.5")) + 
  #scale_x_log10() + 
  labs(x = expression(p/n)) +
  geom_vline(xintercept = thresholdvect.original) +
  theme(axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

p.histo.psurn

print(p.histo.psurn)






# Cmax ---

feature = "Cmax"
Cmaxmax = max(exp(df.bmr.diff$Cmax))
thresholdvect.original = c(0.55,0.65,0.8)
thresholdvect = c(thresholdvect.original, Cmaxmax+1)
df = df.bmr.diff

df.all = data.frame()

for (i in c(1:length(thresholdvect))) {
  df.temp = df
  threshold = thresholdvect[i]
  logthreshold = (threshold)
  df.temp$thresholdValue = threshold
  df.temp$logical.threshold = df[[feature]]>logthreshold
  
  df.all = rbind(df.all, df.temp) 
}
df.all$thresholdValue = as.factor(df.all$thresholdValue)

p <- ggplot(df.all, aes_string("thresholdValue", measure))
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = TRUE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(t), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank(),
              legend.background = element_rect(colour = "black"))
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("Cmax < ",t)),
                                                      expression(paste("Cmax > ",t))))

p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(Cmax[max])))
print(p)
p.Cmax = p



datatest = data.frame(Cmax = df.bmr.diff$Cmax)
p.histo.Cmax = ggplot(datatest, aes(x = Cmax)) + geom_histogram(binwidth = 0.02, alpha = 0.4) +
  scale_x_continuous(breaks=c(thresholdvect.original),
                     labels = c("    0.55", "    0.65", "     0.8")) + 
  #scale_x_log10() + 
  labs(x = expression(Cmax)) +
  geom_vline(xintercept=c(thresholdvect.original)) +
  theme( axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

p.histo.Cmax

print(p.histo.Cmax)


# plot all of it ---
library(cowplot)


plot_grid(p.n,
          p.histo.n, 
          p.p,
          p.histo.p, 
          p.psurn,
          p.histo.psurn,
          p.Cmax,
          p.histo.Cmax,

          #labels=c("A", "B"), 
          ncol = 2, nrow = 4)


plot_grid(p.n, p.p, p.psurn, p.Cmax,
          p.histo.n, 
          p.histo.p, 
          p.histo.psurn,
          p.histo.Cmax,
          
          #labels=c("A", "B"), 
          ncol = 4, nrow = 2,
          align = "v", rel_heights = c(1.8, 1))



## Acc ----
p.acc.n = p.n
p.acc.p = p.p
p.acc.psurn = p.psurn
p.acc.Cmax = p.Cmax


## Brier ----
p.brier.n = p.n
p.brier.p = p.p
p.brier.psurn = p.psurn
p.brier.Cmax = p.Cmax


## Auc ----
p.auc.n = p.n
p.auc.p = p.p
p.auc.psurn = p.psurn
p.auc.Cmax = p.Cmax


## plot all ----

p.acc.n = p.acc.n+theme(legend.position="none")
p.acc.p = p.acc.p+theme(legend.position="none")
p.acc.psurn = p.acc.psurn+theme(legend.position="none")
p.acc.Cmax = p.acc.Cmax+theme(legend.position="none")

p.auc.n = p.auc.n+theme(legend.position="none")
p.auc.p = p.auc.p+theme(legend.position="none")
p.auc.psurn = p.auc.psurn+theme(legend.position="none")
p.auc.Cmax = p.auc.Cmax+theme(legend.position="none")

p.brier.n = p.brier.n+theme(legend.position="none")
p.brier.p = p.brier.p+theme(legend.position="none")
p.brier.psurn = p.brier.psurn+theme(legend.position="none")
p.brier.Cmax = p.brier.Cmax+theme(legend.position="none")

plot_grid(p.acc.n, p.acc.p, p.acc.psurn,  p.acc.Cmax,
          p.auc.n, p.auc.p, p.auc.psurn, p.auc.Cmax,
          p.brier.n, p.brier.p,p.brier.psurn,p.brier.Cmax,
          p.histo.n, 
          p.histo.p, 
          p.histo.psurn,
          p.histo.Cmax,
          
          #labels=c("A", "B"), 
          ncol = 4, nrow = 4)



## Version 2 ----






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

names(df.all)
df.all.n = subset(df.all, select = c("acc.test.mean",  "auc.test.mean", "brier.test.mean", "thresholdValue","logical.threshold"))

head(df.all.n)
df.all.n$id = c(1:length(df.all$acc.test.mean))
library(reshape)
mdata <- melt(df.all.n, id = c("id","thresholdValue", "logical.threshold")) 
head(mdata)


ggplot(mdata, aes(thresholdValue, value, fill=thresholdValue))+
  geom_boxplot(aes(fill = logical.threshold)) + 
  facet_wrap(~variable, switch = "y", dir = "v", scales = "free_y") 



p <- ggplot(df.all, aes_string("thresholdValue", measure))
p = p + geom_boxplot(aes_string(fill = "logical.threshold"), outlier.shape = NA, notch = TRUE)
p = p + scale_y_continuous(limits = y.limits)
p = p + labs(x = expression(t), y = bquote(paste(Delta, .(measure.name))))
p = p + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
p = p + scale_fill_grey(start = 0.4,end = 1, labels=c(expression(paste("n < ",t)),
                                                      expression(paste("n > ",t))))
p = p + scale_x_discrete(labels=c(thresholdvect.original,expression(n[max])))
print(p)
p.n = p



## for all ----

# n
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

names(df.all)
df.all.n = subset(df.all, select = c("acc.test.mean",  "auc.test.mean", "brier.test.mean", "thresholdValue","logical.threshold"))
df.all.n$feature = "n"


# p
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
df.all.p = subset(df.all, select = c("acc.test.mean",  "auc.test.mean", "brier.test.mean", "thresholdValue","logical.threshold"))
df.all.p$feature = "p"


# p sur n 
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
df.all.psurn = subset(df.all, select = c("acc.test.mean",  "auc.test.mean", "brier.test.mean", "thresholdValue","logical.threshold"))
df.all.psurn$feature = "psurn"




# Cmax 
feature = "Cmax"
Cmaxmax = max(exp(df.bmr.diff$Cmax))
thresholdvect.original = c(0.55,0.65,0.8)
thresholdvect = c(thresholdvect.original, Cmaxmax+1)
df = df.bmr.diff

df.all = data.frame()

for (i in c(1:length(thresholdvect))) {
  df.temp = df
  threshold = thresholdvect[i]
  logthreshold = (threshold)
  df.temp$thresholdValue = threshold
  df.temp$logical.threshold = df[[feature]]>logthreshold
  
  df.all = rbind(df.all, df.temp) 
}
df.all$thresholdValue = as.factor(df.all$thresholdValue)
df.all.Cmax = subset(df.all, select = c("acc.test.mean",  "auc.test.mean", "brier.test.mean", "thresholdValue","logical.threshold"))
df.all.Cmax$feature = "Cmax"




# rbind
levels(df.all.n$thresholdValue) = c("100","500","2000","max")
levels(df.all.p$thresholdValue) = c("5","10","20","max")
levels(df.all.psurn$thresholdValue) = c("0.01","0.03","0.1","max")
levels(df.all.Cmax$thresholdValue) = c("0.55","0.65","0.8","max")


df.all = rbind(df.all.n, df.all.p,  df.all.psurn, df.all.Cmax)
df.all$thresholdValue = factor(df.all$thresholdValue, 
                               levels = c("100","500","2000","5","10","20","0.01","0.03","0.1","0.55","0.65","0.8","max"))

mdata <- melt(df.all, id = c("thresholdValue", "logical.threshold", "feature")) 
head(mdata)

# change the levels
levels(mdata$variable) = c(expression(paste(Delta,"acc")),
                           expression(paste(Delta,"auc")),
                           expression(paste(Delta,"brier")))
mdata$feature=factor(mdata$feature, levels = c("n","p","psurn","Cmax"))
levels(mdata$feature)=c("n","p","p/n","Cmax")




ggplot(mdata, aes(thresholdValue, value, fill=thresholdValue))+
  geom_boxplot(aes(fill = logical.threshold)) + 
  facet_grid(variable~feature, scales = c("free"), switch = "y", labeller = label_parsed)+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  theme(legend.position="none") +
  scale_fill_grey(start = 0.4,end = 1)


