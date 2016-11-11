rm(list=ls())
library(ggplot2)
load(file = "Data/Results/df.bmr.RData")
memory.size()




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




plotmeasures = ggplot(mdata, aes(thresholdValue, value, fill=thresholdValue))+
  geom_boxplot(aes(fill = logical.threshold)) + 
  facet_grid(variable~feature, margins = FALSE, scales = c("free"), switch = "y", labeller = label_parsed)+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  theme(legend.position="none") +
  scale_fill_grey(start = 0.4,end = 1)

plotmeasures

## Now the distributions ----

# Odldstyle ----

# n
datatest = data.frame(n = exp(df.bmr.diff$logn))
p.histo.n = ggplot(datatest, aes(x = n)) + geom_histogram(binwidth = 0.1, alpha = 0.4) + 
  scale_x_log10(breaks=c(100,500,2000,10000),
                labels = c("  100", "  500", " 2000", "10000")) +
  #scale_x_log10() +
  geom_vline(xintercept=c(100,500,2000,10000))+
  theme(axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

# p
datatest = data.frame(p = exp(df.bmr.diff$logp))
p.histo.p = ggplot(datatest, aes(x = p)) + geom_histogram(binwidth = 0.1, alpha = 0.4) + 
  scale_x_log10(breaks=c(c(5,10,20),100),
                labels = c("      5", "      10", "      20", "     100")) +
  #scale_x_log10() +
  geom_vline(xintercept=c(5,10,20))+
  theme(axis.title.y = element_blank(),axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

print(p.histo.p)

# psurn

datatest = data.frame(psurn = exp(df.bmr.diff$logpsurn))
p.histo.psurn = ggplot(datatest, aes(x = psurn)) + geom_histogram(binwidth = 0.1, alpha = 0.4) +
  scale_x_log10(breaks=c(0.001, c(1e-2,3e-2,1e-1),0.5), 
                labels = c("0.001", " 0.01", " 0.03", "  0.1", "  0.5")) + 
  #scale_x_log10() + 
  labs(x = expression(p/n)) +
  geom_vline(xintercept = c(1e-2,3e-2,1e-1)) +
  theme(axis.title.y = element_blank(),axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

p.histo.psurn


# Cmax
datatest = data.frame(Cmax = df.bmr.diff$Cmax)
p.histo.Cmax = ggplot(datatest, aes(x = Cmax)) + geom_histogram(binwidth = 0.02, alpha = 0.4) +
  scale_x_continuous(breaks=c(c(0.55,0.65,0.8)),
                     labels = c("    0.55", "    0.65", "     0.8")) + 
  #scale_x_log10() + 
  labs(x = expression(Cmax)) +
  geom_vline(xintercept=c(c(0.55,0.65,0.8))) +
  theme( axis.title.y = element_blank(), axis.text.x=element_text(angle=45, hjust=1, lineheight = 1, debug = FALSE))

p.histo.Cmax


library(cowplot)


distr.plot = plot_grid(
          p.histo.n, 

          p.histo.p, 

          p.histo.psurn,

          p.histo.Cmax,
          
          ncol = 4, nrow = 1)



# Newdstyle ----


df.histos = NULL
df.histo.n = data.frame(metafeaturevalue = exp(df.bmr.diff$logn), metafeaturename = "n")
df.histo.p = data.frame(metafeaturevalue = exp(df.bmr.diff$logp), metafeaturename = "p")
df.histo.psurn = data.frame(metafeaturevalue = exp(df.bmr.diff$logpsurn), metafeaturename = "psurn")
df.histo.cmax = data.frame(metafeaturevalue = df.bmr.diff$Cmax, metafeaturename = "cmax")
df.histos = rbind(df.histo.n,
                  df.histo.p,
                  df.histo.psurn,
                  df.histo.cmax)

levels(df.histos$metafeaturename) = c("n","p","p/n","Cmax")
  
df.histos$metafeaturename=as.factor(df.histos$metafeaturename)
head(df.histos)

p = ggplot(df.histos) + 
  geom_histogram(aes(x=metafeaturevalue)) + 
  scale_x_log10(expand=c(0.01, 0))+
  facet_grid(~metafeaturename, scales = "free_x", switch = "x")+
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())


# all ----
 
plotmeasures



plot.all = plot_grid(
  plotmeasures, distr.plot,

  
  ncol = 1, nrow = 2, rel_heights = c(2.2, 1))
plot.all

