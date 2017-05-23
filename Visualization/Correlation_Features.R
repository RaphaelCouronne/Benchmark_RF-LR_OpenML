rm(list = ls())
load(file = "Data/Results/df.bmr.RData")


df.features = subset(df.bmr.diff, select = c("logp",
                                   "logn",
                                   "logdimension",
                                   "logpsurn",
                                   "logdimensionsurn",
                                   "pnum",
                                   "psymbolic",
                                   "pnumrate",
                                   "Cmax"))


correlationPearson = cor(df.features)
correlationSpearman = cor(df.features, method = "spearman")


correlationPearsonabs = 1-abs(correlationPearson)
correlationSpearmanabs = 1-abs(correlationSpearman)

plot(hclust(as.dist(correlationPearsonabs), method = "single"))
plot(hclust(as.dist(correlationSpearmanabs), method = "average"))

hclust(correlationPearsonabs)

library(Hmisc)
rcorr(perfsAggr.diff, type=" ") # type can be pearson or spearman

# feature selection
