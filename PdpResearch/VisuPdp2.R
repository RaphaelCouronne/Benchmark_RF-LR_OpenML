rm(list=ls())

load(file = "Data/df.all.RData")

names(df.all)

plot(df.all$difference.imp.weight.all, df.all$acc.test.mean)

hist(log(df.all$difference.imp.weight.all))
plot(log(df.all$difference.imp.weight.all), df.all$acc.test.mean)

plot(df.all$difference.fv.weight.all, df.all$acc.test.mean)


## Which datasets do you want to study ?
data.id = df.all$data.id[order(df.all$difference.imp.weight.all)][length(df.all$data.id)]
data.id

# Plot position
id = which(df.all$data.id==data.id)
plot(df.all$difference.imp.weight.all, df.all$acc.test.mean)
points(df.all$difference.imp.weight.all[id], df.all$acc.test.mean[id], col = "red", pch = 15)

# Apply function
res2 = lapply(rep(data.id,20), testPdp10Robustesse)

testPdp10Robustesse(data.id)

res2
res2.df = do.call("rbind", res2) 
res2.df
hist(res2.df$difference.imp.weight.all)


hist(res2.df$difference)
hist(res2.df$difference.weight)
hist(res2.df$difference.imp.all)
hist(res2.df$difference.imp.weight.all)
hist(res2.df$difference)f
hist(res2.df$difference.fv.all)
hist(res2.df$difference.fv.weight.all)
hist(res2.df$difference.top1)
hist(res2.df$differene.weight.top1)
hist(res2.df$difference.top3)
hist(res2.df$difference.weight.top3)


## Study of computation of partial difference



# What happens

# WTF high variance 928, 887, 747
# High variance 444, 448, 50, 724
# Low variance 818



data.id = 448

omldataset = getOMLDataSet(data.id)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
task

res2 = lapply(omldatasets[c(16:40)], ComputeDependenceDifference)
res2.df = do.call("rbind", res2) 
res2.df


hist(data.matrix(res2.df[3,]))

