rm(list = ls())

# If possible increase memory used
options( java.parameters = "-Xmx8g" )

# Check that all packages can be loaded
library(mlr)
library(OpenML)
library(ggplot2)
library(snowfall)
library(cowplot)
library(RWeka) 
library(doParallel)
library(batchtools)

# Enter here nCores and myapikey
nCores = 3 # number of Cpus you want to use
myapikey = "7a4391537f767ea70db6af99497653e5" # OpenML API key
saveOMLConfig(apikey = myapikey, arff.reader = "RWeka", overwrite=TRUE)





## 1 Benchmark Study ======================================================================================

## 1.1 Data Mining ----
# Get the tasks from OpenML
# Generates Data/OpenML/df.infos.RData which gives information about the processing of the datasets
# Generates Data/Results/clas_time.RData which contains information about our dataset pool
# 
# Options
# force = TRUE to force (re)computing of ALL dataset informations
# computeTime = TRUE to compute an estimate of training time for LR and RF. It may take up to several days
source(file = "Benchmark/benchmark_getDataOpenML.R")
get_data_OpenML(target_path = "Data/OpenML/clas_time.RData", force = FALSE, computeTime = FALSE)


## 1.2 Benchmark computation ---
# Batchtools implementation
source(file = "Benchmark/benchmark-batchtools.R")
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

# Set up the benchmark (delete current results)
setBatchtoolsExperiment(seed = 1, ncpus = nCores, clas_used = clas_used)
regis = loadRegistry("Data/Results/Batchtools/batchtool_experiment//")

# Launch benchmark
submitJobs(ids = 1:40, reg = regis) #small datasets
submitJobs(ids = 41:193, reg = regis) #small datasets
submitJobs(ids = 194:231, reg = regis) #medium datasets
submitJobs(ids = 232:278, reg = regis) #big datasets

# Check benchmark
getStatus()


## 2 Visualization  ======================================================================================
rm(list=ls())
 
# 2.1 Conversion of the benchmark results
regis = loadRegistry("Data/Results/Batchtools/batchtool_experiment//")
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)
source(file = "Benchmark/benchmark_resultConversion.R")
convert_results(clas_used = clas_used, result = result, target_path = "Data/Results/df.bmr.RData")

# 2.2 Overall Visualization
load(file = "Data/Results/df.bmr.RData")
source(file = "Visualization/OverallVisualization.R")
overall_visualization(df.bmr.diff)

# 2.3 Inclusion Criteria Visualization
load(file = "Data/Results/df.bmr.RData")
source(file = "Visualization/InclusionCriteriaPlots.R")
inclusion_criteria(df.bmr.diff)


## 3. Analysis  ======================================================================================
rm(list=ls())

# 3.1 Overall results
load(file = "Data/Results/df.bmr.RData")
source(file = "Benchmark/benchmark_ResultsOverview.R")
benchmark_ResultsOverview(df.bmr.diff, res.perfs.df)

# 3.2 Meta Learning
load(file = "Data/Results/df.bmr.RData")
source(file = "Benchmark/benchmark-ResultsMetaLearning.R")
ResultsMetaLearning(df.bmr.diff)

## 4. Simulations  ======================================================================================
rm(list=ls())

# 4.1 Subset analysis on 1 dataset
source("Simulations/Dataset_SubsetAnalysis.R")
load(file = "Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

subsetAnalysis_computeParallel(clas_used, nCores = nCores)
subsetAnalysis_visualization()


# 4.2 Partial dependance plots simulations
source("Simulations/PDP_ExampleSimulations.R")
PlotPartialDependanceExample()

# 4.3  Computation of Difference in Partial Dependance
source("Simulations/PartialDependance_difference.R")
pdpDifferenceAllDatasets(clas = clas_used, visualize = FALSE, force = FALSE,
                         target.path = "Data/Simulations/pdp.difference.RData") 



## 5 Study of partial difference plots  ======================================================================================
rm(list=ls())

## 5.1 Computation of the difference in Partial Dependence ---
# Batchtools implementation
source(file = "PdpResearch/batchtools-pd.R")
load("Data/Results/df.bmr.RData")

# Set up the benchmark (delete current results)
setBatchtoolsPDPExperiment(seed = 1, ncpus = 3, clas_used = clas_used)
regis.pdp = loadRegistry("Data/Results/Batchtools/batchtool_PartialDependance///")

# Launch benchmark
testJob(id = 1)

submitJobs(ids = 1:260, reg = regis.pdp) #small datasets

submitJobs(ids = 220:240, reg = regis.pdp) #small datasets
submitJobs(ids = 194:231, reg = regis.pdp) #medium datasets
submitJobs(ids = 232:278, reg = regis.pdp) #big datasets

# Check benchmark
status = getStatus()
status
getErrorMessages()

## 5.2 Visualization of the results
ids = 1:259
ids = ids[-which(ids %in% c(14))]

result.pdp = reduceResultsList(ids = ids, reg = regis.pdp)
result.pdp.df = do.call("rbind", result.pdp) 


for (i in c(5:17)) {
  plot(data.frame(result.pdp.df)[ ,i], result.pdp.df$diff.acc)
}

Partiaresult.pdp = result.pdp.df

plot(result.pdp$pred.proba.diff.l1, result.pdp$diff.acc)
plot(log(result.pdp$pred.proba.diff.l2), result.pdp$diff.acc)
plot(result.pdp$difference, result.pdp$diff.acc)
plot(result.pdp$difference.weight, result.pdp$diff.acc)
plot(result.pdp$difference.imp.all, result.pdp$diff.acc)
plot(result.pdp$difference.imp.weight.all, result.pdp$diff.acc)
plot(result.pdp$difference.fv.all, result.pdp$diff.acc)
plot(result.pdp$difference.fv.weight.all, result.pdp$diff.acc)
plot(result.pdp$difference.top1, result.pdp$diff.acc)
plot(result.pdp$differene.weight.top1, result.pdp$diff.acc)
plot(result.pdp$difference.top3, result.pdp$diff.acc)
plot(result.pdp$difference.weight.top3, result.pdp$diff.acc)
plot(result.pdp$difference.imp.top3, result.pdp$diff.acc)
plot(result.pdp$difference.imp.weight.top3, result.pdp$diff.acc)



## 5.3  study of 3 datasets with their difference in model and acc

clas_used.pdp = clas_used[ids,]

df.all = data.frame(result.pdp.df, data.id = clas_used.pdp$data.id, 
                    n = clas_used.pdp$number.of.instances,
                    p = clas_used.pdp$number.of.features,
                    meanAcc = df.all$rf.acc+df.all$lr.acc,
                    logpn = log(clas_used.pdp$number.of.features/clas_used.pdp$number.of.instances))

data.id.highDiff = df.all$data.id[order(df.all$difference.imp.weight.all)][length(df.all$data.id)-1] # max diff model
data.id.lowDiff = df.all$data.id[order(df.all$difference.imp.weight.all)][1] # min diff model
data.id.highAcc = df.all$data.id[order(df.all$diff.acc)][length(df.all$diff.acc)-3] # max diff acc


id.highDiff = which(df.all$data.id==data.id.highDiff)
id.lowDiff = which(df.all$data.id==data.id.lowDiff)
id.highAcc = which(df.all$data.id==data.id.highAcc)

plot(df.all$difference.imp.weight.all, df.all$diff.acc)
points(df.all$difference.imp.weight.all[id.highDiff], df.all$diff.acc[id.highDiff], col = "red", pch = 15)
points(df.all$difference.imp.weight.all[id.lowDiff], df.all$diff.acc[id.lowDiff], col = "red", pch = 15)
points(df.all$difference.imp.weight.all[id.highAcc], df.all$diff.acc[id.highAcc], col = "red", pch = 15)


# High acc 1460
data.id = data.id.highAcc
omldataset = getOMLDataSet(data.id)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
task


ComputeDependenceDifference(data.id)

plot_extrem_cases(data.id)



# Low modele

data.id = data.id.lowDiff
omldataset = getOMLDataSet(data.id)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
task


ComputeDependenceDifference(data.id)

plot_extrem_cases(data.id)



# High modele 1479

data.id = data.id.highDiff
data.id
omldataset = getOMLDataSet(data.id)
if (identical(omldataset$target.features, character(0))) {
  omldataset$target.features="Class"
  omldataset$desc$default.target.attribute="Class"
}
task = convertOMLDataSetToMlr(omldataset)
task


ComputeDependenceDifference(data.id)

plot_extrem_cases(data.id)



# plot des p et diff proba contre diff acc ?
df.all$logn = log(df.all$n)

detach(package:reshape2, unload = TRUE)
p = ggplot(df.all, aes_string(y = "logn", x="difference.imp.weight.all", colour = "logn"))
p = p + geom_point(size=3)
plot(p)

p = p+geom_line(size=1) + geom_point(size=3) + ylim(0,1) +
  xlab(features.list[index.temp]) + 
  ylab("Probability") +
  ggtitle(paste("Relative importance :", format(round(permutation.percentage[index.temp], 3), nsmall = 3),
                "Difference :", format(round(df_diff_pdp, 3), nsmall = 3),
                "Difference*Importance", format(round(df_diff_pdp*permutation.percentage[index.temp], 3), nsmall = 3)))
print(p)


# Old
# 1075 for max diff model

# 745 max diff acc

# 835 min acc


# 923 min diff

