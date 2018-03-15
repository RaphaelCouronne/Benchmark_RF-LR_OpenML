# ----
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
library(gridExtra)
library(cowplot)
library(doParallel)
library(tuneRanger)

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
source(file = "Benchmark/benchmark_getData_OpenML.R")
get_data_OpenML(target_path = "Data/OpenML/clas_time.RData", force = FALSE, computeTime = FALSE)

## ----
plot(log10(clas_time$time), log10(clas_time$number.of.features*clas_time$number.of.instances))
plot(log10(clas_time$number.of.features*clas_time$number.of.instances), clas_time$time, ylim = c(0,2000))
lines(c(-3,5),c(6,6))
lines(c(2,2),c(0,10))
title("Computation time vs n*p, log scale")

plot(log10(clas_time$number.of.features*clas_time$number.of.instances), clas_time$time, ylim = c(0,2000))
title("Computation time vs log(n*p)")
lines(c(0,10),c(100,100))
lines(c(6,6),c(0,3000))


## 1.2 Benchmark computation ---

###########################################
########## High Computation time ##########
###########################################

# Batchtools implementation
source(file = "Benchmark/benchmark_batchtools.R")
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)
clas_used = rbind(clas_time_small)[1:10,]

# Set up the benchmark (delete current results)
setBatchtoolsExperiment(seed = 1, ncpus = nCores, clas_used = clas_used)
regis = loadRegistry("Data/Results/Batchtools/batchtool_benchmark//", writeable = TRUE)

# Launch benchmark
submitJobs(ids = 1:193, reg = regis) #small datasets
submitJobs(ids = 194:231, reg = regis) #medium datasets
submitJobs(ids = 232:278, reg = regis) #big datasets

# Check benchmark
getStatus()


## 2 Visualization  ======================================================================================
 
# 2.1 Conversion of the benchmark results
regis = loadRegistry("Data/Results/Batchtools/batchtool_benchmark//", writeable = TRUE)
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)
clas_used = rbind(clas_time_small)[1:10,]
source(file = "Benchmark/benchmark_Results_Conversion.R")
convert_results(clas_used = clas_used, regis = regis, target_path = "Data/Results/df_bmr.RData")

# 2.2 Overall Visualization
load(file = "Data/Results/df_bmr.RData")
source(file = "Visualization/Overall_Visualization.R")
overall_visualization(df.bmr.diff)

# 2.3 Inclusion Criteria Visualization
load(file = "Data/Results/df_bmr.RData")
source(file = "Visualization/Inclusion_Criteria_Plots.R")
inclusion_criteria(df.bmr.diff)


## 3. Analysis  ======================================================================================

# 3.1 Overall results
load(file = "Data/Results/df_bmr.RData")
source(file = "Benchmark/benchmark_Results_Overview.R")
benchmark_ResultsOverview(df.bmr.diff, res.perfs.df)

# 3.2 Meta Learning
load(file = "Data/Results/df_bmr.RData")
source(file = "Benchmark/benchmark_Results_MetaLearning.R")
ResultsMetaLearning(df.bmr.diff)




## 4. Simulations  ======================================================================================

# 4.1 Subset analysis on 1 dataset

# 4.1.1 Computation

###########################################
########## High Computation time ##########
###########################################

source("Simulations/Dataset_Subset_Analysis.R")
load(file = "Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

subsetAnalysis_computeParallel(clas_used, nCores = nCores)

# 4.1.2 Visualization

subsetAnalysis_visualization()


# 4.2 Partial dependance plots simulations
source("Simulations/PDP_Example_Simulations.R")
PlotPartialDependanceExample()





## ===========================
##                           =
## Additional files/code     =
##                           =
## ===========================


## 5 Study of partial difference plots  ======================================================================================

## 5.1 Computation of the difference in Partial Dependence ---

###########################################
########## High Computation time ##########
###########################################

# Batchtools implementation
source(file = "Additional_Files/PartialDependence_Batchtools.R")
load("Data/Results/df_bmr.RData")
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

# Set up the benchmark (delete current results)

setBatchtoolsPDPExperiment(seed = 1, ncpus = 3, clas_used = clas_used)
regis.pdp = loadRegistry("Data/Results/Batchtools/batchtool_PartialDependance///")

# Launch benchmark
submitJobs(ids = 1:193, reg = regis.pdp) #small datasets
submitJobs(ids = 194:231, reg = regis.pdp) #medium datasets
submitJobs(ids = 232:278, reg = regis.pdp) #big datasets

# Check benchmark
getStatus()



## 5.2 Visualization of the results

# requires
load("Data/Results/df_bmr.RData")
regis.pdp = loadRegistry("Data/Results/Batchtools/batchtool_PartialDependance///")
source("Additional_Files/PartialDependence_Extreme_Cases.R")


partialDependenceAnalysis_extremCases()










