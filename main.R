rm(list = ls())
set.seed(1)
library(mlr)
library(OpenML)
library(ggplot2)
library(snowfall)
library(cowplot)
options( java.parameters = "-Xmx8g" )
library(RWeka)

## I Benchmark Study

## I.1 Data Mining ----
  # Get the tasks from OpenML
  # Generates Data/OpenML/classifTasks.infos.RData
  # Remove non usefull datasets using the tasks attributes
  # Load all the datasets, test them, compute their dimension and computation time
  # Generates Data/OpenML/rf.timetrain.RData
  # Generates Data/Results/clas_time.RData
  # 329 datasets in total
source(file = "DataMining-Benchmark-Conversion/benchmark_dataMiningOpenML.R")
data_mining_OpenML(target_path = "Data/Results/clas_time_tiny.RData", force = FALSE, dataset_count = 220)

## I.2 Benchmark computation ----
  # Parallel computation for the benchmark, default is 10 cores
  # Generates Data/Results/benchmark_parallel_snowfall.RData
source(file = "DataMining-Benchmark-Conversion/benchmark_ParallelComputation.R")
load("Data/Results/clas_time_tiny.RData")
clas_used = rbind(clas_time_small, clas_time_medium)
parallel_computation_snowfall(nCores = 10, 
                              clas_used = clas_used,
                              target_path = "Data/Results/benchmark_parallel_snowfall_tiny.RData")


## I.3  Computation of Difference in Partial Dependance  ----
load("Data/Results/Original/clas_time_original.RData")
source("Simulations/PDPsPrinciple.R")
clas_used = clas_time[c(1:377),]
pdp_difference_allDatasets(clas_used, seed = 1, force = TRUE, visualize = FALSE)

load("Data/Results/Pdp_difference/Pdp_difference.RData")
load("Data/Results/benchmark_parallel_snowfall_tiny.RData")
load("Data/Results/df.bmr_tiny.RData")

pdp_difference_tiny = pdp_difference[c(1:147),]

df.bmr.diff$did = clas_used$did
clas_used

did_bmr = df.bmr.diff$did [which(df.bmr.diff$did  %in% pdp_difference_tiny$did)]
did_pdp = pdp_difference_tiny$did[which(pdp_difference_tiny$did  %in% df.bmr.diff$did)]

sort(did_bmr)==sort(did_pdp)
did.chosen = did_bmr


df.bmr.diff_tiny = df.bmr.diff[which(df.bmr.diff$did%in% did.chosen),]
pdp_difference_tiny = pdp_difference[which(pdp_difference$did%in% did.chosen),]

df.bmr.diff_tiny = df.bmr.diff_tiny[order(df.bmr.diff_tiny$did),]
pdp_difference_tiny = pdp_difference_tiny[order(pdp_difference_tiny$did),]

df = data.frame(df.bmr.diff_tiny, pdp_difference_tiny)

plot(df$pdp_l1, df$acc.test.mean, xlim = c(0,0.2))

## II Visualization
rm(list=ls())

## II.0 Preprocessing of the benchmark results ----
load(file = "Data/Results/benchmark_parallel_snowfall_tiny.RData")
source(file = "DataMining-Benchmark-Conversion/benchmark_resultsConversionNew.R")
convert_results(clas_used = clas_used, result = result, target_path = "Data/Results/df.bmr_tiny.RData")

## II.1 Overall Visualization ----
load(file = "Data/Results/df.bmr_tiny.RData")
source(file = "Visualization-Analysis/OverallVisualization.R")
overall_visualization(df.bmr.diff)

## II.2 Inclusion Criteria Visualization ----
source(file = "Visualization-Analysis/InclusionCriteriav2.R")
inclusion_criteria(df.bmr.diff)


## III. Simulations
rm(list=ls())

# Partial dependance plots
source("Simulations/PDPsPrinciple.R")
PartialDependancePlotExample()

# One Dataset simulations
# Enter id, values for n and p to test
