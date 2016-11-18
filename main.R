rm(list = ls())
set.seed(1)
library(mlr)
library(OpenML)
library(ggplot2)
library(snowfall)
library(cowplot)
library(RWeka)

## I Benchmark Study

## I.1 Data Mining ----
  # Get the tasks from OpenML
  # Generates Data/OpenML/classifTasks.infos.RData
  # Remove non usefull datasets using the tasks attributes
  # Load all the datasets, test them, compute their dimension and computation time
  # Generates Data/OpenML/rf.timetrain.RData
  # Generates Data/Results/clas_time.RData
source(file = "DataMining-Benchmark-Conversion/benchmark_dataMiningOpenML.R")
data_mining_OpenML(target_path = "Data/Results/clas_time.RData")

## I.2 Benchmark computation ----
  # Parallel computation for the benchmark, default is 10 cores
  # Generates Data/Results/benchmark_parallel_snowfall.RData
source(file = "DataMining-Benchmark-Conversion/benchmark_ParallelComputation.R")
clas_time = load("Data/Results/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium)[c(1:80),]
parallel_computation_snowfall(nCores = 10, 
                              clas_used = clas_used,
                              target_path = "Data/Results/benchmark_parallel_snowfall.RData")


## II Visualization
rm(list=ls())

## II.0 Preprocessing of the benchmark results ----
load(file = "Data/Results/benchmark_parallel_snowfall.RData")
source(file = "DataMining-Benchmark-Conversion/benchmark_resultsConversionNew.R")
convert_results(clas_used = clas_used, result = result, target_path = "Data/Results/df.bmr.RData")

## II.1 Overall Visualization ----
load(file = "Data/Results/df.bmr.RData")
source(file = "Visualization-Analysis/OverallVisualization.R")
overall_visualiaztion(df.bmr.diff)

## II.2 Inclusion Criteria Visualization ----

## II.3 Meta-Learning Visualization ----


