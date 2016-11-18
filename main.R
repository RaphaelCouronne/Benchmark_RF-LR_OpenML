rm(list = ls())
set.seed(1)

## I Benchmark Study

## I.1 Data Mining ----
# Get the tasks from OpenML
# Remove non usefull datasets using the tasks attributes
# Load all the datasets, test them, compute their dimension and computation time
source(file = "DataMining-Benchmark-Conversion/benchmark_dataMiningOpenML.R")

## I.2 Benchmark computation ----
# Prallel computatin for the benchmark, default is 10 cores
source(file = "DataMining-Benchmark-Conversion/benchmark_ParallelComputation.R")

## II Visualization

## II.1 Overall Visualization ----

## II.2 Inclusion Criteria Visualization ----

## II.3 Meta-Learning Visualization ----


