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

# Create an account in OpenML and use the R apikey
myapikey = "7a4391537f767ea70db6af99497653e5"
saveOMLConfig(apikey = myapikey, arff.reader = "RWeka", overwrite=TRUE)

## I Benchmark Study

## I.1 Data Mining ----
  # Get the tasks from OpenML
  # Generates Data/OpenML/classifTasks.infos.RData
  # Remove non usefull datasets using the tasks attributes
  # Load all the datasets, test them, compute their dimension and computation time
  # Generates Data/OpenML/df.infos.RData which gives information about the processing of the datasets
  # Generates Data/Results/clas_time.RData which 
  # 326 datasets in total
source(file = "DataMining-Benchmark-Conversion/benchmark_getDataOpenML.R")
get_data_OpenML(target_path = "Data/Results/clas_time.RData", force = FALSE, seed = 1)

## I.2 Benchmark computation ----
  # Parallel computation for the benchmark, default is 7 cores
  # Generates Data/Results/benchmark_parallel_snowfall.RData
source(file = "DataMining-Benchmark-Conversion/benchmark_ParallelComputation.R")
load("Data/Results/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)
clas_used = clas_used
parallel_computation_snowfall(nCores = 7, 
                              clas_used = clas_used,
                              target_path = "Data/Results/benchmark_parallel_snowfall.RData",
                              seed = 1)


## I.3  Computation of Difference in Partial Dependance  ----
load("Data/Results/Original/clas_time_original.RData")
source("Simulations/Difference_Modele.R")
clas_used = rbind(clas_time_small, clas_time_medium)
pdp_difference_allDatasets(clas_used, seed = 1, force = TRUE, visualize = FALSE, dataset_count = 20,
                           target_path = "Data/Results/Pdp_difference/Pdp_difference.RData")

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

# study of 3 datasets with their difference in model and acc
source("Simulations/Extrem_cases_pdp.R")
source("Simulations/Difference_Modele.R")

# id = 230 did = 1471
# Delta acc high / Delta Model High
plot_extrem_cases(id = 230, seed = 1)

# id = 171 id = 1068
# Delta acc low / Delta Model High
plot_extrem_cases(id = 171, seed = 1)

# id = 210 did = 725
# Delta acc low / Delta Model low
plot_extrem_cases(id = 210, seed = 1)


# One Dataset simulations
# Enter id, values for n and p to test
