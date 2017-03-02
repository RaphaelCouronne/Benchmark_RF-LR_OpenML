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

# Enter here nCores and myapikey
nCores = 3 # number of cores you want to use
myapikey = "7a4391537f767ea70db6af99497653e5" #OpenML API key
saveOMLConfig(apikey = myapikey, arff.reader = "RWeka", overwrite=TRUE)








## 1 Benchmark Study ======================================================================================

## I.1 Data Mining ----
# Get the tasks from OpenML
# Generates Data/OpenML/df.infos.RData which gives information about the processing of the datasets
# Generates Data/Results/clas_time.RData which contains information about our dataset pool
# 
# Options
# force = TRUE to force (re)computing of ALL dataset informations
# computeTime = TRUE to compute an estimate of training time for LR and RF. It may take up to several days
source(file = "Benchmark/benchmark_getDataOpenML.R")
get_data_OpenML(target_path = "Data/OpenML/clas_time.RData")


## 1.2 Benchmark computation ----
# Parallel computation for the benchmark
# Generates Data/Results/benchmark_parallel_snowfall.RData and Data/Results/benchmark_parallel_snowfall.Rout
source(file = "Benchmark/benchmark_ParallelComputation.R")
load("Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

parallel_computation_snowfall(nCores = nCores, 
                              clas_used = clas_used,
                              target_path = "Data/Results/benchmark_parallel_snowfall.RData")






## 2 Visualization  ======================================================================================
rm(list=ls())
 
# 2.1 Preprocessing of the benchmark results
load(file = "Data/Results/benchmark_parallel_snowfall.RData")
source(file = "Benchmark/benchmark_resultConversion.R")
convert_results(clas_used = clas_used, result = result, target_path = "Data/Results/df.bmr.RData")

# 2.2 Overall Visualization
load(file = "Data/Results/df.bmr.RData")
source(file = "Visualization-Analysis/OverallVisualization.R")
overall_visualization(df.bmr.diff)

# 2.3 Inclusion Criteria Visualization
source(file = "Visualization-Analysis/InclusionCriteriaPlots.R")
inclusion_criteria(df.bmr.diff)




## 3. Simulations  ======================================================================================
rm(list=ls())

# 3.1 Subset analysis on 1 dataset
source("Simulations/Dataset_SubsetAnalysis.R")
load(file = "Data/OpenML/clas_time.RData")
clas_used = rbind(clas_time_small, clas_time_medium, clas_time_big)

subsetAnalysis_computeParallel(clas_used, nCores = nCores)
subsetAnalysis_visualization()


# 3.2 Partial dependance plots simulations
source("Simulations/PDP_ExampleSimulations.R")
PlotPartialDependanceExample()

# 3.3  Computation of Difference in Partial Dependance
source("Simulations/PartialDependance_difference.R")
pdpDifferenceAllDatasets(clas = clas_used, visualize = FALSE, force = FALSE,
                         target.path = "Data/Simulations/pdp.difference.RData") 




## TODO

# Sauver les plots dans un dossier aussi au fur et à mesure


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

