rm(list = ls())
OS = "win"
source(file = "benchmark_defs.R")

## Load and convert the reasults to a data frame ----
load( file = "../Data_BenchmarkOpenMl/Final/Results/Windows/benchmark_results_snow_small-medium-allLearnersFoctor_strat_All.RData")
load(file = "../Data_BenchmarkOpenMl/Final/DataMining/clas_time.RData")


## That you can do without loading the datasets ----

# number  of features
p = clas_used$NumberOfFeatures

# number of numeric attributes
pnum = clas_used$NumberOfNumericFeatures

# number of categorical attributes
psymbolic = clas_used$NumberOfSymbolicFeatures

# number of samples
n = clas_used$NumberOfInstances

# n/p
psurn = p/n

# Numerical attributes rate
pnumrate = pnum/p

# Nominal attributes rate
psymbolicrate = psymbolic/p

# %Cmin Percentage of elements of the minority class
Cmin = clas_used$MinorityClassSize/n

# %Cmax Percentage of elements of the majority class
Cmax = clas_used$MinorityClassSize/n


## My personal meta features ----


lengthResult = length(result)
brierreglog = rep(NA, length.out = lengthResult)

for (i in c(1:lengthResult)) {
  try({
    datatemp = result[[i]]
    datatemptaggr = getBMRAggrPerformances(datatemp, as.df = TRUE)
    brierreglog[i] = datatemptaggr$brier.test.mean[which(datatemptaggr$learner.id=="classif.logreg")]
  })
}
brierreglog

## Need to load the datasets ----

# test on one dataset



#SymMin Minimum number of levels of the symbolic attributes
#SymMax Maximum number of levels of the symbolic attributes
#SymAvg Average number of levels of the symbolic attributes
#SymSd Standard deviation of levels of the symbolic attributes
#SymSum Total number of levels of the symbolic attributes




# Creer un decision tree !