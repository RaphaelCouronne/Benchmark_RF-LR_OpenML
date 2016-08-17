## Example 1 - Multi-core on a single computer
sink('SnowFallExample.Rout', split=TRUE)
.Platform
.Machine
R.version
Sys.info()

library(snowfall) 
# 1. Initialisation of snowfall. 
# (if used with sfCluster, just call sfInit()) 
sfInit(parallel=TRUE, cpus=2)

# 2. Loading data. 
require(mvna) 
data(sir.adm) 

# 3. Wrapper, which can be parallelised. 
wrapper <- function(idx) { 
  # Output progress in worker logfile 
  cat( "Current index: ", idx, "\n" ) 
  index <- sample(1:nrow(sir.adm), replace=TRUE) 
  temp <- sir.adm[index, ] 
  fit <- crr(temp$time, temp$status, temp$pneu) 
  return(fit$coef) 
} 

# 4. Exporting needed data and loading required 
# packages on workers. 
sfExport("sir.adm") 
sfLibrary(cmprsk) 

# 5. Start network random number generator 
# (as "sample" is using random numbers). 
sfClusterSetupRNG() 

# 6. Distribute calculation
start <- Sys.time(); result <- sfLapply(1:1000, wrapper) ; Sys.time()-start
# Result is always in list form. 
mean(unlist(result)) 

# 7. Stop snowfall 
sfStop() 
