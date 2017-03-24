

benchmark_ResultsOverview = function(df.bmr.diff = df.bmr.diff) {

  
## Accuracy
print("Overview of acc results", quote = FALSE)
results.acc.diff = df.bmr.diff$acc.test.mean

# Ranks
print(paste0("  Random Forest has better acc performance than LR on ", 
            sum(results.acc.diff>=0), " of the ",
            length(results.acc.diff), " datasets that didn't produce nas (",
            format(round(sum(results.acc.diff>=0)/length(results.acc.diff), 5), nsmall = 5),"% of datasets)."), quote = FALSE)

# Mean value
print(paste("  Mean value of difference in acc is", 
            format(round(mean(results.acc.diff), 5), nsmall = 5)
            ), quote = FALSE)


# Bootstrap confidence interval
library(boot)
meanFunc <- function(x,i){mean(x[i])}
boot_obj = boot(results.acc.diff, statistic = meanFunc, R = 2000)

res.boot.ci = boot.ci(boot_obj, conf = c(0.90, 0.95),
        type = c("norm", "basic", "perc", "bca"))

print(paste0("  95% (Bca) bootstrap confidence interval for the mean is [",
             format(round(res.boot.ci$bca[2,4], 5), nsmall = 5),
             ", ",
             format(round(res.boot.ci$bca[2,5], 5), nsmall = 5),
             "]"), quote = FALSE)

## Auc
print("Overview of auc results", quote = FALSE)
results.auc.diff = df.bmr.diff$auc.test.mean

# Ranks
print(paste0("  Random Forest has better auc performance than LR on ", 
             sum(results.auc.diff>=0), " of the ",
             length(results.auc.diff), " datasets that didn't produce nas (",
             format(round(sum(results.auc.diff>=0)/length(results.auc.diff), 5), nsmall = 5),"% of datasets)."), quote = FALSE)

# Mean value
print(paste("  Mean value of difference in auc is", 
            format(round(mean(results.auc.diff), 5), nsmall = 5)
), quote = FALSE)


# Bootstrap confidence interval
library(boot)
meanFunc <- function(x,i){mean(x[i])}
boot_obj = boot(results.auc.diff, statistic = meanFunc, R = 2000)

res.boot.ci = boot.ci(boot_obj, conf = c(0.90, 0.95),
                      type = c("norm", "basic", "perc", "bca"))

print(paste0("  95% (Bca) bootstrap confidence interval for the mean is [",
             format(round(res.boot.ci$bca[2,4], 5), nsmall = 5),
             ", ",
             format(round(res.boot.ci$bca[2,5], 5), nsmall = 5),
             "]"), quote = FALSE)


## Brier
print("Overview of brier results", quote = FALSE)
results.brier.diff = df.bmr.diff$brier.test.mean

# Ranks
print(paste0("  Random Forest has better brier performance than LR on ", 
             sum(results.brier.diff<=0), " of the ",
             length(results.brier.diff), " datasets that didn't produce nas (",
             format(round(sum(results.brier.diff<=0)/length(results.brier.diff), 5), nsmall = 5),"% of datasets)."), quote = FALSE)

# Mean value
print(paste("  Mean value of difference in brier is", 
            format(round(mean(results.brier.diff), 5), nsmall = 5)
), quote = FALSE)


# Bootstrap confidence interval
library(boot)
meanFunc <- function(x,i){mean(x[i])}
boot_obj = boot(results.brier.diff, statistic = meanFunc, R = 2000)

res.boot.ci = boot.ci(boot_obj, conf = c(0.90, 0.95),
                      type = c("norm", "basic", "perc", "bca"))

print(paste0("  95% (Bca) bootstrap confidence interval for the mean is [",
             format(round(res.boot.ci$bca[2,4], 5), nsmall = 5),
             ", ",
             format(round(res.boot.ci$bca[2,5], 5), nsmall = 5),
             "]"), quote = FALSE)


}

