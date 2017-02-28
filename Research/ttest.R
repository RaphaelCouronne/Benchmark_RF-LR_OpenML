load(file = "df.bmr_original.RData")

perfs_lr = subset(res.perfs.df, learner.id == "classif.logreg")
perfs_rf = subset(res.perfs.df, learner.id == "classif.randomForest")
n = length(perfs_rf$acc.test.mean)

## Mesures donnees

# acc
acc_diff = df.bmr.diff$acc.test.mean
acc_lr = perfs_lr$acc.test.mean
acc_rf = perfs_rf$acc.test.mean

# auc
auc_diff = df.bmr.diff$auc.test.mean
auc_lr = perfs_lr$auc.test.mean
auc_rf = perfs_rf$auc.test.mean

# brier
brier_diff = df.bmr.diff$brier.test.mean
brier_lr = perfs_lr$brier.test.mean
brier_rf = perfs_rf$brier.test.mean


## plot et tests de normalité

# acc
shapiro.test(acc_diff)
shapiro.test(acc_lr)
shapiro.test(acc_rf)


hist(acc_diff, breaks = 20)
hist(acc_lr, breaks = 20)
hist(acc_rf, breaks = 20)

# auc
shapiro.test(auc_diff)
shapiro.test(auc_lr)
shapiro.test(auc_rf)


hist(auc_diff, breaks = 20)
hist(auc_lr, breaks = 20)
hist(auc_rf, breaks = 20)

# brier
shapiro.test(brier_diff)
shapiro.test(brier_lr)
shapiro.test(brier_rf)


hist(brier_diff, breaks = 20)
hist(brier_lr, breaks = 20)
hist(brier_rf, breaks = 20)



## Intervalle confiance esperance diffence RF LR

# acc
mu = mean(acc_diff)
variance = var(acc_diff)
sigma = sqrt(variance)


histogram = hist(acc_diff, breaks = 20, freq = TRUE)

x = seq(min(acc_diff), max(acc_diff), length.out = 30) 
y = dnorm(x = x,mean = mu, sd = sigma)*diff(histogram$mids[1:2])*n
lines(x,y,col="red")

interval_lower = mu - 1.96*sigma/sqrt(n)
interval_upper = mu + 1.96*sigma/sqrt(n)

lines(c(interval_lower,interval_lower),c(0,100), col = "blue")
lines(c(interval_upper,interval_upper),c(0,100), col = "blue")


# auc
mu = mean(auc_diff)
variance = var(auc_diff)
sigma = sqrt(variance)


histogram = hist(auc_diff, breaks = 20, freq = TRUE)

x = seq(min(auc_diff), max(auc_diff), length.out = 30) 
y = dnorm(x = x,mean = mu, sd = sigma)*diff(histogram$mids[1:2])*n
lines(x,y,col="red")

interval_lower = mu - 1.96*sigma/sqrt(n)
interval_upper = mu + 1.96*sigma/sqrt(n)

lines(c(interval_lower,interval_lower),c(0,100), col = "blue")
lines(c(interval_upper,interval_upper),c(0,100), col = "blue")


# brier
mu = mean(brier_diff)
variance = var(brier_diff)
sigma = sqrt(variance)


histogram = hist(brier_diff, breaks = 20, freq = TRUE)

x = seq(min(brier_diff), max(brier_diff), length.out = 30) 
y = dnorm(x = x,mean = mu, sd = sigma)*diff(histogram$mids[1:2])*n
lines(x,y,col="red")

interval_lower = mu - 1.96*sigma/sqrt(n)
interval_upper = mu + 1.96*sigma/sqrt(n)

lines(c(interval_lower,interval_lower),c(0,100), col = "blue")
lines(c(interval_upper,interval_upper),c(0,100), col = "blue")

# t test echantillons apparies
wilcox.test(acc_rf, acc_lr, paired = TRUE)
wilcox.test(auc_rf, auc_lr, paired = TRUE)
wilcox.test(brier_rf, brier_lr, paired = TRUE)

# t test aussi
t.test(acc_rf, acc_lr, paired=TRUE)
t.test(auc_rf, auc_lr, paired=TRUE)
t.test(brier_rf, brier_lr, paired=TRUE)

# calcul puissance 
