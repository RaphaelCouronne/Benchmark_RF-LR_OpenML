#################################
# Analysis Strobl et al. (2007) #
#################################

# Load the data

arabidopsis_url <- "https://static-content.springer.com/esm/art%3A10.1186%2F1471-2105-5-132/MediaObjects/12859_2004_248_MOESM1_ESM.txt"

arabidopsis <- read.table(arabidopsis_url, header = TRUE,
                          sep = " ", na.string = "X")

save(arabidopsis,file="arabidopsis.RData")
arabidopsis<-arabidopsis[complete.cases(arabidopsis),]
levels(arabidopsis$edit)<-c("0","1")


# Eliminate nucleotides -20 to -6 and 6 to 20
arabidopsis<-arabidopsis[,c(1,17:21,23:27,43:45)]

# Load mlr and tuneRanger
library(mlr)
library(tuneRanger)

# Define task for arabidopsis data
task = makeClassifTask(id = "arabidopsis", data = arabidopsis, target = "edit")

# Define classifiers
lrn.classif.lr = makeLearner("classif.logreg", id="lr", predict.type = "prob")
lrn.classif.lr_lasso = makeLearner("classif.cvglmnet", 
                                   id = "glm_lasso", predict.type = "prob", alpha = 1)
lrn.classif.lr_ridge = makeLearner("classif.cvglmnet", 
                                   id = "glm_ridge", predict.type = "prob", alpha = 0)
lrn.classif.rf = makeLearner("classif.randomForest", id="rf", predict.type = "prob")
lrn.classif.trf = makeLearner("classif.tuneRanger", id="trf",predict.type = "prob")

lrn.list = list(lrn.classif.lr,lrn.classif.lr_lasso,lrn.classif.lr_ridge,lrn.classif.rf,lrn.classif.trf)

# measures
measures = list(acc, brier, auc, timetrain)
# resampling
rdesc = makeResampleDesc("RepCV", folds = 5, reps = 10, stratify = TRUE)
# configure mlr
configureMlr(on.learner.error = "warn", show.learner.output = TRUE)

######################
# Run main benchmark #
######################
set.seed(1)
bmr = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)
save(bmr,file="One_dataset_analysis/bmr.RData")
load(file="One_dataset_analysis/bmr.RData")

###################################
# Run tuneRanger on whole dataset #
###################################
set.seed(1)
my.tunedranger<-tuneRanger(task,iters=500)

##############################
# Investigate effect of mtry #
##############################

lrn.classif.rf.mtry1 = makeLearner("classif.randomForest", id="rfmtry1", predict.type = "prob",par.vals = list(mtry=1))
lrn.classif.rf.mtry3 = makeLearner("classif.randomForest", id="rfmtry3", predict.type = "prob",par.vals = list(mtry=3))
lrn.classif.rf.mtry5 = makeLearner("classif.randomForest", id="rfmtry5", predict.type = "prob",par.vals = list(mtry=5))
lrn.classif.rf.mtry10 = makeLearner("classif.randomForest", id="rfmtry10", predict.type = "prob",par.vals = list(mtry=10))
lrn.classif.rf.mtry13 = makeLearner("classif.randomForest", id="rfmtry13", predict.type = "prob",par.vals = list(mtry=13))

lrn.classif.rf.nodesize2 = makeLearner("classif.randomForest", id="nodesize2", predict.type = "prob",par.vals = list(nodesize=2))
lrn.classif.rf.nodesize5 = makeLearner("classif.randomForest", id="nodesize5", predict.type = "prob",par.vals = list(nodesize=5))
lrn.classif.rf.nodesize10 = makeLearner("classif.randomForest", id="nodesize10", predict.type = "prob",par.vals = list(nodesize=10))
lrn.classif.rf.nodesize20 = makeLearner("classif.randomForest", id="nodesize20", predict.type = "prob",par.vals = list(nodesize=20))

lrn.classif.rf.sampsize50 = makeLearner("classif.randomForest", id="rfsampsize50", predict.type = "prob",par.vals = list(sampsize=round(0.5*nrow(arabidopsis))))
lrn.classif.rf.sampsize75 = makeLearner("classif.randomForest", id="rfsampsize75", predict.type = "prob",par.vals = list(sampsize=round(0.75*nrow(arabidopsis))))

lrn.list = list(lrn.classif.rf.mtry1,lrn.classif.rf.mtry3,lrn.classif.rf.mtry5,lrn.classif.rf.mtry10,lrn.classif.rf.mtry13,
                lrn.classif.rf.nodesize2,lrn.classif.rf.nodesize5,lrn.classif.rf.nodesize10,lrn.classif.rf.nodesize20,
                lrn.classif.rf.sampsize50,lrn.classif.rf.sampsize75)

set.seed(1)
bmr_tuning = benchmark(lrn.list, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)


###########################
# Run logistic regression #
###########################

my.glm<-glm(data=arabidopsis,formula=edit~.,family=binomial)
summary(my.glm)

####################################################
# Run random forest with defaults and compute VIMs #
####################################################
library(randomForest)
importance(randomForest(y=arabidopsis[,1],x=arabidopsis[,-1],importance=TRUE),type=1)


#################################
# Run ridge logistic regression #
#################################

lrn.classif.lr_cvglm = makeLearner("classif.cvglmnet", 
                                       id = "glmnet", predict.type = "prob", alpha = 0)

fit = train(learner = lrn.classif.lr_cvglm, task = task)
plot(fit$learner.model)
coef(fit$learner.model,s=fit$learner.model$lambda.min)

