## Initilialization

library(mlr)




## Function definitions



# The idea here is to have a measure for any dataset of the difference of interpretability between two models that can be coherent 
# for different method of machine learning. We choose here to use the partial dependance plots which can be defined
# for any machine learning model. We compute a "difference of models" by computing the summing the difference of 
# the 1D partial dependance plot with rf and lr model for each of the features of the dataset.


# ===================
# weightedPdpInterpretability
#
# notes : weightedPdpInterpretability compute the weighted L1 difference for all the partial dependance plots 
# The weights are defined using the empirical density of observations in each interval of the partial dependance data


weightedPdpDistance = function(task, gridsize) {
  
  # get the infos
  target = task$task.desc$target
  X.train = task$env$data[!colnames(task$env$data) %in% c(target)]
  
  # get the number and type of features
  type.list = sapply(X.train, class)
  features.list = names(X.train)
  nFeatures = length(features.list)
  
  # get the index of types
  index.numeric = which(type.list == "numeric")
  index.factor = which(type.list == "factor")
  features.list.numeric = features.list[index.numeric] 
  features.list.factor = features.list[index.factor] 
  nFeatures.numeric = length(features.list.numeric)
  nFeatures.factor = length(features.list.factor)
  
  # train the models
  lrn.classif.rf = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.rf = train(lrn.classif.rf, task)
  
  lrn.classif.lr = makeLearner("classif.logreg", predict.type = "prob", fix.factors.prediction = TRUE)
  fit.classif.lr = train(lrn.classif.lr, task)
  
  # launch for numeric features
  diff.abs.numeric.l1 = 0
  diff.abs.numeric.l2 = 0
  diff.abs.numeric.linf = 0
  
  if (nFeatures.numeric>0) {
    
    # create empty matrix to store the result of the difference of partial dependance datas between rf and lr
    pd.diff.numeric.l1 = matrix(data = 0, nrow = gridsize, ncol = nFeatures.numeric)
    pd.diff.numeric.l2 = matrix(data = 0, nrow = gridsize, ncol = nFeatures.numeric)
    pd.diff.numeric.linf = matrix(data = 0, nrow = gridsize, ncol = nFeatures.numeric)
    
    # for each numeric feature
    for (i in c(1:nFeatures.numeric)) {
      
      # get the temporary index and feature name
      index.temp = index.numeric[i]
      feature.temp = features.list[index.temp]
      
      # generate the partial dependance datas for both rf and lr models according to this feature
      pd.rf = generatePartialDependenceData(fit.classif.rf, task, features.list[index.temp], gridsize = gridsize)
      pd.lr = generatePartialDependenceData(fit.classif.lr, task, features.list[index.temp], gridsize = gridsize)
      
      plotPartialDependence(pd.rf)
      plotPartialDependence(pd.lr)
      
      #pd.lr = generatePartialDependenceData(fit.classif.lr, task, features.list[c(1,2)], 
      #                                      gridsize = gridsize, interaction = TRUE)
      
      #pd.rf = generatePartialDependenceData(fit.classif.rf, task, features.list[c(1,2)], 
      #                                      gridsize = gridsize, interaction = TRUE)
      
      #plotPartialDependence(pd.rf, geom = "tile")
      #plotPartialDependence(pd.lr, geom = "tile")
      


      feature_cardinality = length(pd.rf$data[[feature.temp]])
                                   
        
        # compute the difference of probability
        difference = pd.rf$data$Probability-pd.lr$data$Probability
        difference.l1 = abs(difference)
        difference.l2 = (difference)^2
        difference.linf = max(abs(difference))
        
        # compute the density of observation according to the chosen grid for the partial dependance datas
        bins = c(-Inf,pd.rf$data[[feature.temp]],Inf)
        empiricalFeature = task$env$data[[feature.temp]]
        histo = hist(empiricalFeature, breaks = bins, plot = FALSE)
        density = histo$counts/sum(histo$counts)
        weights = rep(NA, feature_cardinality)
        for (j in c(1:feature_cardinality)) {
          weights[j] = 1/2*(density[j]+density[j+1])
        }
        
        
        # reweight the difference with the density
        pd.diff.numeric.l1[,i] = difference.l1 * weights
        pd.diff.numeric.l2[,i] = difference.l2 * weights
        pd.diff.numeric.linf[,i] = difference.linf * weights

    }
    # sum the reweighted difference
    diff.abs.numeric.l1 = sum(apply(pd.diff.numeric.l1,2,function(x) sum(abs(x))))
    diff.abs.numeric.l2 = sum(apply(pd.diff.numeric.l2,2, function(x) sqrt(sum(x))))
    diff.abs.numeric.linf = max(apply(pd.diff.numeric.linf,2, max ))
  }
  
  
  # launch for factor features
  diff.abs.factor.l1 = 0
  diff.abs.factor.l2 = 0
  diff.abs.factor.linf = 0
  
  if (nFeatures.factor>0) {
    
    # create empty matrix to store the result of the difference of partial dependance datas between rf and lr
    # note : size of class changes so cant have fixed size for this matrix
    pd.diff.factor.l1 = matrix(data = NA, nrow = 1, ncol = nFeatures.factor) 
    pd.diff.factor.l2 = matrix(data = NA, nrow = 1, ncol = nFeatures.factor) 
    pd.diff.factor.linf = matrix(data = NA, nrow = 1, ncol = nFeatures.factor) 
    
    for (i in c(1:nFeatures.factor)) {
      
      # get the temporary index and feature name
      index.temp = index.factor[i]
      feature.temp = features.list[index.temp]
      
      # generate the partial dependance datas for both rf and lr models according to this feature
      pd.rf = generatePartialDependenceData(fit.classif.rf, task, features.list[index.temp])
      pd.lr = generatePartialDependenceData(fit.classif.lr, task, features.list[index.temp])
      
      # compute the density of observation according to several classes of this categorical feature
      summed = rep(NA, length(levels(task$env$data[[feature.temp]])))
      summed = sapply(pd.rf$data[[feature.temp]], function(x) length(which(task$env$data[[feature.temp]]==x)))
      density = summed / sum(summed)
      
      # compute the difference of probability
      difference = pd.rf$data$Probability-pd.lr$data$Probability
      difference.l1 = abs(difference)
      difference.l2 = (difference)^2
      difference.linf = max(abs(difference))
      
      pd.diff.factor.l1[i] = sum(difference.l1 * density)
      pd.diff.factor.l2[i] = sum(difference.l2 * density)
      pd.diff.factor.linf[i] = sum(difference.linf * density)
    }
    
    # reweight the difference with the density
    diff.abs.factor.l1 = sum (pd.diff.factor.l1)
    diff.abs.factor.l2 = sum (pd.diff.factor.l2)
    diff.abs.factor.linf = sum (pd.diff.factor.linf)
  }
  
  # sum the difference for numeric and categorical features
  diff.abs.l1 = diff.abs.numeric.l1 + diff.abs.factor.l1
  diff.abs.l2 = diff.abs.numeric.l2 + diff.abs.factor.l2
  diff.abs.linf = diff.abs.numeric.linf + diff.abs.factor.linf
  
  res<-NULL
  res$diff.abs.l1 = diff.abs.l1/nFeatures
  res$diff.abs.l2 = diff.abs.l2/nFeatures
  res$diff.abs.linf = diff.abs.linf
  
  out = data.frame(l1 = res$diff.abs.l1, l2 = res$diff.abs.l2, linf = res$diff.abs.linf)
  
  return(out)
}



