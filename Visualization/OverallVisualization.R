
overall_visualization<-function(bmr) {
  library(ggplot2)
  library(cowplot)
  
  
  ################################################################################################################
  # General vizualisation -----
  ################################################################################################################
  
  ## 1. Barplots of performances
  
  measure.chosen = acc
  matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)
  
  # Barplot of ranks without redundancies
  values = sapply(c(1:nrow(matrixRanks)), function(x) mean(matrixRanks[x,]))
  row.names(matrixRanks)
  
  test = matrixRanks[,1]
  
  rank.shape = function(x) {
    df = NA
    if (x[1]==2) {
      df=data.frame(rank = as.factor(1), learner = "RF")
    } else if (x[1]==1.5) {
      df=data.frame(rank = as.factor(c("Equal performance","Equal performance")), learner = c("RF","LR"))
    } else {
      df=data.frame(rank = 1, learner = "LR")
    }
    return(df)
  }
  
  list.shape = lapply(matrixRanks[1,], rank.shape)
  list.shape.df = do.call("rbind", list.shape) 
  names(list.shape.df)[2] = "Method"
  
  p = ggplot(list.shape.df, aes_string("rank", fill = "Method"))
  p = p + geom_bar(position = "dodge")
  p = p + ylab("Number")
  p.barplot = p
  
  
  ## 2. Plots for the difference measures, performances and ranks
  
  # plots for the measures
  names(perfsAggr.diff.melted)<-c("Measure","Performance")
  p <- ggplot(perfsAggr.diff.melted[-which(perfsAggr.diff.melted$Measure %in% c("timetrain.test.mean", "logloss.test.mean", "mmce.test.mean")),], aes(Measure, Performance))
  p <- p + geom_boxplot(aes(colour = Measure))
  
  
  
  
  # plot of the mean of accuracy rank for one measure
  measure.chosen = acc
  measure.name = measure.chosen$id
  
  matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure.chosen)
  dim = dim(df.bmr.diff)
  n = dim[1]
  
  learners.meanrank = apply(matrixRanks, 1,mean)
  learners.meanrank = sort(learners.meanrank)
  learners.name = names(learners.meanrank)
  names(learners.meanrank) <- NULL
  learners.meanrank.df = data.frame(learners = factor(learners.name, levels = learners.name), average_rank = learners.meanrank)
  learners.meanrank.df = learners.meanrank.df[order(learners.meanrank.df$average_rank),]
  learners.meanrank.df
  
  
  p.rank = ggplot(learners.meanrank.df, aes(x = learners, y = average_rank)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian(ylim=c(1,4)) +
    ggtitle(paste0("Mean Rank ", measure.name, " ")) + ylab(paste("Mean of",measure.name, "rank on", n ,"datasets")) + xlab("learner")
  
  
  # compute the matrix of the means of ranks
  measures.list = list(acc, auc, brier, ber, logloss, timetrain)
  
  getmatrixRanks <- function(res.perfs.df, measure) {
    matrixRanks = convertModifiedBMRToRankMatrix(res.perfs.df, measure = measure)
    learners.meanrank = apply(matrixRanks, 1,mean)
    return(learners.meanrank)
  }
  
  #p.grid.ranks = plot_grid(p.barplot,
  #                         p.rank,
  #                   ncol = 1, nrow = 2)
  
  print(p.barplot)
  print(p.rank)
  
  
  ## 3. Boxplots of performance ----
  
  boxplotPerfsMeasures <- function(perfsAggr.diff, res.perfs.df, measure, measure.name) {
    
    res = NULL
    
    ## Boxplot two
    # Separate LR and RF
    perfsAggr.LR = subset(res.perfs.df, learner.id == "classif.logreg")
    perfsAggr.RF = subset(res.perfs.df, learner.id == "classif.randomForest")
    
    # with the measure
    lr.measure = perfsAggr.LR[[measure]]
    rf.measure = perfsAggr.RF[[measure]]
    df.measure = data.frame(lr.measure = lr.measure, rf.measure = rf.measure)
    names(df.measure) = c("LR", "RF")
    df.measure.melted = reshape2::melt(df.measure)
    names(df.measure.melted) = c("Method", measure.name)
    
    p <- ggplot(df.measure.melted, aes_string("Method", measure.name))
    p = p +  scale_fill_grey(start = 0.4,end = 1)  + ylim(c(0,1))
    p = p + geom_boxplot(aes_string(fill = "Method"), outlier.shape = 1, notch = TRUE) 
    p = p + labs(y = measure.name) + theme(legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_blank())
    p = p + theme(axis.title.x=element_blank())
    #print(p)
    
    res$p.measure = p
    
    ## Difference boxplot
    diff.measure = perfsAggr.diff$measure
    perfsAggr.diff.boxplot = perfsAggr.diff
    perfsAggr.diff.boxplot$dummy = ""
    p <- ggplot(perfsAggr.diff.boxplot, aes_string( "dummy", measure, width = 0.5))
    p = p + geom_boxplot(aes_string(fill = "dummy"), outlier.shape = 1, notch = TRUE, width = 0.5)
    p = p + labs(y = paste((expression(paste(Delta))),measure.name))
    p = p + theme(axis.title.x=element_blank())
    #p = p + ylim(c(-0.1,0.1))
    p = p + scale_fill_manual(values=c("#CCCCCC")) + theme(legend.position="none")
    #print(p)
    
    res$p.measure.diff = p
    
    return(res)
  }
  
  res.acc = boxplotPerfsMeasures(perfsAggr.diff, res.perfs.df, "acc.test.mean", "acc")
  res.acc$p.measure.diff = res.acc$p.measure.diff+labs(y=expression(paste(Delta,"acc")))
  res.auc = boxplotPerfsMeasures(perfsAggr.diff, res.perfs.df, "auc.test.mean", "auc")
  res.auc$p.measure.diff = res.auc$p.measure.diff+labs(y=expression(paste(Delta,"auc")))
  res.brier = boxplotPerfsMeasures(perfsAggr.diff, res.perfs.df, "brier.test.mean", "brier")
  res.brier$p.measure.diff = res.brier$p.measure.diff+labs(y=expression(paste(Delta,"brier")))
  res.brier$p.measure = res.brier$p.measure   + theme(legend.justification=c(1,1), legend.position=c(1,1))
  
  
  p.grid = plot_grid(res.acc$p.measure,
                     res.auc$p.measure, 
                     res.brier$p.measure, 
                     res.acc$p.measure.diff,
                     res.auc$p.measure.diff,
                     res.brier$p.measure.diff,
                     ncol = 3, nrow = 2)
  
  print(p.grid)
  

jpeg(filename = "Data/Pictures/Figure3_MainResults.jpeg", width = 600, height = 400)
plot(p.grid)
dev.off()
  
}

