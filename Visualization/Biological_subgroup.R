biological_subgroup_analysis = function(df.bmr.diff, index_bio, index_not.bio){
  
  
  df.bmr.diff.bio = df.bmr.diff[index_bio,]
  df.bmr.diff.not.bio = df.bmr.diff[index_not.bio,]
  perfsAggr.diff.not.bio = perfsAggr.diff[index_not.bio,]
  res.perfs.df.not.bio = res.perfs.df[c(2*index_not.bio,2*index_not.bio-1),]
  
  # Results Overview
  print("======== Overall Results (non-biological datasets) ========")
  cat("\n")
  cat("\n")
  benchmark_ResultsOverview(df.bmr.diff.not.bio, res.perfs.df.not.bio)
  cat("\n")
  cat("\n")
  
  # T-test
  print("================ t-Test results ====================")
  print(t.test(df.bmr.diff.not.bio$acc.test.mean, df.bmr.diff.bio$acc.test.mean))
  print(t.test(df.bmr.diff.not.bio$auc.test.mean, df.bmr.diff.bio$auc.test.mean))
  print(t.test(df.bmr.diff.not.bio$brier.test.mean, df.bmr.diff.bio$brier.test.mean))
  
  df.bmr.diff$biological = "Yes"
  df.bmr.diff$biological[index_not.bio] = "No"
  
  data = melt(df.bmr.diff[c("acc.test.mean","auc.test.mean","brier.test.mean","biological")])
  colnames(data)[3] = "Performance"
  
  levels(data$variable) = c(expression(paste(Delta,"acc")),
                            expression(paste(Delta,"auc")),
                            expression(paste(Delta,"brier")))
  
  
  p = ggplot(data = data, mapping = aes(x=biological,y=Performance, fill = biological))+
    geom_boxplot()+facet_grid(.~variable,margins = FALSE, 
                              scales = c("free"), switch = "y", labeller = label_parsed)+
    theme(axis.title.y=element_blank())+
    theme(legend.position="none") +
    scale_fill_grey(start = 0.7,end = 1)
  print(p)
  cat("\n")
  cat("\n")
}