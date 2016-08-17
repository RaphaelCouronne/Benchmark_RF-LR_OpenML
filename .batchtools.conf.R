#cluster.functions = makeClusterFunctionsSLURM(template.file = "lmu_lrz.tmpl", 
#                                              list.jobs.cmd = c("squeue", "-h", "-o %i", "-u $USER", 
#                                              "--clusters=serial", "| tail -n +2"))
#cluster.functions = makeClusterFunctionsMulticore()
#debug = TRUE