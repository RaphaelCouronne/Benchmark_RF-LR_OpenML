u = read.csv("data.summary.completed.xlsx")



require(gdata)
df = read.xls ("data.summary.completed.xlsx", sheet = 1, header = TRUE)
index_biological = na.omit(df$data.id[df$is_biology==1])
