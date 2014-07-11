

res_smape <- make.table(stuff_smape, filter.strategies, filter.columns, sort.column)
res_mase <- make.table(stuff_mase, filter.strategies, filter.columns, sort.column)

mat <- rbind(res_smape$final.mat, res_mase$final.mat)
format.mat <- rbind(res_smape$format.mat, res_mase$format.mat)

myfile <- paste("./tables/", prefix, "-", name.file, ".tex",sep="")
if(competition == "M3"){
	myfile <- paste("./tables/", prefix,"-", allow.differencing,"-", name.file, ".tex", sep="") 
}

textdiff <- ifelse(allow.differencing, "Differencing allowed", "Differencing not allowed")

latex(mat, 
file = myfile, 
size = 'tiny',
cellTexCmds = format.mat,
rowlabel = "Strategy",
title = "ABC",
caption = paste("Forecast accuracy measures for the ",competition, " competition.", sep = ""), 
label = paste("tab:", competition, "-", allow.differencing, "-", name.file ,sep=""),
booktabs = T,
cgroup = cgroup,
n.cgroup = my.cgroup,
rgroup = c("SMAPE", "MASE")
)