cgroup <- c("Forecast horizon $(h)$", "Average", "Avg. rank")


if(competition == "M3"){
	
	my.config <- c(list(seq(1,   6)),
				   list(seq(7,  12)),
				   list(seq(13, 18)))
	
}else if(competition == "NN5"){
	
	my.config <- c(list(seq(1,   7)),
					list(seq(8,  14)),
					list(seq(15, 21)),
					list(seq(22, 28)),
					list(seq(29, 35)),
					list(seq(36, 42)),
					list(seq(43, 49)),
					list(seq(50, 56)))
}


for(i in seq_along(my.config))
{
	horizons <- my.config[[i]]
	avg.columns <- paste(head(horizons,1), "-", tail(horizons,1), sep = "")
	rank.column <- ""
	filter.columns <- c(as.character(horizons), avg.columns, rank.column)
	my.cgroup <- c(length(horizons), 1, 1)
	
	sort.column <- ""
	
	name.file <- paste(name.table, "-", i, sep = "")
	source("make.table.R")
	
}

if(competition == "M3"){
	
	horizons <- c(1, 2, 3, 6, 12, 18)

	
	my.config <- c(list(seq(1,   6)),
				   list(seq(1,  12)),
				   list(seq(1, 18)))
	
}else if(competition == "NN5"){
	
	horizons <- c(1, 2, 3, 7, 14, 21, 56)
	
	my.config <- c(list(seq(1,   7)),
				   list(seq(1,  14)),
				   list(seq(1, 21)),
				   list(seq(1,  28)),
				   list(seq(1,  35)),
				   list(seq(1,  42)),
				   list(seq(1,  49)),
				   list(seq(1,  56)))
}

avg.columns <- lapply(my.config, function(item){ paste(head(item,1),"-",tail(item,1), sep="")}); 
rank.column <- ""
filter.columns <- c(as.character(horizons), avg.columns, rank.column)
my.cgroup <- c(length(horizons), length(avg.columns), 1)

sort.column <- ""


# sort.column <-  unlist(tail(aggregates,1))
# aggregates <- head(aggregates,-1)
# filter.columns <- c(as.character(horizons), aggregates ,sort.column, "")
# my.cgroup <- c(length(horizons), length(aggregates)+1, 1)

name.file <- paste(name.table, "-ALL", sep = "")
source("make.table.R")
