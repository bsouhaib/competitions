rm(list=ls())
source("results-utils.R")

#competition <- "NN5"
competition <- "M3"
error.measure <- "SMAPE"
compare.best <- F
do.it <- FALSE

results.folder <- "/projects/mlg/sbentaie/strategies/RESDATA/"

if(competition == "NN5")
{
	load(paste(Sys.getenv("HOME"),"/DATA/NN5/NN5.Rdata",sep=""))
	prefix <- "NN5"
	list.data <- NN5
	
	n.runs <- 111
	ind <- seq(n.runs)
	step <- 1
	H <- 56
	
	# Configs for the matrix
	by<-7
	all.config <- lapply(seq(1,H,by=by),function(item){item + seq(0,6)})
	all.config[[9]] <- seq(H)
	
}else if(competition == "M3")
{
	load(paste(Sys.getenv("HOME"),"/DATA/M3/M3.Rdata",sep=""))
	prefix<-"m3"
	allow.differencing <- F
	list.data <- M3

	
	all.h <- sapply(M3, "[[", "h")
	ind <- which(all.h == 18)
	n.runs <- length(ind)
	step <- 10
	H <- 18
	
	# Configs for the matrix
	all.config <- lapply(seq(H),identity)
	l <- 3
	all.config <- c(all.config, lapply(seq(1, H, by = l),function(item){seq(item,len = l)}) )
	all.config <- c(all.config, list(seq(H)))

	############ Forecasts of M3 competitors ############
	file.competitors <- "M3-competitors.Rdata"
	if(!file.exists(file.competitors) || do.it){
		file.name <- "M3Forecast.xls"
				
		nb.competitors <- sheetCount(file.name)
		all.competitors <- sheetNames(file.name)
		id.competitors <- seq(nb.competitors)
		
		forecasts.competitors <- array(NA,c(n.runs, H, nb.competitors))
		
		sheets <- list()
		for(i in id.competitors) {
			
			sheets[[i]] <- read.xls(file.name, sheet = i, dec = ",", head = F, stringsAsFactors = FALSE)
			
			sheets[[i]] <- sheets[[i]][ind, -c(1,2)]
			for(j in seq(ncol(sheets[[i]])) ){ 
				sheets[[i]][,j] <- as.numeric(sheets[[i]][,j])
			}
			
			forecasts.competitors[,,i] <- as.matrix(sheets[[i]])
			
			print(paste("Sheet ", i, " done !", sep = ""))
		} 
		
		save(file = "M3-competitors.Rdata", list = c("sheets", "forecasts.competitors","all.competitors", "nb.competitors"))
	}
	
	if(compare.best){
		load(file.competitors)
		id.best <- 20
		forecasts.best <- forecasts.competitors[,,id.best]
	}
	
	######################################################
	
}

print(competition)
if(competition == "M3")
print(allow.differencing)


strategies <- c("REC-KNN","DIR-KNN","RFY-KNN")


strategies <- c("REC-KNN","DIR-KNN","RFY-KNN", "AVG-KNN")
#strategies <- c("REC-MLP","DIR-MLP","RFY-MLP", "AVG-MLP")
#strategies <- c("REC-BST1", "DIR-BST1", "RFY-BST1", "AVG-BST1")
#strategies <- c("REC-BST2", "DIR-BST2", "RFY-BST2", "AVG-BST2")

if(compare.best){	
	name.best <- all.competitors[id.best]
	strategies <- c(strategies, name.best)
}
n.strategies <- length(strategies)


forecasts <- array(NA,c(n.runs, H, n.strategies))
future    <- array(NA,c(n.runs, H, n.strategies))

###### Future values for each time series ############
for(i in seq_along(ind))
{
	id.series <- ind[i]
	
	if(competition == "NN5")
	{		
		fut <- NN5[[id.series]]$xx
		length(fut) <- H
		future[i, seq(H), ] <- rep(fut, n.strategies)
				
	}else if(competition == "M3")
	{
		h <- M3[[id.series]]$h
		fut <- M3[[id.series]]$xx
		length(fut) <- H
		future[i, seq(h), ] <- rep(fut, n.strategies)
	}
}

########### Merge forecasts in one matrix ################
all.strategies <- strategies
if(compare.best){
	all.strategies <- strategies[-which(strategies == name.best)]
}

id.avg <- which( lapply(strsplit(strategies,"-"),"[",1) == "AVG")
any.avg <- length(id.avg)>0
if(any.avg){
	all.strategies <- strategies[-id.avg]
}

for(i in seq_along(all.strategies))
{
	n.files <- 0
	for(idstart in seq(1, n.runs, by = step))
	{
		idend <- idstart + (step-1)
		
		if(competition == "NN5"){
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", all.strategies[i], ".Rdata", sep="")
		}else{
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", all.strategies[i], "-", allow.differencing, ".Rdata", sep="")
		}
		
		set.runs <- seq(idstart,idend)
		bad.ids <- which(set.runs > n.runs)

		if(any(bad.ids))
		set.runs <- set.runs[-bad.ids]
		
		if(file.exists(file.name)){
			
			load(file.name)
			forecasts[set.runs, seq(H), i] <- t(results$forecasts[, seq(H) , seq(step)])

			n.files <- n.files + step
		}

	}
	print(paste("n.files for ",all.strategies[i]," : ", n.files, sep=""))
}

if(compare.best)
{
	forecasts[,, n.strategies] <- forecasts.best
}
if(any.avg)
{
	for(item in id.avg){
		strat <- unlist(strsplit(strategies[item],"-"))[1] 
		learner <- unlist(strsplit(strategies[item],"-"))[2]
		
		id.rec <- which(lapply(strsplit(strategies,"-"),"[",1) == "REC" & lapply(strsplit(strategies,"-"),"[",2) == learner)
		id.dir <- which(lapply(strsplit(strategies,"-"),"[",1) == "DIR" & lapply(strsplit(strategies,"-"),"[",2) == learner)
		if(length(id.rec) == 0 || length(id.dir) == 0){
			stop(paste("Missing REC or DIR ! ",sep=""))
		}
		forecasts[,,item] <- (forecasts[,,id.rec] + forecasts[,,id.dir])/2
	}
	
}


all.errors <- error(forecasts, future, error.measure, list.data, ind)



errors.all.horizons <- ranks.all.horizons <- std.all.horizons <- NULL

for(h in seq(H))
{
	errors.horizon <- all.errors[, h, ]
	rank.table <- t(apply(errors.horizon, 1, rank))
	
	errors.all.horizons <- rbind(errors.all.horizons, apply(errors.horizon, 2, mean, na.rm=T))
	std.all.horizons    <- rbind(std.all.horizons,  apply(errors.horizon, 2, std)) 
	ranks.all.horizons  <- rbind(ranks.all.horizons,  apply(rank.table, 2, mean, na.rm=T))
}


my.errors <- my.std <- my.ranks <- NULL
my.rownames <- NULL

for(i in seq_along(all.config))
{
	horizons <- all.config[[i]]
	
	my.errors <- rbind(my.errors, 
					   apply(errors.all.horizons[horizons,,drop=F], 2, mean))
	
	my.std  <- rbind(my.std,
					   apply(std.all.horizons[horizons,,drop=F],  2, mean))
	
	my.ranks  <- rbind(my.ranks,
					   apply(ranks.all.horizons[horizons,,drop=F],  2, mean))
	
	my.rownames <- c(my.rownames, 
				   ifelse(length(horizons) > 1, 
						  paste(head(horizons,1),"-",tail(horizons,1),sep=""),
						  horizons[1]))								
}

# Best (smaller) errors in bold font
is.bold <- apply(my.errors, 1, function(item){
	my.min <- min(item); 
	allid  <- which(item == my.min); 
	RET    <- character(ncol(my.errors)); 
	RET[allid] <- "boldmath"; 
	RET})

form.mat <- matrix(is.bold, ncol = length(strategies), byrow = T)

# Best ranks will be underlined
best.ranks <- as.numeric(apply(my.ranks, 1, which.min))


# Decimals format
my.errors <- format.df(my.errors, dec=2)
my.std    <- format.df(my.std, dec=2)
my.ranks  <- format.df(my.ranks, dec=2)


final.matrix <- matrix("", nrow = nrow(my.errors), ncol = ncol(my.errors))
for(i in seq(nrow(my.errors)))
{
	for(j in seq(ncol(my.errors)))
	{
		if(j == best.ranks[i])
		{
			my.ranks[i,j] <- paste("\\","underline{", my.ranks[i,j],"}",sep="")
		}
		
		final.matrix[i,j] <- paste(my.errors[i,j], "$\\pm$", my.std[i,j], "~(", my.ranks[i,j], ")", sep="")
	}
}		


# Colnames and rownames
colnames(final.matrix)  <- strategies
row.names(final.matrix) <- my.rownames							

latex(final.matrix, 
file = paste("./tables/", competition,".tex",sep=""), 
size = 'tiny',
cellTexCmds = form.mat,
rowlabel = "Horizon",
title = "ABC",
caption = paste("Forecast accuracy measures for the ", competition," competition. \\textbf{Bold} : lowest error; \\underline{Underlined} : highest rank.",sep=""), 
label = paste("tab:", competition, sep=""),
booktabs = T)











