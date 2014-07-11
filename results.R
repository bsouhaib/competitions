rm(list=ls())
source("results-utils.R")


args=(commandArgs(TRUE))
if(length(args)==0){
	
	idcase <- 1
	
}else{
	idcase <- args[1]
}



competition <- "NN5"
#competition <- "M3"

compare.best <- F
do.competitors <- FALSE


results.folder <- paste(Sys.getenv("HOME"),"/WDFOLDER/RESULTS/",sep="")

if(competition == "NN5")
{
	load(paste(Sys.getenv("HOME"),"/DATA/NN5/NN5.Rdata",sep=""))
	prefix <- "nn5thesis"
	list.data <- NN5
	
	n.runs.available <- n.runs.required <- 111
	ind.available <- ind.required <- seq(n.runs.available)
	pos <- seq(n.runs.available)
	
	step <- 1
	H <- 56
	
	# Configs for the matrix
#by<-7
#all.config <- lapply(seq(1,H,by=by),function(item){item + seq(0,6)})
#all.config[[9]] <- seq(H)
	
	set_a <- c(1, 29, 50)
	all.config <- lapply( set_a ,identity)
	all.config <- c(all.config, list(seq(1,   7)))
	all.config <- c(all.config, list(seq(8,  14)))
	all.config <- c(all.config, list(seq(15, 21)))
	all.config <- c(all.config, list(seq(22, 28)))
	all.config <- c(all.config, list(seq(29, 35)))
	all.config <- c(all.config, list(seq(36, 42)))
	all.config <- c(all.config, list(seq(43, 49)))
	all.config <- c(all.config, list(seq(50, 56)))
	all.config <- c(all.config, list(seq(56)))
	my.cgroup <- c(length(set_a),9,1)

	
}else if(competition == "M3")
{
	load(paste(Sys.getenv("HOME"),"/DATA/M3/M3.Rdata",sep=""))
	prefix <- "m3thesis"
	list.data <- M3

	
	all.h <- sapply(M3, "[[", "h")
	all.n <- sapply(M3, "[[", "n")
	all.type <- sapply(M3, "[[", "type")
	all.period <- sapply(M3, "[[", "period")
	
	ind.available <- which(all.h == 18)
	n.runs.available <- length(ind.available)
	ind.required <- ind.available
	pos <- seq(n.runs.available)
	
	if(FALSE){
stop("I AM CONSIDERING PART OF THE SERIES ")
		#ind.required <- which(all.n > 123)
		
		# "DEMOGRAPHIC" "FINANCE" "INDUSTRY" "MACRO"  "MICRO"  "OTHER"	
		
#		myind <- which(all.type %in% c("DEMOGRAPHIC"))
#		myind <- which(all.type %in% c("FINANCE"))
#		myind <- which(all.type %in% c("MACRO"))
#		myind <- which(all.type %in% c("MICRO"))
		myind <- which(all.type %in% c("OTHER"))
		
ind.required <- ind.available

		pos <- as.numeric( unlist( lapply(ind.required, function(item){which(item==ind.available);}) ) )
	}
		
	n.runs.required <- length(ind.required)

	step <- 10
	H <- 18
	
	# Configs for the matrix
#all.config <- lapply(seq(H),identity)
#l <- 3
#all.config <- c(all.config, lapply(seq(1, H, by = l),function(item){seq(item,len = l)}) )
#all.config <- c(all.config, list(seq(H)))
	
	set_a <- c(1,2,3,6,12,14,16,18)
	all.config <- lapply( set_a ,identity)
	all.config <- c(all.config, list(seq(1,6)))
	all.config <- c(all.config, list(seq(7,12)))
	all.config <- c(all.config, list(seq(13,18)))
	all.config <- c(all.config, list(seq(18)))
	my.cgroup <- c(length(set_a),4,1)


	############ Forecasts of M3 competitors ############
	file.competitors <- "M3-competitors.Rdata"
	if(!file.exists(file.competitors) || do.competitors){
		file.name <- "M3Forecast.xls"
				
		nb.competitors <- sheetCount(file.name)
		all.competitors <- sheetNames(file.name)
		id.competitors <- seq(nb.competitors)
		
		forecasts.competitors <- array(NA,c(n.runs.available, H, nb.competitors))
		
		sheets <- list()
		for(i in id.competitors) {
			
			sheets[[i]] <- read.xls(file.name, sheet = i, dec = ",", head = F, stringsAsFactors = FALSE)
			
			sheets[[i]] <- sheets[[i]][ind.available, -c(1,2)]
			for(j in seq(ncol(sheets[[i]])) ){ 
				sheets[[i]][,j] <- as.numeric(sheets[[i]][,j])
			}
			
			forecasts.competitors[,,i] <- as.matrix(sheets[[i]])
			
			print(paste("Sheet ", i, " done !", sep = ""))
		} 
		
		save(file = "M3-competitors.Rdata", list = c("sheets", "forecasts.competitors","all.competitors", "nb.competitors"))
	}
	
#	if(compare.best){
		load(file.competitors)
		id.best <- 20
#	}
	
	######################################################
	
} # END OF M3	

print(competition)

if(competition == "M3"){
	name.best <- "THETA"

#my.strategies <- c("MEAN", "NAIVE", "REC-LIN", "DIR-LIN", "REC-MLP", "DIR-MLP", "REC-KNN", "DIR-KNN", "RFY-KNN", "REC-BST2", "DIR-BST2", "RFY-BST2"); idcase <- 1;
#strategies <- c(my.strategies)

#my.strategies <- c("MEAN", "NAIVE", "AVG-LIN-KNN", "AVG-LIN-MLP", "AVG-LIN-BST2", "AVG-KNN-KNN", "AVG-MLP-MLP", "AVG-BST2-BST2", "RFY-KNN", "RFY-BST2"); idcase <- 2;
#strategies <- c(my.strategies, "REC-LIN", "DIR-LIN", "REC-KNN",  "DIR-KNN", "REC-MLP", "DIR-MLP", "REC-BST2", "DIR-BST2")

	
##########
alls <- c(2)
rec.procedures <-  c("REC", "RTI",  paste("RJT", alls,  sep=""), "RJTL11", "RJT")
dir.procedures <-  c("DIR"                              , "DJTL11", "DJT")

	if(idcase == 3){
		
model <- "MLP"
all.strat <-  paste(rec.procedures,"-", model, sep="")
my.strategies <- c("MEAN", "NAIVE", all.strat); idcase <- 3;
strategies <- c(my.strategies)
	}
	
	if(idcase == 4){
model <- "MLP"
all.strat <-  paste(dir.procedures,"-", model, sep="")
my.strategies <- c("MEAN", "NAIVE", all.strat); idcase <- 4;
strategies <- c(my.strategies)	
	}
	
	if(idcase == 5){
model <- "KNN"
all.strat <-  paste(rec.procedures,"-", model, sep="")
my.strategies <- c("MEAN", "NAIVE", all.strat); idcase <- 5;
strategies <- c(my.strategies)
	}
	
	if(idcase == 6){

model <- "KNN"
all.strat <-  paste(dir.procedures,"-", model, sep="")
my.strategies <- c("MEAN", "NAIVE", all.strat); idcase <- 6;
strategies <- c(my.strategies)
	}


	
	#my.strategies <- c("REC-LIN","DIR-LIN","RFY-KNN"); idcase <- 1;
	#my.strategies <- c("REC-KNN","DIR-KNN","RFY-KNN"); idcase <- 2;
	#my.strategies <- c("RFY-KNN","REC-KNN","DIR-KNN","REC-LIN","DIR-LIN","RFY-BST2"); idcase <- 3;

	# FOR THESIS
	# my.strategies <- c("AVG-KNN-KNN","AVG-LIN-KNN", "RFY-KNN")
	#procedures <- c("RJT1", "RTI", "RJT", "DIR", "DJTB1", "DJTB2", "DJTL1", "DJT")
	#if(idcase == 5){
	#	my.strategies <- paste(procedures, "-LIN", sep="")
	#}else if(idcase == 6){
	#	my.strategies <- paste(procedures, "-MLP", sep="")
	#}else if(idcase == 7){
	#	my.strategies <-  paste(procedures, "-KNN", sep="")
	#}else if(idcase == 8){
	#	my.strategies <- c("DIR-MLPmo", "DJTB1-MLPmo", "DJTB2-MLPmo", "DJTL1-MLPmo", "DJT-MLPmo")
	#}else if(idcase == 9){
	#	my.strategies <-  paste(c("RJT1","RJT2","RJT3","RJT4","RJT5"), "-KNN", sep="")
	#}



	if(compare.best)
	my.strategies <- c(my.strategies, name.best)

   	print(idcase);
        print(my.strategies)

	#############
}else{



#strategies <- c("REC-MLP","DIR-MLP", "RFY-BST2")
#strategies <- c("REC-MLP","DIR-MLP", "REC-LIN", "RFY-BST2")
#strategies <- c("REC-LIN","REC-MLP","DIR-MLP", "RFY-BST2")
#strategies <- c("REC-KNN","DIR-KNN","RFY-KNN", "AVG-KNN")
#strategies <- c("REC-BST1", "DIR-BST1", "RFY-BST1", "AVG-BST1")
#strategies <- c("REC-BST2", "DIR-BST2", "RFY-BST2", "AVG-BST2")
#strategies <- my.strategies <- c("MEAN", "REC-LIN", "DIR-LIN", "REC-MLP", "DIR-MLP", "REC-KNN", "DIR-KNN", "RFY-KNN", "RFY-BST2"); idcase <- 1;
	
#	my.strategies <- c("MEAN", "REC-LIN", "DIR-LIN", "REC-MLP", "DIR-MLP", "REC-KNN", "DIR-KNN", "REC-BST2", "DIR-BST2", "RFY-KNN", "RFY-BST2"); idcase <- 1;
#	strategies <- my.strategies
	
#	my.strategies <- c("AVG-LIN-KNN", "AVG-LIN-MLP", "AVG-LIN-BST2", "AVG-KNN-KNN", "AVG-MLP-MLP", "AVG-BST2-BST2", "RFY-KNN", "RFY-BST2"); idcase <- 2;
#	strategies <- c(my.strategies, "REC-LIN", "DIR-LIN", "REC-KNN",  "DIR-KNN", "REC-MLP", "DIR-MLP", "REC-BST2", "DIR-BST2")

	
	
	alls <- c(2)
	rec.procedures <-  c("REC", "RTI",  paste("RJT", alls,  sep=""), "RJTL11", "RJT")
	dir.procedures <-  c("DIR"                              , "DJTL11", "DJT")
	
	if(idcase == 3){
		
		model <- "MLP"
		all.strat <-  paste(rec.procedures,"-", model, sep="")
		my.strategies <- c("MEAN", all.strat); idcase <- 3;
		strategies <- c(my.strategies)
	}
	
	if(idcase == 4){
		model <- "MLP"
		all.strat <-  paste(dir.procedures,"-", model, sep="")
		my.strategies <- c("MEAN", all.strat); idcase <- 4;
		strategies <- c(my.strategies)	
	}
	
	if(idcase == 5){
		model <- "KNN"
		all.strat <-  paste(rec.procedures,"-", model, sep="")
		my.strategies <- c("MEAN", all.strat); idcase <- 5;
		strategies <- c(my.strategies)
	}
	
	if(idcase == 6){
		
		model <- "KNN"
		all.strat <-  paste(dir.procedures,"-", model, sep="")
		my.strategies <- c("MEAN", all.strat); idcase <- 6;
		strategies <- c(my.strategies)
	}	
	
	
	
	
}

n.strategies <- length(strategies)
all.measures <- c("SMAPE1","MASE")

for(allow.differencing in c(FALSE)){
	
	print("----------------------")
	print(allow.differencing)


#final.matrix <- matrix("", nrow = length(my.strategies) * length(all.measures), ncol = length(all.config)+1)
final.matrix <- final.format <- final.colnames <- final.rownames <- NULL
############## LOOP OVER error.measure ##########
for(error.measure in all.measures){
	
	print(error.measure)

forecasts <- array(NA,c(n.runs.available, H, n.strategies))
future    <- array(NA,c(n.runs.available, H, n.strategies))

###### Future values for each time series ############
for(i in seq_along(ind.available))
{
	id.series <- ind.available[i]
	
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

id.avg <- which( lapply(strsplit(strategies,"-"),"[",1) == "AVG")
any.avg <- length(id.avg)>0

all.notna <- NULL
for(i in seq_along(all.strategies))
{
	print(all.strategies[i])
	n.files <- 0
	index.notna <- NULL 
	for(idstart in seq(1, n.runs.available, by = step))
	{
		idend <- idstart + (step-1)
		
		if(competition == "NN5"){
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", all.strategies[i], ".Rdata", sep="")
		}else{
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", all.strategies[i], "-", allow.differencing, ".Rdata", sep="")
		}
		
		set.runs <- seq(idstart,idend)
		bad.ids <- which(set.runs > n.runs.available)

		if(any(bad.ids))
		set.runs <- set.runs[-bad.ids]
		
		if(file.exists(file.name)){
			
			load(file.name)

			################
			#print(set.runs)
			#print(i)
			###############
			
			forecasts[set.runs, seq(H), i] <- t(results$forecasts[, seq(H) , seq(length(set.runs))])

			n.files <- n.files + 1
			index.notna <- c(index.notna, set.runs)
		}

	}

	if(competition == "M3" && all.strategies[i] != name.best){
	
		if(is.null(all.notna)){
			all.notna <- index.notna
		}else{
			all.notna <- intersect(all.notna, index.notna)
		}
	}
	print(paste("n.files for ",all.strategies[i]," : ", n.files, sep=""))
}

if(competition == "M3"){
	
	############ SUBSETTING (id.required and not na) #########
	index.notna <- all.notna
	finalpos <- intersect(pos, index.notna)
	if(length(finalpos)==0)
		stop("finalpos has zero elements !")
		
	index.required.notna <- ind.available[finalpos]


	forecasts.best <- forecasts.competitors[finalpos,,id.best]
	print(dim(forecasts.best)[1])


	forecasts <- forecasts[finalpos, , , drop = F]
	print(dim(forecasts)[1])

	future <- future[finalpos, , , drop = F]
	#############################

	forecasts[,, which(strategies==name.best)] <- forecasts.best
}else{
	index.required.notna <- ind.available
}

if(any.avg)
{
	for(item in id.avg){
		strat <- unlist(strsplit(strategies[item],"-"))[1] 
		learner_rec <- unlist(strsplit(strategies[item],"-"))[2]
		learner_dir <- unlist(strsplit(strategies[item],"-"))[3]
		
		id.rec <- which(lapply(strsplit(strategies,"-"),"[",1) == "REC" & lapply(strsplit(strategies,"-"),"[",2) == learner_rec)
		id.dir <- which(lapply(strsplit(strategies,"-"),"[",1) == "DIR" & lapply(strsplit(strategies,"-"),"[",2) == learner_dir)
		if(length(id.rec) == 0 || length(id.dir) == 0){
			stop(paste("Missing REC or DIR ! ",sep=""))
		}
		forecasts[,,item] <- (forecasts[,,id.rec] + forecasts[,,id.dir])/2
	}
	
}


my.strategies <- intersect(strategies, my.strategies)
id.compare <- match(my.strategies, strategies)
forecasts <- forecasts[,,id.compare]
future <- future[,,id.compare]

all.errors <- error(forecasts, future, error.measure, list.data, index.required.notna)


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
my.colnames <- NULL

for(i in seq_along(all.config))
{
	horizons <- all.config[[i]]
	
	my.errors <- cbind(my.errors, 
					   apply(errors.all.horizons[horizons,,drop=F], 2, mean))
	
	my.std  <- cbind(my.std,
					   apply(std.all.horizons[horizons,,drop=F],  2, mean))
	
	my.ranks  <- cbind(my.ranks,
					   apply(ranks.all.horizons[horizons,,drop=F],  2, mean))
	
	my.colnames <- c(my.colnames, 
				   ifelse(length(horizons) > 1, 
						  paste(head(horizons,1),"-",tail(horizons,1),sep=""),
						  horizons[1]))								
}
	
	
avg.ranks <- as.numeric(apply(ranks.all.horizons,2,mean))

#id.sorted <- sort(avg.ranks,index=T)$ix	
id.sorted <- sort(my.errors[,ncol(my.errors)],index=T)$ix
	
my.errors <- cbind(my.errors,avg.ranks)
my.errors <- my.errors[id.sorted,]
	
final.rownames <- c(final.rownames, my.strategies[id.sorted])
my.colnames <- c(my.colnames,"Avg. rank")
	

# Best (smaller) errors in bold font
is.bold <- apply(my.errors, 2, function(item){
	my.min <- min(item); 
	allid  <- which(item == my.min); 
	RET    <- character(nrow(my.errors)); 
	RET[allid] <- "boldmath"; 
	RET})
	
id.min <- which.min(my.errors[,ncol(my.errors)])
is.bold[id.min,ncol(is.bold)] <- ""

#form.mat <- matrix(is.bold, ncol = length(all.config)+1, byrow = T)
form.mat <- is.bold
final.format <- rbind(final.format, form.mat)
	
# Best ranks will be underlined
#best.ranks <- as.numeric(apply(my.ranks, 1, which.min))

# Decimals format
my.errors <- format.df(my.errors, dec=2)
my.std    <- format.df(my.std, dec=2)
my.ranks  <- format.df(my.ranks, dec=2)

current.matrix <- my.errors
for(i in seq(nrow(my.errors)))
{
	for(j in seq(ncol(my.errors)))
	{
		
		current.matrix[i,j] <- paste(my.errors[i,j], sep="")
		
		if(j == (ncol(my.errors)-1)){
			current.matrix[i,j] <- paste(my.errors[i,j], "(", my.std[i,j] , ")", sep="")

		}
	}
}		

current.matrix[id.min,ncol(my.errors)] <- paste("\\underline{",my.errors[id.min,ncol(my.errors)],"}", sep="")

	
	
print(dim(current.matrix))
final.matrix <- rbind(final.matrix, current.matrix)
	
} # END ERROR MEASURES 



# Colnames and rownames
colnames(final.matrix)  <- my.colnames
row.names(final.matrix) <- final.rownames

myfile <- paste("./tables/", prefix, "-", idcase, ".tex",sep="")
	
if(competition == "M3"){
	myfile <- paste("./tables/", prefix,"-", allow.differencing,"-", idcase, ".tex", sep="") 
}

textdiff <- ifelse(allow.differencing,"Differencing allowed","Differencing not allowed")

	
latex(final.matrix, 
file = myfile, 
size = 'tiny',
cellTexCmds = final.format,
rowlabel = "Strategy",
title = "ABC",
caption = paste("Forecast accuracy measures - ",textdiff, sep = ""), 
label = paste("tab:", competition,"-",allow.differencing ,sep=""),
booktabs = T,
cgroup = c("Forecast horizon (h)", "Average", "Average rank"),
n.cgroup = my.cgroup,
rgroup = c("SMAPE", "MASE"),
n.cbroup = c(3,3))

} # END DIFFERENCING 






