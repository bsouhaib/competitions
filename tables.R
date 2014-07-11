rm(list=ls())
source("table-utils.R")

#########
args=(commandArgs(TRUE))
if(length(args)==0){
	competition <- "NN5"
	#competition <- "M3"
}else{
	competition <- ifelse(args[1] == 1, "M3", "NN5")
}
#########
results.folder <- paste(Sys.getenv("HOME"),"/WDFOLDER/RESULTS/",sep="")


print(competition)

allow.differencing <- FALSE

compare.best <- F
do.competitors <- FALSE


if(competition == "NN5")
{
	load(paste(Sys.getenv("HOME"),"/DATA/NN5/NN5.Rdata",sep=""))
	prefix <- "nn5thesis"
	list.data <- NN5
	
	N <- 111
	
	step <- 1
	step.aggregates <- 7
	H <- 56
		
	all.config <- c(list(seq(1,   7)),
					list(seq(8,  14)),
					list(seq(15, 21)),
					list(seq(22, 28)),
					list(seq(29, 35)),
					list(seq(36, 42)),
					list(seq(43, 49)),
					list(seq(50, 56)),
					list(seq(1,  56)),
					
					
					list(seq(1,  14)),
					list(seq(1, 21)),
					list(seq(1,  28)),
					list(seq(1,  35)),
					list(seq(1,  42)),
					list(seq(1,  49)))
	
}else if(competition == "M3")
{
	load(paste(Sys.getenv("HOME"),"/DATA/M3/M3.Rdata",sep=""))
	prefix <- "m3thesis"
	list.data <- M3
	
	all.h <- sapply(M3, "[[", "h")
	all.n <- sapply(M3, "[[", "n")
	all.type <- sapply(M3, "[[", "type")
	all.period <- sapply(M3, "[[", "period")
	
	myfilter <- which(all.h == 18)
	M3 <- M3[myfilter]

	
	######### Filter the data  #########
	# myfilter <- which(all.n > 123)
	# "DEMOGRAPHIC" "FINANCE" "INDUSTRY" "MACRO"  "MICRO"  "OTHER"	
	# myfilter <- which(all.type %in% c("DEMOGRAPHIC"))
	# myfilter <- which(all.type %in% c("FINANCE"))
	# myfilter <- which(all.type %in% c("MACRO"))
	# myfilter <- which(all.type %in% c("MICRO"))
    # myfilter <- which(all.type %in% c("OTHER"))
	
	# 	M3 <- M3[myfilter]
	####################################
	

	N <- length(M3)

	step <- 10
	step.aggregates <- 6
	H <- 18
	
	all.config <- c(list(seq(1,   6)),
					list(seq(7,  12)),
					list(seq(13, 18)),
					list(seq(1,  12)),
					list(seq(1, 18)))
	
					
	source("competitors.R")
	
} # END OF M3	

###################
my.strategies <- c("MEAN", "REC-LIN", "DIR-LIN", "REC-MLP", "DIR-MLP", "REC-KNN", "DIR-KNN", "RFY-KNN", "REC-BST2", "DIR-BST2", "RFY-BST2")

alls <- c(2)
rec.procedures <-  c("RTI",  paste("RJT", alls,  sep=""), "RJTL11", "RJT")
dir.procedures <-  c(                                     "DJTL11", "DJT")

my.strategies <- c(my.strategies, paste(dir.procedures, "-MLP", sep=""))
my.strategies <- c(my.strategies, paste(dir.procedures, "-KNN", sep=""))

if(competition != "NN5"){
	my.strategies <- c(my.strategies, paste(rec.procedures, "-MLP", sep=""))
	my.strategies <- c(my.strategies, paste(rec.procedures, "-KNN", sep=""))
	my.strategies <- c(my.strategies, "NAIVE")
}


strategies <- c(my.strategies, "AVG-LIN-KNN", "AVG-LIN-MLP", "AVG-LIN-BST2", "AVG-KNN-KNN", "AVG-MLP-MLP", "AVG-BST2-BST2")
###################
n.strategies <- length(strategies)

final.matrix <- final.format <- final.colnames <- final.rownames <- NULL

forecasts <- array(NA,c(N, H, n.strategies))
future    <- array(NA,c(N, H, n.strategies))

###### Future values for each time series ############
for(i in seq(N))
{
	if(competition == "NN5")
	{		
		fut <- NN5[[i]]$xx
		length(fut) <- H
		future[i, seq(H), ] <- rep(fut, n.strategies)
				
	}else if(competition == "M3")
	{
		h <- M3[[i]]$h
		fut <- M3[[i]]$xx
		length(fut) <- H
		future[i, seq(h), ] <- rep(fut, n.strategies)
	}
}

########### Merge forecasts in one matrix ################
for(i in seq_along(strategies))
{
	n.files <- 0
	for(idstart in seq(1, N, by = step))
	{
		idend <- idstart + (step-1)
		
		if(competition == "NN5"){
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", strategies[i], ".Rdata", sep="")
		}else{
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", strategies[i], "-", allow.differencing, ".Rdata", sep="")
		}
		
		set.runs <- seq(idstart,idend)
		bad.ids <- which(set.runs > N)

		if(any(bad.ids))
		set.runs <- set.runs[-bad.ids]
		
		if(file.exists(file.name)){
			
			load(file.name)
			
			forecasts[set.runs, seq(H), i] <- t(results$forecasts[, seq(H) , seq(length(set.runs))])

			n.files <- n.files + 1
		}

	}
	print(paste("n.files for ",strategies[i]," : ", n.files, sep=""))
}

########## ADD AVG strategies #########
id.avg <- which( lapply(strsplit(strategies,"-"),"[",1) == "AVG")
any.avg <- length(id.avg)>0

if(any.avg)
{
	for(item in id.avg){
		strat <- unlist(strsplit(strategies[item],"-"))[1] 
		learner_rec <- unlist(strsplit(strategies[item],"-"))[2]
		learner_dir <- unlist(strsplit(strategies[item],"-"))[3]
		
		id.rec <- which(strategies == paste("REC",learner_rec, sep = "-"))
		id.dir <- which(strategies == paste("DIR",learner_dir, sep = "-"))
		
		
#id.rec <- which(lapply(strsplit(strategies,"-"),"[",1) == "REC" & lapply(strsplit(strategies,"-"),"[",2) == learner_rec)
#id.dir <- which(lapply(strsplit(strategies,"-"),"[",1) == "DIR" & lapply(strsplit(strategies,"-"),"[",2) == learner_dir)

		if(length(id.rec) == 0 || length(id.dir) == 0){
			stop(paste("Missing REC or DIR ! ",sep=""))
		}
		forecasts[,,item] <- (forecasts[,,id.rec] + forecasts[,,id.dir])/2
	}
	
}
	
all.errors_smape <- error(forecasts, future, "SMAPE1", list.data)
all.errors_mase  <- error(forecasts, future, "MASE"  , list.data)
	
stuff_smape <- make.bigmat(all.errors_smape, all.config, H)
stuff_mase  <- make.bigmat(all.errors_mase , all.config, H)

simple.method <- ifelse(competition == "M3", "NAIVE", "MEAN")
	
print("RFY")
filter.strategies <- c(simple.method, "REC-LIN", "DIR-LIN", "REC-MLP", "DIR-MLP", "REC-KNN", "DIR-KNN", "RFY-KNN", "REC-BST2", "DIR-BST2", "RFY-BST2")
name.table <- "RFY"
source("makeall.tables.R")

	
##########
print("AVG")
filter.strategies <- c("AVG-LIN-KNN", "AVG-LIN-MLP", "AVG-LIN-BST2", "AVG-KNN-KNN", "AVG-MLP-MLP", "AVG-BST2-BST2", "RFY-KNN", "RFY-BST2")
name.table <- "AVG"
source("makeall.tables.R")
	
##########	
print("JNT")
alls <- c(2)
rec.procedures <-  c("REC", "RTI",  paste("RJT", alls,  sep=""), "RJTL11", "RJT")
dir.procedures <-  c("DIR"                              , "DJTL11", "DJT")	
	
if(competition == "M3"){
	filter.strategies <- c(simple.method, paste(rec.procedures,"-KNN", sep=""))
	name.table <- "JNT-REC-KNN"
	source("makeall.tables.R")
		
	filter.strategies <- c(simple.method, paste(rec.procedures,"-MLP", sep=""))
	name.table <- "JNT-REC-MLP"
	source("makeall.tables.R")
}
	

filter.strategies <- c(simple.method, paste(dir.procedures,"-KNN", sep=""))
name.table <- "JNT-DIR-KNN"
source("makeall.tables.R")

filter.strategies <- c(simple.method, paste(dir.procedures,"-MLP", sep=""))
name.table <- "JNT-DIR-MLP"
source("makeall.tables.R")





