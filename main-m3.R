rm(list=ls())
source("/u/sbentaie/multistep/strategies.R")
source("/u/sbentaie/DATA/SIMULATIONS/simts.R")
library(tseries)

args=(commandArgs(TRUE))
if(length(args)==0){
	
	folder <- "/u/sbentaie/competitions/temp/m3-"
	id.job <- 1	
	allow.differencing <- TRUE
	
}else{
    for(i in 1:length(args)){
		eval(parse(text=args[[i]]))
    }
}

load("/u/sbentaie/DATA/M3/M3.Rdata")

################
all.h <- sapply(M3, "[[", "h")
all.n <- sapply(M3, "[[", "n")

#ind <- which(all.n >= 50)
ind <- which(all.h == 18)

nb.runs <- length(ind)
max.H <- max(all.h)

n.runs <- 10
set.runs <- c(1:n.runs + n.runs * (id.job-1))

bad.ids <- which(set.runs > nb.runs)

if(any(bad.ids)){
	set.runs <- set.runs[-bad.ids]
}

runstart <- head(set.runs, 1)
runend <- tail(set.runs, 1)

print(set.runs)
################

strategies <- c("MEAN", "REC-LIN", "DIR-LIN",
"REC-KNN", "RTI-KNN", "RJT-KNN", "RJT4-KNN",
"DIR-KNN", "JNT-KNN", "JNT4-KNN", "RFY-KNN", 
"REC-MLP", "DIR-MLP", "JNT-MLP", "JNT4-MLP", "RFY-MLP",
"REC-BST1", "DIR-BST1", "RFY-BST1",
"REC-BST2", "DIR-BST2", "RFY-BST2")

print(strategies)

#################
#control <- strategy_control("cv", n.fold=5)
control <- strategy_control("ts-cv",train.percentage = 0.7, n.fold=5)

bst1 <- strategy_learner(name = "BST", interactions=1, nu = 0.1, max.mstop = 100)
bst2 <- strategy_learner(name = "BST", interactions=2, nu = 0.1, max.mstop = 100)
knn <-  strategy_learner(name = "KNN")
mlp <-  strategy_learner(name = "MLP", set.hidden = c(0,1,2,3,5), set.decay = c(0.005,0.01,0.05,0.1,0.2,0.3), nb.runs = 1, maxiter = 100)
lin <-  strategy_learner(name = "MLP", set.hidden = 0, set.decay = 0)
#################


#################)
set.embedding <- embeddings.base <- embeddings.rect <- seq(1,5)
Nbtest <- 1
Xtest <- NULL 
#################


#################
forecasts.matrix <- array(NA,c(Nbtest, max.H , length(set.runs)))
all.forecasts <- vector("list",length(strategies))

for(i in seq_along(strategies)){
	istrategy <- strategies[i]
	if(grepl("RFY",istrategy)){
		all.forecasts[[i]] <- list(forecasts = forecasts.matrix, temp.forecasts = forecasts.matrix,  comp1 = forecasts.matrix, comp2 = forecasts.matrix)
	}else{
		all.forecasts[[i]] <- list(forecasts = forecasts.matrix, temp.forecasts = forecasts.matrix, comp1 =  NULL, comp2 = NULL)
	}
}

all.seasonality <- NULL 
#################

for(id.run in seq_along(set.runs))
{
	RUN <- set.runs[id.run]
	id.ts <- ind[RUN]
	
	base.ts <- M3[[id.ts]]$x
	future <- M3[[id.ts]]$xx
	H <- M3[[id.ts]]$h
	
	# Seasonality
	if(frequency(base.ts) == 1)
	{
		seasonality <- rep(0,H)
		trainset <- base.ts
		
	}else{
		
		# Calculating seasonality 
		decomposition <- stl(base.ts, s.window=50)
		sesonal.comp <- decomposition$time.series[,"seasonal"]
		deseasonalized.ts <- base.ts - sesonal.comp
		
		future.year <- as.integer(time(future))
		future.month <- cycle(future)
		seasonality <- window(sesonal.comp, 
							  start=c(future.year[1] - 2, future.month[1]), end = c(tail(future.year, 1) - 2, tail(future.month, 1)))
		seasonality <- as.numeric(seasonality)
		
		trainset <- deseasonalized.ts
	}
	
	# Differencing
	diff.done <- FALSE

	if(allow.differencing)
	{
		init.ts <- trainset
		if(kpss.test(trainset)$p.value <= 0.01)
		{
			trainset <- diff(trainset)
			diff.done <- TRUE
		}
	}
	
	source("/u/sbentaie/multistep/shared-main.R")
	
	# Updating forecasts 
	
	for(i in seq_along(strategies)){
		
		temp.forecasts <- all.forecasts[[i]]$temp.forecasts[, seq(H), id.run] <- all.forecasts[[i]]$forecasts[, seq(H), id.run]
		all.forecasts[[i]]$forecasts[, seq(H), id.run] <- NA

		# Invert differencing
		if(allow.differencing && diff.done)
		{
			temp.forecasts <- tail( diffinv(temp.forecasts, xi = tail(init.ts, 1)), -1)
		}

		# Adding seasonality
		forecasts <- temp.forecasts + seasonality
	
		all.forecasts[[i]]$forecasts[, seq(H), id.run] <- forecasts
	}
	
	length(seasonality) <- max.H
	all.seasonality <- rbind(all.seasonality, seasonality)
	
} # end RUN 

print(" Writing files ...")
for(i in seq_along(strategies))
{
	istrategy <- strategies[i]
	file.name <- paste(folder, runstart , "-" , runend , "-" , istrategy, "-", allow.differencing, ".Rdata", sep="")
	results <- all.forecasts[[i]]
	save(file = file.name, list=c("results", "all.seasonality"))
}

