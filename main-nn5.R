rm(list=ls())
source("/u/sbentaie/multistep/strategies.R")
source("/u/sbentaie/DATA/SIMULATIONS/simts.R")
library(tseries)

args=(commandArgs(TRUE))
if(length(args)==0){
	
	folder <- "/u/sbentaie/competitions/temp/nn5-"
	id.job <- 1	
	allow.differencing <- TRUE
	
}else{
    for(i in 1:length(args)){
		eval(parse(text=args[[i]]))
    }
}

load(paste(Sys.getenv("HOME"),"/DATA/NN5/NN5comp.Rdata",sep=""))

################
ind <- seq(length(NN5))
nb.runs <- length(ind)
max.H <- H <- 56

n.runs <- 1
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

bst1 <- strategy_learner(name = "BST", interactions=1, nu=0.1, max.mstop = 500)
bst2 <- strategy_learner(name = "BST", interactions=2, nu=0.1, max.mstop = 500)
knn <-  strategy_learner(name = "KNN")
mlp <-  strategy_learner(name = "MLP", set.hidden = c(0,1,2,3,5), set.decay = c(0.005,0.01,0.05,0.1,0.2,0.3), nb.runs = 1, maxiter = 100)
lin <-  strategy_learner(name = "MLP", set.hidden = 0, set.decay = 0)
#################


#################)
set.embedding <- embeddings.base <- embeddings.rect <- seq(1,10)
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
	
	base.ts <- NN5[[id.ts]]$tsNoNa
	base.ts <- ts(base.ts, freq=7)
	future <- NN5[[id.ts]]$tsFut
	
	# Removing seasonality
	decomposition <- stl(base.ts, s.window = "periodic")
	sesonal.comp <- decomposition$time.series[,"seasonal"]
	deseasonalized.ts <- base.ts - sesonal.comp
	trainset <- deseasonalized.ts

	# Estimating future seasonality
	date.start <- as.Date("18/03/1996", format="%d/%m/%Y")
	date.past <- seq.Date(date.start, date.start + length(base.ts) - 1, by="day")
	past.weekday <- format(date.past,"%a")
	past.day <- format(date.past,"%d")
	
	date.future <- seq.Date(tail(date.past ,1) + 1, tail(date.past, 1) + H, by = "day")
	future.weekday <- format(date.future, "%a")
	future.day <-format(date.future, "%d")
	
	ind.seasonality <- numeric(H)
	for(i in seq(H))
	{
		ind1 <- which(future.weekday[i] == past.weekday)
		ind2 <- which(future.day[i] == past.day)
		ind.seasonality[i] <- max(intersect(ind1, ind2))
	}
	seasonality <- sesonal.comp[ind.seasonality]
		
	source("/u/sbentaie/multistep/shared-main.R")
	
	# Updating forecasts 
	
	for(i in seq_along(strategies)){
		
		temp.forecasts <- all.forecasts[[i]]$temp.forecasts[, seq(H), id.run] <- all.forecasts[[i]]$forecasts[, seq(H), id.run]
		all.forecasts[[i]]$forecasts[, seq(H), id.run] <- NA

		# Restoring back seasonality
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

