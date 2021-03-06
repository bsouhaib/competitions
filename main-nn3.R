rm(list=ls())
source(paste(Sys.getenv("HOME"),"/multistep/strategies.R",sep=""))

do.arima <- FALSE
if(do.arima){
	library(forecast)
}
library(tseries)


args=(commandArgs(TRUE))
if(length(args)==0){
	
	folder <- paste(Sys.getenv("HOME"),"/competitions/temp/nn3-",sep="")
	id.job <- 1	
	allow.differencing <- TRUE
	
}else{
    for(i in 1:length(args)){
		eval(parse(text=args[[i]]))
    }
}

load(paste(Sys.getenv("HOME"),"/DATA/NN3/NN3.Rdata",sep=""))


################
nb.runs <- 111
max.H <- 18

n.runs <- 1
initial.runs <- c(1:n.runs + n.runs * (id.job-1))

bad.ids <- which(initial.runs > nb.runs)

set.runs <- initial.runs
if(any(bad.ids)){
	set.runs <- initial.runs[-bad.ids]
}


runstart <- head(initial.runs, 1)
runend <- tail(initial.runs, 1)

print(paste(runstart, "-", runend, sep = ""))
print(set.runs)
################

strategies <- c("MEAN", "REC-LIN", "DIR-LIN")
#strategies <- c("REC-MLP", "DIR-MLP", "REC-KNN", "DIR-KNN", "RFY-KNN")
#strategies <- c("REC-BST2", "DIR-BST2", "RFY-BST2")

print(strategies)

#################
#control <- strategy_control("cv", n.fold=5)
control <- strategy_control("ts-cv",train.percentage = 0.7, n.fold=5)

bst1 <- strategy_learner(name = "BST", interactions=1, nu = 0.1, max.mstop = 100)
bst2 <- strategy_learner(name = "BST", interactions=2, nu = 0.1, max.mstop = 100)
knn <-  strategy_learner(name = "KNN")
mlp <-  strategy_learner(name = "MLP", set.hidden = c(0,1,2,3,5), set.decay = c(0.005,0.01,0.05,0.1,0.2,0.3), nb.runs = 1, maxiter = 100, drop.linbias = F)
lin <-  strategy_learner(name = "MLP", set.hidden = 0, set.decay = 0, nb.runs = 1, drop.linbias = F)
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

arima.forecasts <- matrix(NA, nrow = length(set.runs), ncol = max.H)


all.seasonality <- NULL 
#################

for(id.run in seq_along(set.runs))
{
	##### WE START WITH FALSE 
	lin$drop.linbias <- FALSE
	
	id.ts <- RUN <- set.runs[id.run]
	
	base.ts <- NN3[[id.ts]]$x
	future <- NN3[[id.ts]]$xx
	H <- 18
	
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
		
		browser()
		
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
			
			# We drop the bias (the intercept) if differencing	
			lin$drop.linbias <- TRUE
		}
	}
	
	source(paste(Sys.getenv("HOME"),"/multistep/shared-main.R",sep=""))
	
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

	if(do.arima){
	
		# Auto.arima forecasts
		fit <- auto.arima(base.ts)
		forecasts <- forecast(fit, h = H)$mean
		arima.forecasts[id.run,seq(H)] <- forecasts
	
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
	save(file = file.name, list=c("results", "all.seasonality", "arima.forecasts"))
}

