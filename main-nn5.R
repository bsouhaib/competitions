rm(list=ls())
library(forecast)
source(paste(Sys.getenv("HOME"),"/multistep/strategies.R",sep=""))
#source("clean.R")

do.arima <- F

library(tseries)


args=(commandArgs(TRUE))
if(length(args)==0){
	
	folder <- paste(Sys.getenv("HOME"),"/competitions/temp/nn5-",sep="")
	id.job <- 3	
	
}else{
    for(i in 1:length(args)){
		eval(parse(text=args[[i]]))
    }
}

load(paste(Sys.getenv("HOME"),"/DATA/NN5/NN5.Rdata",sep=""))

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

#strategies <- c("MEAN", "REC-LIN", "DIR-LIN",
#"REC-KNN", "RTI-KNN", "RJT-KNN", "RJT4-KNN",
#"DIR-KNN", "JNT-KNN", "JNT4-KNN", "RFY-KNN", 
#"REC-MLP", "DIR-MLP", "JNT-MLP", "JNT4-MLP", "RFY-MLP",
#"REC-BST1", "DIR-BST1", "RFY-BST1",
#"REC-BST2", "DIR-BST2", "RFY-BST2")

#strategies <- c("MEAN", "REC-LIN", "DIR-LIN",
#"REC-KNN",
#"DIR-KNN","RFY-KNN",
#"REC-MLP", "DIR-MLP"
#"RFY-BST2")


#strategies <- c("MEAN", "REC-LIN", "DIR-LIN",
#"REC-KNN",
#"DIR-KNN", "RFY-KNN",
#"REC-MLP", "DIR-MLP")

#strategies <- c("MEAN","REC-LIN", "DIR-LIN")
#strategies <- c("REC-MLP", "DIR-MLP")
#strategies <- c("REC-KNN")
#strategies <- c("DIR-KNN")
#strategies <- c("RFY-KNN")
#strategies <- c("RFY-BST2")
#strategies <- c("REC-BST2", "DIR-BST2")


#alls <- c(2,5)
#rec.procedures <-  c("RTI",  paste("RJT", alls,  sep=""), "RJTL20", "RJTL11", "RJT")

dir.procedures <-  c("DIR"                              , "DJTL20", "DJTL11", "DJT")
strategies <- NULL

#strategies <- paste(dir.procedures,"-MLPmo", sep="")

#allmodels <- c("MLP", "KNN")
allmodels <- "KNN"

for(model in allmodels){

        strategies <- c(strategies,  paste(dir.procedures,"-", model, sep=""))
}

##################################
# MULTI-HORIZON
##################################
#strategies <- NULL
#alls <- seq(2)
#rec.procedures <-  c("RTI",  paste("RJT", seq(5),  sep=""), paste("RJTB",alls, sep=""),  paste("RJTL", alls , sep=""), "RJT")
#dir.procedures <-  c("DIR"                              , paste("DJTB",alls, sep=""),  paste("DJTL", alls , sep=""), "DJT")
#
#
#strategies <- NULL
#strategies <- paste(dir.procedures,"-MLPmo", sep="")
#allmodels <- c("LIN", "MLP", "KNN")
#
#for(model in allmodels){
#	
#	strategies <- c(strategies, paste(rec.procedures,"-", model, sep=""), paste(dir.procedures,"-", model, sep=""))
#}
##################################

print(strategies)

#################
#control <- strategy_control("cv", n.fold=5)
control <- strategy_control("ts-cv",train.percentage = 0.7, n.fold=5)

bst1 <- strategy_learner(name = "BST", interactions=1, nu = 0.1, max.mstop = 500)
bst2 <- strategy_learner(name = "BST", interactions=2, nu = 0.1, max.mstop = 500)
knn <-  strategy_learner(name = "KNN", inc = 5)
mlp <-  strategy_learner(name = "MLP", set.hidden = c(0,1,2,3,5), set.decay = c(0.005,0.01,0.05,0.1,0.2,0.3), nb.runs = 1, maxiter = 100, drop.linbias = F)
lin <-  strategy_learner(name = "MLP", set.hidden = 0, set.decay = 0, nb.runs = 1, drop.linbias = F)
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

arima.forecasts <- matrix(NA, nrow = length(set.runs), ncol = max.H)

all.seasonality <- NULL 
#################

for(id.run in seq_along(set.runs))
{
	RUN <- set.runs[id.run]
	id.ts <- ind[RUN]
	
	base.ts <- ts(NN5[[id.ts]]$x, frequency = 7)
	future <- NN5[[id.ts]]$xx

	# Preprocessing NA and zeroes
	base.ts[base.ts <= 0.0] <- NA
	x <- exp(tsclean(log(base.ts)))
	
	z <- log(x)
	stlz <- stl(z,s.window="periodic",t.window=21)
	seasonality <- rep(head(stlz$time.series[,"seasonal"],7),8)

	deseasonalized.ts  <- seasadj(stlz)
	deseasonalized.ts  <- tsclean(deseasonalized.ts)
	trainset <- deseasonalized.ts
	
		
        source(paste(Sys.getenv("HOME"),"/multistep/shared-main.R",sep=""))


	
	# Updating forecasts 
	for(i in seq_along(strategies)){
		
		temp.forecasts <- all.forecasts[[i]]$temp.forecasts[, seq(H), id.run] <- all.forecasts[[i]]$forecasts[, seq(H), id.run]
		all.forecasts[[i]]$forecasts[, seq(H), id.run] <- NA

		# Restoring back seasonality
		forecasts <- temp.forecasts + seasonality
		forecasts <- exp(forecasts)
	
		all.forecasts[[i]]$forecasts[, seq(H), id.run] <- forecasts

print(lsos())
	}

	if(do.arima){
		# Auto.arima forecasts
		fit <- auto.arima(trainset)
		temp.forecasts <- forecast(fit, h = H)$mean
		forecasts <- temp.forecasts + seasonality
		arima.forecasts[id.run,seq(H)] <- exp(forecasts)
	}
	
	
	length(seasonality) <- max.H
	all.seasonality <- rbind(all.seasonality, seasonality)

	
} # end RUN 

print(" Writing files ...")
for(i in seq_along(strategies))
{
	istrategy <- strategies[i]
	file.name <- paste(folder, runstart , "-" , runend , "-" , istrategy, ".Rdata", sep="")
	results <- all.forecasts[[i]]
	save(file = file.name, list=c("results", "all.seasonality", "arima.forecasts"))
}

