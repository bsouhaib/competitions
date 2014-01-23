rm(list=ls())
library(Hmisc)
std <- function(x) sd(x,na.rm=T)/sqrt(length(which(!is.na(x))))

error <- function(forecasts, future, measure = c("SMAPE1"), list.data = NULL, all.id = NULL){
	
	stopifnot(all(dim(forecasts) == dim(future)))
	residuals <- forecasts - future
	mysum1 <- abs(forecasts) + abs(future)
	mysum2 <- forecasts + future
	
	pe <- residuals/future * 100
	
	if(measure == "SMAPE1"){
		
		RET <- abs(residuals)/(mysum1/2) * 100
		
	}else if(measure == "SMAPE2"){
		
		RET <- abs(residuals)/(mysum2/2) * 100
		
	}else if(measure == "MASE"){
		
		all.ts <- lapply(list.data[all.id], "[[", "x")
		scale <- sapply(all.ts, function(item){ mean(abs(diff(item)), na.rm = TRUE)})
		
		my.dim <- dim(forecasts)
		scale.matrix <- forecasts 
		scale.matrix[seq(my.dim[1]), seq(my.dim[2])] <- scale
		
		RET  <- abs(residuals/scale.matrix)
		
	}else if (measure == "MAPE"){
		RET <- abs(pe)
	}else if (measure == "MPE"){
		RET <- pe
	}
	
	RET
}


load(paste(Sys.getenv("HOME"),"/DATA/M3/M3.Rdata",sep=""))
prefix<-"m3"
list.data <- M3
results.folder <- paste(Sys.getenv("HOME"),"/WDFOLDER/RESULTS/",sep="")
allow.differencing <- FALSE


strategies <- c("REC-LIN","RFY-KNN")
all.strategies <- strategies


all.h <- sapply(M3, "[[", "h")
all.n <- sapply(M3, "[[", "n")
all.type <- sapply(M3, "[[", "type")
all.period <- sapply(M3, "[[", "period")

ind.available <- which(all.h == 18)
n.runs.available <- length(ind.available)
step <- 10
H <- 18


f1 <- f2 <- array(NA,c(n.runs.available, H))
future    <- array(NA,c(n.runs.available, H))

###### Future values for each time series ############
for(i in seq_along(ind.available))
{
	id.series <- ind.available[i]
	
	h <- M3[[id.series]]$h
	fut <- M3[[id.series]]$xx
	length(fut) <- H
	future[i, seq(h)] <- fut
}

	n.files <- 0
	index.notna <- NULL 
	for(idstart in seq(1, n.runs.available, by = step))
	{
		idend <- idstart + (step-1)
		
		for(strategy in all.strategies){
				
			file.name <- paste(results.folder, prefix, "-", idstart, "-",idend, "-", strategy, "-", allow.differencing, ".Rdata", sep="")
					
			set.runs <- seq(idstart,idend)
			bad.ids <- which(set.runs > n.runs.available)
			
			if(any(bad.ids))
			set.runs <- set.runs[-bad.ids]
			
			if(file.exists(file.name)){
				
				load(file.name)
				if(strategy == all.strategies[1]){
					f1[set.runs, seq(H)] <- t(results$forecasts[, seq(H) , seq(length(set.runs))])
				}else{
					f2[set.runs, seq(H)] <- t(results$forecasts[, seq(H) , seq(length(set.runs))])
				}
				
				n.files <- n.files + 1
			}
		}
		
	}


err1 <- error(f1, future, "MASE", list.data, all.id = ind.available)
err2 <- error(f2, future, "MASE", list.data, all.id = ind.available)
res <- err2 - err1; 
boxplot(res,outline=F); 
abline(h=0,col="red"); 
dev.off(); 

#pdf("plot.pdf")
#for(its in seq(n.runs.available))
#{
#	ts.plot(cbind(f1[its,],f2[its,],future[its,]),col=c("blue", "red", "black"))
#}
#dev.off()
	
	
	
