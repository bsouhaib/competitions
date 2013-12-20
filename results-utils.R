library(Hmisc)
std <- function(x) sd(x,na.rm=T)/sqrt(length(which(!is.na(x))))

error <- function(forecasts, future, measure = c("SMAPE"), list.data = NULL, all.id = NULL){
	
	stopifnot(all(dim(forecasts) == dim(future)))
	residuals <- forecasts - future
	mysum <- abs(forecasts) + abs(future)
	
	pe <- residuals/future * 100
	
	if(measure == "SMAPE"){
		
		RET <- abs(residuals)/(mysum/2) * 100
		
	}else if(measure == "MASE"){
		
		all.ts <- lapply(list.data[all.id], "[[", "x")
		scale <- sapply(all.ts, function(item){ mean(abs(diff(item)), na.rm = TRUE)})
		
		my.dim <- dim(forecasts)
		scale.matrix <- forecasts 
		scale.matrix[, seq(my.dim[2]), seq(my.dim[3])] <- scale
		
		RET  <- abs(residuals/scale.matrix)
		
	}else if (measure == "MAPE"){
		RET <- abs(pe)
	}else if (measure == "MPE"){
		RET <- pe
	}
	
	RET
}







