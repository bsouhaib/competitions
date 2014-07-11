library(Hmisc)
std <- function(x) sd(x,na.rm=T)/sqrt(length(which(!is.na(x))))

error <- function(forecasts, future, measure = c("SMAPE1"), list.data = NULL){
		
	stopifnot( all(dim(forecasts) == dim(future)))
	residuals <- forecasts - future
	mysum1 <- abs(forecasts) + abs(future)
	mysum2 <- forecasts + future
		
	pe <- residuals/future * 100
	
	if(measure == "SMAPE1"){
		
		RET <- abs(residuals)/(mysum1/2) * 100
		
	}else if(measure == "SMAPE2"){
		
		RET <- abs(residuals)/(mysum2/2) * 100
	
	}else if(measure == "MASE"){
		
		all.ts <- lapply(list.data[seq(nrow(forecasts))], "[[", "x")
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


make.bigmat <- function(all.errors, all.config, H)
{
	
	errors.all.horizons <- ranks.all.horizons <- std.all.horizons <- sd.all.horizons <- NULL
	
	for(h in seq(H))
	{
		errors.horizon <- all.errors[, h, ]
		rank.table <- t(apply(errors.horizon, 1, rank))
		
		errors.all.horizons <- rbind(errors.all.horizons, apply(errors.horizon, 2, mean, na.rm=T))
		std.all.horizons    <- rbind(std.all.horizons,  apply(errors.horizon, 2, std)) 
		sd.all.horizons    <- rbind(sd.all.horizons,  apply(errors.horizon, 2, sd)) 

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
	
	
	bigmat <- cbind( t(errors.all.horizons), my.errors)
	
	colnames(bigmat) <- c( seq(H), my.colnames)
	row.names(bigmat) <- strategies
	
	list(bigmat =  bigmat, errors.all.horizons = errors.all.horizons, std.all.horizons = std.all.horizons, sd.all.horizons = sd.all.horizons)
}


make.table <- function(res, set.strategies, set.columns, sort.column){
	
	bigmat <- res$bigmat
	all.errors <- t(res$errors.all.horizons)
	
#	all.std <- t(res$std.all.horizons)
#	all.sd <- t(res$sd.all.horizons)

				
# Select row
	first.mat <- filtered.errors <- NULL
	all.rows <- row.names(bigmat)
	for(strat in set.strategies){
		id <- which(strat == all.rows)
	#print(id)
		first.mat <- rbind(first.mat, bigmat[id,])
		filtered.errors <- rbind(filtered.errors, all.errors[id,])
	}
	
	rank.table <- t(apply(filtered.errors, 2, rank))
	avg.ranks <- matrix(apply(rank.table,2,mean),ncol=1)
	row.names(first.mat) <- row.names(avg.ranks) <- set.strategies
	
	
# Select columnns
	final.mat <- NULL
	all.cols <- colnames(first.mat)
	for(col in set.columns){
		id <- which(col == all.cols)
		final.mat <- cbind(final.mat, first.mat[,id])
	}
	
	final.mat <- cbind(final.mat, avg.ranks)
	colnames(final.mat) <- set.columns
	
	
	id.col <- which(sort.column == colnames(final.mat))
	id.sorted <- sort(final.mat[,id.col],index=T)$ix
	final.mat <- final.mat[id.sorted,]
	
# FORMAT OF THE MATRIX
	is.bold <- apply(final.mat, 2, function(item){
					 my.min <- min(item); 
					 allid  <- which(item == my.min); 
					 RET    <- character(nrow(final.mat)); 
					 RET[allid] <- "boldmath"; 
					 RET})
	
#	id.min <- which.min(final.mat[,ncol(final.mat)])
#	is.bold[id.min,ncol(is.bold)] <- ""
		
	
	format.mat <- is.bold
	
	final.mat <- format.df(final.mat, dec=2)

	
	list(final.mat = final.mat, format.mat = format.mat)
}




