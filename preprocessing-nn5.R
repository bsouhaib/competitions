# This function is from http://stats.stackexchange.com/questions/1142/simple-algorithm-for-online-outlier-detection-of-a-generic-time-series
tsoutliers <- function(x,plot=FALSE)
{
    x <- as.ts(x)
    if(frequency(x)>1)
	resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
    else
    {
        tt <- 1:length(x)
        resid <- residuals(loess(x ~ tt))
    }
    resid.q <- quantile(resid,prob=c(0.25,0.75))
    iqr <- diff(resid.q)
    limits <- resid.q + 1.5*iqr*c(-1,1)
    score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
    if(plot)
    {
        plot(x)
        x2 <- ts(rep(NA,length(x)))
        x2[score>0] <- x[score>0]
        tsp(x2) <- tsp(x)
        points(x2,pch=19,col="red")
        return(invisible(score))
    }
    else
	return(score)
}

################# removing NA and zeroes #################
clean <- function(time.series, calinfo, id.bad){

	week.matrix <- t(matrix(time.series, ncol = calinfo$n.weeks))
		
	for(i in seq_along(id.bad)){
		id <- id.bad[i]
		week <- calinfo$weeks[id]
		
		res <- seq(week - 2, week + 2)
		res <- res[which(res > 0 & res <= calinfo$n.weeks)]
		avg <- apply(week.matrix[res,, drop=F], 2, mean, na.rm = T)
		
		week.matrix[week, calinfo$dayofweek[id]] <- time.series[id] <- avg[calinfo$dayofweek[id]]
	}
	time.series
}

#load("/u/sbentaie/DATA/NN5/NN5.Rdata")
load("/Volumes/Hydra/DATA/NN5/NN5.Rdata")

for(its in seq(111)){
	
	time.series <- NN5[[its]]$x
	
	# --------------------
	# Removing missing values and zeroes 
	id.bad <- which(is.na(time.series) | time.series == 0 )
	time.series <- clean(time.series, NN5$calendar.past, id.bad)
	
	# Removing outliers
	scores <- tsoutliers(time.series, plot=F)
	id.bad <- which(scores > 0)
	time.series <- clean(time.series, NN5$calendar.past, id.bad)

	dayofweek  <- NN5$calendar.past$dayofweek
	dayofmonth <- NN5$calendar.past$dayofmonth
	
	# --------------------
	# Computing week averages
	week.average <- tapply(time.series, NN5$calendar.past$weeks, mean, na.rm=T)
	all.wavg <- rep(week.average, each = 7)
	
	# Removing trend
	detrended.ts <- time.series / all.wavg
	
	# --------------------
	# Computing day of week seasonality
	matrix.week <- NULL
	for( dweek in seq(1,7) ){
		res <- detrended.ts[which(dayofweek == dweek)]
		length(res) <- 97
		matrix.week <- cbind(matrix.week, res)
	}
	dayofweek.indices <- apply(matrix.week, 2, median, na.rm=T)
	dayofweek.seasonality <- dayofweek.indices[dayofweek]
	
	# Removing day of week seasonality
	ts.nodweek <- detrended.ts / dayofweek.seasonality 
  
	# --------------------
	# Computing day of  month seasonality
	matrix.month <- NULL
	for( dmonth in seq(1,31) ){
		res <- ts.nodweek[which(dayofmonth == dmonth)]
		length(res) <- 25
		matrix.month <- cbind(matrix.month, res)
	}
	dayofmonth.indices <- apply(matrix.month, 2, median, na.rm=T)
	dayofmonth.seasonality <- dayofmonth.indices[dayofmonth]
	
	# removing day of month seasonality
	ts.nodmonth <- ts.nodweek / dayofmonth.seasonality
	
	browser()
}
dev.off();



#index<- seq(1,300)
#plot.ts(as.numeric(detrended.ts[index])); lines(as.numeric(seasonal.week.ts[index]),col="red")
#index<- seq(301,500)
#plot.ts(as.numeric(detrended.ts[index])); lines(as.numeric(seasonal.week.ts[index]),col="red")
#index<- seq(501,735)
#plot.ts(as.numeric(detrended.ts[index])); lines(as.numeric(seasonal.week.ts[index]),col="red")
#scores <- tsoutliers(ts.nodweek,plot=T)
#browser()

#plot.ts(season.week)
#browser()	
#boxplot(matrix.week, main = paste("Series ",its,sep="")); 	
#	matrix.data <- t(matrix(detrended.ts, ncol=105))
#ts.plot(t(matrix.data), col=rainbow(nrow(matrix.data)))
