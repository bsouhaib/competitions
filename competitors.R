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
	
		load(file.competitors)
		id.best <- 20	
######################################################