import_data_into_xts <- function(dataset)
	{
	 data_ <- read.csv(paste(dataset, ".txt", sep = ""), 
                     sep = ",", dec = ".", header = T,
                     stringsAsFactors = FALSE)
	 data_$Date <- as.Date(as.character(data_$Date), "%Y%m%d")
	 data_ <- xts(data_[,"Close"], data_[,"Date"])
	 names(data_)[1] <- dataset
	 return(data_)
	}