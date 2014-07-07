

loadRData <- function(filename) {
	if (file.exists(filename)) {
		thisenvir = new.env()
		data <- get(load(filename, thisenvir), thisenvir)
		return(data)
	}else{
		err <- paste("Error: File",filename,"not found.")
		stop(err)
	}
}

getDHS <- function(recode="KR", type='prepared', sex=FALSE) {
	if (sex) {
		f <- paste0("data/processed/", recode, "_sex_", type, ".RData")
	} else {
		f <- paste0("data/processed/", recode, "_", type, ".RData")
	}
	loadRData(f)
}



ctryCodeTable <- function(){
	return(read.csv("R/functions/ctrycodetable.csv", stringsAsFactors=FALSE))
}


getRawVariable <- function(recode='KR', v='') {
	source('../dhsraw/R/pre_process_functions.R')
	getRawData(recode=r, vars=v)
}





##
## changeCodes: change variable values
## d: DHS dataframe
## tbl: dataframe with variable, to, from fields

changeCodes <- function(d, tbl){ #change codes using a coding table with variable, to, from fields

	for (n in 1:nrow(tbl)){
		ln <- tbl[n,]
		varname <- ln$variable
		i <- d[,varname] == ln$from
		i[is.na(i)] = FALSE
		d[i,varname] <- ln$to
	}
	return(d)
}

DHSsp <- function(df, longitude='lon', latitude='lat') {
	#df is the data frame you want to make spatial
	#longitude is the name of the longitude variable
	#latitude is the name of the latitude variable
	lonlat <- cbind(df[,longitude], df[,latitude])
	spdf <- SpatialPointsDataFrame(coords=lonlat, data=df, proj4string=CRS("+proj=longlat +ellps=WGS84"))
	return(spdf)
}








