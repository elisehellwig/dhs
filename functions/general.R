

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
	lonlat <- cbind(as.numeric(df[,longitude]), as.numeric(df[,latitude]))
	spdf <- SpatialPointsDataFrame(coords=lonlat, data=df, proj4string=CRS("+proj=longlat +ellps=WGS84"))
	return(spdf)
}

plotDHS <- function(df, variable, region, years='all', country='all', colorpalette='YlOrRd', reverse=FALSE, point_size=0.5, legendlocation='bottomleft', cat=FALSE, fit=FALSE) {
	#df <- is the data frame that contains the data that you want to plot
	#df <- the variable that you want to plot, in quotes
	#region <- the continent code for the continent you want to plot, or some other code for a region we have yet to come up with (ex Subsaharan Africa, or the Sahel) 
	#(AF=Africa, AS=Asia, NO=North Amrica, SA=South America, OC=Oceana, EU=Europe
	#country is country name in quotes
	#cat <- for categorical variables, the category you want to plot the percent of (ex piped for Drinking water source)

	require(sp)
	require(maptools)
	require(fields)
	require(scales)
	require(raster)
	require(classInt)
	require(RColorBrewer)

	load('projects/elise/data/variablecodes.RData')
	load('projects/elise/data/cttc.RData')



	#getting only the region, country, year, and variable of interest
	#region
	reg.rows <- which(df[,'contcode']==region)
	dfc <- df[reg.rows, ]
	
	#variable
	vars <- c('ISO3','contcode','year','v001','lon','lat','alt','URBAN_RURA',variable)
	dfc <- dfc[,vars]	

	#this gets the percent for the category of interest from the data frame 
	if (cat!=FALSE) {
		dfc[,variable] <- dfc[,variable][,cat]
		}
	#print(str(dfc))

	#country
	if (country=='all') {
		iso.rows <- 1:dim(dfc)[1]
		reg.name <- region
	} else {
		reg.name <- country
		iso.name <- ctt[ctt$countryname==country,'ISO3']
		iso.rows <- which(dfc[,'ISO3']==iso.name)
	}
	dfc <- dfc[iso.rows,]

	#year
	if (years=='all') {
		year.rows <- 1:dim(dfc)[1]
	} else {
		year.rows <- which(dfc[,'year']==years)
		reg.name <- paste(reg.name, years)
	}
	dfc <- dfc[year.rows,]

	#gets rid of NAs etc.
	cases <- complete.cases(dfc)
	dfc <- dfc[cases,]

	#remove clusters with (0,0) for lat long
	nonzero.rows <- which(dfc[,'lon']!=0)
	dfc <- dfc[nonzero.rows,]

	#creating spatial data frame
	lonlat <- cbind(dfc[,'lon'],dfc[,'lat'])
	spdf <- SpatialPointsDataFrame(coords=lonlat, data=dfc, proj4string=CRS("+proj=longlat +ellps=WGS84"))

	#creating intervals for colors
	breaks <- summary(spdf@data[,variable])[1:6]


	#making sure intervals encompass the whole data set
	if (breaks[6]<max(spdf@data[,variable],na.rm=TRUE)) {
		breaks[6] <- breaks[6] + 0.1
	}

	if (breaks[1]>min(spdf@data[,variable], na.rm=TRUE)) {
		breaks[1] <- breaks[1] - 0.1
	}


	#creating intervals for colors
	cints <- classIntervals(spdf@data[,variable], style='fixed', fixedBreaks=breaks)

	if (reverse==TRUE) {
		colpal <- rev(brewer.pal(5, colorpalette))
	} else {
		colpal <- brewer.pal(5, colorpalette)
	}
	
	colcode <- findColours(cints, colpal)

	#gets variable name (not code)
	vn <- varcodes[varcodes$varcode==variable,'varname']
	
	#adds category if relevant
	if (cat!=FALSE) {
		vn <- paste(vn, 'fraction', cat)
		}

	#plotting
	data(wrld_simpl)

	plot(spdf, pch=20, col=colcode, cex=point_size)
	legend(legendlocation, title=paste(vn, 'in', reg.name), legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n") ​
	plot(wrld_simpl, add=TRUE)
​

}






