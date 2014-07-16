
intDHS <- function(df, country, variable, years='all', reso=1/12, longitude='lon', latitude='lat', cat='not cat', addfit=FALSE) {
	#df is the dataframe where you have all your studd
	#country is the name of the country you want to interpolate
	#res is the resolution
	#longitude and latitude are the names of the variables that contain the longitude and latitude data in the data frame respectively

	require(raster)
	require(fields)

	iso <- ctt[ctt$countryname==country,'ISO3']

	pol <- getData('GADM', country=iso, level=0, download=TRUE, path='data/gadm')

	vars <- c('DHScode', 'countryname', 'year', longitude, latitude, variable)
	d <- df[,vars]

	#gets the category you want the percentage of if it is a categorical variable
	if (cat!='not cat') {
		d[,variable] <- d[,variable][,cat]
	}
	#gets the country
	c.rows <- which(d[,'countryname']==country)
	dc <- d[c.rows,]

	if (length(which(complete.cases(dc)))<5) {
		stop(paste(variable, 'not present for', country))
		#stop(paste(variable, 'not present for', country))
	}

	#selects years if necessary
	if (years!='all') {
		year.rows <- which(dc[,'year']==years)
		dc <- dc[year.rows,]
	}

	#remove clusters with (0,0) for lat long
	nonzero.rows <- which(dc[,'lon']!=0)
	dc <- dc[nonzero.rows,]

	#converts to spatial data frame and gets extent of sampling area
	sdc <- DHSsp(dc)
	ext1 <- floor(extent(pol))

	#creates raster
	r <- raster(res=reso)
	r <- crop(r, floor(extent(pol)))
	values(r) <- 1:ncell(r)
	

	#fits TPS
	lonlat <- cbind(dc[,longitude], dc[,latitude])
	fit <- Tps(lonlat, dc[,variable])

	#interpolation
	

	if (addfit) {
		return(fit)
	} else {
		interp <- interpolate(r, fit, ext=ext1)
		return(interp)
	}
}


plotint <- function(interp, country, variable, type='Interpolation', reverse=FALSE, breaks=5, lev=0, width=1, colorpalette='YlOrRd', plotpol=FALSE, range=FALSE) {
	require(maptools)
	require(RColorBrewer)
	require(classInt)
	data(wrld_simpl)

	load('projects/elise/data/variablecodes.RData')
	load('projects/elise/data/cttc.RData')
	
	iso <- ctt[ctt$countryname==country,'ISO3']
	vn <- varcodes[varcodes$varcode==variable,'varname']

	pol <- getData('GADM', country=iso, level=lev, download=TRUE, path='data/gadm')

	if (reverse==TRUE) {
		colpal <- rev(brewer.pal(breaks, colorpalette))
	} else {
		colpal <- brewer.pal(breaks, colorpalette)
	}

	z <- mask(interp, pol)


	if (range==FALSE) { 
		plot(z, main=paste(type, 'of', vn, 'in', country), col=colpal)
	} else {
		plot(z, main=paste(type, 'of', vn, 'in', country), col=colpal, zlim=range)

	}

	if (plotpol) {
		plot(pol, add=TRUE, lwd=width)
	}
}


plotintcontinent <- function(interp, country, variable, range, reverse=FALSE, breaks=5, lev=0, width=1, colorpalette='YlOrRd') {
	require(maptools)
	require(RColorBrewer)
	require(classInt)
	data(wrld_simpl)

	load('projects/elise/data/variablecodes.RData')
	load('projects/elise/data/cttc.RData')
	
	iso <- ctt[ctt$countryname==country,'ISO3']
	vn <- varcodes[varcodes$varcode==variable,'varname']

	pol <- getData('GADM', country=iso, level=lev, download=TRUE, path='data/gadm')

	if (reverse) {
		colpal <- rev(brewer.pal(breaks, colorpalette))
	} else {
		colpal <- brewer.pal(breaks, colorpalette)
	}

	z <- mask(interp, pol)
	plot(z, main=paste('Interpolation of', vn, 'in', country), add=TRUE, col=colpal, zlim=range)
	#plot(pol, add=TRUE, lwd=width)
}
datapoints <- function(df, variable, country) {
	vars <- c('countryname', variable)
	df <- df[,vars]
	c.rows <- which(df[,'countryname']==country)
	dfc <- df[c.rows,]
	n <- length(which(complete.cases(dfc)))
	return(n)
}



