
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

intDHS <- function(df, country, variable, years='all', reso=1/12, longitude='lon', latitude='lat', cat='not cat', addfit=FALSE) {
	#df is the dataframe where you have all your studd
	#country is the name of the country you want to interpolate
	#res is the resolution
	#longitude and latitude are the names of the variables that contain the longitude and latitude data in the data frame respectively

	require(raster)
	require(fields)

	source('projects/elise/functions/general.R')
	load('projects/elise/data/cttc.RData')
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
	

	if (addfit==TRUE) {
		return(fit)
	} else {
		interp <- interpolate(r, fit, ext=ext1)
		return(interp)
	}
}


plotint <- function(interp, country, variable, reverse=FALSE, breaks=5, lev=0, width=1, colorpalette='YlOrRd') {
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
	plot(z, main=paste('Interpolation of', vn, 'in', country), col=colpal)
	plot(pol, add=TRUE, lwd=width)
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

	if (reverse==TRUE) {
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



