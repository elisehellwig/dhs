
plotDHS <- function(df, variable, region, years='all', country='all', colorpalette='YlOrRd', reverse=FALSE, point_size=0.5) {
	#df <- is the data frame that contains the data that you want to plot
	#df <- the variable that you want to plot, in quotes
	#region <- the continent code for the continent you want to plot, or some other code for a region we have yet to come up with (ex Subsaharan Africa, or the Sahel) 
	#(AF=Africa, AS=Asia, NO=North Amrica, SA=South America, OC=Oceana, EU=Europe
	#country is country name in quotes


	require(sp)
	require(maptools)
	require(fields)
	require(scales)
	require(raster)
	require(classInt)

	load('/Users/echellwig/Documents/Research/dhs/data/variablecodes.RData')
	load('/Users/echellwig/Documents/Research/dhs/data/cttc.RData')



	#getting only the region, country, year, and variable of interest
	#region
	reg.rows <- which(df[,'contcode']==region)
	dfc <- df[reg.rows, ]
	
	#variable
	vars <- c('ISO3','contcode','year','v001','lon','lat','alt','URBAN_RURA',variable)
	dfc <- dfc[,vars]

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
	cases <- complete.cases(dfc[,variable])
	dfc <- dfc[cases,]


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

	#plotting
	data(wrld_simpl)
	plot(spdf, pch=20, col=colcode, cex=point_size)
	legend("bottomleft", title=paste(vn, 'in', reg.name), legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n") ​
	plot(wrld_simpl, add=TRUE)
​

}





