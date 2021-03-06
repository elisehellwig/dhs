


dhsAggregate <- function(d, type="KR",level='national', sex=FALSE, age=FALSE, ageagg=12, fun=mean) {

	source('R/functions/general.R')


	clustervar <- if (type == "HR") "hv001" else "v001"

	if (level == 'national') {
		d$v001 <- NULL
		d$v002 <- NULL
		x <- c('countryname', 'ISO3', 'year')
	} else 	if (level == 'cluster') {
		d$v002 <- NULL
		x <- c('countryname', 'ISO3', 'year', clustervar)
	
	} else 	if (level == 'region') {
		stop("region not yet implemented")

	} else {
		stop("level should be national, cluster or region")
	}
	
	if (sex) x <- c(x, 'b4')
	if (age) {
		x <- c(x, 'v12')
		v12 <- as.integer(round(v12 / ageagg)) 
	}



	#numeric variables
	x <- c(x, 'DHScode')
	numvars <- colnames(d)[!sapply(d, is.character)]
	numvars <- numvars[!numvars %in% x]
	an <- aggregate(d[, numvars], d[, x], FUN=fun, na.rm=TRUE)
	

	#categorical variables
	
	catvars <- c(colnames(d)[sapply(d, is.character)], colnames(d)[sapply(d, is.factor)])
	catvars <- catvars[!catvars %in% x]


	for (v in catvars) {
		d[,v] <- as.factor(d[,v])
	}


	ac <- aggregate(d[, catvars, drop=F], d[, x,drop=F], function(x) { x <- na.omit(x); table(x) / length(x) })


	#merge the variables with gps coordinates etc.


	if (level == 'cluster') {
		print(1)
		g <- getGPS()
		print(2)
		g$year <- as.integer(g$year)

		if (type == "HR"){
			g$hv001 <- g$v001
		}
		print(2.25)
		#numeric variables
		an <- merge(g, an, by=c('DHScode', 'year', clustervar))
		an <- merge(ctt, an, by='ISO3')
		bn <- an[, 'ISO3', drop=FALSE]
		an$ISO3 <- NULL
		an <- cbind(bn, an)

		print(2.75)
		#categorical variables
		ac <- merge(g, ac, by=c('DHScode', 'year', clustervar))
		ac <- merge(ctt, ac, by='ISO3')
		bc <- ac[, 'ISO3', drop=FALSE]
		ac$ISO3 <- NULL
		ac <- cbind(bc, ac)
	} 

	a <- list(an, ac)
	a
}


getGPS <- function() { 
	gpsvars <- c('country', 'year', 'lon', 'lat', 'v001', 'URBAN_RURA')
	dta <- loadRData('data/processed/gps.RData')[, gpsvars]
	colnames(dta)[colnames(dta) == "country"] <- "DHScode" #DHS Country Code
	return(dta)
}



#WrldSimpl <- function(d) {
#	require(maptools)
#	data(wrld_simpl)
#	merge(wrld_simpl, d, by='ISO3')
#}


