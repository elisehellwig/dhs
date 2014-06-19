
source('R/functions/general.R')


dhsAggregate <- function(d, type="KR",level='national', sex=FALSE, age=FALSE, ageagg=12, fun=mean) {

	clustervar <- if (type == "HR") "hv001" else "v001"

	if (level == 'national') {
		d$v001 <- NULL
		d$v002 <- NULL
		x <- c('countryname', 'ISO3', 'year')
	} else 	if (level == 'cluster') {
		d$v002 <- NULL
		x <- c('DHScode', 'ISO3', 'year', clustervar)
	
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

	vars <- colnames(d)[!sapply(d, is.character)]
	vars <- vars[!vars %in% x]
	a <- aggregate(d[, vars], d[, x], FUN=fun, na.rm=TRUE)

	if (level == 'cluster') {
		g <- getGPS()
		if (type == "HR"){
			g$hv001 <- g$v001
		}
		a <- merge(g, a, by=c('DHScode', 'year', clustervar), all.y=TRUE)
		b <- a[, 'ISO3', drop=FALSE]
		a$ISO3 <- NULL
		a <- cbind(b, a)
	}
	print(5)
	a$country <- NULL
	a
}


getGPS <- function() { 
	gpsvars <- c('country', 'year', 'lon', 'lat', 'alt', 'v001', 'URBAN_RURA')
	dta <- loadRData('data/processed/gps.RData')[, gpsvars]
	colnames(dta)[colnames(dta) == "country"] <- "DHScode" #DHS Country Code
	return(dta)
}




#WrldSimpl <- function(d) {
#	require(maptools)
#	data(wrld_simpl)
#	merge(wrld_simpl, d, by='ISO3')
#}


