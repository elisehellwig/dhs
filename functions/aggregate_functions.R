
source('R/functions/general.R')


dhsAggregate <- function(d, type="KR",level='national', sex=FALSE, age=FALSE, ageagg=12, fun=mean) {



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


	d[,'DHScode'] <- NULL

	print(1)

	#numeric variables
	numvars <- colnames(d)[!sapply(d, is.character)]
	numvars <- numvars[!numvars %in% x]
	an <- aggregate(d[, numvars], d[, x], FUN=fun, na.rm=TRUE)
	

	#categorical variables
	catvars <- c(colnames(d)[sapply(d, is.character)], colnames(d)[sapply(d, is.factor)])
	catvars <- catvars[!catvars %in% x]

	for (v in catvars) {
		d[,v] <- as.factor(d[,v])
	}

	ac <- aggregate(d[, vars2, drop=F], d[, x,drop=F], function(x) { x <- na.omit(x); table(x) / length(x) })

	#merge the two sets of variables
	a <- merge(an, ac, by=c('ISO3', 'year', clustervar))



	print(2)

	print(str(a))
	if (level == 'cluster') {
		g <- getGPS()
		g$year <- as.integer(g$year)

		if (type == "HR"){
			g$hv001 <- g$v001
		}

		a <- merge(g, a, by=c('ISO3', 'year', clustervar))
		a <- merge(ctt, a, by='ISO3')
		#a2 <- merge(g, a2, by=c('DHScode', 'year', clustervar), all.y=TRUE)


		b <- a[, 'ISO3', drop=FALSE]
		a$ISO3 <- NULL
		a <- cbind(b, a)
	} 
	print(3)
	#removes duplicate variables
	# univars <- unique(names(a))
	# a <- a[, univars]
	a$country <- NULL
	#print(str(a))
	a
}




dhsAggregatequal <- function(d, type="KR",level='national', sex=FALSE, age=FALSE, ageagg=12, fun=mean) {


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


	#picks out only the variables we want to aggregate (ie. not country name, ISO3 etc)
	vars <- colnames(d)
	vars <- vars[!vars %in% x]
	vars2 <- vars[!vars %in%'countryname']

	#converts everything to factors 
	#I know there's a better way to do this but I don't feel like coming up with it right now
	for (i in vars2) {
		d[,i] <- as.factor(d[,i])
	}
	

	
	#creates array to store stuff in 
	#a <- array(NA, dim=c(dim(unique(d[,x]))[1], dim(d)[2], maxlev))
	#str(a)


	a <- aggregate(d[, vars2, drop=F], d[, x,drop=F], function(x) { x <- na.omit(x); table(x) / length(x) })
	
	
	#remove NaNs
	# for (i in 1:dim(a)[2]) {
	# 	nans <- is.nan(a[,i])
	# 	a[nans,i] <- NA
	# }


	#print(str(a))

	if (level == 'cluster') {
		g <- getGPS()
		g$year <- as.integer(g$year)

		if (type == "HR"){
			g$hv001 <- g$v001
		}

		a <- merge(g, a, by=c('DHScode', 'year', clustervar))
		a <- merge(ctt, a, by='ISO3')
		#a2 <- merge(g, a2, by=c('DHScode', 'year', clustervar), all.y=TRUE)


		b <- a[, 'ISO3', drop=FALSE]
		a$ISO3 <- NULL
		a <- cbind(b, a)
	} 
	#removes duplicate variables
	# univars <- unique(names(a))
	# a <- a[, univars]
	a$country <- NULL
	#print(str(a))
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


