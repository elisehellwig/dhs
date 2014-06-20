#Note: This is a work in progress. 
setwd("d:/gdrive/projects/DHS/")
setwd("/Users/echellwig/Google Drive/DHS/") #Elise
source("R/functions/general.R")
source("R/functions/reclassify.R")


V <- getDHS("HR", "raw")

#hv001 = Cluster number
#hv002 = Household number
#hv014 = Number of children under 5
#hv025 = Type of place of residence, urban or rural
#hv201 = Source of drinking water
#hv204 = Time to get water
#hv205 = Toilet facilities
#hv206 = Has electricity?
#hv213 = Floor material
#hv228 = Children under five slept under bed net



strvars <- c('DHScode', 'hv025', 'hv201', 'hv205', 'hv206', 'hv213', 'hv228')
intvars <- c('year', 'hv014', 'hv204')
#numvars <- c('hv245')
#percvars <- c() 
#facvars <- c() 

vrs <- unique(c('DHScode', 'year', 'hv001', 'hv002', strvars, intvars))

x <- V[ ,vrs]

#n <- sapply(x[, numvars], function(x) max (nchar(x)))

# turn strings into integers
for (v in intvars) {
	x[ ,v] <- as.integer(x[ ,v])
}

# turn strings into numerics
for (v in numvars) {
	x[ ,v] <- as.numeric(x[ ,v])
}


x$hv025 <- tolower(x$hv025)

x$hv204[x$hv204==996] <- 0
x$hv204[x$hv204==998] <- NA
x$hv204[x$hv204==999] <- NA

x$hv206 <- tolower(x$hv206)
x$hv206[x$hv206=='9'] <- NA
x$hv206[x$hv206=='0'] <- 'no'
x$hv206[x$hv206=='1'] <- 'yes'


x$hv228 <- reclassexp(x$hv228, "projects/elise/reclass/bednetchildhv228.csv", rewrite=TRUE)
x$hv228 <- reclassimp(x$hv228, "projects/elise/reclass/bednetchildhv228.csv", 'mresp', convert.na=TRUE)


x$hv201 <- reclassexp(x$hv201, "projects/elise/reclass/drinkingwaterhv201.csv", rewrite=TRUE)
x$hv201 <- reclassimp(x$hv201, "projects/elise/reclass/drinkingwater.csv", 'mresp', convert.na=TRUE, resp1=8, rep1="missing")

x$hv213 <- reclassexp(x$hv213, "projects/elise/reclass/floormaterial3.csv")
x$hv213 <- reclassimp(x$hv213, "projects/elise/reclass/floormaterial.csv", 'mresp',convert.na=TRUE)

################Toilet################

x$hv205[is.na(x$hv205)] <- "missing"
x$hv205 <- as.factor(x$hv205)
levels(x$hv205)[1:4] <- c('no facility', 'flush toilet', 'pit toilet latrine', 'pit toilet latrine')


x$hv205 <- reclassexp(x$hv205, "projects/elise/reclass/toilet.csv", TRUE)
x$hv205 <- reclassimp(x$hv205, "projects/elise/reclass/toilet.csv", 'mresp',convert.na=TRUE)

####################################################


cc <- ctryCodeTable()

x <- merge(cc[, c('DHScode', 'ISO3', 'countryname')], x, by='DHScode', all.y=TRUE)

save(x, file='data/processed/HR_prepared.RData')

