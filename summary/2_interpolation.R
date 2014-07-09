setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')


load('data/aggregated/KR_cluster_num.RData')

#removes weird characters from Cote d'Ivoire's name

cln$countryname <- sub("C\xf4te d'Ivoire", 'Cote dIvoire', cln$countryname)

#sets up data frame to put interpolations into
vars <- c("v012", "v115", "v137", "hw4", "hw5", "hw7", "hw8" , "hw10", "hw11", "hw53") 

for (i in 1:length(vars)) {
	countries <- unique(cln$countryname)
	m <- which(sapply(countries, function(x) datapoints(cln, vars[i], x)<=5))
	countries <- countries[-m]
	intlist <- sapply(countries, function(x) intDHS(cln, x, vars[i]))
	filepath <- paste0('projects/elise/interpolation/', vars[1], 'n.RData')
	save(intlist, file=filepath)
}




countries1 <- unique(cln$countryname)[1:5]
countries2 <- unique(cln$countryname)[5:15]
countries3 <- unique(cln$countryname)[16:25] 
countries4 <- unique(cln$countryname)[26:35]
countries5 <- unique(cln$countryname)[36:47]

#interpolate all the things!
hw5list <- list(rep(0, length(countries)))

for (i in 1:length(countries)) {
	print(countries[i])
	hw5list[[i]] <- intDHS(cln, countries[i], 'hw5')
}


ex <- sapply(countries1, function(x) intDHS(cln, x, 'hw5'))




