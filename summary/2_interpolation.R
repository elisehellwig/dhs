setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')


load('data/aggregated/KR_cluster_num.RData')
load('data/aggregated/KR_cluster_cat.RData')

#removes weird characters from Cote d'Ivoire's name

cln$countryname <- sub("C\xf4te d'Ivoire", 'Cote dIvoire', cln$countryname)
clc$countryname <- sub("C\xf4te d'Ivoire", 'Cote dIvoire', clc$countryname)


################Numeric#############
#sets up data frame to put interpolations into
vars <- c("v012", "v115", "v137", "hw4", "hw5", "hw7", "hw8" , "hw10", "hw11", "hw53") 

for (i in 1:length(vars)) {
	print(vars[i])
	countries <- unique(cln$countryname)
	m <- which(sapply(countries, function(x) datapoints(cln, vars[i], x)<=5))
	countries <- countries[-m]
	intlist <- sapply(countries, function(x) intDHS(cln, x, vars[i]))
	filepath <- paste0('projects/elise/interpolation/', vars[i], 'n.RData')
	save(intlist, file=filepath)
}


##################Categorical###################

vars2 <- c('h11', 'v113', 'v116', 'v119', 'v127', 'v149', 'v459')
cats <- c('yes last two weeks', 'piped', 'flush', 'yes', 'natural', 'incomplete primary','yes')

clc$h11[,5] <- rep(0, length(clc$h11[,1]))
clc$h11[,4] <- sapply(1:length(clc$h11[,1]), function(i) sum(clc$h11[i,3], clc$h11[i,4]))

for (i in 1:length(vars2)) {
	print(vars2[i])
	countries <- unique(clc$countryname)
	m <- which(sapply(countries, function(x) datapoints(clc, vars2[i], x)<=5))
	countries <- countries[-m]
	intlistc <- sapply(countries, function(x) intDHS(clc, x, vars2[i], cat=cats[i]))
	filepath <- paste0('projects/elise/interpolation/', vars2[i], 'c.RData')
	save(intlistc, file=filepath)
}

