setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')


load('data/aggregated/KR_cluster_num.RData')

#removes weird characters from Cote d'Ivoire's name
civ <- grep('Ivoire', cln$countryname)
cln[civ, 'countryname'] <- 'Cote dIvoire'

#sets up data frame to put interpolations into
vars <- c("URBAN_RURA", "v012", "v115", "v137", "hw4", "hw5", "hw7", "hw8" , "hw10", "hw11", "hw53") 
countries <- unique(cln$countryname)
numints <- as.data.frame(matrix(rep(1, length(vars)*length(countries)), nrow=length(countries), ncol=length(vars)))
colnames(numints) <- vars
rownames(numints) <- countries

#interpolate all the things!
for (j in 1:length(vars)) {
	for (i in 1:length(countries)) {
		numints[i, j] <- intDHS(cln, countries[i], vars[j])
	}
}






