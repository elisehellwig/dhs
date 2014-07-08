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
countries <- unique(cln$countryname)


countries1 <- unique(cln$countryname)[1:5]
#interpolate all the things!


ex <- sapply(countries1, function(x) intDHS(cln, x, 'hw5'))




