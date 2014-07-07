setwd('/Users/echellwig/Drive/DHS/data/aggregated/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')

load('KR_cluster_num.RData')

cln1 <- cln[cln$countryname=='Nigeria',]

ext1 <- extent(xmin=)



lonlat1 <- cbind(cln1[,'lon'], cln1[,'lat'])

fit1 <- Tps()




spd <- DHSsp(cln1)

spd <- spd

fit <- Tps(lonlat1, cln1$hw5)

