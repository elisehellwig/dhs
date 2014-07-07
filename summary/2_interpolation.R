setwd('/Users/echellwig/Drive/DHS/data/aggregated/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')

load('KR_cluster_num.RData')

cln1 <- cln[cln$countryname=='Nigeria',]

spd <- DHSsp(cln1)

ext1 <- extent(spd)

r <- raster(ext1, res=1/10, crs=CRS("+proj=longlat +ellps=WGS84"))
values(r) <- rep(0, ncell(r))


lonlat1 <- cbind(cln1[,'lon'], cln1[,'lat'])
fit <- Tps(lonlat1, cln1$hw5)

interp <- interpolate(r, fit, ext=ext1)




