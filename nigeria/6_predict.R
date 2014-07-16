
setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(randomForest)
library(fields)

load('projects/elise/interpolation/hw8n.RData')
ni8 <- intlist[['Nigeria']]
###################################

wc <- getData('worldclim', res=5, var='bio')
ext <- extent(ni8)
nwc <- crop(wc, ext)

nwc8 <- brick(ni8, nwc)

dat8 <- nwc8@data
v8 <- dat8@values
attributes(v8)$dimnames[[2]][1] <- 'hw8'


fit8 <- Tps(v8[,'bio12'], v8[,'hw8'])