setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)
library(maptools)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')

load("projects/elise/interpolation/hw53n.RData")

#plots interpolation of hw53 for one country

for (i in 1:length(intlist))