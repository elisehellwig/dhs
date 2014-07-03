#this summarizes and plots aggregated DHS data
setwd('/Users/echellwig/Drive/DHS/data/aggregated/')
library(sp)
library(maptools)
library(fields)
library(ggplot2)
library(plyr)
library(scales)
library(raster)
library(classInt)


source('/Users/echellwig/Documents/Research/dhs/functions/plot_functions.R')

#loads data
data(wrld_simpl)
load('KR_national.RData')
load('KR_cluster_num.RData')
load('KR_cluster_cat.RData')


#separates out continents

###########################Africa###########################################

#average number of children under 5 per household
plotDHS(cln, 'v137', 'AF')

#height/age standard deviation
plotDHS(cln, 'hw5', 'AF')

#weight/height standard deviation
plotDHS(cln, 'hw11', 'AF')

plotDHS(cln, 'hw53', 'AF')




