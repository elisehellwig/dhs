#this summarizes and plots aggregated DHS data
setwd('/Users/echellwig/Drive/DHS/data/aggregated/')
setwd("d:/gdrive/projects/DHS/")
library(maptools)


source('/Users/echellwig/Documents/Research/dhs/functions/plot_functions.R')

#loads data
data(wrld_simpl)
load('KR_cluster_num.RData')
load('KR_cluster_cat.RData')


plotvarsn <- names(cln[11:length(cln)])
plotvarsc <- c('v119', 'v459')
###########################Africa###########################################

#numeric variables
for (i in plotvarsn) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('africa', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(cln, i, 'AF')
	dev.off()
}

#categorical variables
for (i in plotvarsc) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('africa', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(clc, i, 'AF', cat='yes')
	dev.off()
}
###########################Asia###########################################

#numeric variables
for (i in plotvarsn) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('asia', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(cln, i, 'AS', legendlocation='bottom')
	dev.off()
}

#categorical variables
for (i in plotvarsc) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('asia', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(clc, i, 'AS',cat='yes')
	dev.off()
}
###########################South America###########################################

#numeric variables
for (i in plotvarsn) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('sa', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(cln, i, 'SA', legendlocation='bottomright')
	dev.off()
}

#cagegorical variables
for (i in plotvarsc) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('sa', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(clc, i, 'SA',cat='yes')
	dev.off()
}
###########################North America###########################################

#numeric variables
for (i in plotvarsn) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('no', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(cln, i, 'NO', legendlocation='bottomright')
	dev.off()
}

#categorial variables
for (i in plotvarsc) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('no', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(clc, i, 'NO', cat='yes')
	dev.off()
}
