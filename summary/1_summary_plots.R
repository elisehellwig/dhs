#this summarizes and plots aggregated DHS data
setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(maptools)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')

#loads data
data(wrld_simpl)
load('data/aggregated/KR_cluster_num.RData')
load('data/aggregated/KR_cluster_cat.RData')


plotvarsn <- names(cln[11:length(cln)])
plotvarsc <- c('h11', 'v113', 'v116', 'v119', 'v127', 'v149', 'v459')
cats <- c('yes last two weeks', 'unprotected', 'no facility', 'yes', 'natural', 'incomplete primary','yes')

#making the categorical variables we want to plot, could do this using the reclassification but I don't feel like rerunning that code right now
clc$h11[,4] <- sapply(1:length(clc$h11[,1]), function(i) sum(clc$h11[i,3], clc$h11[i,4]))
#clc$v116[,6] <- sapply(1:length(clc$v116[,1]), function(i) sum(clc$v116[i,1], clc$v116[i,6]))
clc$v113[,12] <- sapply(1:length(clc$v113[,1]), function(i) sum(clc$v113[i,12], clc$v113[i,6]))
attributes(clc$v113)$dimnames[[2]][12] <- 'unprotected'


###########################Africa###########################################

#numeric variables
for (i in plotvarsn) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('africa', i, '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(cln, i, 'AF')
	dev.off()
}

#categorical variables
for (i in 1:length(plotvarsc)) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('africa', plotvarsc[i], '.png'))
	png(file=ppath, width = 800, height = 800)
		plotDHS(clc, plotvarsc[i], 'AF', cat=cats[i])
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
