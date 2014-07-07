#this summarizes and plots aggregated DHS data
setwd('/Users/echellwig/Drive/DHS/data/aggregated/')
library(maptools)


source('/Users/echellwig/Documents/Research/dhs/functions/plot_functions.R')

#loads data
data(wrld_simpl)
load('KR_cluster_num.RData')
load('KR_cluster_cat.RData')


plotvars <- names(cln[11:length(cln)])
###########################Africa###########################################

for (i in plotvars) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('africa', i, '.png'))
	png(file=ppath)
		plotDHS(cln, i, 'AF')
	dev.off()
}

###########################Asia###########################################

for (i in plotvars) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('asia', i, '.png'))
	png(file=ppath)
		plotDHS(cln, i, 'AS', legendlocation='bottom')
	dev.off()
}


###########################South America###########################################

for (i in plotvars) {
	ppath <- file.path('/Users','echellwig','Drive','DHS','projects','elise','summaryplots', paste0('sa', i, '.png'))
	png(file=ppath)
		plotDHS(cln, i, 'SA', legendlocation='bottomright')
	dev.off()
}
