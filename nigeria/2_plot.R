setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")

#load('data/aggregated/KR_cluster_num.RData')
#load('data/aggregated/KR_cluster_cat.RData')

source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')

load('projects/elise/data/KR_Nigeria_cluster_num.RData')
load('/Users/echellwig/Documents/Research/dhs/data/cttc.RData')

#############renaming############

t <- cln[,-10]

names(t) <- c("ISO3", "contcode", "countryname", "DHScode", "year", "v001", "lon", "lat", "URBAN_RURA", "hw5", "hw8", "hw11")
#############Point Plots############
yrs <- c(1990, 2003, 2008)
vars <- c('hw5', 'hw8', 'hw11')

par(mfrow=c(1,3))
for (i in yrs) {
	dat<- t[t$year==i,]
	plotDHS(dat, 'hw5', 'AF', country='Nigeria', reverse=TRUE, point_size=1,ws=FALSE, outline=TRUE)
}


par(mfrow=c(1,3))
for (i in yrs) {
	dat<- t[t$year==i,]
	plotDHS(dat, 'hw8', 'AF', country='Nigeria', reverse=TRUE, point_size=1,ws=FALSE, outline=TRUE)
}

#############Interp Plots############
############hw5############

hw5ng90 <- intDHS(t, 'Nigeria', 'hw5', years=1990)
save(hw5ng90, file='projects/elise/interpolation/timeseries/hw5ng90.RData')

NGfit90 <- intDHS(t, 'Nigeria', 'hw5', years=1990, addfit=TRUE)

hw5ng03 <- intDHS(t, 'Nigeria', 'hw5', years=2003)
save(hw5ng03, file='projects/elise/interpolation/timeseries/hw5ng03.RData')

NGfit03 <- intDHS(t, 'Nigeria', 'hw5', years=2003, addfit=TRUE)

hw5ng08 <- intDHS(t, 'Nigeria', 'hw5', years=2008)
save(hw5ng08, file='projects/elise/interpolation/timeseries/hw5ng08.RData')

NGfit08 <- intDHS(t, 'Nigeria', 'hw5', years=2008, addfit=TRUE)

NGints <- c(hw5ng90, hw5ng03, hw5ng08)

par(mfrow=c(1,3))
for (i in NGints) {
	plotint(i, 'Nigeria', 'hw5', reverse=TRUE, range=c(-3,1), breaks=9)
}


############hw8############

hw8ng90 <- intDHS(t, 'Nigeria', 'hw8', years=1990)
save(hw8ng90, file='projects/elise/interpolation/timeseries/hw8ng90.RData')

NGfit908 <- intDHS(t, 'Nigeria', 'hw8', years=1990, addfit=TRUE)

hw8ng03 <- intDHS(t, 'Nigeria', 'hw8', years=2003)
save(hw8ng03, file='projects/elise/interpolation/timeseries/hw8ng03.RData')

NGfit038 <- intDHS(t, 'Nigeria', 'hw8', years=2003, addfit=TRUE)

hw8ng08 <- intDHS(t, 'Nigeria', 'hw8', years=2008)
save(hw8ng08, file='projects/elise/interpolation/timeseries/hw8ng08.RData')

NGfit088 <- intDHS(t, 'Nigeria', 'hw8', years=2008, addfit=TRUE)

NGints8 <- c(hw8ng90, hw8ng03, hw8ng08)

par(mfrow=c(1,3))
for (i in NGints8) {
	plotint(i, 'Nigeria', 'hw8', reverse=TRUE, range=c(-3,1), breaks=9)
}

############hw11############

hw11ng90 <- intDHS(t, 'Nigeria', 'hw11', years=1990)
save(hw11ng90, file='projects/elise/interpolation/timeseries/hw11ng90.RData')

NGfit9011 <- intDHS(t, 'Nigeria', 'hw11', years=1990, addfit=TRUE)

hw11ng03 <- intDHS(t, 'Nigeria', 'hw11', years=2003)
save(hw11ng03, file='projects/elise/interpolation/timeseries/hw11ng03.RData')

NGfit0311 <- intDHS(t, 'Nigeria', 'hw11', years=2003, addfit=TRUE)

hw11ng08 <- intDHS(t, 'Nigeria', 'hw11', years=2008)
save(hw11ng08, file='projects/elise/interpolation/timeseries/hw11ng08.RData')

NGfit0811 <- intDHS(t, 'Nigeria', 'hw11', years=2008, addfit=TRUE)

NGints11 <- c(hw11ng90, hw11ng03, hw11ng08)

par(mfrow=c(1,3))
for (i in NGints11) {
	plotint(i, 'Nigeria', 'hw11', reverse=TRUE, range=c(-3,1), breaks=9)
}


#############Examining fits############

fits <- as.data.frame(matrix(rep(0,9), nrow=3))
names(fits) <- c('hw5', 'hw8', 'hw11')

for (i in 1:length(vars)) {
	for (j in 1:length(yrs)) {
		fits[j,i] <- summary(intDHS(t, 'Nigeria', vars[i], years=yrs[j], addfit=TRUE))$lambda
	}
}

fits <- fits[1:3,]


