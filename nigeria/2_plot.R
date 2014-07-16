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

NGint90 <- intDHS(t, 'Nigeria', 'hw5', years=1990)
NGfit90 <- intDHS(t, 'Nigeria', 'hw5', years=1990, addfit=TRUE)

NGint03 <- intDHS(t, 'Nigeria', 'hw5', years=2003)
NGfit03 <- intDHS(t, 'Nigeria', 'hw5', years=2003, addfit=TRUE)

NGint08 <- intDHS(t, 'Nigeria', 'hw5', years=2008)
NGfit08 <- intDHS(t, 'Nigeria', 'hw5', years=2008, addfit=TRUE)

NGints <- c(NGint90, NGint03, NGint08)

par(mfrow=c(1,3))
for (i in NGints) {
	plotint(i, 'Nigeria', 'hw5', reverse=TRUE, range=c(-3,1), breaks=9)
}


############hw8############

NGint908 <- intDHS(t, 'Nigeria', 'hw8', years=1990)
NGfit908 <- intDHS(t, 'Nigeria', 'hw8', years=1990, addfit=TRUE)

NGint038 <- intDHS(t, 'Nigeria', 'hw8', years=2003)
NGfit038 <- intDHS(t, 'Nigeria', 'hw8', years=2003, addfit=TRUE)

NGint088 <- intDHS(t, 'Nigeria', 'hw8', years=2008)
NGfit088 <- intDHS(t, 'Nigeria', 'hw8', years=2008, addfit=TRUE)

NGints8 <- c(NGint908, NGint038, NGint088)

par(mfrow=c(1,3))
for (i in NGints8) {
	plotint(i, 'Nigeria', 'hw8', reverse=TRUE, range=c(-3,1), breaks=9)
}

############hw11############

NGint9011 <- intDHS(t, 'Nigeria', 'hw11', years=1990)
NGfit9011 <- intDHS(t, 'Nigeria', 'hw11', years=1990, addfit=TRUE)

NGint0311 <- intDHS(t, 'Nigeria', 'hw11', years=2003)
NGfit0311 <- intDHS(t, 'Nigeria', 'hw11', years=2003, addfit=TRUE)

NGint0811 <- intDHS(t, 'Nigeria', 'hw11', years=2008)
NGfit0811 <- intDHS(t, 'Nigeria', 'hw11', years=2008, addfit=TRUE)

NGints11 <- c(NGint9011, NGint0311, NGint0811)

par(mfrow=c(1,3))
for (i in NGints11) {
	plotint(i, 'Nigeria', 'hw11', reverse=TRUE, range=c(-3,1), breaks=7)
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


