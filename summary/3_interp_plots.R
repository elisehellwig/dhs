setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(fields)
library(maptools)
library(RColorBrewer)
library(classInt)


source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')
data(wrld_simpl)

load('/Users/echellwig/Documents/Research/dhs/data/variablecodes.RData')
load('/Users/echellwig/Documents/Research/dhs/data/cttc.RData')

########################Africa##################
#Variables by Continent
africa <- c("Burundi", "Benin", "Burkina Faso", "Central African Republic", "Cote dIvoire", "Cameroon", "Democratic Republic of the Congo", "Egypt", "Ethiopia", "Gabon", "Ghana", "Guinea", "Kenya", "Liberia", "Lesotho", "Morocco", "Madagascar", "Mali", "Mozambique", "Malawi", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Swaziland","Tanzania","Uganda","Zambia", "Zimbabwe")                        


#hw8
load("projects/elise/interpolation/hw8n.RData")
africahw8 <- intersect(africa, unique(names(intlist)))
ext1 <- extent(c(-20, 56, -39, 40))
plot(ext1, main='Weight/Age Standard Deviation in Africa')
for (i in africahw8) {
	plotintcontinent(intlist[i][[1]], i, 'hw8', range=c(-2.5,.5), breaks=9, rev=TRUE)
}
plot(wrld_simpl, add=TRUE)


load("projects/elise/interpolation/hw53n.RData")
africahw53 <- intersect(africa, unique(names(intlist)))
ext1 <- extent(c(-20, 56, -39, 40))
plot(ext1, main='Hemoglobin levels (g/dL) in Africa')
for (i in africahw53) {
	plotintcontinent(intlist[[i]], i, 'hw53', range=c(7,14), breaks=9, rev=TRUE)
}
plot(wrld_simpl, add=TRUE)

load("projects/elise/interpolation/v459.RData")





##########################Individual Countries by Variable
####Hemoglobin
vars <- c("v012", "v115", "v137", "hw4", "hw5", "hw7", "hw8" , "hw10", "hw11", "hw53") 


load("projects/elise/interpolation/hw53n.RData")
countries <- names(intlist)
for (i in countries) {
	ipath <- paste0('projects/elise/interpolation/plots/','hw53/', i, '.png')
	png(file=ipath, width = 800, height = 800)
		plotint(intlist[i][[1]], i, 'hw53', rev=TRUE)
	dev.off()
}


# Height/age st. dev (stunting)
load("projects/elise/interpolation/hw8n.RData")
countries <- names(intlist)
for (i in countries) {
	ipath <- paste0('projects/elise/interpolation/plots/','hw8/', i, '.png')
	png(file=ipath, width = 800, height = 800)
		plotint(intlist[i][[1]], i, 'hw8', rev=TRUE, breaks=7)
	dev.off()
}


load("projects/elise/interpolation/hw5n.RData")
countries <- names(intlist)
for (i in countries) {
	ipath <- paste0('projects/elise/interpolation/plots/','hw5/', i, '.png')
	png(file=ipath, width = 800, height = 800)
		plotint(intlist[i][[1]], i, 'hw5', rev=TRUE, breaks=7)
	dev.off()
}

load("projects/elise/interpolation/hw11n.RData")
countries <- names(intlist)
for (i in countries) {
	ipath <- paste0('projects/elise/interpolation/plots/','hw11/', i, '.png')
	png(file=ipath, width = 800, height = 800)
		plotint(intlist[i][[1]], i, 'hw11', rev=TRUE, breaks=7)
	dev.off()
}




load("projects/elise/interpolation/v115n.RData")
countries <- names(intlist)
for (i in countries) {
	ipath <- paste0('projects/elise/interpolation/plots/','v115/', i, '.png')
	png(file=ipath, width = 800, height = 800)
		plotint(intlist[i][[1]], i, 'v115')
	dev.off()
}

