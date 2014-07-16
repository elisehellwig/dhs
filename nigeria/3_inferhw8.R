
setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(randomForest)
library(fields)

source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')

pol <- getData('GADM', country=NGA, level=0, download=TRUE, path='data/gadm')


##########################HW8HW8HW8HW8HW8########################################
load('projects/elise/interpolation/hw8n.RData')
ni8 <- intlist[['Nigeria']]



wc <- getData('worldclim', res=5, var='bio')
ext <- extent(ni8)
nwc <- crop(wc, ext)

wcb <- brick(ni8, nwc)

wcdat <- as.data.frame(wcb@data@values)
names(wcdat)[1] <- 'hw8'

cc <- complete.cases(wcdat)
wcval <- wcdat[cc,]

########################Random Forest#############################


#########################Full Model#########################
mod8f <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval)

p8f <- predict(wcb, mod8f)

plotint(p8f, 'Nigeria', 'hw8', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)

resid8 <- p8f - ni8
plotint(resid8, 'Nigeria', 'hw8', reverse=TRUE, type='RF Residuals', colorpalette='RdBu', breaks=4, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw8)



#########################Reduced model#########################
mod8r <- randomForest(hw8 ~ bio1 + bio2 +bio4 + bio12, data=wcval)

p8r <- predict(wcb, mod8r)

plotint(p8r, 'Nigeria', 'hw8', reverse=TRUE, type='RF Prediction (Reduced Model)',range=c(-2.5,-0.25), breaks=6)

resid8 <- p8f - ni8
plotint(resid8, 'Nigeria', 'hw8', reverse=TRUE, type='RF Residuals (Reduced Model)', colorpalette='RdBu', breaks=8, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw8)

##########################Time Series##########################################

###########1990############
load('projects/elise/interpolation/timeseries/hw8ng90.RData')

wcb90 <- brick(hw8ng90, nwc)

wcdat90 <- as.data.frame(wcb90@data@values)
names(wcdat90)[1] <- 'hw8'

cc <- complete.cases(wcdat90)
wcval90 <- wcdat90[cc,]

mod8_90 <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval90)
p8_90 <- predict(wcb90, mod8_90)

plotint(p8_90, 'Nigeria', 'hw8', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)

###########2003############
load('projects/elise/interpolation/timeseries/hw8ng03.RData')

wcb03 <- brick(hw8ng03, nwc)

wcdat03 <- as.data.frame(wcb03@data@values)
names(wcdat03)[1] <- 'hw8'

cc <- complete.cases(wcdat03)
wcval03 <- wcdat03[cc,]

mod8_03 <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval03)
p8_03 <- predict(wcb03, mod8_03)

plotint(p8_03, 'Nigeria', 'hw8', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)



###########2008############
load('projects/elise/interpolation/timeseries/hw8ng08.RData')

wcb08 <- brick(hw8ng08, nwc)

wcdat08 <- as.data.frame(wcb08@data@values)
names(wcdat08)[1] <- 'hw8'

cc <- complete.cases(wcdat08)
wcval08 <- wcdat08[cc,]

mod8_08 <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval08)
p8_08 <- predict(wcb08, mod8_08)

plotint(p8_08, 'Nigeria', 'hw8', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)


#####All Together Now#########
hw8preds <- c(p8_90, p8_03, p8_08)


par(mfrow=c(1,3))
for (i in hw8preds) {
	plotint(i, 'Nigeria', 'hw8', reverse=TRUE,type='RF Prediction', range=c(-3,1), breaks=9)
}
##################Thin Plate Splines###########################
vars <- c(2,3,5,13)
tpsclim <- wcval[,2:13]
tpshw8 <- wcval[,1]

tps8 <- Tps(tpsclim, tpshw8)



