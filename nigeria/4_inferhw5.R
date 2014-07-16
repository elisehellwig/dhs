
setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(randomForest)
library(fields)

source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')

pol <- getData('GADM', country=NGA, level=0, download=TRUE, path='data/gadm')


##########################hw5hw5hw5hw5hw5########################################
load('projects/elise/interpolation/hw5n.RData')
ni5 <- intlist[['Nigeria']]

wc <- getData('worldclim', res=5, var='bio')
ext <- extent(ni5)
nwc <- crop(wc, ext)

wcb <- brick(ni5, nwc)

wcdat <- as.data.frame(wcb@data@values)
names(wcdat)[1] <- 'hw5'

cc <- complete.cases(wcdat)
wcval <- wcdat[cc,]

########################Random Forest#############################


#########################Full Model#########################
mod5f <- randomForest(hw5 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval)

p5f <- predict(wcb, mod5f)

plotint(p5f, 'Nigeria', 'hw5', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=8)

resid5 <- p5f - ni5
plotint(resid5, 'Nigeria', 'hw5', reverse=TRUE, type='RF Residuals', colorpalette='RdBu', breaks=4, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw5 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw5)



#########################Reduced model#########################
mod5r <- randomForest(hw5 ~ bio1 + bio2 +bio4 + bio12, data=wcval)

p5r <- predict(wcb, mod5r)

plotint(p5r, 'Nigeria', 'hw5', reverse=TRUE, type='RF Prediction (Reduced Model)',range=c(-2.5,-0.25), breaks=6)

resid5 <- p5f - ni5
plotint(resid5, 'Nigeria', 'hw5', reverse=TRUE, type='RF Residuals (Reduced Model)', colorpalette='RdBu', breaks=4, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw5 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw5)

##########################Time Series##########################################

###########1990############
load('projects/elise/interpolation/timeseries/hw5ng90.RData')

wcb90 <- brick(hw5ng90, nwc)

wcdat90 <- as.data.frame(wcb90@data@values)
names(wcdat90)[1] <- 'hw5'

cc <- complete.cases(wcdat90)
wcval90 <- wcdat90[cc,]

mod5_90 <- randomForest(hw5 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval90)
p5_90 <- predict(wcb90, mod5_90)

plotint(p5_90, 'Nigeria', 'hw5', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)

###########2003############
load('projects/elise/interpolation/timeseries/hw5ng03.RData')

wcb03 <- brick(hw5ng03, nwc)

wcdat03 <- as.data.frame(wcb03@data@values)
names(wcdat03)[1] <- 'hw5'

cc <- complete.cases(wcdat03)
wcval03 <- wcdat03[cc,]

mod5_03 <- randomForest(hw5 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval03)
p5_03 <- predict(wcb03, mod5_03)

plotint(p5_03, 'Nigeria', 'hw5', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)



###########2008############
load('projects/elise/interpolation/timeseries/hw5ng08.RData')

wcb08 <- brick(hw5ng08, nwc)

wcdat08 <- as.data.frame(wcb08@data@values)
names(wcdat08)[1] <- 'hw5'

cc <- complete.cases(wcdat08)
wcval08 <- wcdat08[cc,]

mod5_08 <- randomForest(hw5 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval08)
p5_08 <- predict(wcb08, mod5_08)

plotint(p5_08, 'Nigeria', 'hw5', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)


#####All Together Now#########
hw5preds <- c(p5_90, p5_03, p5_08)


par(mfrow=c(1,3))
for (i in hw5preds) {
	plotint(i, 'Nigeria', 'hw5', reverse=TRUE,type='RF Prediction', range=c(-3,1), breaks=9)
}


##################Thin Plate Splines###########################
vars <- c(2,3,5,13)
tpsclim <- wcval[,2:13]
tpshw5 <- wcval[,1]

tps8 <- Tps(tpsclim, tpshw5)



