
setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(randomForest)
library(fields)

source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')

pol <- getData('GADM', country=NGA, level=0, download=TRUE, path='data/gadm')


##########################hw11hw11hw11hw11hw11########################################
load('projects/elise/interpolation/hw11n.RData')
ni11 <- intlist[['Nigeria']]



wc <- getData('worldclim', res=5, var='bio')
ext <- extent(ni11)
nwc <- crop(wc, ext)

wcb <- brick(ni11, nwc)

wcdat <- as.data.frame(wcb@data@values)
names(wcdat)[1] <- 'hw11'

cc <- complete.cases(wcdat)
wcval <- wcdat[cc,]

########################Random Forest#############################


#########################Full Model#########################
mod11f <- randomForest(hw11 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval)

p11f <- predict(wcb, mod11f)

plotint(p11f, 'Nigeria', 'hw11', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=8)

resid11 <- p11f - ni11
plotint(resid11, 'Nigeria', 'hw11', reverse=TRUE, type='RF Residuals', colorpalette='RdBu', breaks=4, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw11 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw11)



#########################Reduced model#########################
mod11r <- randomForest(hw11 ~ bio1 + bio2 +bio4 + bio12, data=wcval)

p11r <- predict(wcb, mod11r)

plotint(p11r, 'Nigeria', 'hw11', reverse=TRUE, type='RF Prediction (Reduced Model)',range=c(-2.5,0.5), breaks=8)

resid11 <- p11f - ni11
plotint(resid11, 'Nigeria', 'hw11', reverse=TRUE, type='RF Residuals (Reduced Model)', colorpalette='RdBu', breaks=4, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw11 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw11)


##########################Time Series##########################################

###########1990############
load('projects/elise/interpolation/timeseries/hw11ng90.RData')

wcb90 <- brick(hw11ng90, nwc)

wcdat90 <- as.data.frame(wcb90@data@values)
names(wcdat90)[1] <- 'hw11'

cc <- complete.cases(wcdat90)
wcval90 <- wcdat90[cc,]

mod11_90 <- randomForest(hw11 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval90)
p11_90 <- predict(wcb90, mod11_90)

plotint(p11_90, 'Nigeria', 'hw11', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)

###########2003############
load('projects/elise/interpolation/timeseries/hw11ng03.RData')

wcb03 <- brick(hw11ng03, nwc)

wcdat03 <- as.data.frame(wcb03@data@values)
names(wcdat03)[1] <- 'hw11'

cc <- complete.cases(wcdat03)
wcval03 <- wcdat03[cc,]

mod11_03 <- randomForest(hw11 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval03)
p11_03 <- predict(wcb03, mod11_03)

plotint(p11_03, 'Nigeria', 'hw11', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)



###########2008############
load('projects/elise/interpolation/timeseries/hw11ng08.RData')

wcb08 <- brick(hw11ng08, nwc)

wcdat08 <- as.data.frame(wcb08@data@values)
names(wcdat08)[1] <- 'hw11'

cc <- complete.cases(wcdat08)
wcval08 <- wcdat08[cc,]

mod11_08 <- randomForest(hw11 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcval08)
p11_08 <- predict(wcb08, mod11_08)

plotint(p11_08, 'Nigeria', 'hw11', reverse=TRUE, type='RF Prediction', range=c(-2.5,0.5), breaks=9)


#####All Together Now#########
hw11preds <- c(p11_90, p11_03, p11_08)


par(mfrow=c(1,3))
for (i in hw11preds) {
	plotint(i, 'Nigeria', 'hw11', reverse=TRUE,type='RF Prediction', range=c(-3,1), breaks=9)
}
##################Thin Plate Splines###########################
vars <- c(2,3,5,13)
tpsclim <- wcval[,2:13]
tpshw11 <- wcval[,1]

tps8 <- Tps(tpsclim, tpshw11)
