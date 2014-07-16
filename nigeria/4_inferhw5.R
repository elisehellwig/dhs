
setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")
library(raster)
library(randomForest)
library(fields)

source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/summary_functions.R')

pol <- getData('GADM', country=NGA, level=0, download=TRUE, path='data/gadm')


##########################HW8HW8HW8HW8HW8########################################
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

p8f <- predict(wcb, mod5f)

plotint(p5f, 'Nigeria', 'hw5', reverse=TRUE, type='RF Prediction', range=c(-2.5,-0.25), breaks=6)

resid5 <- p5f - ni5
plotint(resid5, 'Nigeria', 'hw5', reverse=TRUE, type='RF Residuals', colorpalette='RdBu', breaks=8, range=c(-0.16,0.16))


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
plotint(resid5, 'Nigeria', 'hw5', reverse=TRUE, type='RF Residuals (Reduced Model)', colorpalette='RdBu', breaks=8, range=c(-0.16,0.16))


#########train/test#############
test <- sample(1:dim(wcval)[1], dim(wcval)[1]*0.5, replace=FALSE)
wcvaltrain <- wcval[-test,]
wcvaltest <- wcval[test,]

trainf <- randomForest(hw8 ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15, data=wcvaltrain)
testf <- predict(wcb, trainf)

testfdat <- testf@data@values
r2(testfdat, wcdat$hw8)


##################Thin Plate Splines###########################
vars <- c(2,3,5,13)
tpsclim <- wcval[,2:13]
tpshw8 <- wcval[,1]

tps8 <- Tps(tpsclim, tpshw8)



