setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")

source('/Users/echellwig/Documents/Research/dhs/functions/general.R')
source('/Users/echellwig/Documents/Research/dhs/functions/aggregate_functions.R')


###########################################################
V <- getDHS("KR", "raw")

strvars <- c('DHScode', 'b4', 'v025')
intvars <- c('year', 'v001', 'v002')
numvars <- c('hw5','hw8', 'hw11')
vrs <- unique(c('DHScode', 'recode', 'year', 'v001', 'v002', strvars, intvars, numvars))

x <- V[ ,vrs]
n <- sapply(x[, numvars], function(x) max (nchar(x)))

x <- x[x$DHScode!="AL",]
x <- x[x$DHScode!="AM",]


# turn strings into integers
for (v in intvars) {
	x[ ,v] <- as.integer(x[ ,v])
}

# turn strings into numerics
for (v in numvars) {
	x[ ,v] <- as.numeric(x[ ,v])
}

for (v in numvars[n==4]) {
	x[which(x[ ,v] > 9000), v] <- NA
}

for (v in numvars[n==3]) {
	x[which(x[ ,v] > 993), v] <- NA
}


x$b4 <- tolower(x$b4)
x$b4[x$b4==1] <- 'male'
x$b4[x$b4==2] <- 'female'


x$v025 <- tolower(x$v025)

x$hw5 <- x$hw5 / 100
x$hw8 <- x$hw8 / 100
x$hw11 <- x$hw11 / 100


cc <- ctryCodeTable()

x <- merge(cc[, c('DHScode', 'ISO3', 'countryname')], x, by='DHScode', all.y=TRUE)

save(x, file='projects/elise/data/KR_nigeria_prepared.RData')


############aggregation###############################################

	
d <- x[x$DHScode=='NG',]
	

if (class(d) == 'try-error') next

cl <- dhsAggregate(d, level='cluster')
cln <- cl[[1]]
#clc <- cl[[2]]
p <- paste0('projects/elise/data/', 'KR_Nigeria')
#write.csv(cl, paste0(p, '_cluster.csv'), row.names=FALSE)
save(cln, file=paste0(p, '_cluster_num.RData'))
#save(clc, file=paste0(p, '_cluster_cat.RData'))





