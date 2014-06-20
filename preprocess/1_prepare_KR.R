
setwd("d:/gdrive/projects/DHS/")
setwd("/Users/echellwig/Google Drive/DHS/") #Elise
source("R/functions/general.R")
source("/Users/echellwig/Google Drive/DHS/projects/erica/R/erica_all_dhs_functions.R")


#v001 = Cluster number
#v002 = Household number
#v012 = Age
#v025 = Type of place of residence, urban or rural EH 2014/4/18
#v102 = Type of place of residence, urban or rural EH 2014/4/18
#v113 = Source of drinking water EH 2014/4/25
#v115 = Time to drinking water EH 2014/4/25
#v116 = Toilet facilities EH 2014/4/25
#v119 = Has electricity? EH 2014/4/25
#v137 = number of children under 5 (de jure) EH 2014/4/25
#v149 = Highest level of educational attainment EH 2014/4/25
#v459 = Children sleeping under mosquito net
#b4   = Sex of child
#h11  = Had diarrhea recently EH 2014/4/18
#hw4  = Height/Age percentile EH 2014/4/18
#hw5  = Height/Age standard deviation                   1
#hw7  = Weight/Age percentile EH 2014/4/18
#hw10 = Weight/Height percentile EH 2014/4/18
#hw11 = Weight/Height standard deviation                1
#hw53 = Hemoglobin level in g/dl with 1 implied decimal
#hw70 = Height for age standard deviation (according to WHO)
#hw72 = Weight for height standard deviations (according to WHO)


V <- getDHS("KR", "raw")

#foodvars <- colnames(V)[which(substr(colnames(V), 1, 4) %in% paste0('v', 409:414))]
strvars <- c('DHScode', 'b4', 'v025', 'v102', 'h11', 'v113', 'v116', 'v119', 'v127', 'v149','v459')
intvars <- c('year', 'v001', 'v002', 'v012','v115', 'v137')
numvars <- c('hw5', 'hw11', 'hw53')
percvars <- c('hw4','hw7', 'hw10') #EH 2014/4/18 percentile variables
#facvars <- c() 

vrs <- unique(c('DHScode', 'recode', 'year', 'v001', 'v002', strvars, intvars, numvars, percvars))

x <- V[ ,vrs]

n <- sapply(x[, numvars], function(x) max (nchar(x)))

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


for (v in percvars) { #EH 2014/4/18 prepares percentile data
	x[ ,v] <- as.numeric(x[ ,v])
	x[ ,v] <- x[ ,v]/100
	x[which(x[ ,v] == 99.99), v] <- NA
}


x$b4 <- tolower(x$b4)
x$b4[x$b4==1] <- 'male'
x$b4[x$b4==2] <- 'female'

x$v025 <- tolower(x$v025) #EH 2014/4/18

#EH 2014/4/18 reclassifying obvious responses, and removing non-obvious ones (4216 out of 1492628)
x$v102 <- tolower(x$v102)
x$v102[x$v102=='areas metropolitanas'] <-'urban'
x$v102[x$v102=='menos de 2,500'] <-'rural'
x$v102[x$v102=='2,500 - 19,999' | x$v102=='20,000 y mas'] <- NA


#EH 2014/4/25
x$v113 <- reclassexp(x$v113, "projects/elise/reclass/drinkingwaterv113.csv", rewrite=FALSE)
x$v113 <- reclassimp(x$v113, "projects/elise/reclass/drinkingwaterv113.csv", 'mresp', convert.na=TRUE)

x<-recode_var(x, '/Users/echellwig/Google Drive/DHS/projects/erica/mapfile', v113, major=TRUE)

x$v113 <- as.factor(x$v113)
write_reclass(x$v113, "projects/elise/reclass/drinkingwaterv113.csv")




#EH 2014/4/25 I can't figure out what some of the numbers above 900 mean, but they are not in the range of accepted values so I changed them to NA
x$v115[x$v115==996] <- 0
x$v115[x$v115==997] <- NA
x$v115[x$v115==998] <- 'do not know'
x$v115[x$v115>900] <- NA

#EH 2014/4/25
x$v116 <- reclassexp(x$v116, "projects/elise/reclass/toiletv116.csv", rewrite=FALSE)
x$v116 <- reclassimp(x$v116, "projects/elise/reclass/toiletv116.csv", 'mresp', convert.na=TRUE)


x$v116 <- as.factor(x$v116)
write_reclass(x$v116, "projects/elise/reclass/toiletv116.csv")



#EH 2014/4/25
x$v119 <- reclassexp(x$v119, "projects/elise/reclass/electricv119.csv", rewrite=FALSE)
x$v119 <- reclassimp(x$v119, "projects/elise/reclass/electricv119.csv", 'mresp', convert.na=TRUE)

#EH 2014/4/26
x$v127 <- reclassexp(x$v127, "projects/elise/reclass/floorv127.csv", rewrite=FALSE)
x$v127 <- reclassimp(x$v127, "projects/elise/reclass/floorv127.csv", 'mresp', convert.na=TRUE)


#EH 2014/4/25
x$v149 <- reclassexp(x$v149, "projects/elise/reclass/highestedv149.csv", rewrite=FALSE)
x$v149 <- reclassimp(x$v149, "projects/elise/reclass/highestedv149.csv", 'mresp', convert.na=TRUE)

#EH 2014/4/25
x$v459[x$v459==0] <- 'no'
x$v459[x$v459==1] <- 'yes'
x$v459[x$v459==9] <- NA


#EH 2014/4/25
x$h11 <- reclassexp(x$h11, "projects/elise/reclass/diarrheah11.csv", rewrite=FALSE)
x$h11 <- reclassimp(x$h11, "projects/elise/reclass/diarrheah11.csv", 'mresp', convert.na=TRUE)

x$hw5 <- x$hw5 / 100
x$hw11 <- x$hw11 / 100
x$hw53 <- x$hw53 / 10

# remove bad data

# RH, 2014/4/15
# hw53 can range between 1 and 40. 
# There are 1842 records with a value > 50, 1796 are from IA-2005 (out of 51555)
# I have removed these 
x$hw53 <- x$hw53[x$hw53 > 40] <- NA

cc <- ctryCodeTable()

x <- merge(cc[, c('DHScode', 'ISO3', 'countryname')], x, by='DHScode', all.y=TRUE)

save(x, file='data/processed/KR_prepared.RData')
