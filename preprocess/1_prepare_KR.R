
setwd("d:/gdrive/projects/DHS/")
setwd("/Users/echellwig/Drive/DHS/") #Elise
source("R/functions/general.R")
source("/Users/echellwig/Documents/Research/dhs/functions/reclass_functions.R")


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

#removing countries with year problems until they are sorted

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


for (v in percvars) { #EH 2014/4/18 prepares percentile data
	x[ ,v] <- as.numeric(x[ ,v])
	x[ ,v] <- x[ ,v]/100
	x[which(x[ ,v] == 99.99), v] <- NA
}

#load mapfile for recoding
mapfile <- read.csv('projects/erica/mapfile/standard_dhs_map.csv', stringsAsFactors=FALSE)
#converting to UTF-8 encoding
x$v113<- iconv(x$v113, '', 'UTF-8')
x$v116<- iconv(x$v116, '', 'UTF-8')
x$v119<- iconv(x$v119, '', 'UTF-8')
x$v127<- iconv(x$v127, '', 'UTF-8')
x$v149<- iconv(x$v149, '', 'UTF-8')
x$h11<- iconv(x$h11, '', 'UTF-8')

#x2<-x
#recoding with major categories
x <- recode_var(x, mapfile, c('v113','v116', 'v127'), major=TRUE)

#recoding with minor categories
x <- recode_var(x, mapfile, c('v119','v149', 'h11'), major=FALSE)


#sex of individual
x$b4 <- tolower(x$b4)
x$b4[x$b4==1] <- 'male'
x$b4[x$b4==2] <- 'female'

#urban/rural
x$v025 <- tolower(x$v025) #EH 2014/4/18

#Urban/Rural
x$v102 <- tolower(x$v102)
x$v102[x$v102=='areas metropolitanas'| x$v102=='20,000 y mas'] <-'urban'
x$v102[x$v102=='2,500 - 19,999' | x$v102=='menos de 2,500'] <-'rural'

#Source of drinking water
x$v113 <- as.factor(x$v113)
#write_reclass(x2$v113, "projects/elise/reclass/v113.csv")
x$v113 <- read_reclass(x$v113,'major',"projects/elise/reclass/v113.csv")
x$v113 <- as.character(x$v113)


#Time to drinking water
x$v115[x$v115==996] <- 0
x$v115[x$v115==997] <- NA
x$v115[x$v115==998] <- 'do not know'
x$v115[x$v115>900] <- NA

#Toilet facilities
x$v116 <- as.factor(x$v116)
#write_reclass(x$v116, "projects/elise/reclass/v116.csv")
x$v116 <- read_reclass(x$v116,'response',"projects/elise/reclass/v116.csv")


#Has electricity
x$v119 <- as.factor(x$v119)
#write_reclass(x$v119, "projects/elise/reclass/v119.csv")
x$v119 <- read_reclass(x$v119,'response',"projects/elise/reclass/v119.csv")

#Floor Material
x$v127 <- as.factor(x$v127)
#write_reclass(x$v127, "projects/elise/reclass/v127.csv")
x$v127 <- read_reclass(x$v127,'response',"projects/elise/reclass/v127.csv")

#Highest level of education
x$v149 <- as.factor(x$v149)
#write_reclass(x$v149, "projects/elise/reclass/v149.csv")
x$v149 <- read_reclass(x$v149,'response',"projects/elise/reclass/v149.csv")

#Children sleeping under mosquito net
x$v459[x$v459==0] <- 'no'
x$v459[x$v459==1] <- 'yes'
x$v459[x$v459==9] <- NA


#Had Diarrhea recently
x$h11<- as.factor(x$h11)
#write_reclass(x$h11, "projects/elise/reclass/h11.csv")
x$h11 <- read_reclass(x$h11,'response',"projects/elise/reclass/h11.csv")

#anthropometry percentiles
x$hw4 <- x$hw4 / 100
x$hw7 <- x$hw7 / 100
x$hw10 <- x$hw10 / 100

# remove bad data

# RH, 2014/4/15
# hw53 can range between 1 and 40. 
# There are 1842 records with a value > 50, 1796 are from IA-2005 (out of 51555)
# I have removed these 
#Iron levels
x$hw53 <- x$hw53[x$hw53 > 40] <- NA

#removes recode variable since it is not needed now
var <- names(x)[!names(x) %in% c('recode')]
x <- x[,var]

cc <- ctryCodeTable()

x <- merge(cc[, c('DHScode', 'ISO3', 'countryname')], x, by='DHScode', all.y=TRUE)

save(x, file='data/processed/KR_prepared.RData')

