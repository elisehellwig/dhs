setwd('/Users/echellwig/Drive/DHS/')
setwd("d:/gdrive/projects/DHS/")

load('data/aggregated/KR_cluster_num.RData')
load('data/aggregated/KR_cluster_cat.RData')


cln$countryname <- sub("C\xf4te d'Ivoire", 'Cote dIvoire', cln$countryname)
clc$countryname <- sub("C\xf4te d'Ivoire", 'Cote dIvoire', clc$countryname)


#############Numeric Variables

countries <- unique(cln$countryname)

numyr <- sapply(countries, function(x) length(unique(cln[cln$countryname==x, 'year'])))

rows3 <- which(numyr >= 3)
ts3 <- countries[rows3]

rows2 <- which(numyr >= 2)
ts2 <- countries[rows2]