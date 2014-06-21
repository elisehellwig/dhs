#
# 4/27/14: recode_var: feed it a DHS dataframe, the main map file (standard_dhs_map.csv), and the variable name, and it will recode the variables for you
#need to convert encoding to UTF-8 before running

recode_var <- function(dtf, map, varname, major=FALSE){
	require(plyr)
	require(foreach)
	require(doParallel)
	registerDoParallel()
	fn <- function(x, map1=map, varname1=v1, major2=major){
		for (v in varname){

			#gets a subset of the map file
			submap1 <- map1[which(map1$DHScode == x$DHScode[1] & tolower(map1$name) == v & map1$year == as.integer(x$year[1]) & map1$recode == x$recode[1]),]
			#converts the codes to numbers from characters/factors
			submap1$code2 <- as.numeric(as.character(submap1$code))
			#converts the values to characters from factors
			submap1$value2 <- as.character(submap1$value)


			if (nrow(submap1) != 0){

				#strip spaces and make lowercase
				x[,v] <- tolower(as.character(x[,v]))

				x[,v] <- gsub("[[:space:]]+","",x[,v])

				#gets the numbers for the variables
				code <- match(x[,v],submap1$code)
				code <- ifelse(is.na(code), match(x[,v],gsub("[[:space:]]+","",submap1$value)), code)

				#gets the values for the minor codes
				minor <- ifelse(is.na(code), x[,v], submap1$value2[code])


				if (major2){
					#rounnd's numbers to the nearest ten so you only get the major code
					majnum <- round(as.numeric(submap1$code2[code])-4, -1)
					majnum[majnum==100] <-99 #puts back all the 99s in becuase 100 is not a code number


					#gets the values of the major codes
					major <- submap1$value2[match(majnum,submap1$code2)]

					x[,v] <- paste0(major,' ~ ',minor) #puts recodes the data with major and minor codes
				}else{

					x[,v] <- paste0(minor) #recodes the data with only minor codes

				}

			}
		}

		return(x)
	}

	return(ddply(dtf, .(DHScode, recode, year), .fun=fn, .parallel=F))
}

write_reclass <- function(var_, path, varname="variable") {
#var = variable to be reclassified
#path = the file name to be used in reclassification
#var_[is.na(var_)] <- "missing" #converts NAs to missing so they won't be lost in the conversion to factors - this screws up translation later


if (!is.factor(var_)) stop("covert var to factor first")


uni <- unique(var_)
y = as.data.frame(cbind(varnum=as.integer(uni),varname=as.character(uni)))
z <- as.data.frame(table(var_))
out <- merge(y, z, by.x="varname", by.y="var_")
out <- out[,c(2,1,3)]
write.csv(out, path, row.names=F) #writes all of the responses to a csv file
}



#reclassifies responses from a string variable using specifications from a csv file
read_reclass <- function(var_, conv_varname, filename) {
if (!is.factor(var_)) stop("covert var to factor first")
r <- read.csv(filename, sep=",", header=TRUE, stringsAsFactors=FALSE, encoding="UTF8")
#print(var_["varnum"])
#r <- r[order("varnum"),]
#print(r)
return(r[,conv_varname][as.numeric(var_)])
}




