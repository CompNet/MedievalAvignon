#############################################################################################
# Functions used to handle dates.
# Note: function taken from our BrefInit project.
# 
# 10/2019 Vincent Labatut
#
# source("res/common/dates.R")
#############################################################################################




#############################################################################################
# Takes a date, adds n months, and returns the resulting date.
# This function was written by StackOverflow user Jacob Amos, and is available there:
# https://stackoverflow.com/a/25025767/1254730
#
# date: input date.
# n: number of months to add.
#
# returns: resulting date.
#############################################################################################
addMonth <- function(date, n=1)
{	if (n == 0){return(date)}
	if (n %% 1 != 0){stop("Input Error: argument 'n' must be an integer.")}
	
	# Check to make sure we have a standard Date format
	if (class(date) == "character"){date = as.Date(date)}
	
	# Turn the year, month, and day into numbers so we can play with them
	y = as.numeric(substr(as.character(date),1,4))
	m = as.numeric(substr(as.character(date),6,7))
	d = as.numeric(substr(as.character(date),9,10))
	
	# Run through the computation
	i = 0
	# Adding months
	if (n > 0){
		while (i < n){
			m = m + 1
			if (m == 13){
				m = 1
				y = y + 1
			}
			i = i + 1
		}
	}
	# Subtracting months
	else if (n < 0){
		while (i > n){
			m = m - 1
			if (m == 0){
				m = 12
				y = y - 1
			}
			i = i - 1
		}
	}
	
	# If past 28th day in base month, make adjustments for February
	if (d > 28 & m == 2){
		# If it's a leap year, return the 29th day
		if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){d = 29}
		# Otherwise, return the 28th day
		else{d = 28}
	}
	# If 31st day in base month but only 30 days in end month, return 30th day
	else if (d == 31){if (m %in% c(1, 3, 5, 7, 8, 10, 12) == FALSE){d = 30}}
	
	# Turn year, month, and day into strings and put them together to make a Date
	y = as.character(y)
	
	# If month is single digit, add a leading 0, otherwise leave it alone
	if (m < 10){m = paste('0', as.character(m), sep = '')}
	else{m = as.character(m)}
	
	# If day is single digit, add a leading 0, otherwise leave it alone
	if (d < 10){d = paste('0', as.character(d), sep = '')}
	else{d = as.character(d)}
	
	# Put them together and convert return the result as a Date
	return(as.Date(paste(y,'-',m,'-',d, sep = '')))
}




#############################################################################################
# Takes a date and returns its year.
#
# date: input date.
#
# returns: year of the date.
#############################################################################################
get.year <- function(date)
{	#year <- as.numeric(substr(as.character(date),1,4))	
	year <- sapply(strsplit(as.character(date), "-"), function(vect) vect[1])
	return(year)
}




#############################################################################################
# Takes a date and returns its month.
#
# date: input date.
#
# returns: month of the date.
#############################################################################################
get.month <- function(date)
{	#month <- as.numeric(substr(as.character(date),6,7))
	month <- sapply(strsplit(as.character(date), "-"), function(vect) vect[2])
	return(month)
}




#############################################################################################
# Takes a date and returns its day.
#
# date: input date.
#
# returns: day of the date.
#############################################################################################
get.day <- function(date)
{	#day <- as.numeric(substr(as.character(date),9,10))	
	day <- sapply(strsplit(as.character(date), "-"), function(vect) vect[3])
	return(day)
}




#############################################################################################
# Takes a date and returns the date of the first day of the same month.
#
# date: input date.
#
# returns: date of the first day of the same month.
#############################################################################################
get.first.day <- function(date)
{	#cat("Date: ",format(date),"\n",sep="")
	
	# break down the date
	y = as.numeric(substr(as.character(date),1,4))
	m = as.numeric(substr(as.character(date),6,7))
	d = as.numeric(substr(as.character(date),9,10))
	
	# set the day to first 
	d <- 1
	
	# put together and return
	result <- as.Date(paste(y,"-",m,"-",d,sep=""))
	return(result)
}




#############################################################################################
# Checks whether the specified periods intersect.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: TRUE iff the periods intersect.
#############################################################################################
date.intersect <- function(start1, end1, start2, end2)
{	#cat(format(start1),"--",format(end1)," vs ",format(start2),"--",format(end2),"\n",sep="")
	
	if(is.na(start1))
		start1 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(start2))
		start2 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end1))
		end1 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end2))
		end2 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	
	result <- start1<=start2 && end1>=start2 || start1>=start2 && start1<=end2
	
#	if(start1<start2)
#	{	if(end1<start2)
#			result <- FALSE
#		else
#			result <- TRUE
#	}
#	else
#	{	if(start1>end2)
#			result <- FALSE
#		else
#			result <- TRUE
#	}
	
	return(result)
}




#############################################################################################
# Returns the duration, in days, of the intersection between the periods, or NA if they do
# not intersect.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: duration of the intersection, or NA if there is none.
#############################################################################################
date.intersect.val <- function(start1, end1, start2, end2)
{	#cat(format(start1),"--",format(end1)," vs ",format(start2),"--",format(end2),"\n",sep="")
	
	if(is.na(start1))
		start1 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(start2))
		start2 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end1))
		end1 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end2))
		end2 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	
	result <- min(end1,end2) - max(start1,start2)
	if(result<0)
		result <- NA
	
	# tests
	#date.intersect.val(as.Date("2000/1/1"),as.Date("2002/1/1"),as.Date("2004/1/1"),as.Date("2006/1/1"))
	#date.intersect.val(as.Date("2000/1/1"),as.Date("2005/1/1"),as.Date("2004/1/1"),as.Date("2006/1/1"))
	#date.intersect.val(as.Date("2000/1/1"),as.Date("2002/1/1"),as.Date("2001/1/1"),as.Date("2006/1/1"))
	#date.intersect.val(as.Date("2000/1/1"),as.Date("2012/1/1"),as.Date("2004/1/1"),as.Date("2006/1/1"))
	
	return(result)
}
