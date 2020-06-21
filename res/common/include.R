#############################################################################################
# Loads all the script of this project, in the appropriate order.
# 
# 06/2020 Vincent Labatut
#
# source("res/common/include.R")
#############################################################################################




#############################################################################################
# handling of warnings

#options(warn=1)			# as they happen
options(warn=2)				# as errors
#options(error=recover)		# debug




#############################################################################################
# packages
library("igraph")			# graphs




#############################################################################################
# source code
source("src/common/constants.R")
source("src/common/colors.R")
source("src/common/dates.R")
source("src/common/logging.R")




#############################################################################################
# global options
