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
source("res/common/constants.R")
source("res/common/colors.R")
source("res/common/dates.R")
source("res/common/logging.R")




#############################################################################################
# global options
