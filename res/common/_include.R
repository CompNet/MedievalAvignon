#############################################################################################
# Loads all the scripts of this project, in the appropriate order.
# 
# 06/2020 Vincent Labatut
#
# source("res/common/_include.R")
#############################################################################################




#############################################################################################
# handling of warnings

#options(warn=1)			# as they happen
#options(warn=2)				# as errors
options(error=recover)		# debug




#############################################################################################
# packages
library("igraph")			# graphs
library("SDMTools")			# colors
library("CINNA")			# additional centrality measures
library("future.apply")		# parallel processing
library("sjstats")			# eta correlation ratio
library("GoodmanKruskal")	# Goodman-Kruskal tau
library("DescTools")		# Cramér's V
library("stringr")			# string operations
library("plotfunctions")	# helper plotting functions
library("viridis")			# palette
library("Hmisc")			# fast spearman correlation computation
library("pcaPP")			# fast kendall correlation computation
library("ggplot2")			# additional plot types



#############################################################################################
# source code
source("res/common/constants.R")
source("res/common/colors.R")
source("res/common/dates.R")
source("res/common/logging.R")
source("res/common/plot.R")
source("res/common/graphs.R")




#############################################################################################
# global options
