#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/measures/_compute_measures.R")
#############################################################################################



#############################################################
# measure names
MEAS_MODE_DIR <- "directed"
MEAS_MODE_UNDIR <- "undirected"
MEAS_MODE_IN <- "in"
MEAS_MODE_OUT <- "out"
# measure long names
MEAS_LONG_NAMES <- c()
MEAS_LONG_NAMES[MEAS_MODE_DIR] <- c("Directed")
MEAS_LONG_NAMES[MEAS_MODE_UNDIR] <- c("Undirected")
MEAS_LONG_NAMES[MEAS_MODE_IN] <- c("Incoming")
MEAS_LONG_NAMES[MEAS_MODE_OUT] <- c("Outgoing")




#############################################################
# load measure computation functions and constants
source("res/measures/articulation.R")
source("res/measures/assortativity.R")
source("res/measures/attributes.R")
source("res/measures/betweenness.R")
source("res/measures/closeness.R")
source("res/measures/components.R")
source("res/measures/comstruct.R")
source("res/measures/connectivity.R")
source("res/measures/degree.R")
source("res/measures/distance.R")
source("res/measures/eccentricity.R")
source("res/measures/eigencentrality.R")
source("res/measures/structsim.R")
source("res/measures/transitivity.R")




#############################################################
# Reads or inititalizes the statistic file, then returns the
# corresponding table.
#
# stat.file: file containing the statistics.
#
# returns: loaded or initialized table.
#############################################################
retrieve.stats <- function(stat.file)
{	if(file.exists(stat.file))
		stats <- read.csv(file=stat.file, header=TRUE, row.names=1)
	else
		stats <- data.frame(Value=as.numeric(),Mean=as.numeric(),Stdv=as.numeric())
	return(stats)
}




#############################################################
# Main method for the graph analysis. Generates a bunch of plots 
# and CSV files to store the results. Also updates the graphml
# file with the results whenever possible, for later external
# use.
#
# gname: name of the graph, used to get the path of its graphml file.
# out.folder: main output folder.
# fast: whether to perform a fast computation (without certain measures).
#
# returns: the graph with updated attributes.
#############################################################
analyze.network <- function(gname, out.folder, fast=FALSE)
{	# load graph
	file.path <- file.path(out.folder, gname, FILE_GRAPH)
	g <- load.graphml.file(file=file.path)
	
	if(gsize(g)>=30)
	{	# compute attribute stats 
		# (must be done first, before other results are added as attributes)
		g <- analyze.net.attributes(g, out.folder, fast)
		
		# compute diameters, eccentricity, radius
		g <- analyze.net.eccentricity(g, out.folder, fast)
		
		# compute degree
		g <- analyze.net.degree(g, out.folder, fast)
		
		# compute eigencentrality
		g <- analyze.net.eigencentrality(g, out.folder, fast)
		
		# compute betweenness
		g <- analyze.net.betweenness(g, out.folder, fast)
		
		# compute closeness
		g <- analyze.net.closeness(g, out.folder, fast)
		
		# compute harmonic closeness
		g <- analyze.net.harmonic.closeness(g, out.folder, fast)
		
		# compute distances
		g <- analyze.net.distance(g, out.folder, fast)
		
		# compute articulation points
		g <- analyze.net.articulation(g, out.folder, fast)
		
		# detect communities
		g <- analyze.net.comstruct(g, out.folder, fast)
		
		# compute transitivity
		g <- analyze.net.transitivity(g, out.folder, fast)
		
		# compute vertex connectivity
		g <- analyze.net.connectivity(g, out.folder, fast)
		
		# compute components
		g <- analyze.net.components(g, out.folder, fast)

		# correlation between component size and attributes
		g <- analyze.net.components.corr(g, out.folder, fast)
		
		# compute assortativity
		g <- analyze.net.assortativity(g, out.folder, fast)
		
		# compute structural similarity
		g <- analyze.net.structsim(g, out.folder, fast)			
	}
	
	return(g)
}
