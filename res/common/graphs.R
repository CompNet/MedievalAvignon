###############################################################################
# Loads all the script of this project, in the appropriate order.
# 
# 06/2020 Vincent Labatut
#
# source("res/common/include.R")
###############################################################################




###############################################################################
# Loads a graphml file and replaces "NA" by actual NA whenever necessary.
#
# file: the graphml file to load.
# 
# returns: the loaded graph. 
###############################################################################
load.graphml.file <- function(file)
{	g <- read.graph(file=file, format="graphml")
	
	# clean vertex attributes
	for(att in vertex_attr_names(g))
	{	vals <- vertex_attr(graph=g, name=att)
		vals[vals=="NA"] <- NA
		g <- set_vertex_attr(graph=g, name=att, value=vals)
	}
	
	# clean edge attributes
	for(att in edge_attr_names(g))
	{	vals <- edge_attr(graph=g, name=att)
		vals[vals=="NA"] <- NA
		g <- set_edge_attr(graph=g, name=att, value=vals)
	}
	
	return(g)
}




###############################################################################
# Updates the label node attribute of the graph, in order to restrict values
# only to top nodes according to the specified node attribute. This is convenient
# when there are too many nodes, and displaying all names makes graph plots
# difficult to read.
#
# g: graph to update.
# vals: values used to rank the nodes. 
#
# returns: updated graph.
###############################################################################
update.node.labels <- function(g, vals)
{	if(!hasArg(vals))
		vals <- degree(g)
	
	V(g)$label <- rep(NA, gorder(g))
	vals[is.nan(vals)] <- NA
	vals[is.infinite(vals)] <- NA
	na.nbr <- length(which(is.na(vals)))
	lim <- min(5, length(which(!is.na(vals))))
	if(lim==0)
		bottom.idx <- 1:gorder(g)
	else
		bottom.idx <- order(vals, decreasing=TRUE)[1:lim]
	V(g)$label[bottom.idx] <- vertex_attr(g, ND_NAME_FULL, bottom.idx)
	
	return(g)
}
