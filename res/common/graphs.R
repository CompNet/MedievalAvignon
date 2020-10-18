###############################################################################
# Various functions related to graphs.
# 
# 06/2020 Vincent Labatut
#
# source("res/common/graphs.R")
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
		if(is.numeric(vals))
			vals[vals==-666] <- NA
		else
			vals[vals=="NA"] <- NA
		g <- set_vertex_attr(graph=g, name=att, value=vals)
	}
	
	# clean edge attributes
	for(att in edge_attr_names(g))
	{	vals <- edge_attr(graph=g, name=att)
		if(is.numeric(vals))
			vals[vals==-666] <- NA
		else
			vals[vals=="NA"] <- NA
		g <- set_edge_attr(graph=g, name=att, value=vals)
	}
	
	return(g)
}




###############################################################################
# Records a graphml file and replaces NA by -1 whenever necessary.
#
# file: the graphml file to record to.
# 
# returns: the recorded graph. 
###############################################################################
write.graphml.file <- function(g, file)
{	# clean vertex attributes
	for(att in vertex_attr_names(g))
	{	vals <- vertex_attr(graph=g, name=att)
		if(is.numeric(vals) && any(is.na(vals)))
		{	vals[is.na(vals)] <- -666
			g <- set_vertex_attr(graph=g, name=att, value=vals)
		}
	}
	
	# clean edge attributes
	for(att in edge_attr_names(g))
	{	vals <- edge_attr(graph=g, name=att)
		if(is.numeric(vals) && any(is.na(vals)))
		{	vals[is.na(vals)] <- -666
			g <- set_edge_attr(graph=g, name=att, value=vals)
		}
	}
	
	write.graph(g, file=file, format="graphml")
}




###############################################################################
# Updates the label node attribute of the graph, in order to restrict values
# only to top nodes according to the specified node attribute. This is convenient
# when there are too many nodes, and displaying all names makes graph plots
# difficult to read.
#
# g: graph to update.
# vals: values used to rank the nodes. 
# best.low: whether the low vals values are better (TRUE) or worse (FALSE). 
#
# returns: updated graph.
###############################################################################
update.node.labels <- function(g, vals, best.low=FALSE)
{	if(best.low)
		vals <- -vals
	if(!hasArg(vals))
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
	if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
		V(g)$label[bottom.idx] <- get.person.names(g, vs=bottom.idx)
	else
		V(g)$label[bottom.idx] <- get.location.names(g, vs=bottom.idx)
	return(g)
}




#############################################################
# Removes the links of the targeted nodes, in order to isolate them.
#
# g: graph to process.
# nodes: nodes to isolate.
#
# returns: the modified graph.
#############################################################
disconnect.nodes <- function(g, nodes)
{	# process each node one by one
	for(n in nodes)
	{	# old version: not good, plot-wise
#		g <- delete_vertices(g,1)
		
		es <- incident(graph=g, v=n, mode="all")
		g <- delete.edges(g,es)
	}
	
	return(g)
}
