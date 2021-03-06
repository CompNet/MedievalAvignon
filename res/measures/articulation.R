#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_ARTICULATION <- "articulation"
MEAS_LONG_NAMES[MEAS_ARTICULATION] <- "Articulation point levels"




#############################################################
# Recursively computes articulation points.
#
# An articulation point is a vertex whose removal makes the
# graph disconnected (i.e. it contains several components).
# This function recursively identifies them, i.e. it looks for
# first level articulation points, removes them, then look 
# for articulation points in the resulting components to get
# second level ones, and so on until no more articulation
# point is detected. Edge directions are ignored by igraph here.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.articulation <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# init 
	tlog(2,"Computing articulation points")
	g1 <- g
	level <- 1
	art <- articulation_points(g1)
	art0 <- length(art)
	
	# repeat until no more articulation point
	while(length(art)>0)
	{	tlog(4,"Level ",level)
		# disconnect the articulation nodes
		g1 <- disconnect.nodes(g1, nodes=art)
		# mark them
		vals <- apply(cbind(rep(level,length(art)),V(g1)$Articulation[art]),1,function(v) min(v,na.rm=TRUE))
		V(g1)[art]$Articulation <- vals
		# proceed with the next level
		art <- articulation_points(g1)
		level <- level + 1
	}
	V(g1)$Articulation[is.na(V(g1)$Articulation)] <- level
	vals <- V(g1)$Articulation
	
	# possibly create folder
	articulation.folder <- file.path(out.folder,g$name,MEAS_ARTICULATION)
	dir.create(path=articulation.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot distribution
	custom.hist(vals, name=MEAS_LONG_NAMES[MEAS_ARTICULATION], file=file.path(articulation.folder,paste0(MEAS_ARTICULATION,"_histo")))
	
	# export CSV with articulation
	df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
	colnames(df) <- c("Id","Name",MEAS_ARTICULATION) 
	write.csv(df, file=file.path(articulation.folder,paste0(MEAS_ARTICULATION,"_values.csv")), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	g <- set_vertex_attr(graph=g, name=MEAS_ARTICULATION, value=vals)
	g <- set_graph_attr(graph=g, name=MEAS_ARTICULATION, value=art0)
	stats[MEAS_ARTICULATION, ] <- list(Value=art0, Mean=NA, Stdv=NA)
	
	# plot graph using color for articulation level
	g <- update.node.labels(g, vals, best.low=TRUE)
	custom.gplot(g=g, col.att=MEAS_ARTICULATION, file=file.path(articulation.folder,paste0(MEAS_ARTICULATION,"_graph")))
	#custom.gplot(g=g, col.att="articulation")
	
	# export CSV with number of articulation points
	write.csv(stats, file=stat.file, row.names=TRUE)

	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
